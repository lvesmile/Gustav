source("gustav.R")
library(sp)
library(gstat)
library(maps)
library(geoR)
library(sf)
library(tidyverse)
library(STAR)
library(magrittr)
library(ggplot2)
library(grid)
library(ggsn)

#rain
#lag: number of days from date when storm was closest to the county
#CHOOSE: 0 indicates the date the storm was closest to the county
gustav_rain <- filter(rain, rain$storm_id == "Gustav-2008" & rain$lag == 0)
#counties' latitude and longitude
data(county_centers)


#wind
data(storm_winds)

#filter needed data for Gustav
gustav_winds <- filter(storm_winds, storm_id=="Gustav-2008")

#combine three datasets into one dataset, including all needed variables
gustav_county <- county_centers %>% inner_join(gustav_rain,by="fips")
gustav <- gustav_county %>% inner_join(gustav_winds, by=c("fips", "storm_id", "usa_atcf_id"))


#variogram
#pick needed variables and make sure data frame only contain numeric data
gustav_var <- gustav[, c(1, 5, 6, 9:13)]
gustav_var$fips = as.numeric(gustav_var$fips)

#counties' fips code
data(county.fips)
class(county.fips)
M=st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2]=colnames(M)[1]
M=left_join(M,county.fips,'ID')
gustav_var <- right_join(M,gustav_var, "fips")

#sf::sf_use_s2(FALSE)
#gustav_var1<- st_join(M,gustav_var,#join = st_within,largest = TRUE)
class(gustav_var)
epsg_wgs84 <- 4326
gustav_var2<-gustav_var %>% st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(epsg_wgs84)%>%
  st_transform(epsg_wgs84)
gustav_var2 <- gustav_var1[, c(1,2, 5:10)]

#make dataset as list for variogram
gustav_var$y = as.numeric(gustav_var$latitude)
gustav_var$x = as.numeric(gustav_var$longitude)
#set up spatial coordinates 
coordinates(gustav_var) = ~x+y
#spjerical variogram function obtained from Luis' discussion notes
spherical_variogram <- function (n, ps, r) function (h) {
  h <- h / r
  n + ps * ifelse(h < 1, 1.5 * h - .5 * h ^ 3, 1)
}

##variogram: precip_max

v <- variogram(precip_max ~ 1, gustav_var2)
#fit model to variogram
v_fit <- fit.variogram(v, vgm("Sph"))
v_f <- spherical_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])
h <- seq(0, 1500, length = 1000)
#plot variogram and fitted line
plot(v$dist, v$gamma,
     xlab = "distance", ylab = "semivariogram")
lines(h, v_f(h))
abline(v = v_fit$range[2], col = "gray")

## automap package helps us to find the best fitted model for the variogram
#install.packages("automap")
#library(automap)
#v_mod_OK <- automap::autofitVariogram(precip_max ~ x+y, gustav_var)$var_model
#plot(automap::autofitVariogram(vmax_gust ~ x+y, gustav_var))

#variogram: vmax_gust

#gaussian model is the best fit for this variable
gaussian_variogram <- function (n, ps, r)
  function (h) n + ps * (1 - exp(-(h / r) ^ 2))
v_f <- gaussian_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])
v <- variogram(vmax_gust ~ 1, gustav_var)
v_fit <- fit.variogram(v, vgm("Gau"))
h <- seq(0, 50, length = 1000)

#plot variogram and fitted line
plot(v$dist, v$gamma,
     xlab = "distance", ylab = "semivariogram")
lines(h, v_f(h))
abline(v = v_fit$range[2], col = "gray")

#v_mod_OK <- automap::autofitVariogram(vmax_gust ~ x+y, gustav_var)$var_model
#plot(automap::autofitVariogram(vmax_gust ~ x+y, gustav_var))

#empirical variogram
coord <- gustav_var[,2:3]
v_v <-variog(coords = coord, data = gustav_var$vmax_gust,estimator.type='modulus')
plot(v_v)
#directional variogram
v_d <- variog4(coords = coord, data = gustav_var$vmax_gust, max.dist=1)
plot(v_d)


# Helper functions
spherical_variogram <- function (n, ps, r) function (h) {
  h <- h / r
  n + ps * ifelse(h < 1, 1.5 * h - .5 * h ^ 3, 1)
}

gaussian_variogram <- function (n, ps, r)
  function (h) n + ps * (1 - exp(-(h / r) ^ 2))

exponential_variogram <- function (n, ps, r)
  function (h) n + ps * (1 - exp(-(h / r)))

# solves `A * x = v` where `C = chol(A)` is the Cholesky factor:
chol_solve <- function (C, v) backsolve(C, backsolve(C, v, transpose = TRUE))
# Euclidean distance:
dist2 <- function (x, y) norm(as.matrix(x - y), type = "2")

kriging_smooth_spherical <- function (formula, data, ...) {
  v <- variogram(formula, data)
  v_fit <- fit.variogram(v, vgm("Sph", ...))
  v_f <- spherical_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])
  
  Sigma <- v_f(as.matrix(dist(coordinates(data)))) # semivariogram
  Sigma <- sum(v_fit$psill) - Sigma # prior variance
  tau2 <- v_fit$psill[1] # residual variance
  C <- chol(tau2 * diag(nrow(data)) + Sigma)
  y <- model.frame(formula, data)[, 1] # response
  x <- model.matrix(formula, data)
  # generalized least squares:
  xt <- backsolve(C, x, transpose = TRUE)
  beta <- coef(lm.fit(xt, backsolve(C, y, transpose = TRUE))) # prior mean
  names(beta) <- colnames(x)
  beta_se <- sqrt(diag(chol2inv(chol(crossprod(xt)))))
  
  chol_sigma <- chol(Sigma)
  Sigma_inv <- chol2inv(chol_sigma)
  C <- chol(Sigma_inv + diag(nrow(data)) / tau2)
  # posterior mean (smoother):
  mu <- drop(chol_solve(C, y / tau2 + Sigma_inv %*% x %*% beta))
  
  sinv_mu <- chol_solve(chol_sigma, mu - x %*% beta)
  krige <- function (new_data) { # prediction function
    D <- apply(coordinates(data), 1,
               function (coord) apply(coordinates(new_data), 1, dist2, coord))
    V <- sum(v_fit$psill) - v_f(D)
    
    t <- delete.response(terms(formula))
    xp <- model.matrix(t, model.frame(t, new_data))
    drop(xp %*% beta + V %*% sinv_mu)
  }
  
  list(smooth = mu, prior_coef = beta, prior_coef_se = beta_se,
       variogram = v_fit, krige = krige)
}


# Example: Meuse river study

library(sp) # spatial point data frames
minority_ks <- kriging_smooth_spherical(precip_max ~ 1, gustav_var1)

#Kriging check (What does this tell us?)
y <- gustav_var$precip_max
op <- par(mfrow = c(1, 2))
plot(minority_ks$smooth, y); abline(0, 1, col = "red")
plot(minority_ks$smooth, type = "l", ylab = "y")
points(y, pch = 19, col = "gray")
abline(h = minority_ks$prior_mean)
par(op)

#Add smoothing and residuals to datasets
joined_nonnulls$MinorPerc_t <- qlogis(joined_nonnulls$MinorPerc)
joined_nonnulls$MinorPerc_smooth <- minority_ks$smooth
joined_nonnulls$MinorPerc_resid <- joined_nonnulls$MinorPerc_t - joined_nonnulls$MinorPerc_smooth 
centroids$MinorPerc_smooth <- minority_ks$smooth
centroids$MinorPerc_resid <- centroids$MinorPerc - centroids$MinorPerc_smooth 

#Plots that show smoothing
gustav_var <- as.data.frame(gustav_var)
precip <- ggplot() + geom_sf(data = gustav_var, aes(fill = precip_max)) + 
  scale_fill_distiller(palette="Reds", trans = "reverse")

ggplot(joined_nonnulls) + geom_sf(aes(fill = MinorPerc_smooth)) + 
  scale_fill_distiller(palette="Reds", trans = "reverse")

latitude<- c(25.93, 25.92, 30.09, 29.22, 28.50, 28.78, 29.20, 15.28)
longitude<- c(-89.65, -85.60, -88.77, -94.40, -84.50, -86.00, -88.23, -67.47)
buoy_names <- as.character(c(42001, 42003, 42007, 42035, 42036, 42039, 42040, 42059))
buoy<- as.data.frame(cbind(buoy_names, latitude, longitude))
my_sf <- buoy %>% st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(epsg_wgs84)
ggplot() + geom_sf(data = gustav_var, aes(fill = precip_max)) + 
  scale_fill_distiller(palette="Reds", trans = "reverse")+
  ggtitle("Rain Gustav and Buoys") +
  geom_point(data=my_sf,aes(y=latitude, x=longitude, col=factor(buoy_names),shape=factor(buoy_names)))
scale_shape_manual(values=c(9, 12, 13, 14, 15, 16, 17, 18))


library(tmap) # thematic map plotting
#tmap_mode("view")
tm_shape(gustav_var2) +
  tm_bubbles(col = "vmax_gust", palette = "-RdYlBu", size = .2, alpha = .5)


tm_shape(gustav_var2) + tm_fill(col = "vmax_gust", palette = "-RdYlBu")
