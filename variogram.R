source("gustav.R")
library(sp)
library(gstat)
library(tmap)
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

gus_grid <- makegrid(gustav_var, cellsize = 0.005)
gus_grid$x1<- as.numeric(gus_grid$x1)
gus_grid$x2 <- as.numeric(gus_grid$x2)
gridded(gus_grid)= ~ x1 + x2
d <- krige(precip_max ~1, gustav_var, gus_grid, model= v_fit)
spplot(d["var1.pred"], main="Kriging Prediction on Max Percipitation of Gustav")


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


library(raster)
library(lattice)
#map('county',fill = TRUE, col = palette())
proj4string(gustav_var) <- CRS("+init=epsg:28992")
l3 = list("sp.polygons", gustav_var, lwd=.3, first=FALSE)
grd= spsample(gustav_var, n= 5000, type="regular")
#gus_grid <-points2grid(gustav_var, tolerance=0.76, round=1)

grd=as(grd, "SpatialPixels")
gus_kri <- krige(precip_max~1, gustav_var, newdata=grd, model=v_fit)
spplot(gus_kri, "var1.pred", col.regions= rev(topo.colors(20)), 
       main="Kriging Prediction on Max Percipitation of Gustav", sp.layout=list(l3))


library(sp)
library(sf)
epsg_wgs84 <- 4326
gustav_var %>% st_as_sf(coords = c("latitude", "longitude"))%>% st_set_crs(epsg_wgs84)
grid <- makegrid(gustav_var, cellsize = 0.005)
gridded(grid)= ~x+y
d <- krige(precip_max ~1, gustav_var, grid, model= v_fit)
spplot(d["var1.pred"], main="Kriging Prediction on Max Percipitation of Gustav")

gus_grid <- SpatialPoints(grid, proj4string = CRS(proj4string(gustav_var)))
l4 = list("sp.polygons", gus_grid, lwd=.3, first=FALSE)
