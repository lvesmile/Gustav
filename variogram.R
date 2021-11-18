source("gustav.R")
library(sp)
library(gstat)
library(tmap)

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

v <- variogram(precip_max ~ 1, gustav_var)
#fit model to variogram
v_fit <- fit.variogram(v, vgm("Sph"))
v_f <- spherical_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])
h <- seq(0, 100, length = 1000)
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

