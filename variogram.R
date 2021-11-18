source("gustav.R")

gustav_rain <- filter(rain, rain$storm_id == "Gustav-2008" & rain$lag ==0)

data(county_centers)
data(storm_winds)

gustav_winds <- filter(storm_winds, storm_id=="Gustav-2008")


gustav_county<- county_centers %>% inner_join(gustav_rain,by="fips")
gustav <- gustav_county %>% inner_join(gustav_winds, by=c("fips", "storm_id", "usa_atcf_id"))


#variogram
library(sp)
library(gstat)
library(tmap)
gustav_var <- gustav[, c(1, 5, 6, 9:13)]

#variogram: precip_max
gustav_var$y = as.numeric(gustav_var$latitude)
gustav_var$x = as.numeric(gustav_var$longitude)
coordinates(gustav_var) = ~x+y
spherical_variogram <- function (n, ps, r) function (h) {
  h <- h / r
  n + ps * ifelse(h < 1, 1.5 * h - .5 * h ^ 3, 1)
}
v_f <- spherical_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])
v <- variogram(precip_max ~ 1, gustav_var)
v_fit <- fit.variogram(v, vgm("Sph"))

h <- seq(0, 100, length = 1000)
plot(v$dist, v$gamma,
     xlab = "distance", ylab = "semivariogram")
lines(h, v_f(h))
abline(v = v_fit$range[2], col = "gray")

#install.packages("automap")
#library(automap)
#v_mod_OK <- automap::autofitVariogram(precip_max ~ x+y, gustav_var)$var_model
#plot(automap::autofitVariogram(vmax_gust ~ x+y, gustav_var))

#variogram: vmax_gust
gaussian_variogram <- function (n, ps, r)
  function (h) n + ps * (1 - exp(-(h / r) ^ 2))
v_f <- gaussian_variogram(v_fit$psill[1], v_fit$psill[2], v_fit$range[2])
v <- variogram(vmax_gust ~ 1, gustav_var)
v_fit <- fit.variogram(v, vgm("Gau"))

h <- seq(0, 50, length = 1000)
plot(v$dist, v$gamma,
     xlab = "distance", ylab = "semivariogram")
lines(h, v_f(h))
abline(v = v_fit$range[2], col = "gray")

#v_mod_OK <- automap::autofitVariogram(vmax_gust ~ x+y, gustav_var)$var_model
#plot(automap::autofitVariogram(vmax_gust ~ x+y, gustav_var))
