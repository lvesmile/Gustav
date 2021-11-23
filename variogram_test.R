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
library(tmap)
library(mapview)

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


#empirical variogram
coord <- gustav_var[,2:3]
vmax_empir <-variog(coords = coord, data = gustav_var$vmax_gust,estimator.type='modulus')
plot(vmax_empir,main = "empirical variogram of vmax_gust")

precip_empir <- variog(coords = coord, data = gustav_var$precip_max,estimator.type='modulus')
plot(precip_empir, main = "empirical variogram of precip_max")

#directional variogram
#v_d <- variog4(coords = coord, data = gustav_var$vmax_gust, max.dist=1)
#plot(v_d)

epsg_wgs84 <- 4326
gustav_var1<-gustav_var %>% st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(epsg_wgs84)
  


#ggplot
latitude<- c(25.93, 25.92, 30.09, 29.22, 28.50, 28.78, 29.20, 15.28)
longitude<- c(-89.65, -85.60, -88.77, -94.40, -84.50, -86.00, -88.23, -67.47)
buoy_names <- as.character(c(42001, 42003, 42007, 42035, 42036, 42039, 42040, 42059))
buoy<- as.data.frame(cbind(buoy_names, latitude, longitude))
my_sf <- buoy %>% st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(epsg_wgs84)

library(tmap) # thematic map plotting
breaks <- seq(4.5, 8, by = .5)
class(my_sf)
tmap_mode("view")
tmap_precip <- tm_basemap(leaflet::providers$Esri.WorldTopoMap)+
  tm_shape(gustav_var1) +
  tm_bubbles(col = "precip_max", palette = "-RdYlBu", size = .1,alpha = .8)+
  tm_shape(my_sf) +
  tm_bubbles(col = "buoy_names", size = .1)
png_precip <- tmap_leaflet(tmap_precip)
mapshot(png_precip, file = "tmap_precip.png")

tmap_vmax<- tm_basemap(leaflet::providers$Esri.WorldTopoMap)+
  tm_shape(gustav_var1) +
  tm_bubbles(col = "vmax_gust", palette = "-RdYlBu", size = .1)+
  tm_shape(my_sf) +
  tm_bubbles(col = "buoy_names", size = .1)
png_vmax <- tmap_leaflet(tmap_vmax)
mapshot(png_vmax, file = "tmap_vmax.png")

