source("gustav.R")
source("variogram.R")
source("buoy data/timeseries.R")
library(ggplot2)
library(maps)
library(sf)
#create a data frame for buoys' longitude and latitude
latitude<- c(25.93, 25.92, 30.09, 29.22, 28.50, 28.78, 29.20, 15.28)
longitude<- c(-89.65, -85.60, -88.77, -94.40, -84.50, -86.00, -88.23, -67.47)
buoy_names <- as.character(c(42001, 42003, 42007, 42035, 42036, 42039, 42040, 42059))
buoy<- as.data.frame(cbind(buoy_names, latitude, longitude))

# change buoy data into coordinates system
my_sf <- st_as_sf(buoy, coords = c('longitude', 'latitude'))

#add buoy data to hurricane rain map
rain_map <- map_counties(storm = "Gustav-2008", metric = "rainfall") +
  ggtitle("rain Gustav 2008")
rain_map+
  ggtitle("Rain Gustav and Buoys") +
  geom_point(data=my_sf,aes(y=latitude, x=longitude, 
                            col=factor(buoy_names), shape=factor(buoy_names)))+
  scale_shape_manual(values=c(9, 12, 13, 14, 15, 16, 17, 18))

# add buoy data to hurricane wind map
wind_map <- map_counties(storm = "Gustav-2008", metric = "wind", wind_var = "vmax_gust")
wind_map +
  ggtitle("Gustav Wind and Buoys") +
  geom_point(data=my_sf,aes(y=latitude, x=longitude, 
                            col=factor(buoy_names), shape=factor(buoy_names)))+
  scale_shape_manual(values=c(9, 12, 13, 14, 15, 16, 17, 18))


#add buoy data to hurricane flood and tracks map  
gustav_map <- map_event_exposure(storm = "Gustav-2008", event_type = "flood")
gus_flood<- map_tracks(storms = "Gustav-2008", plot_object = gustav_map, plot_points = FALSE, 
           color = "darkgray")
gus_flood + 
  ggtitle("Gustav Flood and Buoys") +
  geom_point(data=my_sf,aes(y=latitude, x=longitude, 
                            col=factor(buoy_names), shape=factor(buoy_names)))+
  scale_shape_manual(values=c(9, 12, 13, 14, 15, 16, 17, 18))

