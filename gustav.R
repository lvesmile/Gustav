library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)

addRepo("geanders")


data("hurr_tracks")

data("rain")

head(hurr_tracks)

head(rain, 15)


map_counties(storm = "Gustav-2008", metric = "rainfall") +
  ggtitle("Gustav-2008") +
  theme(plot.title = element_text(hjust = 0.5))



map_rain_exposure(storm ="Gustav-2008", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3) +
  ggtitle("Gustav-2008") +
  theme(plot.title = element_text(hjust = 0.5))

map_counties(storm = "Gustav-2008", metric= "rainfall", days_included = -1:0) +
  ggtitle("Rain Gustav")



map_counties(storm = "Gustav-2008", metric = "rainfall", days_included = -5:3) +
  ggtitle("rain Gustav 2008")



map_counties(storm = "Gustav-2008", metric = "wind")


map_counties("Gustav-2008", metric = "wind", wind_var = "sust_dur")



map_counties("Gustav-2008", metric = "wind", wind_source = "ext_tracks")


map_counties(storm = "Gustav-2008", metric = "distance")

map_distance_exposure(storm = "Gustav-2008", dist_limit = 75)
