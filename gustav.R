library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(tmap)
library(gstat)
library(sp)
library(gstat)
library(geoR)
addRepo("geanders")


data("hurr_tracks")

data("rain")

head(hurr_tracks)

head(rain, 15)

#rainfall map of eastern U.S.
map_counties(storm = "Gustav-2008", metric = "rainfall") +
  ggtitle("Gustav-2008") +
  theme(plot.title = element_text(hjust = 0.5))


#rainfall map over 175mm of exposed counties 
map_rain_exposure(storm ="Gustav-2008", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3) +
  ggtitle("Gustav-2008") +
  theme(plot.title = element_text(hjust = 0.5))

#rainfall map of eastern U.S. with a day before and on the day of landing 
map_counties(storm = "Gustav-2008", metric= "rainfall", days_included = -1:0) +
  ggtitle("Rain Gustav")


#rainfall map of eastern U.S. with days of before and on landing
map_counties(storm = "Gustav-2008", metric = "rainfall", days_included = -5:3) +
  ggtitle("rain Gustav 2008")


#wind map
map_counties(storm = "Gustav-2008", metric = "wind")


#wind map with sustained duration
map_counties("Gustav-2008", metric = "wind", wind_var = "sust_dur")


#counties map with wind and hurricane exist tracks
map_counties("Gustav-2008", metric = "wind", wind_source = "ext_tracks")

#distance map
map_counties(storm = "Gustav-2008", metric = "distance")

#distance exposure map
map_distance_exposure(storm = "Gustav-2008", dist_limit = 75)

library(weathermetrics)

#wind exposure map with speed over 17.5 m/s
map_wind_exposure(storm = "Gustav-2008",
                  wind_limit = convert_wind_speed(34, "knots","mps"))

# flood exposure map 
map_event_exposure(storm = "Gustav-2008", event_type = "flood")

#tornado exposure map
map_event_exposure(storm = "Gustav-2008", event_type = "tornado")

#hurricane track map 
map_tracks(storms = "Gustav-2008")
