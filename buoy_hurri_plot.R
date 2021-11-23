source("buoy data/timeseries.R")
library(hurricaneexposuredata)
library(hurricaneexposure)
data("hurr_tracks")
head(hurr_tracks)

wind_data <- hurr_tracks %>% filter(storm_id == "Gustav-2008")
wind_data <- wind_data[1:43,]
wind_data <- wind_data[-c(18, 26, 34),]

data42001_c <- data42001_clean %>% filter(Time == "00:50" | Time == "06:50" | Time == "12:50" | Time == "18:50") 
data42003_c <- data42003_clean %>% filter(Time == "00:49" | Time == "06:49" | Time == "12:49" | Time == "18:49") 
data42007_c <- data42007_clean %>% filter(Time == "00:50" | Time == "06:50" | Time == "12:50" | Time == "18:50") 
data42035_c <- data42035_clean %>% filter(Time == "00:50" | Time == "06:50" | Time == "12:50" | Time == "18:50") 
data42036_c <- data42036_clean %>% filter(Time == "00:50" | Time == "06:50" | Time == "12:50" | Time == "18:50") 
data42039_c <- data42039_clean %>% filter(Time == "00:50" | Time == "06:50" | Time == "12:50" | Time == "18:50") 
data42040_c <- data42040_clean %>% filter(Time == "00:50" | Time == "06:50" | Time == "12:50" | Time == "18:50") 
data42059_c <- data42059_clean %>% filter(Time == "00:50" | Time == "06:50" | Time == "12:50" | Time == "18:50") 

data42001_c$No <- 1:40
wind_data$No <- 1:40
wind <- inner_join(wind_data, data42001_c, by = "No")
wind <- wind[,c(6,8,9,23)]

colors <- c("42001" = "plum3", "42003" = "gray", "42007" = "tomato", "42035" = "mediumseagreen", 
            "42036" = "blue", "42039" = "yellow", "42040" = "skyblue", "42059" = "orange", "wind" = "black")

buoy_he <- ggplot()+
  geom_point(data = data42001_c, aes(x=date_time, y=as.numeric(GST), color = "42001")) +
  geom_point(data = data42003_c, aes(x=date_time, y=as.numeric(GST), color = "42003")) +
  geom_point(data = data42007_c, aes(x=date_time, y=as.numeric(GST), color = "42007")) +
  geom_point(data = data42035_c, aes(x=date_time, y=as.numeric(GST), color = "42035")) +
  geom_point(data = data42036_c, aes(x=date_time, y=as.numeric(GST), color = "42036")) +
  geom_point(data = data42039_c, aes(x=date_time, y=as.numeric(GST), color = "42039")) +
  geom_point(data = data42040_c, aes(x=date_time, y=as.numeric(GST), color = "42040")) +
  geom_point(data = data42059_c, aes(x=date_time, y=as.numeric(GST), color = "42059")) +
  geom_point(data = wind, aes(x=date_time, y=as.numeric(wind), color = "wind"), shape = 18, size = 3.9) +
  labs(y = "wind", x = "time", color = "Legend")+
  scale_color_manual(values = colors)

#-------------------------------------------------------------------------------

