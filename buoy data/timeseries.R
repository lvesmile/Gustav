library(geoR)
library(tidyr)
library(ggplot2)

data42001 <- read.csv("buoy data/42001.csv")
data42003 <- read.csv("buoy data/42003.csv")
data42007 <- read.csv("buoy data/42007.csv")
data42035 <- read.csv("buoy data/42035.csv")
data42036 <- read.csv("buoy data/42036.csv")
data42039 <- read.csv("buoy data/42039.csv")
data42040 <- read.csv("buoy data/42040.csv")
data42059 <- read.csv("buoy data/42059.csv")

################################################################################
# data42003 <- read.csv("buoy data/42003.csv")
# 
# data42003 <- data42003[-1,]
# data42003$hh <- formatC(as.numeric(data42003$hh), flag = 0, width = 2)
# data42003$DD <- formatC(as.numeric(data42003$DD), flag = 0, width = 2)
# data42003$MM <- formatC(as.numeric(data42003$MM), flag = 0, width = 2)
# 
# data42003 <- unite(data42003, X.YY, MM, DD, col = "Datetime", sep = "-")
# data42003 <- unite(data42003, hh, mm, col = "Time", sep = ":")
# data42003$date_time <- paste(data42003$Datetime, data42003$Time, sep = " ")
# data42003$date_time <- as.POSIXct(data42003$date_time,
#                                   format="%Y-%m-%d %H:%M") #format time
# 
# # try <- data42003[1:20,]
# # ggplot(data = try)+
# #   geom_line(aes(x=date_time, y=as.numeric(WDIR)))

################################################################################
# data clean
change_time <- function(x){
  
  x <- x[-1,]
  
  #change the format like 01, 07, 25...
  x$hh <- formatC(as.numeric(x$hh), flag = 0, width = 2)
  x$DD <- formatC(as.numeric(x$DD), flag = 0, width = 2)
  x$MM <- formatC(as.numeric(x$MM), flag = 0, width = 2)
  
  # paste together multiple columns into one: year-month-day; hour:minute
  x <- unite(x, X.YY, MM, DD, col = "Datetime", sep = "-")
  x <- unite(x, hh, mm, col = "Time", sep = ":")
  
  #convert to date-time
  x$date_time <- paste(x$Datetime, x$Time, sep = " ")
  x$date_time <- as.POSIXct(x$date_time,format="%Y-%m-%d %H:%M") #format time
  
  return(x)
  
}


#add date-time to prepare for plotting time-series graphs
#select the time between 8/25-9/3 when the Gustav hurricane occured
data42001_clean <- change_time(data42001)
data42001_clean <- data42001_clean[c(5682:5921),]

data42003_clean <- change_time(data42003)
data42003_clean <- data42003_clean[c(5500:5658),]#missing 9-11

data42007_clean <- change_time(data42007)#8/29 begins
data42007_clean <- data42007_clean[c(4286:4413),]

data42035_clean <- change_time(data42035)
data42035_clean <- data42035_clean[c(5684:5923),]

data42036_clean <- change_time(data42036)
data42036_clean <- data42036_clean[c(3956:4194),]

data42039_clean <- change_time(data42039)
data42039_clean <- data42039_clean[c(5675:5913),]

data42040_clean <- change_time(data42040)
data42040_clean <- data42040_clean[c(3453:3691),]

data42059_clean <- change_time(data42059)
data42059_clean <- data42059_clean[c(5684:5921),]

################################################################################
# 
# 
# colors <- c("42003" = "gray", "42007" = "tomato", "42039" = "pink", "42040" = "skyblue", "42059" = "orange")
# 
# ggplot()+
#   geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42003"))+
#   geom_line(data = data42007_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42007")) +
#   geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42039")) +
#   geom_line(data = data42040_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42040")) +
#   geom_line(data = data42059_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42059")) +
#   labs(y = "WDIR", x = "time", color = "Legend")+
#   scale_color_manual(values = colors)
# 
# 
# ggplot()+
#   geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric(GST), color = "42003"))+
#   geom_line(data = data42007_clean, aes(x=date_time, y=as.numeric(GST), color = "42007")) +
#   geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric(GST), color = "42039")) +
#   geom_line(data = data42040_clean, aes(x=date_time, y=as.numeric(GST), color = "42040")) +
#   geom_line(data = data42059_clean, aes(x=date_time, y=as.numeric(GST), color = "42059")) +
#   labs(y = "GST", x = "time", color = "Legend")+
#   scale_color_manual(values = colors)

################################################################################

time_series_buoy <- function(variable, name){
  
  colors <- c("42001" = "plum3", "42003" = "gray", "42007" = "tomato", "42035" = "mediumseagreen", 
              "42036" = "blue", "42039" = "yellow", "42040" = "skyblue", "42059" = "orange")
  
  ggplot()+
    geom_line(data = data42001_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42001"))+
    geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42003"))+
    geom_line(data = data42007_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42007")) +
    geom_line(data = data42035_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42035"))+
    geom_line(data = data42036_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42036"))+
    geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42039")) +
    geom_line(data = data42040_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42040")) +
    geom_line(data = data42059_clean, aes(x=date_time, y=as.numeric({{variable}}), color = "42059")) +
    labs(y = name, x = "time", color = "Legend")+
    scale_color_manual(values = colors)
  
}

time_series_buoy(WDIR, "WDIR")
time_series_buoy(WSPD, "WSPD")
time_series_buoy(GST, "GST")

time_series_buoy(ATMP, "ATMP")
time_series_buoy(WTMP, "WTMP")

#time_series_buoy(DPD, "DPD")
#time_series_buoy(APD, "APD")
#time_series_buoy(MWD, "MWD")


#PRES --------------------------------------------------------------------------

data42059_clean$PRES <- as.numeric(data42059_clean$PRES)
j <- data42059_clean %>% filter(PRES != 9999)

colors <- c("42001" = "plum3", "42003" = "gray", "42007" = "tomato", "42035" = "mediumseagreen", 
            "42036" = "blue", "42039" = "yellow", "42040" = "skyblue", "42059" = "orange")

ggplot()+
  geom_line(data = data42001_clean, aes(x=date_time, y=as.numeric(PRES), color = "42001")) +
  geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric(PRES), color = "42003")) +
  geom_line(data = data42007_clean, aes(x=date_time, y=as.numeric(PRES), color = "42007")) +
  geom_line(data = data42035_clean, aes(x=date_time, y=as.numeric(PRES), color = "42035")) +
  geom_line(data = data42036_clean, aes(x=date_time, y=as.numeric(PRES), color = "42036")) +
  geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric(PRES), color = "42039")) +
  geom_line(data = data42040_clean, aes(x=date_time, y=as.numeric(PRES), color = "42040")) +
  geom_line(data = j, aes(x=date_time, y=as.numeric(PRES), color = "42059")) +
  labs(y = "PRES", x = "time", color = "Legend")+
  scale_color_manual(values = colors)

#DEWP --------------------------------------------------------------------------

data42040_clean$DEWP <- as.numeric(data42040_clean$DEWP)
j1 <- data42040_clean %>% filter(DEWP != 999)

ggplot()+
  geom_line(data = data42001_clean, aes(x=date_time, y=as.numeric(DEWP), color = "42001")) +
  geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric(DEWP), color = "42003")) +
  geom_line(data = data42007_clean, aes(x=date_time, y=as.numeric(DEWP), color = "42007")) +
  geom_line(data = data42036_clean, aes(x=date_time, y=as.numeric(DEWP), color = "42036")) +
  geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric(DEWP), color = "42039")) +
  geom_line(data = j1, aes(x=date_time, y=as.numeric(DEWP), color = "42040")) +
  geom_line(data = data42059_clean, aes(x=date_time, y=as.numeric(DEWP), color = "42059")) +
  labs(y = "DEWP", x = "time", color = "Legend")+
  scale_color_manual(values = colors)

#WVHT --------------------------------------------------------------------------

data42040_clean$WVHT <- as.numeric(data42040_clean$WVHT)
data42007_clean$WVHT <- as.numeric(data42007_clean$WVHT)
data42036_clean$WVHT <- as.numeric(data42036_clean$WVHT)
j2 <- data42007_clean %>% filter(WVHT != 99)
j3 <- data42040_clean %>% filter(WVHT != 99)
j4 <- data42036_clean %>% filter(WVHT != 99)

ggplot()+
  geom_line(data = data42001_clean, aes(x=date_time, y=as.numeric(WVHT), color = "42001")) +
  geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric(WVHT), color = "42003"))+
  geom_line(data = j2, aes(x=date_time, y=as.numeric(WVHT), color = "42007")) +
  geom_line(data = data42035_clean, aes(x=date_time, y=as.numeric(WVHT), color = "42035")) +
  geom_line(data = j4, aes(x=date_time, y=as.numeric(WVHT), color = "42036")) +
  geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric(WVHT), color = "42039")) +
  geom_line(data = j3, aes(x=date_time, y=as.numeric(WVHT), color = "42040")) +
  geom_line(data = data42059_clean, aes(x=date_time, y=as.numeric(WVHT), color = "42059")) +
  labs(y = "WVHT", x = "time", color = "Legend")+
  scale_color_manual(values = colors)



