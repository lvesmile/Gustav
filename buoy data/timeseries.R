library(geoR)
library(tidyr)
library(ggplot2)

data42003 <- read.csv("buoy data/42003.csv")
data42007 <- read.csv("buoy data/42007.csv")
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
hi <- function(x){
  
  
  x <- x[-1,]
  x$hh <- formatC(as.numeric(x$hh), flag = 0, width = 2)
  x$DD <- formatC(as.numeric(x$DD), flag = 0, width = 2)
  x$MM <- formatC(as.numeric(x$MM), flag = 0, width = 2)
  x <- unite(x, X.YY, MM, DD, col = "Datetime", sep = "-")
  x <- unite(x, hh, mm, col = "Time", sep = ":")
  x$date_time <- paste(x$Datetime, x$Time, sep = " ")
  x$date_time <- as.POSIXct(x$date_time,
                                     format="%Y-%m-%d %H:%M") #format time
  return(x)
}



data42003_clean <- hi(data42003)
data42003_clean <- data42003_clean[c(5500:5658),]#missing 9-11

data42007_clean <- hi(data42007)#8/29 begins
data42007_clean <- data42007_clean[c(4286:4413),]

data42039_clean <- hi(data42039)
data42039_clean <- data42039_clean[c(5675:5913),]

data42040_clean <- hi(data42040)
data42040_clean <- data42040_clean[c(3453:3691),]

data42059_clean <- hi(data42059)
data42059_clean <- data42059_clean[c(5684:5921),]

################################################################################
colors <- c("42003" = "gray", "42007" = "tomato", "42039" = "pink", "42040" = "skyblue", "42059" = "orange")

ggplot()+
  geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42003"))+
  geom_line(data = data42007_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42007")) +
  geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42039")) +
  geom_line(data = data42040_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42040")) +
  geom_line(data = data42059_clean, aes(x=date_time, y=as.numeric(WDIR), color = "42059")) +
  labs(y = "WDIR", x = "time", color = "Legend")+
  scale_color_manual(values = colors)


ggplot()+
  geom_line(data = data42003_clean, aes(x=date_time, y=as.numeric(GST), color = "42003"))+
  geom_line(data = data42007_clean, aes(x=date_time, y=as.numeric(GST), color = "42007")) +
  geom_line(data = data42039_clean, aes(x=date_time, y=as.numeric(GST), color = "42039")) +
  geom_line(data = data42040_clean, aes(x=date_time, y=as.numeric(GST), color = "42040")) +
  geom_line(data = data42059_clean, aes(x=date_time, y=as.numeric(GST), color = "42059")) +
  labs(y = "GST", x = "time", color = "Legend")+
  scale_color_manual(values = colors)

