#library
library(tidyverse)
library(tidyr)
#prepare for the data
library(drat)
library(hurricaneexposure)
library(hurricaneexposuredata)
addRepo("geanders")
data("hurr_tracks")
head(hurr_tracks)
#read data(modified some code from Professor Wright)
url_half1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename="
url_half2 <- "h2008.txt.gz&dir=data/historical/stdmet/"
buoys <- c('42003','42007','42039','42040','42059','42001','42035','42036')
urls <- str_c(url_half1, buoys, url_half2, sep = "")
N <- length(urls) #N should equal the number of buoys used
#loop
Buoys <- data.frame(Date=as.Date(character()),
                 File=character(),
                 User=character(),
                 stringsAsFactors=FALSE)
for (i in 1:N){
  suppressMessages(  ###  This stops the annoying messages on your screen.
    file <- read_table(urls[i], col_names = TRUE)
  )
    file %<>% mutate(BUOY_ID = buoys[i]) %>% filter(MM=="08" | MM=="09")
    names(file)[1]<-"YEAR"
    Buoys <- rbind(Buoys,file )
}
#period: 8-25 to 9-3
Buoys_1 <- filter(Buoys, MM=="08" & DD>"24")
Buoys_2 <- filter(Buoys, MM=="09" & DD<"04")
Buoys <- rbind(Buoys_1,Buoys_2)
#separate data
str(Buoys)
Buoys$HOUR = as.numeric(Buoys$hh)
Buoys$DAY = as.numeric(Buoys$DD)
Buoys$WSPD1 = as.numeric(Buoys$WSPD)
Buoys$Buoys_4parts <- ifelse(Buoys$HOUR<=6,1,ifelse(Buoys$HOUR>6 & Buoys$HOUR<=12,2,ifelse(Buoys$HOUR>12 & Buoys$HOUR<=18,3,4)))
cutoff <- Buoys %>% filter(HOUR == 0|HOUR == 6|HOUR == 12|HOUR == 18)
cutoff <- cutoff[,c("BUOY_ID","MM","DD","hh", "WSPD1")] %>% mutate(date =paste0("2008",MM,DD,hh,"00"))
#create lat and long
BUOY_ID <- c('42003','42007','42039','42040','42059', '42001','42035','42036')
lat <- c(25.92,30.09,28.78,29.2,15.28, 25.93,29.22,28.5)
long <- c(-85.6,-88.77,-86,-88.23,-67.47, -89.65,-94.4,-84.5)
Buoys_position <- cbind(BUOY_ID,lat,long)
Buoys_position <- as.data.frame(Buoys_position)
#final data
cutoff <- left_join(cutoff,Buoys_position, by = "BUOY_ID" )
Buoys_mean <- cutoff %>% group_by(MM,DD,hh) %>% summarise(WSPD2=mean(WSPD1)) %>% mutate(date =paste0("2008",MM,DD,hh,"00"))
cutoff <- cutoff %>% mutate(long = as.numeric(long), lat = as.numeric(lat)) %>% mutate(long2 = -1*long)
Gustav <- select(filter(as.data.frame(hurr_tracks), storm_id == "Gustav-2008"), -c(usa_atcf_id,storm_id))
Buoys_final1 <- inner_join(cutoff,Gustav,by = "date")
Buoys_final2 <- inner_join(Buoys_mean,Gustav,by ="date")
#plot1
ggplot(data=Buoys_final1) +
  geom_point(mapping=aes(x=date, y = wind), color="black", shape = 16, size = 2) +
  geom_point(mapping= aes(date, y=WSPD1, color=BUOY_ID, show.legend=TRUE,), shape = 16, size = 2) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right") + 
  labs("date","wind",title = "Wind Speeds of Hurricane vs Different Buoys") 
#plot2
ggplot(data=Buoys_final2) + 
  geom_point(mapping=aes(x=date, y=wind), color="black", shape= 16, size = 2) +
  geom_point(mapping=aes(date, y=WSPD2, color=WSPD2, show.legend=TRUE), shape = 16, size = 2) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),legend.position = "right") + 
  labs("date","wind",title="Wind Speeds of Hurricane vs Average Buoys")





