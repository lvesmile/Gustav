source("gustav.R")
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

source("buoy data/timeseries.R")
library(kableExtra)
library(dplyr)

table_variogram <- gustav %>%
  rename(county = "county_name") %>%
  rename(state = "state_name")

data_1 <- kable(gustav[c(913, 910, 915, 953, 900, 938, 922), c(1:3, 5, 6, 9:11)], 
      caption = "Data frame for variogram in daily precipitation", booktabs = T, row.names = F) %>%
  kable_styling(latex_option = "striped")

data_2 <- kable(gustav[c(913, 910, 915, 953, 900, 938, 922), c(1:3, 5, 6, 9, 12:13)], 
      caption = "Data frame for variogram in gust wind speed", booktabs = T, row.names = F) %>%
  kable_styling(latex_option = "striped")

data_3 <- kable(data42001_clean[c(1:6), c(1:4, 4:6, 10)], caption = "Data frame for buoy 42001", 
      booktabs = T, row.names = F) %>%
  kable_styling(latex_option = "striped")
