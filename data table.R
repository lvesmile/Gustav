source("variogram.R")
library(kableExtra)

kable(table1[1:5,], caption = "Buoy Data in Every 6 Minutes", booktabs = T)
table2 <- calculate_stat_per6h() %>%
  rename(WDIR6h = "WDIR_mid_6h") %>% 
  rename(WSPD6h = "WSPD_mid_6h") %>%
  rename(GST6h = "GST_mid_6h") %>% 
  rename(PRES6h = "PRES_mid_6h") %>%
  rename(ATMP6h = "ATMP_mid_6h") %>%
  rename(WTMP6h = "WTMP_mid_6h")
kable(table2[1:5,], caption = "Median Buoy Data in Every 6 Hours", booktabs = T) %>%
  kable_styling(latex_option = "striped")

kable(gustav[c(1:3, 5:11),], caption = "Median Buoy Data in Every 6 Hours", booktabs = T) %>%
  kable_styling(latex_option = "striped")
