library(dplyr)
library(hflights)

rm(list = ls())

hflights = tbl_df(hflights)

lut = c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

hflights$UniqueCarrier = lut[hflights$UniqueCarrier]

d1 = hflights %>%
  filter(Distance >= 3000)

d2 = hflights %>%
  filter(UniqueCarrier %in% c('JetBlue', 'Southwest', 'Delta'))

d3 = hflights %>%
  filter(TaxiIn + TaxiOut > AirTime)


  
  
  

