library(dplyr)
library(ggmap)
library(ggplot2)
library(mapdata)
library(maps)

rm(list = ls())

m = map_data('worldHires')

countries = m %>%
  distinct(region)

korea = m %>%
  filter(region %in% c('South Korea', 'North Korea'))


# -------------------- controls------------------------------
RATIO = 1.15
N_POINTS = 10
LONG_POINTS = min(korea$long) + (max(korea$long) - min(korea$long)) *runif(N_POINTS)
LAT_POINTS = min(korea$lat) + (max(korea$lat) - min(korea$lat)) *runif(N_POINTS)
# -------------------- controls------------------------------


gg = ggplot() + geom_polygon(data = korea, aes(x=long, y = lat, group = group), fill = 'white', color = 'black') + 
  coord_fixed(RATIO)
 
points =  data.frame(
  long = LONG_POINTS,
  lat = LAT_POINTS
) 

gg + geom_point(data = points, aes(x = long, y = lat), color = "black", size = 1)


cheese = function(x) {
  if (x > 0) {
    print('cheese')
  } else {
    print('beans')
  }
}
  
  
  