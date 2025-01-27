
library(tidyverse)
library(maps)
?map_data

#US STATES
us_states = map_data("state")

ggplot(us_states, aes(x=long, y= lat, group = group ))+
  geom_polygon(color = "black", fill="white")

#WORLD MAP
my_world_map = map_data("world")

ggplot(my_world_map, aes(x=long, y= lat, group = group ))+
  geom_polygon(color = "black", fill="white")

#SELECT COUNTRIES BY COUNTRY NAME
pakistan = filter(my_world_map, region == "Pakistan")

ggplot(pakistan, aes(x=long, y= lat, group = group ))+
  geom_polygon(color = "black", fill="white")

#SELECT EXACT REGION BY COUNTRY NAMES - BEST TO USE CONTIGUOUS COUNTRIES
fr_grm = filter(my_world_map, region =="France" | region =="Germany")

ggplot(fr_grm, aes(x=long, y= lat, group = group ))+
  geom_polygon(color = "black", fill="white")

#SELECT GENERAL REGION BY LAT AND LONG
a_region = filter(my_world_map, long >-10 & long <15.1 & lat>32 & lat <55)

ggplot(a_region, aes(x=long, y= lat, group = group ))+
  geom_polygon(color = "black", fill="white")
