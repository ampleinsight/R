#ADD DATA TO MAPS BY MERGING DATA INTO THE MAP DATAFRAME

library(tidyverse)
library(maps)
?map_data

#MAP DATAFRAME OF US STATES
us_states = map_data("state")

#Make pretend data to add in for 48 contiguous states + DC -- so 49 total

region = unique(us_states$region)

qual_value = c(rep(LETTERS[1:10],4),LETTERS[1:9])

quant_value = runif(49,0,5)

some_data_values = data.frame(region, qual_value, quant_value)

#MERGE (JOIN) THE DATA with MAP DATAFRAME
mapa_data_combined = left_join(us_states, some_data_values, by = "region")

#Make Chloropleths with default R colors

#With qualitative values
ggplot(mapa_data_combined, aes(x=long, y= lat, group = group, fill = qual_value))+
  geom_polygon(color = "black")

#Fancier colors
library(RColorBrewer)
ggplot(mapa_data_combined, aes(x=long, y= lat, group = group, fill = qual_value))+
  geom_polygon(color = "black")+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  scale_fill_brewer(palette = "Spectral")

#Switch out for a quantitative values

ggplot(mapa_data_combined, aes(x=long, y= lat, group = group, fill = quant_value))+
  geom_polygon(color = "black")+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  scale_fill_distiller(palette = "Set1")
