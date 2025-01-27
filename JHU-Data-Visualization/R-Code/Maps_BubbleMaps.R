library(tidyverse)
library(maps)
?map_data

#MAP DATAFRAME OF CITIES IN JAPAN
world = map_data("world")
japan = filter(world, region =="Japan")

#Make Bubble chart with default R colors

ggplot(japan, aes(x=long, y= lat, group = group))+
  geom_polygon(color = "black", fill = "white")

#mps package has cities data
head(maps::world.cities)
my_cities = maps::world.cities

japan_cities = filter(my_cities, country.etc == "Japan")

#make a point for every city
ggplot(japan, aes(x=long, y= lat, group = group))+
  geom_polygon(color = "black", fill = "white")+
  geom_point(data = japan_cities, aes(x=long, y=lat, group = NULL, color = "red"))
  #Note the new data table used for Geom Point -- no need to merge into original data here b/c long and lat are same anyway
  #Null group b/c no need for it -- it's already all for one country


#let's pick just big cities

japan_big_cities = filter(japan_cities, pop>= 500000)

ggplot(japan, aes(x=long, y= lat, group = group))+
  geom_polygon(color = "black", fill = "white")+
  geom_point(data = japan_big_cities, aes(x=long, y=lat, group = NULL, color = "red"))

#vary size of point (bubble) by city size

ggplot(japan, aes(x=long, y= lat, group = group))+
  geom_polygon(color = "black", fill = "white")+
  geom_point(data = japan_big_cities, aes(x=long, y=lat, group = NULL, color = "red", size = pop))

#now vary color of bubbles by, say, some qualitative measure

japan_big_cities$qual = sample(LETTERS [1:5], nrow(japan_big_cities), replace = TRUE)

ggplot(japan, aes(x=long, y= lat, group = group))+
  geom_polygon(color = "black", fill = "white")+
  geom_point(data = japan_big_cities, aes(x=long, y=lat, group = NULL, color = qual, size = pop))

#do some tweaking
#no scientific notation for legend using scales package
library(scales)

#Change column names to make Legend nicer
japan_big_cities$Population = japan_big_cities$pop
japan_big_cities$Qualitative = japan_big_cities$qual


ggplot(japan, aes(x=long, y= lat, group = group))+
  geom_polygon(color = "black", fill = "white")+
  geom_point(data = japan_big_cities, aes(x=long, y=lat, group = NULL, color = Qualitative, size = Population))+
  scale_size_continuous(label=comma)+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  labs(title = "Japan: Big Cities by Qualitiative Value")
