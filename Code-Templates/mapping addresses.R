library(tidyverse)
library(rlang)
library(mapview)

#Map North Carolina Starbucks

starbucks <- read_csv("https://raw.githubusercontent.com/libjohn/mapping-with-R/master/data/All_Starbucks_Locations_in_the_US_-_Map.csv")

starbucksNC <- starbucks  %>% 
  filter(State == "NC")

starbucksNC %>% 
  glimpse()

mapview(starbucksNC, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE)

#Map 2 NC Starbucks and a random spot in Torrance (looked up in Google maps)

starbucksTest = data_frame(
  Latitude = c(36.29099, 35.97197, 33.811789777595095),
  Longitude = c(-76.25259, -77.81310, -118.35971737960215)
)

starbucksTest = as.tibble(starbucksTest)

mapview(starbucksTest, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE, legend= FALSE)

#Map a few actual spots from survey -- Manually looked up coordinates on Google Maps online

Respondent_Locs = data_frame(
  Name = c("ICNA Relief USA", "Love On Purpose Outreach Ministries Inc. (LOP)", "Place 3", "Place 4", "Place 5", "Place 6"),
  Latitude = c(33.870982275209755, 34.686941, 34.046347324705884,  34.050061236200435, 34.16396151595042, 34.20159407617469),
  Longitude = c(-117.91513913181812, -118.149891, -118.24859784530695, -117.93392981647145, -118.26461863180954, -118.6027414029731)
)

Respondent_Locs = as_tibble(Respondent_Locs)

mapview(Respondent_Locs, xcol = "Longitude", ycol = "Latitude", crs = 4269, grid = FALSE, legend= FALSE, label = "Name")

#Map a few actual spots from survey USING GOOGLE API TO AUTOMATICALLY LOOKUP COORDINATES Alhamdulillah :)!!
#See and run file "mapping addresses using google api.r"

Respondent_Locs_new = origAddress

mapview(Respondent_Locs_new, xcol = "lon", ycol = "lat", crs = 4269, grid = FALSE, legend= FALSE, label = "Name")
