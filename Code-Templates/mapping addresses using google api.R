# Geocoding a csv column of "addresses" in R

#load libraries
library(tidyverse)
library(rlang)
library(mapview)
library(ggmap)

#Register MY personal Google Maps API
register_google(key = "AIzaSyCPntubqhnGEHYwQSGEjz_LQF9IJLE9UsQ")
#Can go to https://console.cloud.google.com/apis/credentials to delete API key, 
#create a new one, use it then delete it :)
#That way no hacker can somehow gain access and use my key iA

#Confirm key loaded
google_key()

#Set working directory
setwd('/Users/ahmedkhan/Downloads')

# Select the file from the file chooser
#fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <- read.csv('Respondent Addresses.csv', stringsAsFactors = FALSE)

# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

#CLEANUP ADDRESSES AND NAMES
library(stringr)

#REMOVE THE "#" CHARACTER OR ELSE WILL ERROR OUT! Just a Google API thing, weird but true
origAddress$FullAddress = str_replace_all(string = origAddress$FullAddress, pattern = "#", replacement = "")

#CHECK IF ADDRESS STARTS WITH A NUMBER, FILTER OUT FOR MAPPING IF NOT
origAddress$CheckAddress1 = str_starts(string = origAddress$FullAddress, pattern = "\\d")
origAddress = origAddress %>% filter(origAddress$CheckAddress1 == "TRUE")

#CLEAN UP NAMES
origAddress$Faith.Based.Organization..FBO..Name = str_to_upper(origAddress$Faith.Based.Organization..FBO..Name)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddress))
{
  # Print("Working...")
  result <- geocode(origAddress$FullAddress[i], output = "latlona", source = "google")
  origAddress$lon[i] <- as.numeric(result[1])
  origAddress$lat[i] <- as.numeric(result[2])
  origAddress$geoAddress[i] <- as.character(result[3])
}

#Map Addresses USING GOOGLE API TO AUTOMATICALLY LOOKUP COORDINATES Alhamdulillah :)!!
mapview(origAddress, xcol = "lon", ycol = "lat", crs = 4269, grid = FALSE, legend= FALSE, label = "Faith.Based.Organization..FBO..Name")

# Write a CSV file containing origAddress to the working directory
#write.csv(origAddress, "geocoded_new.csv", row.names=FALSE)
