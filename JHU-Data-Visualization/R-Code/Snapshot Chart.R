library(shiny)
library(tidyverse)
library(dplyr)

setwd("/Users/ahmedkhan/Documents")

#Import Data
FAO = read_csv("UNFAO_by_Year.csv")

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value")

#Recode Region Names for easier reading
FAO$Region_Recode <- recode(FAO$Region,"Latin America And The Caribbean"="Latin/Caribbean","Northern America And Europe"="N. America/Europe", "Africa" = "Africa", "Asia" = "Asia", "Oceania"="Oceania")



#Filter to get countries
FAO_Water = filter(FAO, Indicator ==  "Percentage of people using safely managed drinking water services"  & IsCountry == "Y") 

#Get all years for each country
FAO_Water_Dat = FAO_Water %>% 
  #slice(which.max(Year)) %>%
  select(Region, Regions.Subregions.Countries, Year, Value)

#Remove NAs
FAO_Water_Dat <- drop_na(FAO_Water_Dat)

mplot = 
  ggplot(
  filter(FAO_Water_Dat, Year == 2017), aes(x=Value,color=Region,fill=Region)) + 
  geom_density(alpha=.5)+
  #geom_histogram( fill="#69b3a2", color="#e9ecef", alpha=0.5)+
  theme_classic() +
  theme(axis.line = element_blank(), legend.position = "bottom", plot.subtitle = element_text(size = 8)) +
  labs(x = "% with Clean Water Access", y= "Density", title = "Access to Clean Water by Region (2000)" , caption="Source: World Health Organization. Points are country years.")

mplot

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("WaterAccess_2017.svg", plot=mplot)
