#####Data Wrangling with the tidyverse

### install the tidyverse if you don't have it installed. You only have to do this once.
#install.packages("tidyverse")

###load the tidyverse functions #### Do this everytime you want to use tidyverse commands
library(tidyverse)
library(dplyr)
####Use read_csv instead of read.csv

#### make sure you have the file in your working directory, or use the complete file path. Use setwd() if you need to.

setwd("/Users/ahmedkhan/Documents")


FAO = read_csv("UNFAO_by_Year.csv")
#Imports as a Tibble

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value")


#Filter to get countries
FAO_Water = filter(FAO, Indicator ==  "Percentage of people using safely managed drinking water services"  & IsCountry == "Y")

#Make a plot for access to clean water by region
#Get values available for each country
FAO_Water_Dat = FAO_Water %>% 
  select(Region, Regions.Subregions.Countries, Year, Value)%>%
  rename(Country = Regions.Subregions.Countries)


#Ungroup then regroup at Region level for totals by region
FAO_Water_Dat_Ungp = FAO_Water_Dat %>% ungroup()
FAO_Water_Dat_Gp_Region = FAO_Water_Dat_Ungp %>% group_by(Region, Year)
FAO_Water_Dat_Sum = summarise(FAO_Water_Dat_Gp_Region, Avg_H20_Access = mean(Value, na.rm = TRUE))

#Recode Region Names for easier reading
FAO_Water_Dat_Sum$Region_Recode <- recode(FAO_Water_Dat_Sum$Region,"Latin America And The Caribbean"="Latin & Caribbean","Northern America And Europe"="N. America & Europe", "Africa" = "Africa", "Asia" = "Asia", "Oceania"="Oceania")


####make a heat map
library(RColorBrewer)
library(gganimate)
library(gifski)
library(transformr)

My_Plot = 
  ggplot(FAO_Water_Dat_Sum, aes(x=Year, y=Region_Recode, fill= Avg_H20_Access)) + 
  geom_tile()+
  coord_flip()+
  theme_classic()+
  scale_fill_distiller('Percent Access', palette = "Set1")+
  # Paired Accent Blues
  theme(plot.title = element_text(size = 14), plot.subtitle = element_text(size = 12), plot.caption = element_text(hjust = -0.3, size = 10), axis.text.x = element_text(angle = 90, hjust = 0, size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_blank())+
  labs(title = "Percent of People by Region with Access to Clean Water", subtitle = "Clean water access has improved, but variation by region persists", caption = "Source: UN Food and Agriculture Organization. Tiles are mean values.")+
  #transition_layers()
  transition_states(Year)


MyAnimation = animate(My_Plot, renderer = gifski_renderer(loop = F)) #Stops animation loop at end

####Export - the animation will save to your working directory
anim_save("Weel11AssignmentKhanGGanimateAnimation2.gif",animation=MyAnimation)


