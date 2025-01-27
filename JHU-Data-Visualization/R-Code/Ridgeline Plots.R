library(tidyverse)
library(dplyr)
####Use read_csv instead of read.csv

#### make sure you have the file in your working directory, or use the complete file path. Use setwd() if you need to.

setwd("/Users/ahmedkhan/Documents")

NILS = read_csv("WHO_NILS_ALL.csv")
FAO = read_csv("UNFAO_by_Year.csv")
GFSI = read_csv("GlobalFoodSecurityIndex_2019.csv")
#Imports as a Tibble

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value")


#Filter to get countries
FAO_Pop = filter(FAO, Indicator ==  "Total population"  & IsCountry == "Y")

#Get latest value available for each country
FAO_Pop_Max = FAO_Pop %>% group_by(Regions.Subregions.Countries) %>%
  slice(which.max(Year)) %>%
  select(Region, Regions.Subregions.Countries, Value)

library(ggridges)

#How about: By economy level -- how has nourishment changed over time?
#Also can show if nourishment levels differs by economy

FAO_Econ = FAO %>% 
  filter(Indicator == "Average dietary energy supply adequacy") %>%
  filter(Subregion == "Low-income economies" | Subregion == "Lower-middle-income economies" | Subregion =="Upper-middle-income economies" | Subregion == "High-income economies")%>%
  select(Subregion, Year, Value)

#Order factor levels manually from Low to High -- so they show that way on the plot
FAO_Econ$factor =  factor(FAO_Econ$Subregion, levels = c("Low-income economies", "Lower-middle-income economies", "Upper-middle-income economies", "High-income economies"))

myplot3=
ggplot(FAO_Econ, aes(x = Value, y = factor, color = factor, fill = factor)) +
  geom_density_ridges ()+
  #xlim(0, 300)+
  theme_ridges()+
  theme(plot.title = element_text(hjust=1, size = 11) ,plot.subtitle = element_text(hjust=1, size = 10), axis.title=element_text(size=10, face = "bold"), axis.text.x = element_text(face="bold"), legend.position = "none", axis.title.y = element_blank(), plot.caption = element_text(size = 10)) +
  scale_fill_brewer(palette="RdYlGn")+
  scale_color_brewer(palette="RdYlGn")+
  labs(x= "% of Minimum Dietary Need Met",
       title="Impact of Economic Activity on Meeting Dietary Needs from 2001 to 2017", subtitle = "Distribution of country scores", caption="Source: UN Food and Agriculture Organization")

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("EconDiet.svg", plot=myplot3)
