#####Data Wrangling with the tidyverse

### install the tidyverse if you don't have it installed. You only have to do this once.
#install.packages("tidyverse")

###load the tidyverse functions #### Do this everytime you want to use tidyverse commands
library(tidyverse)
library(dplyr)
####Use read_csv instead of read.csv

#### make sure you have the file in your working directory, or use the complete file path. Use setwd() if you need to.

setwd("/Users/ahmedkhan/Documents")

#FAO = read_csv("UNFAO_by_Year.csv")
NILS = read_csv("WHO_NILS_ALL.csv")
#GFSI = read_csv("GlobalFoodSecurityIndex_2019.csv")
#Imports as a Tibble



#Format the NILS data, select certain variables to plot
NILS_Tibble = as.tibble(NILS)

NILS_Tibble$Indicator =  
  recode(NILS_Tibble$Indicator, 
         "Population below minimum level of dietary energy requirement (undernourishment) (%)" = "% Undernourished",
         "Seats held by women in national parliament (%)" = "% Women in Parliament",
         "Averaged aggregate governance indicators" = "Avg Governance Indicator",
         "Total expenditure on health as % of gross domestic product" = "Health Spend %",
         "Gender Inequality Index (GII)" = "Gender Inequality Index",
         "Global Hunger Index (GHI)" = "Global Hunger Index",
         "GDP per capita (PPP US$)" = "GDP per Capita (US$)"
  )



Undnourish = filter(NILS_Tibble, Indicator =="% Undernourished")
#Has dups and no year info so just get best amount (in this case lowest under-nourished %) and subtract from 100% to get % Nourished 
Undnourish_Max = Undnourish %>% group_by(Country) %>% 
  summarize(nourished = 100-min(Value))

GDP = filter(NILS_Tibble, Indicator =="GDP per Capita (US$)")
#Has dups and no year info so just get best amount (in this case lowest under-nourished %) and subtract from 100% to get % Nourished 
GDP_Max = GDP %>% group_by(Country) %>% 
  summarize(GDP_Per_Cap = max(Value))


####make a map
library(RColorBrewer)
library(maps)
#?map_data

#WORLD MAP
my_world_map = map_data("world")

#SELECT GENERAL REGION BY LAT AND LONG
a_region = filter(my_world_map, long <60 & long >-34 & lat <40 & lat >-50)



#MERGE (JOIN) THE DATA with MAP DATAFRAME
mapa_data_combined2 = left_join(a_region, Undnourish_Max, by = c("region" = "Country"), copy =
                                 FALSE )

mapa_data_combined3 = left_join(mapa_data_combined2, GDP_Max, by = c("region" = "Country"), copy =
                                  FALSE)
mapa_data_combined3$GDP_Per_Cap_B = ifelse(mapa_data_combined3$GDP_Per_Cap >= 50000, "Over $50K", "Under $50K")

#Bring in Long and Lat for Capital Cities Only (to map cleanly)

Cap_cities = filter(world.cities, capital == 1)

mapa_data_combined4 = left_join(mapa_data_combined3, Cap_cities, by = c("region" = "country.etc"), copy =
                                  FALSE)
plot2= 
  ggplot(mapa_data_combined3, aes(x=long, y= lat, group = group, fill = nourished))+
  geom_polygon(color = "black")+
  scale_fill_distiller(palette = "Reds", direction = -1)+
 # geom_point(data = mapa_data_combined4, aes(x=long.y, y= lat.y, group = region, color = GDP_Per_Cap_B))+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

plot2

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("Week9_Map_v2.svg", plot=plot2)


