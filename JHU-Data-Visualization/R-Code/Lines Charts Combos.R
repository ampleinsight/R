library(tidyverse)
library(scales)

setwd("/Users/ahmedkhan/Documents")

#NILS = read_csv("WHO_NILS_ALL.csv")
FAO = read_csv("UNFAO_by_Year.csv")
#GFSI = read_csv("GlobalFoodSecurityIndex_2019.csv")
#Imports as a Tibble

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value")

#Drop weird column "X23"
#FAO = select (FAO, -X23)

#Fix year format
#FAO$year = as.numeric(FAO$year)

#head(FAO)
#head(NILS)
#head(GFSI)

#unique(FAO$Indicator)



#Global Level
FAO_Avg_Diet_Adeq = filter(FAO, Indicator =="Average dietary energy supply adequacy" & Regions.Subregions.Countries == "WORLD")

#names(FAO)
fig_data_FAO_Diet_Adeq = tibble("Year"=as.numeric(FAO_Avg_Diet_Adeq$Year), "Diet_Adequacy" = FAO_Avg_Diet_Adeq$Value)

#str(fig_data_FAO_Diet_Adeq)

ggplot(fig_data_FAO_Diet_Adeq, aes(x=Year, y=Diet_Adequacy)) + 
  geom_line(color = "orange", size = 0.5) +
  theme_classic() +
  theme(plot.title = element_text(face="bold"), axis.text.x = element_text(face="bold")) +
  xlim(2000,2020) + #ylim(110,120) +
  #scale_y_continuous(labels = percent_format(accuracy = 1))
  labs(x="Year",y="% of Dietary Need Met",title="World Food Adeqancy Rate Trends (2000-2017)", subtitle = "Percent of Overall Dietary Need Met at the Global Level", caption = "Source: UN Food and Agriculture Organization")



##Regional Level

FAO_Avg_Diet_Adeq_Reg = 
  filter(FAO, Indicator =="Average dietary energy supply adequacy" & 
           (
             Regions.Subregions.Countries == "NORTHERN AMERICA AND EUROPE"|
               Regions.Subregions.Countries == "LATIN AMERICA AND THE CARIBBEAN"|
               Regions.Subregions.Countries == "OCEANIA"|
               Regions.Subregions.Countries == "ASIA"|
               Regions.Subregions.Countries == "AFRICA" |
               Regions.Subregions.Countries == "WORLD"
           )
  )

fig_data_FAO_Diet_Adeq_Reg = tibble("Year"=as.numeric(FAO_Avg_Diet_Adeq_Reg$Year), "Diet_Adequacy" = FAO_Avg_Diet_Adeq_Reg$Value, "Region" = FAO_Avg_Diet_Adeq_Reg$Regions.Subregions.Countries)


#Recode Region Names for easier reading
fig_data_FAO_Diet_Adeq_Reg$Region_Recode <- recode(fig_data_FAO_Diet_Adeq_Reg$Region,"LATIN AMERICA AND THE CARIBBEAN"="Latin/Caribbean","NORTHERN AMERICA AND EUROPE"="N.America/Europe", "AFRICA" = "Africa", "ASIA" = "Asia", "OCEANIA"="Oceania", "WORLD" = "World")

library(RColorBrewer)
mine = ggplot(fig_data_FAO_Diet_Adeq_Reg, aes(x=Year, y=Diet_Adequacy, group = Region_Recode, color = Region_Recode)) + 
  geom_line() + 
  theme_classic() +
  theme(axis.line = element_line(size = 0.3), legend.title = element_blank(), plot.title = element_text(face="bold", size = 12), axis.text = element_text(size = 10), legend.position="bottom")+
  xlim(2000,2020) +
  #scale_y_continuous(labels = percent_format(accuracy = 1)) Just divide Y aesthetic by 100 to use this line
  labs(x="Year",y="% of Dietary Need Met",title="Food Adeqancy Rate Trends by Region (2000-2017)", subtitle = "Percent of Dietary Need Met by Regional Level") +
  scale_color_brewer(palette = "Dark2")

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("Week11_LinearTrends.svg", plot=mine)

