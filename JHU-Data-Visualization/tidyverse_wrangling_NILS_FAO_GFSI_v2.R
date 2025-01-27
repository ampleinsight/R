#####Data Wrangling with the tidyverse

### install the tidyverse if you don't have it installed. You only have to do this once.
#install.packages("tidyverse")

###load the tidyverse functions #### Do this everytime you want to use tidyverse commands
library(tidyverse)
library(dplyr)
####Use read_csv instead of read.csv

#### make sure you have the file in your working directory, or use the complete file path. Use setwd() if you need to.

setwd("/Users/ahmedkhan/Downloads")

FAO = read_csv("UNFAO_by_Year.csv")
NILS = read_csv("WHO_NILS_ALL.csv")
GFSI = read_csv("GlobalFoodSecurityIndex_2019.csv")
#Imports as a Tibble

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Value")

#Recode Region Names for easier reading
FAO$Region_Recode <- recode(FAO$Region,"Latin America And The Caribbean"="Latin & Caribbean","Northern America And Europe"="N. America & Europe", "Africa" = "Africa", "Asia" = "Asia", "Oceania"="Oceania")


head(FAO)
head(NILS)
head(GFSI)
names(FAO)
class(FAO)
vignette("tibble")

unique(NILS$Indicator)

#Filter to get countries
FAO_Water = filter(FAO, Indicator ==  "Percentage of people using safely managed drinking water services"  & IsCountry == "Y")

#Get latest value available for each country
FAO_Water_Dat = FAO_Water %>% group_by(Regions.Subregions.Countries) %>%
  slice(which.max(Year)) %>%
  select(Region, Regions.Subregions.Countries, Year, Value)

#Remove NAs
FAO_Water_Dat <- drop_na(FAO_Water_Dat)


#Ungroup then regroup at Region level for totals
FAO_Water_Dat_Ungp = FAO_Water_Dat %>% ungroup()
FAO_Water_Dat_Gp_Region = FAO_Water_Dat_Ungp %>% group_by(Region, Year)
FAO_Water_Dat_Sum = summarise(FAO_Water_Dat_Gp_Region, Avg_H20_Access = mean(Value, na.rm = TRUE))

#FAO_Pop_Max_Sum = FAO_Pop_Max_Sum %>% mutate(Total_Pop2 = comma(Total_Pop))





####make a bar plot
#INSTRUCTOR FEEDBACK: MAYBE A LOLLIPOP CHART WOULD BE BETTER? So Week 8 ASSIGNMENT HAS LILLIPOP PLOT INSTEAD FOR THIS SAME DATA :)
library(RColorBrewer)

ggplot(FAO_Water_Dat, aes(x=Value)) + 
  geom_histogram()

ggplot(FAO_Pop_Max,aes(x=Region,y = Value, fill=Region))+
  geom_bar(stat="sum",na.rm=TRUE) +
  theme_classic() +
  theme(legend.position = "null", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.subtitle = element_text(size = 8)) +
  scale_fill_brewer(palette="Set1")+
  labs(title="Chart 1. Population Distribution by World Region (2017)", subtitle = "Population in Millions", caption="Source: UN Food and Agriculture Organization")








####make a map
library(RColorBrewer)
library(maps)
?map_data

#WORLD MAP
my_world_map = map_data("world")



#MERGE (JOIN) THE DATA with MAP DATAFRAME
mapa_data_combined = left_join(my_world_map, FAO_Water_Dat, by = c("region" = "Regions.Subregions.Countries"), copy =
                                 FALSE, )

ggplot(mapa_data_combined, aes(x=long, y= lat, group = group, fill = Value))+
  geom_polygon(color = "black")+
  theme_classic()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())+
  scale_fill_distiller(palette = "Set1")

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("myplot.svg", plot=plot2)







##### if you need to switch back and forth between tibble and dataframe for some reason

cces_dataframe <- as.data.frame(cces)
cces_tibble <- as_tibble(cces_dataframe)

####drop rows with missing data -- Use judgment when using this
cces <- drop_na(cces) #Okay that my data set doesn't use this

##### Use the filter function
####selects only women respondents
women = filter(cces, gender ==2)

####remember the other logical operators

# >
# <
# <=
# >=
# & <- This is the AND operator
# | <- This is the OR operator
# %in% <- This is the CONTAINS operators
dim(NILS) #Dimensions of table (Row and Column Counts)
dim(women)


###when you summarise grouped data, you get summaries by group
grouped_gender <- cces %>% group_by(gender)
summarise(grouped_gender,mean_pid7=mean(pid7, na.rm = TRUE),mean_faminc=mean(faminc_new))






