library(tidyverse)
library(dplyr)
library(plotly)

setwd("/Users/ahmedkhan/Documents")

NILS = read_csv("WHO_NILS_ALL.csv")
#FAO = read_csv("UNFAO_by_Year.csv")
#GFSI = read_csv("GlobalFoodSecurityIndex_2019.csv")

#Long format the FAO data (prep for Tidyverse use)
#FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "values")

#Fix year format
#FAO$year = as.numeric(FAO$year)

#Format the NILS data, select certain variables to plot
NILS_Tibble = as.tibble(NILS)

NILS_Tibble$Indicator =  
  recode(NILS_Tibble$Indicator, 
         "Population below minimum level of dietary energy requirement (undernourishment) (%)" = "% Undernourished",
         "Seats held by women in national parliament (%)" = "% Women in Parliament",
         "Averaged aggregate governance indicators" = "Avg Governance Indicator",
         "Total expenditure on health as % of gross domestic product" = "Health Spend %",
         "Gender Inequality Index (GII)" = "Gender Inequality Index",
         "Global Hunger Index (GHI)" = "Global Hunger Index"
  )

#Exploring variables
#Exploring variables
parl_seats = filter(NILS_Tibble, Indicator =="% Women in Parliament")
#Get latest value available
parl_seats_max = parl_seats %>% group_by(Country) %>%
  #slice(which.max(Year)) %>%
  select(Country, Year, Value)%>%
  rename(Percent_Women = Value)

#Undnourish = filter(NILS_Tibble, Indicator =="% Undernourished")
#Has dups and no year info so just get best amount (in this case lowest under-nourished %) and subtract from 100% to get % Nourished 
#Undnourish_Max = Undnourish %>% group_by(Country) %>%
 # summarize(Year = Year, nourished = 100-Value)%>%
  #select (Country, Year, nourished)

Health_spend = filter(NILS_Tibble, Indicator =="Health Spend %")
#Get latest value available
Health_Max = Health_spend %>% group_by(Country) %>%
  #slice(which.max(Year)) %>%
  select(Country, Year, Value) %>%
  rename(Healthspend = Value)

Governance = filter(NILS_Tibble, Indicator =="Avg Governance Indicator")
#Get latest score available
Governance_Max = Governance %>% group_by(Country) %>%
  #slice(which.max(Year)) %>%
  select(Country, Year, Value) %>%
  rename(Governance = Value)


#Join the data
part_seats_nrsh = 
  parl_seats_max %>%
  left_join(Health_Max, by = c("Country", "Year")) %>%
  left_join(Governance_Max, by = c("Country", "Year"))%>%
  drop_na(Governance)%>%
  drop_na(Healthspend)%>%
  drop_na(Percent_Women)
  

#Country Level using NILS Data

#Plot relationship between % of women in parliament and undernourishment
#No real relationship found b/w % of women in parliament and health spending as % of total budget
#However, clustering of colors (greens tend to stay high spending, reds tend to stay low spending) suggest that better governance is consistently associated with higher spending on health care  :)
library(RColorBrewer)
 
  myplot = ggplot(part_seats_nrsh, aes(x=Percent_Women, y=Healthspend, color = Governance, frame=Year))+
  geom_point(aes(text=Country)) + 
  geom_smooth(se=FALSE, size = 0.75, method = "loess", color ="grey")+
  theme_classic() +
  #ylim(50,110)+
  theme(plot.title = element_text(face="bold", size = 10)) +
  labs(x="% Women in Parliament",y="Health Spending as % of GDP",title="Health Spending of Countries by Percent of Women in Parliament and Governance Score (Higher is better)", subtitle= "Gender mix of legislature is not associated with more health spending, but governance score is", color = "Governance Score")+
  scale_color_distiller(palette = "RdYlGn", direction=1) #Reverses direction of gradient colors :)

#   scale_color_gradient2(low="red", high="green")

fig = ggplotly(myplot) 

fig <- fig %>% 
  animation_opts(
    1000, easing = "linear", redraw = FALSE 
  )

#Animation
fig

#Standalone snapshot plot

myplot2 = ggplot(
  filter(part_seats_nrsh, Year == 2014)
  , aes(x=Percent_Women, y=Healthspend, color = Governance, frame=Year))+
  geom_point(aes(text=Country)) + 
  geom_smooth(se=FALSE, size = 0.75, method = "loess", color ="grey")+
  theme_classic() +
  #ylim(50,110)+
  theme(plot.title = element_text(face="bold", size = 10)) +
  labs(x="% Women in Parliament",y="Health Spending as % of GDP",title="Health Spending of Countries by Percent of Women in Parliament and Governance Score (Higher is better)", subtitle= "Gender mix of legislature is not associated with more health spending, but governance score is", color = "Governance Score")+
  scale_color_distiller(palette = "RdYlGn", direction=1) #Reverses direction of gradient colors :)

myplot2

#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("GenderMix.svg", plot=myplot2)

