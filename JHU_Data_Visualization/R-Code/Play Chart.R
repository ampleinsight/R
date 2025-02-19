library(tidyverse)
library(dplyr)

setwd("/Users/ahmedkhan/Downloads")

NILS = read_csv("WHO_NILS_ALL.csv")
FAO = read_csv("UNFAO_by_Year.csv")
GFSI = read_csv("GlobalFoodSecurityIndex_2019.csv")

#Long format the FAO data (prep for Tidyverse use)
FAO = FAO %>% pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "values")

#Fix year format
FAO$year = as.numeric(FAO$year)

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
  slice(which.max(Year)) %>%
  select(Country, Value)

Undnourish = filter(NILS_Tibble, Indicator =="% Undernourished")
#Has dups and no year info so just get best amount (in this case lowest under-nourished %) and subtract from 100% to get % Nourished 
Undnourish_Max = Undnourish %>% group_by(Country) %>%
  summarize(nourished = 100-min(Value))

Health_spend = filter(NILS_Tibble, Indicator =="Health Spend %")
#Get latest value available
Health_Max = Health_spend %>% group_by(Country) %>%
  slice(which.max(Year)) %>%
  select(Country, Value) %>%
  rename(healthspend = Value)

Governance = filter(NILS_Tibble, Indicator =="Avg Governance Indicator")
#Get latest score available
Governance_Max = Governance %>% group_by(Country) %>%
  slice(which.max(Year)) %>%
  select(Country, Value) %>%
  rename(governance = Value)


#Join the data
part_seats_nrsh = 
  parl_seats_max %>%
  left_join(Undnourish_Max, by = "Country") %>%
  left_join(Health_Max, by = "Country") %>%
  left_join(Governance_Max, by = "Country") %>%
  drop_na(nourished)


#Plot relationship between % of women in parliament and undernourishment
#No real relationship found b/w women and nourishment
#However, colors suggest that beter governance = higher nourishment rates :)
ggplot(part_seats_nrsh, aes(x=Value, y=nourished, color = governance)) +
  geom_jitter() + 
  geom_smooth(se=FALSE, method = "loess", size = 0.4, color ="gray")+
  #geom_text(label= part_seats_nrsh$Country)+
  annotate("text",x=22.2,y=99,label="Venezuela: Surprisingly High",color="gray")+
  theme_classic() +
  ylim(50,110)+
  theme(plot.title = element_text(face="bold")) +
  labs(x="% Women in Parliament",y="% Nourished Population ",title="Fig 1. Sufficiently Nourished Population by Country", subtitle = "Data points are countries (using latest available indicators)", caption = "Higher Governance Score is better. Source: World Health Organization NLiS", color = "Governance Score")+
  scale_color_gradient2(low="red", high="green")
#+scale_color_gradientn(colours=topo.colors(3),na.value = "transparent",
#                    n.breaks=3,labels=c("Worst","Neutral","Best"))
