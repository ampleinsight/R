library(tidyverse) 
library(dplyr)
us_states <- map_data("state")
us_counties <- map_data("county")
head(us_counties)

ca_counties = filter(us_counties, region == "california")

ca_counties_covid = read_csv(url("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv"))

#Lower case county names
ca_counties_covid$county_lower = tolower(ca_counties_covid$county)

#Join the tables
ca_counties_all = left_join(ca_counties_covid,ca_counties, by=c("county_lower"="subregion"))

counties_data = ca_counties_all%>%
  group_by(county)
#%>%
 # slice(which.max(date)) #Was removing all the lat and long coordinates too

ggplot( filter(counties_data, date=="2021-01-10"),
       mapping = aes(x = long, y = lat,
                     group = group, fill = newcountconfirmed))+
  geom_polygon() +
  theme_classic()+
  theme(axis.title = element_blank(), axis.line = element_blank(), axis.text= element_blank(), axis.ticks = element_blank()) +
  labs(fill = "New Cases", title= "California COVID-19 Cases by County", subtitle = "For 2021-01-10", caption = "Source: https://covid19.ca.gov")
