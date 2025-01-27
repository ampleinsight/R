library(tidyverse)

years <- seq(from=2001,to=2020,by=1)

price <- rnorm(20,mean=15,sd=5)

fig_data <- tibble("year"=years,"stock_price"=price)

ggplot(fig_data,(aes(x=years,y=price)))+
  geom_line()


####make data for the first of two stocks

fig_data$stock_id=rep("Stock_1",20)
#rep means replicate x number of times in a column -- so this just creates a new column with "Stock 1" replicated 20 times

stock_1_time_series <- fig_data

#####create data for the second company

stock_id <- rep("Stock_2",20)

years <- seq(from=2001,to=2020,by=1)

price <- rnorm(20,mean=10,sd=3)

stock_2_time_series <- tibble("stock_id"=stock_id,"year"=years,"stock_price"=price)

####combine with bind_rows()

all_stocks_time_series <- bind_rows(stock_1_time_series,stock_2_time_series)
  
  #This does the same thing as above
  all_stocks_time_series = rbind(stock_1_time_series,stock_2_time_series)

View(all_stocks_time_series)

ggplot(all_stocks_time_series,(aes(x=year,y=stock_price,group=stock_id)))+
  geom_line()

ggplot(all_stocks_time_series,(aes(x=year,y=stock_price,group=stock_id,linetype=stock_id,color=stock_id)))+
  geom_line()+
  facet_wrap(~stock_id)


#####Practice with another data set
#Helps us see if Dems and Repubs have gotten more polarized in ideology over time

cel <- read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1"))

cel$Party <- recode(cel$dem,`1`="Democrat",`0`="Republican")

#dwnom1 is a measure of ideology in Poli Sci based on voting records (lower scores= more liberal, higher = more conservative)
cong_data <- cel %>% 
  group_by(Party,year) %>% 
  summarize("Ideology"=mean(dwnom1,na.rm=T))
  
View(cong_data)

ggplot(cong_data,(aes(x=year,y=Ideology,group=Party,color=Party)))+
  geom_line()+
  scale_color_manual(values=c("blue","red"))
  


# Ahmed: Build my own using FAO data 
ggplot(fig_data_FAO_Diet_Adeq, aes(x=Year, y=Diet_Adequacy)) + 
  geom_line(color = "orange", size = 0.5) + theme_classic() +
  theme(plot.title = element_text(face="bold", color = "orange"), axis.text.x = element_text(face="bold"), axis.text.y = element_text(color = "orange")) +
#   + 
 #  +
  xlim(2000,2020) + ylim(110,120) +
  labs(x="Year",y="% of Dietary Need Met",title="World Food Adeqancy Rate Trends (2000-2019)", subtitle = "Percent of Overall Diteray Need Met at the Global Level", caption = "Source: UN Food and Agriculture Organization")

##By Region

FAO_Avg_Diet_Adeq_Reg = 
  filter(FAO, Indicator =="Average dietary energy supply adequacy" & 
          (
            Regions.Subregions.Countries == "NORTHERN AMERICA AND EUROPE"|
            Regions.Subregions.Countries == "LATIN AMERICA AND THE CARIBBEAN"|
            Regions.Subregions.Countries == "OCEANIA"|
            Regions.Subregions.Countries == "ASIA"|
            Regions.Subregions.Countries == "AFRICA"
          )
         )

fig_data_FAO_Diet_Adeq_Reg = tibble("Year"=as.numeric(FAO_Avg_Diet_Adeq_Reg$year), "Diet_Adequacy" = FAO_Avg_Diet_Adeq_Reg$values, "Region" = FAO_Avg_Diet_Adeq_Reg$Regions.Subregions.Countries)

ggplot(fig_data_FAO_Diet_Adeq_Reg, aes(x=Year, y=Diet_Adequacy, group = Region, color = Region)) + 
  geom_line() + theme_classic() +
  theme(plot.title = element_text(face="bold", color = "orange"), axis.text.x = element_text(face="bold"), legend.position="bottom")+
  xlim(2000,2020) +
  labs(x="Year",y="% of Dietary Need Met",title="Fig 2. Food Adeqancy Rate Trends by Region (2000-2019)", subtitle = "Percent of Overall Dietary Need Met at the Regional Level", caption = "Source: UN Food and Agriculture Organization") +
  scale_color_discrete(labels = function(x) str_wrap(x, width = 8))
  
