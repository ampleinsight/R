#####Data Wrangling with the tidyverse

### install the tidyverse if you don't have it installed. You only have to do this once.
#install.packages("tidyverse")

###load the tidyverse functions #### Do this everytime you want to use tidyverse commands
library(tidyverse)
library(dplyr)
library(scales)
####Use read_csv instead of read.csv

#### make sure you have the file in your working directory, or use the complete file path. Use setwd() if you need to.

setwd("/Users/ahmedkhan/Documents")

CPS = read_csv("CPS_FS_Clean_v2.csv")

#Get rid of nulls
CPS_Data = CPS %>% filter(insecure == 1 | insecure == 0)

#Recode
CPS_Data$Insecure_Recode = recode(CPS_Data$insecure, '1' = "Insecure", '0' = "Secure")

#Order factor levels manually from Low to High -- so they show that way on the plot
CPS_Data$hefaminc_factor =  
  factor(CPS_Data$hefaminc, 
  levels = 
  c("LESS THAN $5,000", "5,000 TO 7,499", "7,500 TO 9,999", 
    "10,000 TO 12,499","12,500 TO 14,999", "15,000 TO 19,999", "20,000 TO 24,999",
    "25,000 TO 29,999", "30,000 TO 34,999", "35,000 TO 39,999", "40,000 TO 49,999",
    "50,000 TO 59,999", "60,000 TO 74,999", "75,000 TO 99,999", "100,000 TO 149,999",
    "150,000 OR MORE"))

bar_plot= 
  ggplot(CPS_Data,aes(x=hefaminc_factor, fill = Insecure_Recode))+
  geom_bar(position="fill")+
  #geom_segment(aes(x=Region, xend = Region, y=0, yend = Total_Pop), color = "gray")+
  #geom_point(size = 3, color = "#69b3a2")+
  #geom_text(aes(label = Total_Pop),nudge_x = -0.2, size = 3)+
  theme_classic() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.title = element_text(size = 12), axis.text = element_text(size = 10), axis.text.x = element_text(angle = 90,  hjust=0.95,vjust=0.2), axis.title.y = element_blank(), plot.subtitle = element_text(size = 8), plot.caption =element_text(vjust=-1))+
  #scale_fill_brewer(palette="BuGn")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#f8766dff", "#00b0f6ff"))+
  labs(x="Income Level",
       title="Proportion of Food Insecurity by Income Level in the U.S. (2019)",caption="Source: US Department of Agriculture")+
  guides(fill=guide_legend(title="")) #Blank legend title....not needed

bar_plot

circular_bar_plot = 
  ggplot(CPS_Data,aes(x=hefaminc_factor, fill = Insecure_Recode))+
  geom_bar(position="fill", width = 0.8)+
  coord_polar(start = 0)+
  #geom_segment(aes(x=Region, xend = Region, y=0, yend = Total_Pop), color = "gray")+
  #geom_point(size = 3, color = "#69b3a2")+
  #geom_text(aes(label = Total_Pop),nudge_x = -0.2, size = 3)+
  theme_classic() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), axis.text.y = element_blank(), axis.text = element_text(size = 10, hjust=0.95,vjust=0.2), axis.title.y = element_blank(), plot.subtitle = element_text(size = 8), plot.caption =element_text(vjust=-1))+
  #scale_fill_brewer(palette="BuGn")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#f8766dff", "#00b0f6ff"))+
  labs(x="Income Level",
       title="Proportion of Food Insecurity by Income Level in the U.S. (2019)",caption="Source: US Department of Agriculture")+
  guides(fill=guide_legend(title="")) #Blank legend title....not needed

circular_bar_plot #May not be as intuitive, actually
  
  
#Export to edit in Inkscape or Adobe Illustrator
library(svglite)
ggsave("bar_plot.svg", plot=bar_plot)
ggsave("circular_bar_plot.svg", plot=circular_bar_plot)



