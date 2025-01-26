library(tidyverse)
library(scales)       

#Set working directory
setwd('/Users/ahmedkhan/Downloads')

data = read_csv("Race and Ethnicity.csv")

data = data %>% filter(data$Year == "2020")

Data2 = 
data %>%
  mutate(Race_New = case_when(
    `ID Race` == 0 & `ID Ethnicity` == 1 ~ "White (Hispanic)",
    `ID Race` == 0 & `ID Ethnicity` == 0 ~ "White (Non-Hispanic)",
    `ID Race` == 1 & `ID Ethnicity` ==1 ~ "Black (Hispanic)",
    `ID Race` == 1 & `ID Ethnicity` == 0  ~ "Black (Non-Hispanic)",
    `ID Race` == 2 & `ID Ethnicity` == 1 ~ "Native (Hispanic)",
    `ID Race` == 2 & `ID Ethnicity` == 0 ~ "Native (Non-Hispanic)",
    `ID Race` == 3 & `ID Ethnicity` == 1 ~ "Asian (Hispanic)",
    `ID Race` == 3 & `ID Ethnicity` == 0 ~ "Asian (Non-Hispanic)",
    `ID Race` == 4 & `ID Ethnicity` == 1 ~ "Haw/Pacific (Hispanic)",
    `ID Race` == 4 & `ID Ethnicity` == 0 ~ "Haw/Pacific (Non-Hispanic)",
    `ID Race` == 5 & `ID Ethnicity` == 1 ~ "Other (Hispanic)",
    `ID Race` == 5 & `ID Ethnicity` == 0 ~ "Other (Non-Hispanic)",
    `ID Race` == 6 & `ID Ethnicity` == 1 ~ "Multi (Hispanic)",
    `ID Race` == 6 & `ID Ethnicity` == 0 ~ "Multi (Non-Hispanic)",
    TRUE ~ "Other"
  )
  )

Data2 = 
  Data2 %>%
mutate(Race_New_Updated = paste(Race_New," ",percent(Data2$share, accuracy = 0.2),sep = "\n")
  )
           
library(treemap) 

# Create data
group <- Data2$Race_New_Updated
value = Data2$Population
dataplot = data.frame(group,value)

# treemap
treemap(dataplot,
        index="group",
        vSize="value",
        
        # Main
        title=("2020 Hollister, CA Population Distribution (Source: DataUSA.io)"),
        palette="Dark2")

library(d3Tree)


