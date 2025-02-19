#R - Generic Starter
# :)

print('Bismillah. Alhamdulillah. Allah hu Akbar. La ilaha Illallah. Allah SWT. Muhammad SAW is His Rasul.')
library(tidyverse)

#See current library for packages
.libPaths()

#Set working directory
setwd('/Users/ahmedkhan/Downloads')

#Delete packages not needed anymore
#remove.packages("hrbrthemes", lib = "/Library/Frameworks/R.framework/Versions/4.0/Resources/library")

#Import Data
example1 = read_csv("surveyFA2021.csv")

head(example1)
view(example1)

#See summary of all variables - raw data
summary(example1)

#Format data
# If variable is nominal use factor() to attach labels
example1$gender <- factor(example1$gender,
                          levels = c(1,2),
                          labels = c("Male", "Female"))

#Re-code Variables using Case Statement
example1$Over45 = case_when(
  example1$age >45 ~1,
  example1$age <=45 ~0
)

# if a variable is ordinal use ordered() to attach labels
example1$educ <- ordered(example1$educ,
                         levels = c(1,2,3,4,5),
                         labels = c("Some HS", "HS diploma", "Some college/AA", "BA/BS", "Grad Degree"))

#Change data formats for variables that need it
example1$Khan = as.numeric(example1$Khan)


#Summary for each variable after reformatting -- Population
summary(example1$age)
summary(example1$gender)

#Table with proportions
round(prop.table(table(example1$gender)) *100,2)


#SAMPLE n=400 observations without replacement
#first draw a sample of row numbers
set.seed(5)
rows<- sample(1:nrow(example1), 400)

#then create the data set with only those rows included
sample<-example1[rows,]

head(sample)

#Summary for each variable after reformatting -- Sample
summary(sample$age)
summary(sample$gender)
round(prop.table(table(sample$gender)) *100,2)

#save the new sample as a csv
write.csv(sample, "sample_Khan.csv")

#Calculate Statistics

#t.test to test difference in means
males = sample$Khan[sample$gender=="Male"]
females = sample$Khan[sample$gender=="Female"]

t.test(males, females)

#Regression -- for just 1 Indep var, this ends up just being a difference in means too -- same results

#Simple linear
sample$female =
  case_when(
    sample$gender == "Male" ~ 0,
    sample$gender == "Female" ~1)

gender_support_lm <- lm(Khan ~ female, data = sample)

summary(gender_support_lm)

#Multiple linear

gender_support_lm_multiple <- lm(Khan ~ female + white + college + conservative + payattention + doingbetter , data = sample)

summary(gender_support_lm_multiple)

#Univariate and Bivariate Analyses
#Univariate

summary(sample$gender)
round(prop.table(table(sample$gender)) *100,2)

summary(sample$Khan)
sd(sample$Khan, na.rm = TRUE)

###################
#HISTOGRAMS (Categorical Vars) AND BOXPLOTS (Ordinal Vars)
###################

#Convert to tibble
sample_tibble = tibble(sample)

#Plot Histograms for Categorical Vars

#Histogram to see distribution of ratings - POPULATION
example1 %>%
  ggplot(aes(x=Khan)) +
  geom_histogram( binwidth=3, fill="#BCE5E2", color="#69b3a2", alpha=0.9) +
  theme_minimal() +
  theme(plot.title = element_text(size=15), plot.subtitle = element_text(size = 13))+
  labs(title = "Histogram of Support Ratings for Liberty", subtitle = "Population Results, n=1000", caption = "Source: Amazon Turks Survey, Dec 2021",
       x="Support Rating", y = "Count of Observations")

#Barplots
#Raw Counts (frequencies)
ggplot(sample_tibble, aes(x = gender, fill = gender))+
  geom_bar()+
  labs(x = "", y="Count", title = "Sample: Count of Male vs Female")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = c("#90e2f7","#f77676"))+
  theme_minimal()+
  theme(plot.title = element_text(size = 12), legend.title = element_blank(), legend.position = "")

#Percent bar chart (proportions)
ggplot(sample_tibble, aes(x = gender,
                          y = ..count.. /sum(..count..),
                          fill = gender))+
  geom_bar()+
  labs(x = "", y="Percent", title = "Sample: Percent Male vs Female")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#90e2f7","#f77676"))+
  theme_minimal()+
  theme(plot.title = element_text(size = 12), legend.title = element_blank(), legend.position = "")

#Plot Boxplot for Interval Vars
#Boxplots by Gender - POPULATION
example1 %>%
  ggplot( aes(x=gender, y=Khan, color=gender)) +
  geom_boxplot() +
  scale_color_manual(values = c("#10bdec","#f56666"))+
  # scale_fill_manual(values = c("#FFB3B3", "#BEE3ED"))+
  theme_minimal() +
  theme(
    legend.position="right",
    legend.title = element_blank(),
    plot.title = element_text(size=15), plot.subtitle = element_text(size = 13), axis.text.x = element_blank(),
  ) +
  labs(title = "Boxplots - Liberty Support by Gender", subtitle = "Population Results, n=1000", caption = "Source: Amazon Turks Survey, Dec 2021", 
       x="", y="Percent Approval")
