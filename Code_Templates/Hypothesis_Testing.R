###############################################################################
#Performing Welch's t-test in R to compare means between two independent groups
#(including: when it is not assumed that the two groups have equal variances)
#Reference Youtube Video: https://www.youtube.com/watch?v=ChmasDBiD1A
###############################################################################

#Assumptions:
#1. Independence of the observations (each observation belongs only to one group)
#2. No significant outliers in the two groups
#3. Normality (each group is roughly normally distributed)

#Hypotheses
#H0 (Null Hypothesis): There is NO significant diff in the means of Groups 1 and 2
#H1 (Alternative Hypothesis): There IS a statistically significant difference in means of Gps 1 and 2
#Formally states:
#Null: In a comparison of crop varieties, the average yield of Group 1 and Group 2 are NOT significantly different.
#Alt: In a comparison of corp varieties, the average yield of Grps 1 and 2 ARE significantly different.


#Create sample data of 1000 observations with random normal distro, split into 2 groups 

group = c("Group 1", "Group 2")
char = sample(group, 1000, replace = TRUE)

random_norm_distr = rnorm(1000,mean = 10, sd = 2) #1000 obs, with mean 10 and st dev 2

WT= data.frame(Var = char, GY=random_norm_distr)
head(WT)
WT = as_tibble(WT)

#Load libraries
library(tidyverse)
library(rstatix)

#Set working directory
setwd('/Users/ahmedkhan/Downloads')

#Summary stats
WT %>%
  group_by(Var) %>%
  get_summary_stats(GY)
  #summarise(average = mean(GY), stdev = sd(GY))

#Visualize
aku = ggplot (WT, aes(x = Var, y = GY, fill = Var, colour = Var)) + 
  geom_boxplot()+
  theme_minimal()+
  labs(title = "Boxplots: Garin Yield by Crop Variety", subtitle = "Histogram, n=1000", caption = "Source: Hypothetical Data, Mar 2025", 
       x="Varieties", y="Grain Yield")
aku

#Computation
stat.test = WT %>%
  t_test(GY ~Var) %>%
  add_significance()
stat.test
  #Result: t statistic is 0.0477 with degrees of freedom 99998 (HIGH!!) and p value 0.962, 
  #which is much higher than 0.05 and therefore not sigifnicant (p.siginif also shows this)
  #therefore we can NOT reject the null hypothesis

#or

t.test(WT$GY~WT$Var)
 #Same result as above
  #95% confidence interval includes 0 -- so it's very likely that there is 0 difference b/w
  #means of the two groups. Therefore we can NOT reject the null hypothesis.
  #The means actually look quite close too: 9.9307 and 9.9301.

#or

#Cohen's d -- Allows for non-equal variances to be considered
WT %>% 
  cohens_d(GY ~Var, var.equal = FALSE)
  #Same result. Negligible difference in means. Therefore cannot reject Null Hypothesis.s

