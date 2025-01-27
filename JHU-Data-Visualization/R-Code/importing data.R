####Importing Data in R

####Import CSV
cces_sample <- read.csv("/Users/ahmedkhan/Downloads/cces_sample_coursera.csv")

####Write CSV
write.csv(cces_sample,"/Users/ahmedkhan/Downloads/test.csv")

####type in your directory path in setwd() or use the Session-->Set Working Directory menu options
#Working Directory = Your defailt folder on the computer for R to use

getwd()

setwd("/Users/ahmedkhan/Downloads/")
#Can also set using point and click in "Files" menu to the right

#### Don't need the whole file path now
cces_sample <- read.csv("cces_sample_coursera.csv")

class(cces_sample)

median(cces_sample$pew_religimp,na.rm=T)

table(cces_sample$race)


