#Salam -- Word Cloud :)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tidyverse)

setwd('/Users/ahmedkhan/Downloads')

#If using corpus of text (Speech, paper etc.)
library(tm)

#Grab data

  #Create manually -- simple enough
  #data = "Allah SWT Bismillah Allah SWT Allah SWT Allah SWT Allah SWT Muhammad SAW Muhammad SAW Muhammad SAW Alhamdulillah"
  
  #How to Import text Data
    #1. Save a text as plain text file
    #2. Rename text file as name.r (use the .r extension)
    #3. Open file in R, enclose text in "", set it equal to variable "data"
    # OR
    #4. #Do it programatically. Example: 
        #Import Data
        #responses = read_csv("2023 Year End Tawasaw Community Survey.csv")
        
        #text_positive = responses$`Any positive feedback or testimonial you'd like to share with us? ✅ (Optional)`
 
#Create a vector containing only the text
  text <- data #Cak use text_positive variable here
  # Create a corpus  
  docs <- Corpus(VectorSource(text))

#Clean the data
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))

#Create daraframe of words along with count of each word
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)

#Generate cloud
  set.seed(1234) # for reproducibility 
  
  wordcloud(words = df$word, freq = df$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.1,
            colors=brewer.pal(8, "Dark2"))

#Credit/Instructions hee
  #https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
