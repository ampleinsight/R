#Manipulate data with DPLYR package
#Follows Cheat Sheet

library(dplyr)
data(mtcars) #Built-in sample dataset

#Explore Data
summarise(mtcars) #Summarizes table overall -- Doesn't quite work??
  summarise(mtcars, dplyr::n()) #Gives # of rows -- Works!
  summarise(mtcars, dplyr::n_distinct(mpg)) #Gives unique counts of       specified variable -- Works!
  summarise(mtcars, sum(!is.na(cyl))) #Gives counts of non-blanks --       Works!
  
  #See backside of DPLYR cheat sheet to see Functions to use with         Summarize

count(mtcars)

mean(mtcars$mpg, na.rm = TRUE) -- Mean after removing blanks

#Pivot Table (Grouping) - Method 1 (Not using a table in Base R but using DPLYR)
mtcars %>% #Dataset
  group_by(cyl) %>% #Pivot on
  summarise(avg = mean(mpg)) #Calculation desired

#Manipulate Rows (Cases)

  #Filter rows of data
  filter(mtcars, mpg>20)
  #Logical and boolean operators to use with filter()
  #See ?base::Logic and ?Comparison for help.
  
  #Remove dup rows
  distinct(mtcars, .keep_all = FALSE)
  
  #Get random fraction or number of observations (w/sampling replacement)
  sample_frac(mtcars, 0.5, replace = TRUE)
  
  sample_n(mtcars, 10, replace = TRUE)
  
  #Select rows by row number
  slice (mtcars, 3:5)
  
  #Arrange (sort) rows by specified variable
  arrange(mtcars, mpg) #Ascending
  arrange(mtcars, desc(mpg)) #Descending
  
  #Add rows
  add_row(mtcars, mpg=30, cyl = 4)

#Manipulate Columns (Variables)
  
  #Filter columns
  pull(mtcars,mpg) #Extracts as vector (no header)
  select (mtcars,mpg) #Extracts as table (w/ header and row label)
    #See DPLYR Cheat Sheet for helpers with select (),
    #e.g. select(iris, contains("mp"))

  #Add/Edit columns
  mutate(mtcars,mpg_adj = mpg/10) #Adds new column
    #See backside of DPLYR cheat sheet to see Vector Functions to use     with Mutate
  transmute(mtcars, mpg_adj = mpg/10) #Returns ONLY new column (drops     others)
 rename(mtcars, Miles_per_Gallon = mpg) #New name goes first here

#Combine Tables :)
 
 #See cheat sheet for placing two tables side by side
 
 #Joins
 inner_join(mtcars, mtcars_sampl, by = c("mpg" = "mpg"), copy =
              FALSE, suffix =c("_T1", "_T2"))
 #Above: Joining by Column Name in Table 1, Column Name in Table 2. If we   change this to by = NULL, looks like R will join on EACH column it       finds in common
 #Can also use left, inner, or full join in function name...just replace  "inner"