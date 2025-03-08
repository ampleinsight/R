#The following are mostly Parametric dataset tests. This means data are structured and reliable for stat analysis.
#Non parametric are unstructured data and require big data analysis like clustering.

#Initial view of data
  head(iris)
  
  iris = iris #Just so I can see it in my environment
  
  median(iris$Petal.Length)

#Correlation: Are petal length and sepal length correlated?
  
  ?cor
  cor(iris$Sepal.Length, iris$Petal.Length, method = "pearson")
  #Strong correlation: 87%!
  
  #Plot the correlation
  plot(iris$Sepal.Length, iris$Petal.Length)
  #Looks pretty correlated

  
#Regression model: Helps explain causal relations and predict outcomes
  #y = mx + b
  #lm(x ~ y, data = datataset)
  
  model = lm(iris$Petal.Length ~ iris$Sepal.Length, data = iris)
  summary(model)
  # Regression equation: Sepal_Length = 1.858xPetal_Length - 7.101
  
  #Plot data with regression line
  abline(model)

  #R2 is somewhat low. So there are likely other factors that affect the outcome.
  #P value is quite small. So results can be generalized to the population. 
  
#Comparison of means b/w two samples: t test
  #t tests used when two samples normally distributed
  #t.test(data1,data2)
  
  #Compare petal lengths of two different species. Stat sig difference?
  data1 = subset(iris$Petal.Length, iris$Species == "setosa")
  data2 = subset(iris$Petal.Length, iris$Species == "virginica")

  t.test(data1,data2)  

  #Null Hypo: No difference in means. t stat is VERY strongly small and EXTREMELY unlikely for Null Hyp to be teur.
  #Reject Null Hypo. Data suggest that, on averegae, there IS a stat sig diff in petal lengths
  #b/w these two species. Means looks quite diff too so this makes sense.
  
#Comparison: ANOVA
  #Analysis of variance
  #Used to tell if groups are diff from each other. ANOVA can handle more than 2 datasets, unlike t test.
  #aov(data1, data 2~data = dataset)
  
  aov.model = aov(Petal.Length ~ Species, data = iris)
  summary(aov.model)
  #P value is very small (and sum sqs is high?) -- so we're quite confident that the 2 datasets above are very diff 
  #from each other
  
#Data Visualization
  #Show rather than tell
  library(pheatmap)
    #allows clustering and plotting at the same time
  
  pheatmap(iris[,1:4],
           cluster_rows= TRUE,
           cluster_cols = TRUE,
           clustering_method = "complete"
                )
  #Sepal Width and Petal Length closest to one another in their behavior patterns (clusters),
  #while Sepal Length behaves very differently from them.
  
  #PCA with FactoMineR
  #Principal Component Analysis: Helps see multi-dimentional data by reducing
  #to the most important dimentions)
  
  library(FactoMineR)
  library(factoextra)
  
  iris.pca = PCA(iris[,1:4],scale.unit = TRUE, ncp = 5)
  #Sepal Width is very far from other variables
  
  fviz_pca_ind(iris.pca, col.ind = iris$Species)
  #Two of the species are close to each other in how they cluster, while the third behaves
  #very diferently
  
