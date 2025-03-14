library(ggplot2)
library(tidyverse)
data("mtcars"); head(mtcars)


#Regression
d <- mtcars
fit <- lm(mpg ~ wt, data = d) # fit the model
summary(fit)

plot(fit, which=1, col=c("blue")) # Residuals vs Fitted Plot

d$predicted <- predict(fit)   # Save the predicted values
d$residuals <- residuals(fit) # Save the residual values

res = resid(fit) #get list of residuals 
plot(density(res)) #plot historgram of residuals to check if residuals are more or less normally distributed

ggplot(d, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()
