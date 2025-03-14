#Load packages + load/view the data
library(tidyverse)
library(gapminder)
gapminder

#Create ggplots
p = ggplot(data = gapminder, mapping = aes(x= gdpPercap, y = lifeExp))

p+geom_point() #Scatter
p+geom_smooth() #Best fit line with standard errors
p+geom_point() + geom_smooth() #Both together

#Note how R tells us that smooth was plotted using method "gam". This suggests we can use other types of fits too. Let's see ?geom_smooth for options
?geom_smooth
p+geom_point() + geom_smooth(method = "lm") #With straight line fit
p+geom_point() + geom_smooth(method = "lm") + scale_x_log10() #Change the x axis scale to Log10 basis
p+geom_point() + geom_smooth() + scale_x_log10(labels = scales::dollar) #Format x axis to Dollars by calling a package called "scales" (has various formats and scales) and applying the "dollar" format from within that package

#Get fancy :)

#With straight line fit
p+geom_point() + geom_smooth(method = "lm")

#Change the x axis scale to Log10 basis
p+geom_point() + geom_smooth(method = "lm") + scale_x_log10()

#Format x axis to Dollars by calling a package called "scales" (has various formats and scales) and applying the "dollar" format from within that package
p+geom_point() + geom_smooth() + scale_x_log10(labels = scales::dollar) 

#Make the dots colorful (not the same as color coding by a variable -- to do that use color = option in the aesthetic mapping)
p+geom_point(color = "purple") + geom_smooth() + scale_x_log10(labels = scales::dollar) 

#Get more fancy :)

#Transparency of dots = 30%... makes it easier to see overlapping data (where bulk of it is)
#Fit line: color the fit line, get rid of Standard Errors (SE) shade, line thickness = 1
p+geom_point(color = "purple", alpha = 0.3) + geom_smooth(color = "orange", se=FALSE, size = 1) + scale_x_log10(labels = scales::dollar) 

#Add labels!
p+geom_point(color = "purple", alpha = 0.3) + geom_smooth(color = "orange", se=FALSE, size = 1) + scale_x_log10(labels = scales::dollar) + labs(x="GDP per Capita", y= "Life Expectancy (Years)", title = "Life Expectancy by Economic Growth", subtitle = "Data point are country-years", caption = "Source: Gapminder")

#Color code by variables!! :)

#Color by continent (break out by continent)
# Build into aesthetic mapping first
p = ggplot(data = gapminder, mapping = aes(x=gdpPercap, y= lifeExp, color = continent))
#Plot using Loess regression
p+geom_point() + geom_smooth(method = "loess") + scale_x_log10()


#Fill Standard Errors using continent color too
#Built into aesthetic mapping first
p = ggplot(data = gapminder, mapping = aes(x=gdpPercap, y= lifeExp, color = continent, fill = continent))
#Plot using Loess regression
p+geom_point() + geom_smooth(method = "loess") + scale_x_log10()

#One overall fit line but still different colors by continent
#Edit the ggplot overall aes mappings first
p = ggplot(data = gapminder, mapping = aes(x=gdpPercap, y= lifeExp))
#Build mappings in the gemo, not the ggplot, to control at a per Geom level. Then plot.
p+geom_point(mapping = aes(color = continent)) + geom_smooth(method = "loess") + scale_x_log10()

#Color using continuous variable -- gives a gradient look :). Using log or population (continuous variable) here.
p + geom_point(mapping = aes(color = log(pop))) + geom_smooth(method = "loess") + scale_x_log10(labels = scales::dollar) + labs(x = "GDP Per Capita", y= "Life Expectancy", title = "Economic Growth and Life Expectancy", subtitle = "Dots are country-years", caption = "Source: Gapminder")

