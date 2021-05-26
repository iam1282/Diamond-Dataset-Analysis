
# ggplot2 is a R package dedicated to data visualization.
library(ggplot2)

# Loading the Diamond Data
data(diamonds)

# Diamond Dataset
View (diamonds)

#Summerize the dataset
summary(diamonds)

# Retrieve the size of all dimensions from a data frame
##We can answer how many observations (53,940), how many variables? (only 10)
dim(diamonds)


#Let's test the power of GGplot2 with a simple histogram of diamond prices.
#In ggplot2 , aesthetic means "something you can see"
#$ , lets you access variables within a data set

ggplot(data=diamonds) + geom_histogram(binwidth=500, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$") + ylab("Frequency") + theme_minimal()
#This is a long tail distribution, with a high concentration of observations below the U$5,000 mark.



#We can get mean and median by running simple R commands:

mean(diamonds$price) 

median(diamonds$price)


#Supposed we want to know the following:


#How many cost less than U$500?
sum(diamonds$price < 500)

#How many cost less than U$250?
sum(diamonds$price < 250)

#How many cost equal to U$15,000 or more?
sum(diamonds$price >= 15000)


#The binwidth is definitively not the best, as we left it at 500 and put a limit of value to less than U$2,500.
#Maybe we should lower the binwidth to see how that changes the picture:
ggplot(data=diamonds) + geom_histogram(binwidth=500, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$ - Binwidth 500") + ylab("Frequency") + theme_minimal() + xlim(0,2500)


#Did you see what happened? By changing geom_histogram(binwidth=100 the frequency dropped from 10,000 to 2,000 in diamonds between U$500 and U$1,000.
#This is a real change in the graph but impossible to see unless you change the bins manually.
ggplot(data=diamonds) + geom_histogram(binwidth=100, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$- Binwidth 100") + ylab("Frequency") + theme_minimal() + xlim(0,2500)


#Let's kick it up one notch with bins of 50:
ggplot(data=diamonds) + geom_histogram(binwidth=50, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution") + xlab("Diamond Price U$ - Binwidth 50") + ylab("Frequency") + theme_minimal() + xlim(0,2500)

# Conclusion: Bin selection will play a significant role in visualizations, with a possible change in frequency readouts and shape of the curve or function.


# facet_wrap() makes a long ribbon of panels and wraps it into 2d.
ggplot(data=diamonds) + geom_histogram(binwidth=100, aes(x=diamonds$price)) + ggtitle("Diamond Price Distribution by Cut") + xlab("Diamond Price U$") + ylab("Frequency") + theme_minimal() + facet_wrap(~cut)

#What if we want to see the cut for the highest priced diamond?
subset(diamonds, price == max(price))
#So answer is Premium cut for a diamond of 2.29 carat that sold at U$18,823! 


#Getting the cut of the lowest priced diamond is a similar task.
subset(diamonds, price == min(price))
# Looks like we have a tie between to units, both sold at U$326, one of 0.23 carat and Ideal cut, and another of 0.21 carats and Premium cut.


#The last question is which cut has the lowest median price.
#easy way is to use the which command to subset data vectors and then get the median of those
a = diamonds[which(diamonds$cut == "Fair"),]
b = diamonds[which(diamonds$cut == "Good"),]
c = diamonds[which(diamonds$cut == "Very Good"),]
d = diamonds[which(diamonds$cut == "Premium"),]
e = diamonds[which(diamonds$cut == "Ideal"),]
median(a$price)

median(b$price)

median(c$price)

median(d$price)

median(e$price)


#Let's get different frequency scales (the y axis) to accomodate for specific patterns.

ggplot(diamonds, aes(factor(cut), price, fill=cut)) + geom_boxplot() + ggtitle("Diamond Price according Cut") + xlab("Type of Cut") + ylab("Diamond Price U$") + coord_cartesian(ylim=c(0,7500))
#You can now see how different graphs have different Y scales.
#For example Fair cut diamonds have a Y scale maximizing at 600, while Ideal diamonds have a Y scale topping at 2,500.
#This is just the effect of using scale="free_y" in the facet_wrap layer.



#we will investigate carat weight using a frequency polygon chart. 
ggplot(data=diamonds, aes(x=carat)) + geom_freqpoly() + ggtitle("Diamond Frequency by Carat") + xlab("Carat Size") + ylab("Count")



ggplot(diamonds, aes(diamonds$color)) + geom_bar(fill = "#0073C2FF")

hist(diamonds$depth, ylab = 'Frequency', xlab = 'Diamonds Depth')

plot(diamonds$depth)

ggplot(diamonds, aes(diamonds$cut)) + geom_bar(fill='orange')

ggplot(diamonds) + geom_boxplot(aes(x = cut, y = depth, color=diamonds$clarity))

ggplot(data = diamonds) + geom_point(mapping = aes(x = price, y = depth, color = cut, shape= cut))

ggplot(diamonds) + geom_violin(aes(x = cut, y=price, color= color))

ggplot(diamonds) + geom_point(aes(x=price, y= carat)) + facet_wrap(~ clarity, nrow = 2)

ggplot(diamonds, aes(x=carat, y=price, color=clarity)) + geom_point()

ggplot(diamonds, aes(x=price, color=cut)) + geom_density()

#Investigating the price of diamonds using box plots
ggplot(diamonds, aes(factor(cut), price, fill=cut)) + geom_boxplot() + ggtitle("Diamond Price according Cut") + xlab("Type of Cut") + ylab("Diamond Price U$") + coord_cartesian(ylim=c(0,7500))

#It's hard to draw conclusions; it seems that cut of all types carry prices of all types,
#Let's see the same chart using clarity
ggplot(diamonds, aes(factor(clarity), price, fill=clarity)) + geom_boxplot() + ggtitle("Diamond Price according Clarity") + xlab("Clarity") + ylab("Diamond Price U$") + coord_cartesian(ylim=c(0,7500))


#we investigate the price per carat of diamonds across the different colors of diamonds using boxplots
ggplot(diamonds, aes(factor(color), (price/carat), fill=color)) + geom_boxplot() + ggtitle("Diamond Price per Carat according Color") + xlab("Color") + ylab("Diamond Price per Carat U$")

#Now that is a big quantity of outliers for color D



#we will investigate carat weight using a frequency polygon chart.
ggplot(data=diamonds, aes(x=carat)) + geom_freqpoly(binwidth = 0.025) + ggtitle("Diamond Frequency by Carat") + xlab("Carat Size") + ylab("Count") + scale_x_continuous(minor_breaks = seq(0, 5.5, 0.1))



ggplot(data = diamonds) + geom_point(mapping = aes(x = price, y = depth, color = cut, shape= cut))

ggplot(diamonds, aes(x=carat, y=price, color=clarity)) + geom_point()


# table() function in R Language is used to create a categorical representation of data with variable name and the frequency in the form of a table.
table(diamonds$color)


table(diamonds$clarity)

plot(diamonds$depth~diamonds$price)

month.name

head(diamonds)
