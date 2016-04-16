## Week 1

# Quick Questions for Installing R
# (2 points possible)
# When you start R, you should see a window titled "R Console". In this window, there is some text, and then at the bottom there should be a > symbol (greater than symbol), followed by a blinking cursor. At the cursor, type:
sd(c(5,8,12))

# and then hit enter. You should see [1] followed by a number. What is this number?

# Now type:
which.min(c(4,1,6))
# at the cursor, and hit enter. You should again see [1], followed by a number. What is this number? 

# Video 5: Data Analysis - Summary Statistics and Scatterplots
WHO <- read.csv("WHO.csv")
str(WHO)
summary(WHO)  

WHO_Europe <- subset(WHO, Region == "Europe")
str(WHO)
summary(WHO_Europe)
write.csv(WHO_Europe, "WHO_Europe.csv")
ls()
rm(WHO_Europe)
ls()

Under15
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]

WHO$Country[which.min(WHO$Under15)]
WHO$Country[which.max(WHO$Under15)]

plot(WHO$GNI, WHO$FertilityRate)
Outliers <- subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country", "GNI", "FertilityRate")]

# Quick Question
# (3 points possible)
# Please answer the following questions using the entire data frame WHO (and not one of the subsets we have created in R).
# What is the mean value of the "Over60" variable?
mean(WHO$Over60)

# Which country has the smallest percentage of the population over 60?
WHO$Country[which.min(WHO$Over60)]

# Which country has the largest literacy rate?
WHO$Country[which.max(WHO$LiteracyRate)]

# Video 6: Data Analysis - Plots and Summary Tables
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expectancy",
        main="Life Expectancy of Countries by Region")

table(WHO$Region)
as.data.frame(table(WHO$Region))[1, 2]
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$Over60, WHO$Country, mean)
max(tapply(WHO$Over60, WHO$Country, mean))
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm = T)

# Quick Question
# (1 point possible)
# Use the tapply function to find the average child mortality rate of countries in each region.
# Which region has the lowest average child mortality rate across all countries in that region?
WHO$Region[which.min(tapply(WHO$ChildMortality, WHO$Region, min, na.rm = T))]
WHO$Country[which.min(tapply(WHO$ChildMortality, WHO$Country, min, na.rm = T))]


