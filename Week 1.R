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
# Use the tapply function to find the average child mortality rate of countries in each region.
# Which region has the lowest average child mortality rate across all countries in that region?
  WHO$Region[which.min(tapply(WHO$ChildMortality, WHO$Region, min, na.rm = T))]
  WHO$Country[which.min(tapply(WHO$ChildMortality, WHO$Country, min, na.rm = T))]

## Homework 
# AN ANALYTICAL DETECTIVE  
# Problem 1.1 - Loading the Data
# Read the dataset mvtWeek1.csv into R, using the read.csv function, and call 
# the data frame "mvt". Remember to navigate to the directory on your computer 
# containing the file mvtWeek1.csv first. It may take a few minutes to read in 
# the data, since it is pretty large. Then, use the str and summary functions 
# to answer the following questions.
  mvt <- read.csv("mvtWeek1.csv")
# How many rows of data (observations) are in this dataset? 
  dim(mvt)
  
# Problem 1.2 - Loading the Data
# How many variables are in this dataset?  
  
  
# Problem 1.3 - Loading the Data
# Using the "max" function, what is the maximum value of the variable "ID"?
  max(mvt$ID)
  
# Problem 1.4 - Loading the Data
# What is the minimum value of the variable "Beat"?
  min(mvt$Beat)
  
# Problem 1.5 - Loading the Data
# How many observations have value TRUE in the Arrest variable (this is the 
# number of crimes for which an arrest was made)?
  summary(mvt$Arrest)
  
# Problem 1.6 - Loading the Data
# How many observations have a LocationDescription value of ALLEY?
 table(mvt$LocationDescription == "ALLEY")  
  
# Problem 2.1 - Understanding Dates in R
# In many datasets, like this one, you have a date field. Unfortunately, 
# R does not automatically recognize entries that look like dates. We need
# to use a function in R to extract the date and time. Take a look at the 
# first entry of Date (remember to use square brackets when looking at a 
# certain entry of a variable).
 
# In what format are the entries in the variable Date?  
mvt$Date[1]

# Problem 2.2 - Understanding Dates in R
# Now, let's convert these characters into a Date object in R. In your R console, type
  DateConvert <- as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

# This converts the variable "Date" into a Date object in R. Take a look 
# at the variable DateConvert using the summary function.
  summary(DateConvert)
# What is the month and year of the median date in our dataset? Enter your 
# answer as "Month Year", without the quotes. (Ex: if the answer was 2008-03-28, 
# you would give the answer "March 2008", without the quotes.)
  mean(DateConvert)

# Problem 2.3 - Understanding Dates in R
# Now, let's extract the month and the day of the week, and add these 
# variables to our data frame mvt. We can do this with two simple 
# functions. Type the following commands in R:
  mvt$Month <- months(DateConvert)
  mvt$Weekday <- weekdays(DateConvert)
  
# This creates two new variables in our data frame, Month and Weekday, 
# and sets them equal to the month and weekday values that we can extract 
# from the Date object. Lastly, replace the old Date variable with 
# DateConvert by typing:
  mvt$Date <- DateConvert
  
# Using the table command, answer the following questions.
# In which month did the fewest motor vehicle thefts occur?
  table(mvt$Month)
  min(table(mvt$Month))

# Problem 2.4 - Understanding Dates in R
# On which weekday did the most motor vehicle thefts occur?
  table(mvt$Weekday)
  max(table(mvt$Weekday))
  
# Problem 2.5 - Understanding Dates in R
# Each observation in the dataset represents a motor vehicle theft, 
# and the Arrest variable indicates whether an arrest was later made 
# for this theft. Which month has the largest number of motor vehicle 
# thefts for which an arrest was made?  
  table(mvt$Arrest, mvt$Month)
  
# Problem 3.1 - Visualizing Crime Trends
# Now, let's make some plots to help us better understand how crime 
# has changed over time in Chicago. Throughout this problem, and in 
# general, you can save your plot to a file. For more information, 
# this website very clearly explains the process.
  
# First, let's make a histogram of the variable Date. We'll add an 
# extra argument, to specify the number of bars we want in our histogram. 
# In your R console, type
  hist(mvt$Date, breaks=100)
  
# Looking at the histogram, answer the following questions.
# In general, does it look like crime increases or decreases 
# from 2002 - 2012?  
  tmp <- subset(mvt, Date >= "2002-01-01" & Date <= "2012-12-31")  
  hist(tmp$Date, breaks=100)

# In general, does it look like crime increases or decreases 
# from 2005 - 2008?
  tmp <- subset(mvt, Date >= "2005-01-01" & Date <= "2008-12-31")  
  hist(tmp$Date, breaks=100)
  
# In general, does it look like crime increases or decreases from 
# 2009 - 2011?

  
# Problem 3.2 - Visualizing Crime Trends
# Now, let's see how arrests have changed over time. Create a boxplot of 
# the variable "Date", sorted by the variable "Arrest" (if you are not 
# familiar with boxplots and would like to learn more, check out this 
# tutorial). In a boxplot, the bold horizontal line is the median value 
# of the data, the box shows the range of values between the first quartile 
# and third quartile, and the whiskers (the dotted lines extending outside 
# the box) show the minimum and maximum values, excluding any outliers 
# (which are plotted as circles). Outliers are defined by first computing 
# the difference between the first and third quartile values, or the height 
# of the box. This number is called the Inter-Quartile Range (IQR). Any point 
# that is greater than the third quartile plus the IQR or less than the first 
# quartile minus the IQR is considered an outlier.
  
# Does it look like there were more crimes for which arrests were made in 
# the first half of the time period or the second half of the time period? 
# (Note that the time period is from 2001 to 2012, so the middle of the time 
# period is the beginning of 2007.)    
  boxplot(mvt$Date ~ mvt$Arrest)
  
# Problem 3.3 - Visualizing Crime Trends
# Let's investigate this further. Use the table function for the next 
# few questions.
  
# For what proportion of motor vehicle thefts in 2001 was an arrest made?
# Note: in this question and many others in the course, we are asking 
# for an answer as a proportion. Therefore, your answer should take a value 
# between 0 and 1.
  
  tmp <- subset(mvt, Year == "2001")
  table(mvt$Year == "2001", mvt$Arrest)[4] / 
    (table(mvt$Year == "2001", mvt$Arrest)[2] + table(mvt$Year == "2001", mvt$Arrest)[4])
  
# Problem 3.4 - Visualizing Crime Trends
# For what proportion of motor vehicle thefts in 2007 was an arrest made?
  myYear <- "2007"
  tmp <- subset(mvt, Year == myYear)
  table(mvt$Year == myYear, mvt$Arrest)[4] / 
    (table(mvt$Year == myYear, mvt$Arrest)[2] + table(mvt$Year == myYear, mvt$Arrest)[4])
  
# Problem 3.5 - Visualizing Crime Trends
# For what proportion of motor vehicle thefts in 2012 was an arrest made?
  myYear <- "2012"
  tmp <- subset(mvt, Year == myYear)
  table(mvt$Year == myYear, mvt$Arrest)[4] / 
    (table(mvt$Year == myYear, mvt$Arrest)[2] + table(mvt$Year == myYear, mvt$Arrest)[4])
  
# Problem 4.1 - Popular Locations
# Analyzing this data could be useful to the Chicago Police Department when 
# deciding where to allocate resources. If they want to increase the number 
# of arrests that are made for motor vehicle thefts, where should they focus 
# their efforts?
  
# We want to find the top five locations where motor vehicle thefts occur. 
# If you create a table of the LocationDescription variable, it is 
# unfortunately very hard to read since there are 78 different locations in 
# the data set. By using the sort function, we can view this same table, 
# but sorted by the number of observations in each category. In your R console, 
# type:
  sort(table(mvt$LocationDescription))
  tmp <- as.data.frame(sort(table(mvt$LocationDescription))) 
  as.data.frame(tmp[(nrow(tmp)-5):nrow(tmp), ])
  
# Which locations are the top five locations for motor vehicle thefts, 
# excluding the "Other" category? You should select 5 of the following options.
  
# Problem 4.2 - Popular Locations
# Create a subset of your data, only taking observations for which the theft 
# happened in one of these five locations, and call this new data set "Top5". 
# To do this, you can use the | symbol. In lecture, we used the & symbol to use 
# two criteria to make a subset of the data. To only take observations that have 
# a certain value in one variable or the other, the | character can be used in 
# place of the & symbol. This is also called a logical "or" operation.
  
# Alternately, you could create five different subsets, and then merge them together into one data frame using rbind.
  
# How many observations are in Top5?  
  Top5 <- subset(mvt, LocationDescription == "STREET" | 
                  LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                  LocationDescription == "ALLEY" | 
                  LocationDescription == "GAS STATION" | 
                  LocationDescription == "DRIVEWAY - RESIDENTIAL")
  
 dim(Top5) 
  
# Problem 4.3 - Popular Locations
# R will remember the other categories of the LocationDescription variable 
# from the original dataset, so running table(Top5$LocationDescription) will 
# have a lot of unnecessary output. To make our tables a bit nicer to read, 
# we can refresh this factor variable. In your R console, type:
   Top5$LocationDescription <- factor(Top5$LocationDescription)
 
# If you run the str or table function on Top5 now, you should see that 
# LocationDescription now only has 5 values, as we expect.
 
# Use the Top5 data frame to answer the remaining questions.
 
# One of the locations has a much higher arrest rate than the other locations. 
# Which is it? Please enter the text in exactly the same way as how it looks 
# in the answer options for Problem 4.1.
 
  summary(Top5) 
  tmp <- as.matrix.data.frame(table(Top5$LocationDescription, Top5$Arrest))
  tmp <- as.data.frame(tmp)
  tmp <- cbind(c("ALLEY", "DRIVEWAY - RESIDENTIAL", "GAS STATION",
                 "PARKING LOT/GARAGE(NON.RESID.)", "STREET"), tmp)

  tmp$V1 <- as.numeric(as.character(tmp$V1))
  tmp$V2 <- as.numeric(as.character(tmp$V2))
  tmp$Rate <- tmp[3] / (tmp[2] + tmp[3])
  tmp

# Problem 4.4 - Popular Locations
# On which day of the week do the most motor vehicle thefts at gas stations happen?
  table(Top5$LocationDescription == "GAS STATION", Top5$Weekday)  
  
# Problem 4.5 - Popular Locations  
# On which day of the week do the fewest motor vehicle thefts in residential 
# driveways happen?
  table(Top5$LocationDescription == "DRIVEWAY - RESIDENTIAL", Top5$Weekday)  
  
  
# STOCK DYNAMICS
# Download and read the following files into R, using the read.csv function: 
# IBMStock.csv, GEStock.csv, ProcterGambleStock.csv, CocaColaStock.csv, and BoeingStock.csv.  
# Call the data frames "IBM", "GE", "ProcterGamble", "CocaCola", and "Boeing", respectively.   
  IBM <- read.csv("IBMStock.csv")
  GE <- read.csv("GEStock.csv")
  ProcterGamble <- read.csv("ProcterGambleStock.csv")
  CocaCola <- read.csv("CocaColaStock.csv")
  Boeing <- read.csv("BoeingStock.csv")
  
  
  
  
    
  