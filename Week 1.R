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
  
# Problem 1.1 - Summary Statistics
# Before working with these data sets, we need to convert the dates into a format 
# that R can understand. Take a look at the structure of one of the datasets using 
# the str function. Right now, the date variable is stored as a factor. We can 
# convert this to a "Date" object in R by using the following five commands (one 
# for each data set):
  IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
  GE$Date <- as.Date(GE$Date, "%m/%d/%y")
  CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
  ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
  Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")

# The first argument to the as.Date function is the variable we want to convert, 
# and the second argument is the format of the Date variable. We can just 
# overwrite the original Date variable values with the output of this function. 
# Now, answer the following questions using the str and summary functions.
  
# Our five datasets all have the same number of observations. How many observations 
# are there in each data set?  
  dim(Boeing)  
  dim(GE)  

# Problem 1.2 - Summary Statistics
#  What is the earliest year in our datasets?
  summary(Boeing$Date)  
  min(Boeing$Date)

# Problem 1.3 - Summary Statistics
#  What is the latest year in our datasets?
  max(Boeing$Date)
  
# Problem 1.4 - Summary Statistics
# What is the mean stock price of IBM over this time period?  
  mean(IBM$StockPrice)
  
# Problem 1.5 - Summary Statistics
# What is the minimum stock price of General Electric (GE) over this time period? 
  min(GE$StockPrice)
  
# Problem 1.6 - Summary Statistics
# What is the maximum stock price of Coca-Cola over this time period?  
  max(CocaCola$StockPrice)
  
# Problem 1.7 - Summary Statistics
# What is the median stock price of Boeing over this time period? 
  median(Boeing$StockPrice)
  
# Problem 1.8 - Summary Statistics
# What is the standard deviation of the stock price of Procter & Gamble 
# over this time period?  
  sd(ProcterGamble$StockPrice)
  
# Problem 2.1 - Visualizing Stock Dynamics
# Let's plot the stock prices to see if we can visualize trends in stock 
# prices during this time period. Using the plot function, plot the Date 
# on the x-axis and the StockPrice on the y-axis, for Coca-Cola.
  
# This plots our observations as points, but we would really like to see 
# a line instead, since this is a continuous time period. To do this, add 
# the argument type="l" to your plot command, and re-generate the plot (the 
# character is quotes is the letter l, for line). You should now see a line 
# plot of the Coca-Cola stock price.
  
# Around what year did Coca-Cola has its highest stock price in this time period?
# Around what year did Coca-Cola has its lowest stock price in this time period?  
  plot(CocaCola$Date, CocaCola$StockPrice, type = "l")

# Problem 2.2 - Visualizing Stock Dynamics
# Now, let's add the line for Procter & Gamble too. You can add a line to a plot 
# in R by using the lines function instead of the plot function. Keeping the plot 
# for Coca-Cola open, type in your R console:
  lines(ProcterGamble$Date, ProcterGamble$StockPrice)
  
# Unfortunately, it's hard to tell which line is which. Let's fix this by giving 
# each line a color. First, re-run the plot command for Coca-Cola, but add the 
# argument col="red". You should see the plot for Coca-Cola show up again, but this 
# time in red. Now, let's add the Procter & Gamble line (using the lines function 
# like we did before), adding the argument col="blue". You should now see in your 
# plot the Coca-Cola stock price in red, and the Procter & Gamble stock price in blue.
  plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
  lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
  
# As an alternative choice to changing the colors, you could instead change the line 
# type of the Procter & Gamble line by adding the argument lty=2. This will make the 
# Procter & Gamble line dashed.
  plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
  lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty = 2)
  
# Using this plot, answer the following questions.
  
# In March of 2000, the technology bubble burst, and a stock market crash occurred. 
# According to this plot, which company's stock dropped more?  
  
# To answer this question and the ones that follow, you may find it useful to draw 
# a vertical line at a certain date. To do this, type the command
  abline(v=as.Date(c("2000-03-01")), lwd=2)
  
# in your R console, with the plot still open. This generates a vertical line at the 
# date March 1, 2000. The argument lwd=2 makes the line a little thicker. You can 
# change the date in this command to generate the vertical line in different locations.  
  
# Problem 2.3 - Visualizing Stock Dynamics
# Answer these questions using the plot you generated in the previous problem.
  
# Around 1983, the stock for one of these companies (Coca-Cola or Procter and Gamble) 
# was going up, while the other was going down. Which one was going up?
  abline(v=as.Date(c("1983-07-01")), lwd=3, col = "gray")
  
# In the time period shown in the plot, which stock generally has lower values?  
  median(CocaCola$StockPrice)
  mean(CocaCola$StockPrice)
  
  median(ProcterGamble$StockPrice)
  mean(ProcterGamble$StockPrice)  
  
# Problem 3.1 - Visualizing Stock Dynamics 1995-2005
# Let's take a look at how the stock prices changed from 1995-2005 for all 
# five companies. In your R console, start by typing the following plot command:
  plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
  
# This will plot the CocaCola stock prices from 1995 through 2005, which are the 
# observations numbered from 301 to 432. The additional argument, ylim=c(0,210), 
# makes the y-axis range from 0 to 210. This will allow us to see all of the stock 
# values when we add in the other companies.
  
# Now, use the lines function to add in the other four companies, remembering to only 
# plot the observations from 1995 to 2005, or [301:432]. You don't need the "type" 
# or "ylim" arguments for the lines function, but remember to make each company a 
# different color so that you can tell them apart. Some color options are "red", 
# "blue", "green", "purple", "orange", and "black". To see all of the color options 
# in R, type colors() in your R console.
  
  lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = "blue", lty = 2)
  lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = "green", lty = 2)
  lines(GE$Date[301:432], GE$StockPrice[301:432], col = "gray", lty = 2)
  lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = "black", lty = 2)
  
# (If you prefer to change the type of the line instead of the color, here are some 
# options for changing the line type: lty=2 will make the line dashed, lty=3 will make 
# the line dotted, lty=4 will make the line alternate between dashes and dots, and 
# lty=5 will make the line long-dashed.)
  
# Use this plot to answer the following four questions.
  
# Which stock fell the most right after the technology bubble burst in March 2000?  
  abline(v=as.Date(c("2000-03-01")), lwd=3, col = "gray")
  
# Which stock reaches the highest value in the time period 1995-2005?  
  
# Problem 3.3 - Visualizing Stock Dynamics 1995-2005
# In October of 1997, there was a global stock market crash that was caused by an 
# economic crisis in Asia. Comparing September 1997 to November 1997, which companies 
# saw a decreasing trend in their stock price? (Select all that apply.)  
  abline(v=as.Date(c("1997-09-01")), lwd=1, col = "grey")
  abline(v=as.Date(c("1997-11-15")), lwd=1, col = "grey")
  
  
# Problem 3.4 - Visualizing Stock Dynamics 1995-2005
# In the last two years of this time period (2004 and 2005) which stock seems to be 
# performing the best, in terms of increasing stock price? 
  abline(v=as.Date(c("2004-01-01")), lwd=1, col = "grey")
  abline(v=as.Date(c("2005-12-31")), lwd=1, col = "grey")
  
# Problem 4.1 - Monthly Trends
# Lastly, let's see if stocks tend to be higher or lower during certain 
# months. Use the tapply command to calculate the mean stock price of IBM, 
# sorted by months. To sort by months, use
  months(IBM$Date)
  
# as the second argument of the tapply function.
  
# For IBM, compare the monthly averages to the overall average stock price. 
# In which months has IBM historically had a higher stock price (on average)? 
# Select all that apply.
  tapply(IBM$StockPrice, months(IBM$Date), mean)
  sort(tapply(IBM$StockPrice, months(IBM$Date), mean), decreasing = TRUE)
  mean(IBM$StockPrice)
  
# Problem 4.2 - Monthly Trends
# Repeat the tapply function from the previous problem for each of the other 
# four companies, and use the output to answer the remaining questions.
  
# General Electric and Coca-Cola both have their highest average stock price 
# in the same month. Which month is this?  
  sort(tapply(GE$StockPrice, months(GE$Date), mean), decreasing = TRUE)
  sort(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean), decreasing = TRUE)
  
# Problem 4.3 - Monthly Trends
# For the months of December and January, every company's average stock is 
# higher in one month and lower in the other. In which month are the stock 
# prices lower?  
  
  
# DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES
  
# The observations in the dataset represent people surveyed in the 
# September 2013 CPS who actually completed a survey. While the full 
# dataset has 385 variables, in this exercise we will use a more compact 
# version of the dataset, CPSData.csv, 
  
# Problem 1.1 - Loading and Summarizing the Dataset
# Load the dataset from CPSData.csv into a data frame called CPS, and view the 
# dataset with the summary() and str() commands.
  CPS <- read.csv("CPSData.csv")

# How many interviewees are in the dataset?  
  dim(CPS)  
  
# Problem 1.2 - Loading and Summarizing the Dataset
# Among the interviewees with a value reported for the Industry variable, 
# what is the most common industry of employment? Please enter the name 
# exactly how you see it.
  summary(CPS)
  
# Problem 1.3 - Loading and Summarizing the Dataset
# Recall from the homework assignment "The Analytical Detective" that you 
# can call the sort() function on the output of the table() function to obtain 
# a sorted breakdown of a variable. For instance, sort(table(CPS$Region)) sorts 
# the regions by the number of interviewees from that region.
  
# Which state has the fewest interviewees?  
  sort(table(CPS$State))   
  
# Problem 1.4 - Loading and Summarizing the Dataset
# What proportion of interviewees are citizens of the United States?    
  table(CPS$Citizenship != "Non-Citizen")[2] / 
    (table(CPS$Citizenship != "Non-Citizen")[2] + table(CPS$Citizenship != "Non-Citizen")[1])
  
# Problem 1.5 - Loading and Summarizing the Dataset
# The CPS differentiates between race (with possible values American Indian, 
# Asian, Black, Pacific Islander, White, or Multiracial) and ethnicity. A 
# number of interviewees are of Hispanic ethnicity, as captured by the Hispanic 
# variable. For which races are there at least 250 interviewees in the CPS 
# dataset of Hispanic ethnicity? (Select all that apply.)  
  table(CPS$Hispanic == 1, CPS$Race)
  summary(CPS)
  
# Problem 2.1 - Evaluating Missing Values
# Which variables have at least one interviewee with a missing (NA) value? 
# (Select all that apply.)  
  summary(CPS)
  
# Problem 2.2 - Evaluating Missing Values
# Often when evaluating a new dataset, we try to identify if there is a 
# pattern in the missing values in the dataset. We will try to determine 
# if there is a pattern in the missing values of the Married variable. 
# The function is.na(CPS$Married) returns a vector of TRUE/FALSE values for 
# whether the Married variable is missing. We can see the breakdown of whether 
# Married is missing based on the reported value of the Region variable with 
# the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:  
  table(CPS$Region, is.na(CPS$Married))
  table(CPS$Sex, is.na(CPS$Married))
  table(CPS$Age, is.na(CPS$Married))
  table(CPS$Citizenship, is.na(CPS$Married))
  
# Problem 2.3 - Evaluating Missing Values
# As mentioned in the variable descriptions, MetroAreaCode is missing if an 
# interviewee does not live in a metropolitan area. Using the same technique 
# as in the previous question, answer the following questions about people 
# who live in non-metropolitan areas.
  
# How many states had all interviewees living in a non-metropolitan area 
# (aka they have a missing MetroAreaCode value)? For this question, treat 
# the District of Columbia as a state (even though it is not technically a state).
  table(CPS$State, is.na(CPS$MetroAreaCode))
  
# Problem 2.4 - Evaluating Missing Values
# Which region of the United States has the largest proportion of interviewees 
# living in a non-metropolitan area?
  table(CPS$Region, is.na(CPS$MetroAreaCode))
  
# Problem 2.5 - Evaluating Missing Values
# While we were able to use the table() command to compute the proportion of 
# interviewees from each region not living in a metropolitan area, it was 
# somewhat tedious (it involved manually computing the proportion for each region) 
# and isn't something you would want to do if there were a larger number of options. 
# It turns out there is a less tedious way to compute the proportion of values that 
# are TRUE. The mean() function, which takes the average of the values passed to it, 
# will treat TRUE as 1 and FALSE as 0, meaning it returns the proportion of values 
# that are true. For instance, mean(c(TRUE, FALSE, TRUE, TRUE)) returns 0.75. Knowing 
# this, use tapply() with the mean function to answer the following questions:
  
# Which state has a proportion of interviewees living in a non-metropolitan area 
# closest to 30%? 
  as.data.frame(sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean, na.rm = T)))
  
# Which state has the largest proportion of non-metropolitan interviewees, ignoring 
# states where all interviewees were non-metropolitan?

# Problem 3.1 - Integrating Metropolitan Area Data
# Codes like MetroAreaCode and CountryOfBirthCode are a compact way to encode 
# factor variables with text as their possible values, and they are therefore 
# quite common in survey datasets. In fact, all but one of the variables in this 
# dataset were actually stored by a numeric code in the original CPS datafile.
  
# When analyzing a variable stored by a numeric code, we will often want to convert 
# it into the values the codes represent. To do this, we will use a dictionary, which 
# maps the the code to the actual value of the variable. We have provided dictionaries 
# MetroAreaCodes.csv and CountryCodes.csv, which respectively map MetroAreaCode and 
# CountryOfBirthCode into their true values. Read these two dictionaries into data 
# frames MetroAreaMap and CountryMap.
  MetroAreaMap <- read.csv("MetroAreaCodes.csv")
  CountryMap <- read.csv("CountryCodes.csv")
  
# How many observations (codes for metropolitan areas) are there in MetroAreaMap?  
  dim(MetroAreaMap)
  summary(MetroAreaMap)
  dim(CountryMap)
  
# Problem 3.2 - Integrating Metropolitan Area Data
# To merge in the metropolitan areas, we want to connect the field MetroAreaCode 
# from the CPS data frame with the field Code in MetroAreaMap. The following command 
# merges the two data frames on these columns, overwriting the CPS data frame with 
# the result:
    CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
  
# The first two arguments determine the data frames to be merged (they are called 
# "x" and "y", respectively, in the subsequent parameters to the merge function). 
# by.x="MetroAreaCode" means we're matching on the MetroAreaCode variable from the 
# "x" data frame (CPS), while by.y="Code" means we're matching on the Code variable 
# from the "y" data frame (MetroAreaMap). Finally, all.x=TRUE means we want to keep 
# all rows from the "x" data frame (CPS), even if some of the rows' MetroAreaCode 
# doesn't match any codes in MetroAreaMap (for those familiar with database 
# terminology, this parameter makes the operation a left outer join instead of an 
# inner join).
  
# Review the new version of the CPS data frame with the summary() and str() functions. 
# What is the name of the variable that was added to the data frame by the 
# merge() operation?  
  summary(CPS)  
  
# Problem 3.3 - Integrating Metropolitan Area Data
# Which of the following metropolitan areas has the largest number of interviewees?  
  as.data.frame(summary(CPS$MetroArea))
  
# Problem 3.4 - Integrating Metropolitan Area Data
# Which metropolitan area has the highest proportion of interviewees of Hispanic 
# ethnicity? Hint: Use tapply() with mean, as in the previous subproblem. Calling 
# sort() on the output of tapply() could also be helpful here.
  sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
  
# Problem 3.5 - Integrating Metropolitan Area Data
# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of whether 
# an interviewee is Asian, determine the number of metropolitan areas in the 
# United States from which at least 20% of interviewees are Asian.  
  as.data.frame(sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean)))

# Problem 3.6 - Integrating Metropolitan Area Data
# Normally, we would look at the sorted proportion of interviewees from each 
# metropolitan area who have not received a high school diploma with the command:
  sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean))
  
# However, none of the interviewees aged 14 and younger have an education value 
# reported, so the mean value is reported as NA for each metropolitan area. To 
# get mean (and related functions, like sum) to ignore missing values, you can pass 
# the parameter na.rm=TRUE. Passing na.rm=TRUE to the tapply function, determine 
# which metropolitan area has the smallest proportion of interviewees who have 
# received no high school diploma.
  as.data.frame(sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE)))[1,]
  
# Problem 4.1 - Integrating Country of Birth Data
# Just as we did with the metropolitan area information, merge in the country 
# of birth information from the CountryMap data frame, replacing the CPS data 
# frame with the result. If you accidentally overwrite CPS with the wrong values, 
# remember that you can restore it by re-loading the data frame from CPSData.csv 
# and then merging in the metropolitan area information using the command provided 
# in the previous subproblem.
  CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
  
# What is the name of the variable added to the CPS data frame by this merge operation?  
  
# Problem 4.2 - Integrating Country of Birth Data
# Among all interviewees born outside of North America, which country was the most 
# common place of birth?  
  as.data.frame(summary(CPS$Country))
  
# Problem 4.3 - Integrating Country of Birth Data
# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, 
# NY-NJ-PA" metropolitan area have a country of birth that is not the United States? 
# For this computation, don't include people from this metropolitan area who have 
# a missing country of birth.  
  x <- subset(CPS, CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
  (181 + 105 + 96 + 95 + 1191) / (3736 + 181 + 105 + 96 + 95 + 1191)
  
  y <- table(CPS$Country != "United States", CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
  y[2 , 2] / (y[2 , 2] + y[1 , 2])
  
# Problem 4.4 - Integrating Country of Birth Data
# Which metropolitan area has the largest number (note -- not proportion) of 
# interviewees with a country of birth in India? Hint -- remember to include 
# na.rm=TRUE if you are using tapply() to answer this question.
  summary(CPS)
  as.data.frame(sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm = T)), descending = F)

  my_Country <- "India"
   as.data.frame(sort(tapply(CPS$Country == my_Country, CPS$MetroArea, sum, na.rm = T)), descending = F)

# In Brazil?     
   my_Country <- "Brazil"
   as.data.frame(sort(tapply(CPS$Country == my_Country, CPS$MetroArea, sum, na.rm = T)), descending = F)
   
# In Somalia?  
   my_Country <- "Somalia"
   as.data.frame(sort(tapply(CPS$Country == my_Country, CPS$MetroArea, sum, na.rm = T)), descending = F)
   
  
  
  