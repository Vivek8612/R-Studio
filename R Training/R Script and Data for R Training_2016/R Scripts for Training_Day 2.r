##############################################################################
# R Session - Day 2 Scripts						                             #
# Date:  05/13/2016									                         #
# Author: Ravishankar Rajagopalan							                 #	
##############################################################################

# Setting working directory
getwd()
setwd("D:\\R\\R Training\\R Script and Data for R Training_2016")
getwd()


##############################################################################
# Reading large files       						                         #
##############################################################################

# Read CSV Files
# Using read.csv
startTime <- Sys.time()
csvDataFrame <- read.csv("optusLargeCSV.csv")
Sys.time()-startTime

# Using fread
install.packages("data.table")
library(data.table)
startTime <- Sys.time()
fastCSVDataFrame <- fread("optusLargeCSV.csv")
Sys.time()-startTime

# Reading Text Files
# Using read.table
startTime <- Sys.time()
textDataFrame <- read.table("optusLargeData.txt",header = TRUE, sep = "\t",quote="",comment.char = "",row.names = NULL, stringsAsFactors = FALSE) 
Sys.time()-startTime

# Using fread
startTime <- Sys.time()
fastText <- fread("optusLargeData.txt")
Sys.time()-startTime

# Reading large Excel files
library(readxl)
startTime <- Sys.time()
excelData <- read_excel("optusXLSX.xlsx")
Sys.time()-startTime

##############################################################################
# Section 5: Exploratory Data Analysis  						             #
##############################################################################

csvDataFrame <- read.csv("optusDataCSV.csv")

# Summary
summary(csvDataFrame)
summary(csvDataFrame$TOS)

# Summary Statistics
x <- c(1,2,3,4,NA)
mean(x)

#Do not consider NA for calculation
mean(x,na.rm = TRUE)

mean(csvDataFrame$TOS,na.rm = TRUE)
median(csvDataFrame$TOS,na.rm = TRUE)
min(csvDataFrame$TOS,na.rm = TRUE)
max(csvDataFrame$TOS,na.rm = TRUE)
sd(csvDataFrame$TOS,na.rm = TRUE)
quantile(csvDataFrame$TOS,c(0.01,0.05,0.1,0.5,0.90,0.95,1.0))


# Summary Statistics by Segments
summary1 <- aggregate(csvDataFrame["NOP"], by=csvDataFrame[c("DayOfWeek")], FUN=mean)
summary2 <- aggregate(NOP ~ DayOfWeek, data = csvDataFrame, FUN=mean)


#aggregate( . ~ Species, data = iris, mean)

# Using ddply plyr package
library(plyr)
summary3 <- ddply(csvDataFrame, ~ DayOfWeek, summarise, mean=mean(NOP))
summary3

summary4 <- ddply(csvDataFrame, ~ DayOfWeek, summarise, meanValue=mean(NOP),stdDev=sd(NOP))
summary4

# Practice - Summary
summary5 <- ddply(csvDataFrame, ~ PURCHASE_FLAG, summarise, countValue=length(NOP),meanValue=mean(NOP),stdDev=sd(NOP),medianValue=median(NOP))
summary5

#Pivot on multiple variables
summary6 <- ddply(csvDataFrame, ~ PURCHASE_FLAG+DayOfWeek, summarise, countValue=length(NOP),meanValue=mean(NOP),stdDev=sd(NOP),medianValue=median(NOP))
summary6



# Crosstabulation
crossTab1 <- table(csvDataFrame$OS,csvDataFrame$Browser)
crossTab1 

crossTab2 <- xtabs(~OS + Browser,data = csvDataFrame)
crossTab2 

overallProportion <- prop.table(crossTab1)
overallProportion 


# Practice CrossTab

crossTab3 <- xtabs(PURCHASE_FLAG~OS + Browser,data = csvDataFrame)
crossTab3 

overallProportion1 <- prop.table(crossTab3)
overallProportion1 
prop.table(crossTab3,2) #Column Sum = 100%
prop.table(crossTab3,1) #Row Sum = 100%


crossTab4 <- xtabs(~OS + Browser + PURCHASE_FLAG,data = csvDataFrame)
crossTab4 

overallProportion2 <- prop.table(crossTab4)
overallProportion2 
prop.table(crossTab4,2) #Column Sum = 100%
prop.table(crossTab4,1) #Row Sum = 100%


##############################################################################
# Section 6: Visualization								                     #
##############################################################################

# R Visualization Gallery - http://www.r-graph-gallery.com/

# Reading CSV File
csvDataFrame <- read.csv("optusDataCSV.csv") 
data(iris)

# Line Chart and Scatter Plot
plot(csvDataFrame$NOP,type = "o",main = "Line Chart",xlab = "Index",ylab = "NOP", col = "red",lwd = 1,lty = 2)
plot(iris$Sepal.Length,type = "o",main = "Line Chart",xlab = "Index",ylab = "NOP", col = "red",lwd = 1,lty = 2)

plot(csvDataFrame$NOP,csvDataFrame$TOS, main = "Scatter Plot",xlab = "NOP",ylab = "TOS", col = "red")
par(mfrow = c(2,1))
plot(csvDataFrame$NOP,type = "o",main = "Line Chart",xlab = "Index",ylab = "NOP", col = "red",lwd = 1,lty = 2)
plot(iris$Sepal.Length,type = "o",main = "Line Chart",xlab = "Index",ylab = "NOP", col = "red",lwd = 1,lty = 2)
par(mfrow = c(1,1))
par(ask = TRUE)
plot(csvDataFrame$NOP,type = "o",main = "Line Chart",xlab = "Index",ylab = "NOP", col = "red",lwd = 1,lty = 2)
plot(iris$Sepal.Length,type = "o",main = "Line Chart",xlab = "Index",ylab = "NOP", col = "red",lwd = 1,lty = 2)
par(ask = FALSE)

# Saving a plot
pdf('scatterPlot.pdf')
plot(csvDataFrame$NOP,csvDataFrame$TOS, main = "Scatter Plot",xlab = "NOP",ylab = "TOS", col = "red")
dev.off()

# Histogram
hist(csvDataFrame$NOP)

# Box Plots
boxplot(iris$Sepal.Length)
boxplot(Sepal.Length ~ Species,data = iris)

# Barplots/Stacked BarPlots
browserCounts <- table(csvDataFrame$Browser)
barplot(browserCounts,col="red")
barplot(prop.table(browserCounts))

browserOSCounts <- table(csvDataFrame$Browser,csvDataFrame$OS)
barplot(browserOSCounts,legend = rownames(browserOSCounts),args.legend = list(x = "topleft", inset=c(0.05, 0)))
barplot(prop.table(browserOSCounts,2),legend = rownames(browserOSCounts),mar=c(4, 4, 3, 3),args.legend = list(x = "topright", inset=c(0.17, 0)))

#Lattice Plots
library(lattice)
densityplot(~csvDataFrame$NOP,main="Kernel Density Plot")
densityplot(~csvDataFrame$NOP|csvDataFrame$Browser,main="Kernel Density Plot")
bwplot(~csvDataFrame$NOP|csvDataFrame$Browser,main = "Box Plot")
xyplot(csvDataFrame$NOP~csvDataFrame$TOS|csvDataFrame$Browser,main="Scatterplots by Browsert")
splom(csvDataFrame[,c("TimeofDay","NOP","TOS")]) 
cloud(Sepal.Length~Sepal.Width*Petal.Length|Species,data=iris,main="3D Scatterplot by Cylinders") 

#qplot
install.packages("ggplot2")
library(ggplot2)

# Choose different colors
qplot(Sepal.Length,Sepal.Width,data = iris, colour = Species)

# Choose different shapes
qplot(Sepal.Length,Sepal.Width,data = iris, shape = Species)

# Boxplot
qplot(Species,Sepal.Length, data = iris, geom="boxplot")

# Histogram
qplot(Sepal.Length, data = iris, geom="histogram")
qplot(Sepal.Length, data = iris, geom="histogram",fill = Species)
qplot(Sepal.Length, data = iris, geom="histogram",colour = Species)


# Density Plotss
qplot(Sepal.Length, data = iris, geom="density")
qplot(Sepal.Length, data = iris, geom="density",colour=Species)
qplot(Sepal.Length, data = iris, geom="density",fill=Species)

# Bar Plot
qplot(Browser, data = csvDataFrame, geom="bar")


# Arranging multiple qplots
install.packages("gridExtra")
library(gridExtra)
plot1 <- qplot(Sepal.Length, data = iris, geom="histogram")
plot2 <- qplot(Sepal.Length, data = iris, geom="density")
grid.arrange(plot1, plot2, nrow=1, ncol=2)

# Practice - Visualization

#Add a barplot of Referrer
#referrerCounts <- table(csvDataFrame$Referrer)
plot1<-barplot(referrerCounts,col="red")

#Add a boxplot of NOP by device_details
#boxplot(NOP ~ device_details,data = csvDataFrame)
plot2<-qplot(device_details,NOP, data = csvDataFrame, geom="boxplot")


#Add a histogram of NOP and shade it by device_details
plot3<-qplot(NOP, data = csvDataFrame, geom="histogram",fill = device_details)

#Add a density plot of TOS
plot4<-qplot(TOS, data = csvDataFrame, geom="density")

plot1<-qplot(Browser, data = csvDataFrame, geom="bar",)
plot2<-qplot(device_details,NOP, data = csvDataFrame, geom="boxplot")
plot3<-qplot(NOP, data = csvDataFrame, geom="histogram",fill = device_details)
plot4<-qplot(TOS, data = csvDataFrame, geom="density")
grid.arrange(plot1,plot2,plot3,plot4,nrow=2, ncol=2)
##############################################################################
# Section 7: Function and Control Statements       						     #
##############################################################################

# Functions in R

returnSubset <- function(dataFrameName,columnNames)
{
  outDataFrame <- dataFrameName[,columnNames]
  outDataFrame <- outDataFrame[outDataFrame$TOS > 1000, ]
  return(outDataFrame)
}

returnDf <- returnSubset(csvDataFrame,c("TOS","DayOfWeek","TimeofDay"))
returnDf

# Practice 6

# FOR loops

# Using Index

for (i in 1:ncol(csvDataFrame))
{
  print(i)
  print(colnames(csvDataFrame)[i])
  print(summary(csvDataFrame[,i]))
}

# Using values
for (column in colnames(csvDataFrame))
{
  print(column)
  print(summary(csvDataFrame[,column]))
}

# IF Condition

csvDataFrame$highLowVar <- NA

for(i in 1:nrow(csvDataFrame))
{
  if(csvDataFrame$TOS > 1000)
  {
    csvDataFrame$lowHighVar <- "High"

  } else {
  
    csvDataFrame$lowHighVar <- "Low"

  }
}

# ifelse 

csvDataFrame$lowHighVar <- ifelse(csvDataFrame$TOS > 1000,"High","Low")

# Practice 7

##############################################################################
# Section 8: Statistical Modeling and Data Mining				             #
##############################################################################

# Creating binned variables
bins <- c(0,200,500,1000,3000,5000,Inf)
csvDataFrame$tosBinned <- cut(csvDataFrame$TOS,bins,dig.lab = 10,include.lowest = TRUE)
table(csvDataFrame$tosBinned)

# Creating training and test data
set.seed(1000)
csvDataFrame$random <- runif(nrow(csvDataFrame))
hist(csvDataFrame$random)
trainingData <- subset(csvDataFrame,random <= 0.8)
dim(trainingData)
testData <- subset(csvDataFrame,random > 0.8)

# Logistic Regression Model
# Build model with Training Data 
lrModel <- glm(PURCHASE_FLAG ~ tosBinned + SMARTPHONE + device_details,data = trainingData,family = binomial)
summary(lrModel)

# Prediction for the Test data
testPrediction <- predict(lrModel,testData,type = "response")

# ROC Curves
library(Epi)
ROC( form = testData$PURCHASE_FLAG ~ testPrediction, plot="ROC")

# Practice Model Building

