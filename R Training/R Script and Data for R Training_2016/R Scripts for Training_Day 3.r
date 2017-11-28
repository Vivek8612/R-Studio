##############################################################################
# R Session - Day 3 Scripts						                             #
# Date:  05/19/2016									                         #
# Author: Ravishankar Rajagopalan							                 #	
##############################################################################

# Setting working directory
getwd()
setwd("D:\\R\\R Training\\R Script and Data for R Training_2016")
getwd()

# Read data from CSV

csvDataFrame <- read.csv("optusDataCSV.csv") 

str(csvDataFrame)

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

str(returnDf)

# Practice 6

returnSubset1 <- function(dataFrameName,columnNames,tosVar,thrs)
{
  outDataFrame <- dataFrameName[,columnNames]
  outDataFrame <- outDataFrame[tosVar > thrs, ]
  return(outDataFrame)
}


returnDf1 <- returnSubset1(csvDataFrame,c("TOS","DayOfWeek","TimeofDay"),"TOS",1000)

str(returnDf1)

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

csvDataFrame$lowHighVar <- NA

for(i in 1:nrow(csvDataFrame))
{
  if(csvDataFrame$TOS > 1000)
  {
    csvDataFrame$lowHighVar[i] <- "High"

  } else {
  
    csvDataFrame$lowHighVar[i] <- "Low"

  }
}

table(csvDataFrame$lowHighVar)


# ifelse - Vectorized

csvDataFrame$lowHighVar <- ifelse(csvDataFrame$TOS > 1000,"High","Low")

table(csvDataFrame$lowHighVar)
# Practice 7

returnSubset2 <- function(dataFrameName,columnNames,thrs)
{
  outDataFrame2 <- dataFrameName[,columnNames]
  outDataFrame2 <- outDataFrame2[outDataFrame2$TOS > thrs, ]
  outDataFrame2$lowHighVar <- NA
  outDataFrame2$lowHighVar <- ifelse(outDataFrame2$TOS > thrs,"High","Low")
 
  outlist <- list()

  for(column in colnames(outDataFrame2))
  {
    outlist[[column]] <- unique(outDataFrame2[,column])

  }
  return(outlist)
}


returnDf2 <- returnSubset2(csvDataFrame,c("TOS","DayOfWeek","TimeofDay"),5000)

str(returnDf2)

##############################################################################
# Section 8: String Manipulation								             #
##############################################################################
# stringr Packages for string manipulation
install.packages("stringr")
library(stringr)

# Count a pattern in a string
sampleStrings <- c("Optus","Grainger","Hilton","SearsHS","SearsOnline","SearsPD")
str_count(tolower(sampleStrings),"sears")

# Length of string
str_length(sampleStrings)

# Length of string when NA
sampleStrings01 <- c("Optus","Grainger","Hilton","SearsHS","SearsOnline","SearsPD",NA)
str_length(sampleStrings01)


# Extract a pattern
str_extract(sampleStrings,"[A-Z]+")
str_extract_all(sampleStrings,"[A-Z]+")

sampleStrings02 <- c("OPtus","GRainger","Hilton","SearsHS","SearsOnline","SearsPD",NA)
str_extract(sampleStrings02,"[A-Z]+")

str_extract(sampleStrings,"[A-Z a-z]+")

# Location of a pattern
str_locate(sampleStrings,"S")
str_locate_all(sampleStrings,"S")

# Replace a pattern
str_replace(sampleStrings,"[aeiou]","_")
str_replace_all(sampleStrings,"[aeiou]","_")

# Split a string
sampleStrings1 <- c("Optus and Grainger and Hilton","SearsHS and SearsOnline and SearsPD")
str_split(sampleStrings1," ")

# Pipe - Match one of two patterns
str_extract(sampleStrings,"Sears|Hilton")
str_extract(sampleStrings,"Sears.*|Hilton.*")

# Positive Lookahead - Match multiple patterns (AND)
sampleStrings3 <- c("Optus and Grainger and Hilton","SearsHS","SearsOnline","Sears HS and SearsOnline and SearsPD")
str_extract_all(sampleStrings2,"(^Sears(?=.*HS)(?=.*Online)).*")

# Negative Lookahead - Match multiple patterns (AND)
sampleStrings4 <- c("Optus and Grainger and Hilton","SearsHS","SearsOnline","Sears HS and SearsOnline and SearsPD")
str_extract_all(sampleStrings2,"(^Sears(?!.*HS)(?=.*Online)).*")

# Matching special characters
sampleStrings <- c("Optus?","Grainger*","Hilton","SearsHS","SearsOnline","SearsPD")
str_match(sampleStrings,"[\\?\\*]")

?regexp

##############################################################################
# Section 9: Statistical Modeling and Data Mining				             #
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
dim(testData)

# Logistic Regression Model
# Build model with Training Data 
lrModel <- glm(PURCHASE_FLAG ~ tosBinned + SMARTPHONE + device_details,data = trainingData,family = binomial)
lrModel
summary(lrModel)

# Prediction for the Test data
testPrediction <- predict(lrModel,testData,type = "response")
testPrediction
str(testPrediction)

hist(testPrediction)

# ROC Curves
library(Epi)
ROC( form = testData$PURCHASE_FLAG ~ testPrediction, plot="ROC")

# Practice Model Building

bins <- c(0,2,4,6,8,10,12,20,Inf)
csvDataFrame$nopBinned1 <- cut(csvDataFrame$NOP,bins,dig.lab = 10,include.lowest = TRUE)
table(csvDataFrame$nopBinned1)

lrModel <- glm(PURCHASE_FLAG ~ nopBinned1 + Browser + OS + EXTERNAL_SEARCH_FLAG,data = trainingData,family = binomial)
lrModel
summary(lrModel)

testPrediction1 <- predict(lrModel,testData,type = "response")
testPrediction1
str(testPrediction)
hist(testPrediction)

ROC( form = testData$PURCHASE_FLAG ~ testPrediction, plot="ROC")


#check confusion matrix on wikipedia

