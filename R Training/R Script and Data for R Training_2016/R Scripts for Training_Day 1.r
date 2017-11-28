##############################################################################
# R Session - Day 1 Scripts						                             #
# Date:  05/12/2016									                         #
# Author: Ravishankar Rajagopalan							                 #	
##############################################################################

##############################################################################
# Section 1: Getting Started with R 						                 #
##############################################################################

# Setting working directory
getwd()
setwd("D:\\R\\R Training\\R Script and Data for R Training_2016")
getwd()

# Installing Packages
install.packages("readr")

# Using a package
library(readr)

# Practice 0
# Install and load the following packages:
# Epi,readxl,RODBC,sqldf,plyr,stringr
install.packages("Epi")
install.packages("readxl")
install.packages("RODBC")
install.packages("sqldf")
install.packages("plyr")
install.packages("stringr")




# R Help
?matrix
help(matrix)

##############################################################################
# Section 2: Data Types in R 	       						                 #
##############################################################################

# Scalar 
x <- 1

# Vector
y <- c(1,2,3,4,5)
y[1]

# Matrix
z <- matrix(c(1,2,3,4,5,6,7,8,9,10),nrow = 5, ncol = 2)
str(z)
z[1,1]
z[1,]
z[,1]

# Data Frame
df <- data.frame(y1 = y, y2 = y, y3 = c("a","b","c","d","e"))
str(df)
df$y1 # $ operator is very powerful and often used
head(df)
tail(df)

# List
list1 <- list(x = x, y = y, z = z, df = df)
str(list1)
list1[[1]]
mat1 <- list1[[3]]
df1 <- list1[[4]]

# Practice 1

##############################################################################
# Section 3: Data Input/Output       						                 #
##############################################################################

# Reading a Text File
textDataFrame <- read.table("optusDataText.txt",header = TRUE,sep = "\t",comment.char = "") 

# To read larger text files...look in to readr package

# Reading a CSV File
csvDataFrame <- read.csv("optusDataCSV.csv") 

# Reading a large CSV File
startTime <- Sys.time()
largeCSV <- read.csv("optusLargeData.csv",fill=TRUE,header = TRUE,sep = "\t",comment.char = "") 
Sys.time()-startTime

library(readr)
startTime <- Sys.time()
fastCSV <- read_csv("optusLargeData.csv") 
Sys.time()-startTime


# Reading large Excel sheets
library(readxl)
excelData <- read_excel("optusDataXLSX.xlsx")

# Reading directly from Vertica
# Step 1: Download and install Vertica ODBC Client
# Step 2: Configure ODBC source
# Step 3: Install RODBC Package

library(RODBC)
myconn <-odbcConnect("Vertica")
queryResult <- sqlQuery(myconn, "select * idm.ALLIANZ_JOINED_VIEW_V_3_1")

# Practice 2

query <- paste("select browser,count(*)", 
               "from simod.OPTUS_STATIC_DATA_v_2_1",
			   "group by 1")
testDataFrame <- sqlQuery(myconn, query)

# Practice 3


# Writing to a CSV File
write.csv(csvDataFrame,"csvDataOutput.csv") 

##############################################################################
# Section 4: Data Manipulation       						                 #
##############################################################################
# Extracting Columns from a data frame
newDataFrame1 <- csvDataFrame[,c(1,2)]
str(newDataFrame)
colnames(csvDataFrame1)
newDataFrame2 <- csvDataFrame[,c("browser_session_id","Referrer")]
str(newDataFrame2)
newDataFrame <- subset(csvDataFrame,select = c("browser_session_id","Referrer"))
str(newDataFrame3)

# Adding/deleting a column
newDataFrame$newColumn <- csvDataFrame$LandingPage
str(newDataFrame)
newDataFrame$newColumn <- NULL

# Selecting Rows
unique(csvDataFrame$Browser)
table(csvDataFrame$Browser)
newDataFrameRows1 <- csvDataFrame[csvDataFrame$Browser == "MSIE", ]
nrow(newDataFrameRows1)
newDataFrameRows2 <- subset(csvDataFrame, Browser == "MSIE")
nrow(newDataFrameRows2)

# Selecting Rows and Columns
newDataFrameRows3 <- csvDataFrame[(csvDataFrame$Browser == "MSIE" | csvDataFrame$Browser == "Chrome"),c("Browser","OS")]
str(newDataFrameRows3)
newDataFrameRows4 <- subset(csvDataFrame, (Browser == "MSIE" | Browser == "Chrome"),select = c("Browser","OS"))
str(newDataFrameRows4)

# Combining Dataframes
#Column wise
smallColumnDataFrame1 <- csvDataFrame[,c("Browser","OS")] 
str(smallColumnDataFrame1)
smallColumnDataFrame2 <- csvDataFrame[,c("TOS","NOP")]
str(smallColumnDataFrame2)
newDataFrame <- cbind(smallColumnDataFrame1,smallColumnDataFrame2)

#Row wise
smallRowDataFrame1 <- subset(csvDataFrame,EXTERNAL_SEARCH_FLAG == 1)
dim(smallRowDataFrame1)
unique(smallRowDataFrame1$EXTERNAL_SEARCH_FLAG)
smallRowDataFrame2 <- subset(csvDataFrame,EXTERNAL_SEARCH_FLAG == 0)
dim(smallRowDataFrame2)
unique(smallRowDataFrame2$EXTERNAL_SEARCH_FLAG)
newDataFrame1 <- rbind(smallRowDataFrame1,smallRowDataFrame2)

# Merging two dataframes
csvDataFrame$uniqueid <- seq(1:nrow(csvDataFrame))
smallDataFrame1 <- csvDataFrame[,c("uniqueid","OS","Browser")]
smallDataFrame2 <- csvDataFrame[,c("uniqueid","TOS","NOP")]
mergedDataFrame <- merge(smallDataFrame1,smallDataFrame2,by = "uniqueid")

# Practice 4

# Try sqldf package
sqlDataFrame <- sqldf('select t1.uniqueid,OS,Browser,TOS,NOP from smallDataFrame1 t1 INNER JOIN smallDataFrame2 t2 ON t1.uniqueid=t2.uniqueid')

# Practice 5 - Repeat Practice 4 using sqldf

# Sorting a Data Frame
head(csvDataFrame[,c("DayOfWeek","TimeofDay")])
sortedData <- csvDataFrame[order(csvDataFrame$DayOfWeek,csvDataFrame$TimeofDay),]
head(sortedData[,c("DayOfWeek","TimeofDay")])

# Renaming columns
smallDataFrame1 <- csvDataFrame[,c("uniqueid","OS","Browser")]
csvDataFrame$uniqueid <- seq(1:nrow(csvDataFrame))
smallDataFrame1 <- csvDataFrame[,c("uniqueid","OS","Browser")]
names(smallDataFrame1)[1] <- "ID"


##############################################################################
# Section 5: Functions and Control Statements       						 #
##############################################################################

# Functions in R

returnSubset <- function(dataFrameName,columnNames)
{
  outDataFrame <- dataFrameName[,columnNames]
  outDataFrame <- outDataFrame[outDataFrame$TOS > 1000, ]
  return(outDataFrame)
}

returnDf <- returnSubset(csvDataFrame,c("TOS","DayOfWeek","TimeofDay"))

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




