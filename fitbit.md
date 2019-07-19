---
title: "fitbit Nike"
author: "Manes Munyanyi"
date: "18 July 2019"
output: html_document
---

##Preparing the R environment

###Setting echo equal to TRUE and results equal to 'hold' as global options for this document so that someone else will be able to read the code.

```{r libraries}
#Loading knitr library and setting echo equal to true
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
#Loading other required libraries
library(data.table)
# ggplot2 is the the packadge of choice for plotting figures
library(ggplot2) 
```

#Synopsis
##The work is based on data from a personal activity monitoring device. The device collects data at a 5 minute interval throughout the day. The data is for an anonymous individual whaich was collected over a two month period of October and November in 2012.
##The objectives are to preprocess the data for analysis, plot graphs based number of steps done per 5 min interval, calculate the mean and median as well as add missing values

##Loading and preprocessing the data
###The data is loaded and preprocessed befor the actual processing. This includes reading the .csv file, formating the date among others

```{r preprocess}

#Reading data in actvity.csv and Transforming data to a format suitable for your analysis
ActivityData <- read.csv('activity.csv', header = TRUE, sep = ",", colClasses=c("numeric", "character", "numeric"))

#The following set of code converts the date field to Date class and interval field to Factor class.
ActivityData$date <- as.Date(ActivityData$date, format = "%Y-%m-%d")
ActivityData$interval <- as.factor(ActivityData$interval)

#Checking the structure of data after preprocessing
head(ActivityData)
```

###Calculating the mean total number of steps day

```{r meanTotal}

dataStepsDays <- aggregate(steps ~ date, ActivityData, sum)
colnames(dataStepsDays) <- c("date","steps")
head(dataStepsDays)
```

###Plotting a histogram of the total number of steps taken per day, plotted with appropriate bin interval

```{r histogram}
#Plotting the histogram
ggplot(dataStepsDays, aes(x = steps)) + 
       geom_histogram(fill = "brown", binwidth = 750) + 
        labs(title="Steps Taken per Day", 
             x = "Number of Steps per Day", y = "No of times per day")

```

##Calculating mean and median of number of steps taken per day

```{r meanmedian}
#calculating mean
meanSteps   <- mean(dataStepsDays$steps, na.rm=TRUE)
meanSteps
#calculating median
medianSteps <- median(dataStepsDays$steps, na.rm=TRUE)
medianSteps
```

##Calculating the average daily activity pattern?
###This is achieved by aggregating the steps by at 5-minute interval and assigning the value to variable which is a data frame

```{r intervalsteps}
dataStepsIntMean <- aggregate(ActivityData$steps, 
                                by = list(interval = ActivityData$interval),
                                FUN=mean, na.rm=TRUE)
#converting values to integers
dataStepsIntMean$interval <- 
        as.integer(levels(dataStepsIntMean$interval)[dataStepsIntMean$interval])
colnames(dataStepsIntMean) <- c("interval", "steps")

#Plotting time series of the average number of steps
ggplot(dataStepsIntMean, aes(x=interval, y=steps)) +   
        geom_line(color="orange", size=1) +  
        labs(title="Mean Daily Activity Pattern", x="Interval", y="No of steps") 
```

##Finding the 5-minute interval with the maximum number of steps

```{r maximum}
maxStepsInterval <- dataStepsIntMean[which.max(  
        dataStepsIntMean$steps),]
maxStepsInterval
```

##Fixing missing values:
###First Count missing values: Accomplished by using is.na() to check whether the value is missing then using sum to find the total missing values

```{r missing}
totalMissingValues <- sum(is.na(ActivityData$steps))
totalMissingValues
```

###Lastly, Replace all of the missing values in the dataset with the mean value at the same interval across days. This is accomplished by introducing a function where the data argument is ActivityData and pervalue argument is the dataStepsIntMean

```{r replace}
naReplace <- function(data, pervalue) {
        naIndex <- which(is.na(data$steps))
        naSub <- unlist(lapply(naIndex, FUN=function(idx){
                interval = data[idx,]$interval
                pervalue[pervalue$interval == interval,]$steps
        }))
        stepsReplace <- data$steps
        stepsReplace[naIndex] <- naSub
        stepsReplace
}
#filling of missing data by use of function
ActivityDataReplaced <- data.frame(  
        steps = naReplace(ActivityData, dataStepsIntMean),  
        date = ActivityData$date,  
        interval = ActivityData$interval)
head(ActivityDataReplaced)
```

###Checking for any missing values not input

```{r checkmissing}
sum(is.na(ActivityDataReplaced$steps))
#Zero output means no missing values
```

###Histogram of the total number of steps taken each day after replacing missing values

```{r histAftermissingValue}
replaceDataStepsDays <- aggregate(steps ~ date, ActivityDataReplaced, sum)
colnames(replaceDataStepsDays) <- c("date","steps")
##plotting the histogram

ggplot(replaceDataStepsDays, aes(x = steps)) + 
       geom_histogram(fill = "brown", binwidth = 750) + 
        labs(title="Steps Taken per Day", 
             x = "No of Steps per day", y = "No of times in a day") 
```

##Calculating the mean and median after replacing missing values

```{r recalcMeanMedian}
#Calculating the mean number of steps per day
meanStepsReplaced   <- mean(replaceDataStepsDays$steps, na.rm=TRUE)
meanStepsReplaced
#Calculating the median number of steps per day
medianStepsReplaced <- median(replaceDataStepsDays$steps, na.rm=TRUE)
medianStepsReplaced
```

###The values for the initial mean and median calcualtions do differ slightly from the recalculated values after repalcing the missing values
###After replacing the missing values, the mean and median are equal
###Replacing the missing values has changed only the median and the mean remained the same

###The peak has slightly skewed but not necessarily changed the observation hence the replacement of the missing values did not affect interpretation of the results


##Activity patterns between weekdays and weekends
###This is achieved by:
###First, Augmenting the data table with a column that indicates the day of the week
###Secondly, Subseting the data table into two sections namely weekdays and weekends.
###Thirdly, Put in a table the average steps per interval for each data set.
###Lastly, Plot the two charts one for weekdays and the other other fro weekends

```{r weekDaysEnd}
stepsDays <- function(data) {
    stepsDays <- aggregate(data$steps, by=list(interval = data$interval),
                          FUN=mean, na.rm=T)
    # convert to integers for plotting
    stepsDays$interval <- 
            as.integer(levels(stepsDays$interval)[stepsDays$interval])
    colnames(stepsDays) <- c("interval", "steps")
    stepsDays
}

dataPerWeekdays <- function(data) {
    data$weekday <- 
            as.factor(weekdays(data$date)) # weekdays
    weekendActivityData <- subset(data, weekday %in% c("Saturday","Sunday"))
    weekdayActivityData <- subset(data, !weekday %in% c("Saturday","Sunday"))
    
    stepsWeekend <- stepsDays(weekendActivityData)
    stepsWeekday <- stepsDays(weekdayActivityData)

    stepsWeekend$dayofweek <- rep("weekend", nrow(stepsWeekend))
    stepsWeekday$dayofweek <- rep("weekday", nrow(stepsWeekday))

    dataPerweekdays <- rbind(stepsWeekend, stepsWeekday)
    dataPerweekdays$dayofweek <- as.factor(dataPerweekdays$dayofweek)
    dataPerweekdays
}

dataWeekdays <- dataPerWeekdays(ActivityDataReplaced)
```

##Plots of average number of steps per 5 minute interval by Weekday and Weekend
```{r plotDayEnd}
ggplot(dataWeekdays, aes(x=interval, y=steps)) + 
        geom_line(color="brown") + 
        facet_wrap(~ dayofweek, nrow=2, ncol=1) +
        labs(x="Interval", y="Number of steps")
```
