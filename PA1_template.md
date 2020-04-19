---
title: "Reproducible Research - Course Project 1"
author: "Luccas Betton"
date: "18/04/2020"
output: 
  html_document: 
    keep_md: yes
---



## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## 1. Load the Data
If the file do not exist in the working directory it's downloades. After that, the CSV file is read in the rawdata variable.


```r
filename1 <- "./repdata_data_activity/activity.csv"

if(!file.exists(filename1)){
    zipname <- "repdata_data_activity.zip"
    
    if(!file.exists(zipname)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileURL, destfile = zipname, method = "curl")
    }
    
    directory <- "./repdata_data_activity"
    if(!dir.exists(directory)) dir.create(directory)
    unzip(zipname, exdir = directory)
}
rawdata <- read.csv(filename1)
```

## 2. Processing Data

Libraries necessary for analysis


```r
library(dplyr)
library(ggplot2)
library(knitr)
```

Process raw data to a tidy data. Convert intervals in date time and date column to date format.


```r
tidydata <- rawdata
#Convert intervals to format HH:MM
tidydata <- mutate(tidydata, Time = sprintf("%04d",tidydata$interval))
tidydata$Time <- gsub("(..)(..)", "\\1:\\2", tidydata$Time) 
# Convert date column to date format
tidydata$date <- as.Date(tidydata$date)
```

## 3. Results

- **Number of steps taken each day**


```r
tidydata <- group_by(tidydata,date) #Group by date
Stepday <- summarise(tidydata,StepsSum = sum(steps)) #Sum of steps per day
tidydata <- ungroup(tidydata) #Ungroup data

meansteps <- mean(Stepday$StepsSum,na.rm = TRUE)
mediansteps <- median(Stepday$StepsSum,na.rm = TRUE)

ggplot(Stepday,aes(x = StepsSum))+geom_histogram(binwidth = 5000,colour = "black",fill = "red")+theme_bw(base_size = 15)+xlab("Steps per day")+ylab("Frequency")+ggtitle("Qty of Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The mean of steps per day is 10766 and the median is 10765.

- **Average Daily Activity Pattern**


```r
tidydata <- group_by(tidydata,Time) # Group by day time
Steptime <- summarise(tidydata,StepsMean = mean(steps,na.rm = TRUE)) #Mean of steps in each day time
tidydata <- ungroup(tidydata) #Ungroup data

#Graph Plot
ggplot(Steptime,aes(x=Time,y=StepsMean,group = 1))+geom_line()+geom_hline(yintercept = mean(Steptime$StepsMean),colour = "red",lwd = 1)+theme_bw(base_size = 15)+labs(x ="Day Time", y="Qty of Steps",title = "Steps during day")+scale_x_discrete(breaks = Steptime$Time[seq(1,288,24)])
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#Calculate Maximum Mean Steps from the 5-Minutes Interval 
MaxStepInterval <- paste(Steptime$Time[which.max(Steptime$StepsMean)],Steptime$Time[which.max(Steptime$StepsMean)+1],sep = " - ")
```

The 5-minutes interval with higher activity during the day is 08:35 - 08:40 in average.

## Impute Missing Values

The missing values from tidy data are replaced by the mean for its respective 5-minutes interval.


```r
#Remove NA Values from tidy data
tidydata_NNA <- tidydata
NAPos <- which(is.na(tidydata_NNA$steps)) #Position of NA values in tidy data
NAPos_Time <- tidydata_NNA$Time[NAPos] #Time interval of each NA Vaue
NAPos_Time2 <- match(NAPos_Time,Steptime$Time) #Mean of steps for each NA value
tidydata_NNA[NAPos,1] <- Steptime$StepsMean[NAPos_Time2] #Tidy data without NA values
```

Calculate the new mean and median after replacement of NA values.


```r
tidydata_NNA <- group_by(tidydata_NNA,date) #Group by date
Stepday_NNA <- summarise(tidydata_NNA,StepsSum = sum(steps)) #Sum for each day
tidydata_NNA <- ungroup(tidydata_NNA) #Ungroup data

#Graph plot
ggplot(Stepday_NNA,aes(x = StepsSum))+geom_histogram(binwidth = 5000,colour = "black",fill = "red")+theme_bw(base_size = 15)+xlab("Steps per day")+ylab("Frequency")+ggtitle("Qty of Steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
meansteps_NNA <- mean(Stepday_NNA$StepsSum)
mediansteps_NNA <- median(Stepday_NNA$StepsSum)
```

The total missing values are 2304. The mean of steps per day is 10766 and the median is 10766. There is an insignificant change in the median, and no modification in the calculated mean after the NA replacement.

- **Activity patterns between weekdays and weekends**

Adding factor for data in two levels: weekday and weekend. It will be used the tidy data with NA values.


```r
tidydata_Week <- mutate(tidydata, Week = as.POSIXlt(tidydata$date)$wday) #Weekday for each measurement
tidydata_Week <- mutate(tidydata_Week, Weekday = ifelse(Week<5,"Weekday","Weekend")) #Factor with two levels: Weekday or Weekend

tidydata_Week <- group_by(tidydata_Week,Weekday,Time) #Group by day time
StepWeekday <- summarise(tidydata_Week,StepsMean = mean(steps,na.rm = TRUE)) #Calculate mean of steps by factor Weekday and day time

#Graph Plot
ggplot(StepWeekday,aes(x=Time,y=StepsMean,group = Weekday))+geom_line()+facet_grid(Weekday~.)+theme_bw(base_size = 15)+labs(x ="Day Time", y="Qty of Steps",title = "Steps during day - Weekday vs Weekend")+scale_x_discrete(breaks = StepWeekday$Time[seq(1,288,24)])
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Processing R markdown file.


