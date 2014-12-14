---
title: "Reproducible Research: Peer Assessment 1"
author: "Kristine Beckers"
date: "December 11, 2014"
output: 
  html_document:
    keep_md:true
---

# Reproducible Research: Peer Assessment 1

## Synopsis

This is a report that uses the data in the activity.zip file, which contains - 
once unzipped - the file activity.csv. This .zip file is present in this Github 
Repository, make sure you unzip it in your working directory before running the 
program.
This file contains personal activity monitoring data collected at 5 minute 
intervals throughout the day during a period of 2 months (Oct. and Nov. 2012) 
from an anonymous individual and includes the number of steps taking in 5 minute 
intervals each day.


```r
sessionInfo()
```

```
## R version 3.1.1 (2014-07-10)
## Platform: x86_64-apple-darwin13.1.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] lattice_0.20-29   plyr_1.8.1        stringr_0.6.2     knitr_1.8        
## [5] R.utils_1.34.0    R.oo_1.18.0       R.methodsS3_1.6.1
## 
## loaded via a namespace (and not attached):
## [1] evaluate_0.5.5 formatR_1.0    grid_3.1.1     markdown_0.7.4
## [5] mime_0.2       Rcpp_0.11.3    tools_3.1.1    yaml_2.1.13
```

## Loading and preprocessing the data

- Loading the data  


```r
## check if working file exists in working directory, if not, download it
## and unzip
if(!file.exists("./activity.csv")){
    fileUrl = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl, "temp.zip", method ="curl", mode="wb")
    unzip ("temp.zip")
}
## read the data file into a data frame named actdf (activity data frame)
actdf<-read.csv("./activity.csv", header = TRUE, stringsAsFactors=FALSE)
```

- Changing the date column to a date format and change the interval to time
for reference purposes as we keep the intervals as the values for the x-axis 
in ts plots.


```r
## load required packages if not installed already
if(!require(stringr)){
    install.packages("stringr")
    library(stringr)
}
## date needs to be changed into a proper date class
actdf$date<-as.Date(actdf$date, "%Y-%m-%d")
## changing the interval to a proper time class in newtime
## this could be used in ts plots later
actdf$time<-as.character(actdf$interval)
actdf$time<-str_pad(actdf$time, width=4, side="left",pad="0")
actdf$time<-str_c(str_sub(actdf$time,1,2),":",str_sub(actdf$time,-2))
actdf$newtime<-strptime(paste(actdf$date, actdf$time, sep= " "), "%Y-%m-%d %H:%M")
```

## What is mean total number of steps taken per day?  

For the first part of this report the missing values are ignored.  



```r
## load required packages if not installed already
if(!require(stats)){
    install.packages("stats")
    library(stats)
    }
if(!require(plyr)){
    install.packages("plyr")
    library(plyr)
    }
## find the total of steps by day and store them in a dataframe 
## named totalbyday. I used aggregate which assumes according to 
## https://stat.ethz.ch/R-manual/R-devel/library/stats/html/aggregate.html
## that "The default is to ignore missing values in the given variables."
totalbyday<-aggregate(actdf$steps, by = list(date=actdf$date), FUN = sum)
colnames(totalbyday)[2]<-"totalSteps"

## calculate the mean and median of the total steps over the 61 days
mean <- round(mean(totalbyday$totalSteps,na.rm = TRUE),0)
median <- median(totalbyday$totalSteps,na.rm = TRUE)
## plot a histogram of the total steps by day and include the mean/median values
## I chosen 20 breaks as that gives a clear indication to what happens with the 
## data after we manipulated the data in the second histogram.
hist(totalbyday$totalSteps, main = "Histogram of total number of steps taken 
     each day", xlab = "total steps taken each day", col = "green", breaks =20)
abline(v=mean, lwd = 5, col = "blue")
abline(v=median, lwd = 2, col = "red")
```

![plot of chunk totalSteps](figure/totalSteps-1.png) 

We calculate and report the mean and median total number of steps taken per day, after 
calculating the mean number of steps per day, which is depicted in green on the histogram.
The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>and is depicted in blue.
The median total number of steps taken per day is 10765 and is depicted in red.  
The mean and median values are not significantly different, which means that the data is 
evenly divided around the mean. 



## What is the average daily activity pattern?

Now we are also curious as to what the average daily activity pattern looks like. We 
therefore calculate the averages of the different intervals across all days. The result 
is displayed in the following time series plot of the 5-minute interval (x-axis) 
and the average number of steps taken, averaged across all days (y-axis)


```r
## calculating the avg by interval over the number of days
avgbyinterval<-aggregate(actdf$steps, 
                         by = list(interval=actdf$interval), 
                         FUN = mean, na.rm=TRUE)
colnames(avgbyinterval)[2]<-"avgSteps"

## plotting a ts plot, initially was going to use the newdate variable, but the 
## example in the course just uses interval and ask for interval, so I kept it 
## simple by just plotting the intervals over the average number of steps
par(mgp=c(2,0.5,0))
plot(avgbyinterval$interval, avgbyinterval$avgSteps,type = "l", 
     main="Time Series plot \n of average number of steps taken per 5-minute time interval", 
     xlab= "5-minute interval", ylab = "Average number of steps taken \n averaged over each day" )
```

![plot of chunk averageByInterval](figure/averageByInterval-1.png) 



```r
## what is the interval with the most average steps?
intervalmoststeps<-avgbyinterval[which.max(avgbyinterval$avgSteps),1]
```

The 5-minute interval, on average across all the days in the dataset, which contains
the maximum number of steps is 835.


## Imputing missing values

In the dataset there are a number of days/intervals where there are missing values
(coded as NA). NA presence of missing days may introduce bias into some calculations 
or summaries of the data.


```r
## calculate the number of records without a value
## 288 intervals in a day, daysmissing indicates how many days the data is missing for
numna<-sum(is.na(actdf$steps))
daysmissing<- numna/nrow(avgbyinterval)
```

There are 2304 records with missing values in the dataset, which means that no data was 
recorded during roughly 8 of days.

That is a significant number of missing values. In order to get a more correct representation
of the data, we will create a new dataset in which we will replace all of the missing values 
with the mean value for that interval.


```r
## merging of two tables by interval to create a table which has the avg by
## interval in its columns, I temporarily created an id column to the original 
## data frame, in order to get the same ordered file after merging
## this column gets dismissed after use as it is redundant
actdf$id<-1:nrow(actdf)
newactdf<-merge(actdf, avgbyinterval, by = "interval")
newactdf<-newactdf[order(newactdf$id),]
actdf$id<-NULL
newactdf$id<-NULL

## checking the value of steps in each row and if it is equal to NA, 
## replace it by the avgSteps value, which is the avg number of steps
## by interval for that given interval 
for (i in 1:nrow(newactdf)){
  if (is.na(newactdf[i,"steps"])){
   newactdf[i,"steps"]<-newactdf[i,"avgSteps"]
  }
}
```



```r
## with new values in place, repeat the steps from the beginning by 
## calculating the new total by day and plotting it in a histogram
## to compare with the original one
newtotalbyday<-aggregate(newactdf$steps, by = list(date=newactdf$date), FUN = sum)
colnames(newtotalbyday)[2]<-"totalSteps"
mean <- round(mean(newtotalbyday$totalSteps,na.rm = TRUE),0)
median <- round(median(newtotalbyday$totalSteps,na.rm = TRUE),0)
hist(newtotalbyday$totalSteps, main = "Histogram of total number of steps taken 
     each day", xlab = "total steps taken each day", col = "green", breaks =20)
abline(v=mean, lwd = 5, col = "blue")
abline(v=median, lwd = 2, col = "red")
```

![plot of chunk newtotalSteps](figure/newtotalSteps-1.png) 


We calculate and report the mean and median total number of steps taken per day, after 
calculating the mean number of steps per day, which is depicted in green on the histogram.
The mean total number of steps taken per day is 1.0766 &times; 10<sup>4</sup>and is depicted in blue.
The median total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> and is depicted in red.  
The mean and median values are not significantly different, which means that the data is 
evenly divided around the mean, once again. 

What is different however is the frequency of the bin where the mean and median are situated. 
It has increased by 8 which is logical, as there were 8 days where the data were missing. As 
we have replaced the data with the mean values, it means that the mean bin has increased by 
this amount of days.


## Are there differences in activity patterns between weekdays and weekends?

To find out if there is any difference in activity pattern between the weekdays
and weekends, we added a factor variable to the dataset with two levels --"weekday" 
and "weekend". Based on this variable, we recalculated the total number of steps 
taken, averaged across all weekday days or weekend days. 




```r
## Still working on the new data frame, we determine which days the dates 
## represent and then classify them by either "weekday" or "weekend" in a new
## factor variable called weekendweek
newactdf$day<-weekdays(newactdf$date)
newactdf$weekendweek<-rep(1:2,nrow(newactdf)/2)
newactdf$weekendweek<-factor(newactdf$weekendweek, labels=c("weekday","weekend"))

for (i in 1:nrow(newactdf)) {
        if(newactdf[i,"day"] == "Saturday") {
                newactdf[i,"weekendweek"] <- "weekend"
        } else {
                 if(newactdf[i,"day"] == "Sunday") {
                         newactdf[i,"weekendweek"] <- "weekend"
                         }
                 else {
                         newactdf[i, "weekendweek"] <- "weekday"
                 }
               
        }
}
```

The panel plot below contains a time series plot of the 5-minute interval(x-axis) 
and the average number of steps taken, averaged across all weekday days or weekend days
(y-axis). 



```r
## install lattice if not installed yet
if(!require(lattice)){
    install.packages("lattice")
    library(lattice)
}

## we subset the dataframe and find the new means for weekdays and weekend
## by use of the ddply function
abiww<-ddply(newactdf, c("interval","weekendweek"), summarize, abi = mean(steps,na.rm = TRUE))
## plot the outcomes of the new means into a panel plot which makes comparing both values 
## between the two levels "weekend" and "weekday" very easy
xyplot (abiww$abi~abiww$interval | abiww$weekendweek,layout = c(1,2), type='l', 
        xlab="5-minute interval", 
        ylab = "Average number of steps taken \n averaged over each day")
```

![plot of chunk plotbyweekday](figure/plotbyweekday-1.png) 

From this plot you can conclude, that during weekdays, activity increased earlier in 
the day, with a peak when this individual probably prepared for the day, with a 
decline duringnormal working hours, a slight peaking again around dinner time and a steadying off at night.
The weekends looked slightly different on average, the data increased later in the day, with more peaks throughout the day, which probably indicates to an active lifestyle. 
