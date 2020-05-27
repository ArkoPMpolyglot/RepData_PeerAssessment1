---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

`
`
```


## What is mean total number of steps taken per day?

```r
activity <- read.csv("activity.csv")

activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
mean(activity_total_steps$steps)
```

```
## [1] 9354.23
```



## What is the average daily activity pattern?
We do a time series plot of the average daily activity pattern 

```r
average_daily_activity <- aggregate(steps~interval, data=activity, mean,na.rm=TRUE)
plot(steps~interval, data=average_daily_activity, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
png("TimeSeries.png")
```



## Imputing missing values
We replace the missing values with mean value of the interval and putting in the place of missing values

```r
#We replace the missing values with mean of the interval
getMeanaverage_daily_activity<-function(interval){
  average_daily_activity[average_daily_activity$interval==interval,]$steps
}
# Create a new dataset which is same as original data set replaced 
#mean with missing values
activityDataNoNA<-activity
for(i in 1:nrow(activityDataNoNA)){
  if(is.na(activityDataNoNA[i,]$steps)){
    activityDataNoNA[i,]$steps <- getMeanaverage_daily_activity(activityDataNoNA[i,]$interval)
  }
}
```
We also then do a histogram and saved it in Hist.NoNA.png


## Are there differences in activity patterns between weekdays and weekends?

```r
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
  if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
    activityDataNoNA[i,]$day<-"weekend"
  }
  else{
    activityDataNoNA[i,]$day<-"weekday"
  }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
#Making a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days axis)
#y-axis
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
We see on weekends the activity remains high and same throughout whereas in case in weekdays it remains very high at particular interval and then falls
