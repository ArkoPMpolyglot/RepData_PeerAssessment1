#Code for doing the project
activity <- read.csv('activity.csv')
#total number of missing data
sum(is.na(activity$steps))/dim(activity)[[1]]
#transform date column using the lubridate
library(lubridate)
activity$date <- ymd(activity$date)
length(unique(activity$date))
#we counted the no of unique dates above

#Histogram of total number of steps taken each day
library(ggplot2)
P2 <- data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
P2$date <- rownames(P2)
rownames(P2) <- NULL
names(P2)[[1]]<-"Total Steps"
png("plot1.png")
#Total Steps by date bar chart
ggplot(P2,aes(y='Total Steps',x=date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
dev.off()
ggplot(P2,aes(y='Total Steps',x=date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
#Histogram of total steps
qplot('Total Steps' ,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
png("plot1.1.png")
qplot('Total Steps',geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
dev.off()
#Q3 Calculating the mean and median of steps taken each day
activity <- read.csv("activity.csv")

activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
activity_total_steps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(activity_total_steps) <- c("date", "steps")
mean(activity_total_steps$steps) ## Result 9354.23
median(activity_total_steps$steps) ## Result 10395
# Time series plot of the average number of steps taken
average_daily_activity <- aggregate(steps~interval, data=activity, mean,na.rm=TRUE)
plot(steps~interval, data=average_daily_activity, type="l")
png("TimeSeries.png")

dev.off()
##Which 5 minute interval has the maximum number of steps
intervalWthMaxSteps <- average_daily_activity[which.max(average_daily_activity$steps),]$interval
intervalWthMaxSteps ## Ans 835
#Total missing Values
totalValuesMissings <- sum(is.na(activity$steps))
totalValuesMissings ## 2304
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
#Making a histogram and saving it
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
#Calculating the mean and median steps perday with no NA
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps) #10766.19
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps) #10766.19

#Are there any activity differences between weekdays and weekends
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





