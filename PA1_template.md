---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
move <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

move$date <- as.Date(move$date, format = "%Y-%m-%d")

move$interval <- factor(move$interval)

NA_index <- is.na(as.character(move$steps))

move_no_NA <- move[!NA_index,]

## What is mean total number of steps taken per day?
stepsday <- aggregate(steps ~ date, data = move_no_NA, sum)

colnames(stepsday) <- c("date", "steps")

hist(as.numeric(stepsday$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

mean(stepsday$steps)

# 10766.19

## What is the average daily activity pattern?
stepsinterval <- aggregate(move_no_NA$steps, by=list(interval=move_no_NA$interval), FUN=mean)

colnames(stepsinterval) <- c("interval", "average_steps")

plot(as.integer(levels(stepsinterval$interval)), stepsinterval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

meantable <- data.frame(stepsinterval$average_steps, as.integer(levels(stepsinterval$interval)))

meantable[meantable$stepsinterval.average_steps==max(meantable$stepsinterval.average_steps),][2]

# 8:35-8:40 is the interval

## Imputing missing values
# this imputation makes a copy of the data, then fills in the missing data points
# by matching the NA's with our average steps per interval calculation
sum(is.na(move$steps))

sum(is.na(move$steps))/length(stepsinterval$average_steps)

move2 <- move

move2$steps<- ifelse(is.na(move$steps),stepsinterval$average_steps[match(move$interval,stepsinterval$interval)],move$steps)

fullstepsperday<- aggregate(move2$steps, list(move2$date), FUN=sum)

hist(fullstepsperday$x, xlab = "Steps Per Day", main = "Histogram of Frequency of Steps per Day")

mean(fullstepsperday$x)

# 10766.19

median(fullstepsperday$x)

# 10766.19

# since the NA's were replaced by average values, it's no surprise that the 
# mean and median do not change much, if at all

## Are there differences in activity patterns between weekdays and weekends?
mergeag <- aggregate(steps ~ interval, move2, mean, rm.na=T)

weekend<-move[weekdays(as.Date(move$date)) %in% c("Saturday", "Sunday"),]

weekday<-move[weekdays(as.Date(move$date)) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),]

weekendag <- aggregate(steps ~ interval, weekend, mean, rm.na=T)

weekdayag <- aggregate(steps ~ interval, weekday, mean, rm.na=T)

par(mfrow = c(2, 1))

plot(weekendag$interval, weekendag$steps, type = "l", xlab = "5 Minute Intervals Averages (Mean)", ylab = "Steps", main = "5 Minute Intervals \n Average Number of Steps Taken Across Study Interval\nWeekends", col = "green")

plot(weekdayag$interval, weekdayag$steps, type = "l", xlab = "5 Minute Intervals Averages (Mean)", ylab = "Steps", main = "5 Minute Intervals \n Average Number of Steps Taken Across Study Interval\nWeekdays", col = "blue")

# Yes, there are differences in activity patterns between weekdays and weekends.
