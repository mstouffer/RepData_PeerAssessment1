move <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
move$date <- as.Date(move$date, format = "%Y-%m-%d")
move$interval <- factor(move$interval)
library(ggplot2)
NA_index <- is.na(as.character(move$steps))
move_no_NA <- move[!NA_index,]
stepsinterval <- aggregate(move_no_NA$steps, by=list(interval=move_no_NA$interval), FUN=mean)
colnames(stepsinterval) <- c("interval", "average_steps")
sum(is.na(move$steps))
sum(is.na(move$steps))/length(stepsinterval$average_steps)
move2 <- move
move2$steps<- ifelse(is.na(move$steps),stepsinterval$average_steps[match(move$interval,stepsinterval$interval)],move$steps)
mergeag <- aggregate(steps ~ interval, move2, mean, rm.na=T)
weekend<-move[weekdays(as.Date(move$date)) %in% c("Saturday", "Sunday"),]
weekday<-move[weekdays(as.Date(move$date)) %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),]
weekendag <- aggregate(steps ~ interval, weekend, mean, rm.na=T)
weekdayag <- aggregate(steps ~ interval, weekday, mean, rm.na=T)
par(mfrow = c(2, 1))
plot(weekendag$interval, weekendag$steps, type = "l", xlab = "5 Minute Intervals Averages (Mean)", ylab = "Steps", main = "5 Minute Intervals \n Average Number of Steps Taken Across Study Interval\nWeekends", col = "green")
plot(weekdayag$interval, weekdayag$steps, type = "l", xlab = "5 Minute Intervals Averages (Mean)", ylab = "Steps", main = "5 Minute Intervals \n Average Number of Steps Taken Across Study Interval\nWeekdays", col = "blue")