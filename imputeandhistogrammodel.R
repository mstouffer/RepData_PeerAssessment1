move <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
move$date <- as.Date(move$date, format = "%Y-%m-%d")
move$interval <- factor(move$interval)
NA_index <- is.na(as.character(move$steps))
move_no_NA <- move[!NA_index,]
stepsinterval <- aggregate(move_no_NA$steps, by=list(interval=move_no_NA$interval), FUN=mean)
colnames(stepsinterval) <- c("interval", "average_steps")
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