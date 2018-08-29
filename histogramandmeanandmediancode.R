# mean and median down below
move <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
move$date <- as.Date(move$date, format = "%Y-%m-%d")
move$interval <- factor(move$interval)
NA_index <- is.na(as.character(move$steps))
move_no_NA <- move[!NA_index,]
stepsday <- aggregate(steps ~ date, data = move_no_NA, sum)
colnames(stepsday) <- c("date", "steps")
hist(as.numeric(stepsday$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")
mean(stepsday$steps)
# 10766.19
median(stepsday$steps)
# 10765