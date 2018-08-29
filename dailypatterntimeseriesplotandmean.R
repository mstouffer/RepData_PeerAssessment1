move <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
move$date <- as.Date(move$date, format = "%Y-%m-%d")
move$interval <- factor(move$interval)
NA_index <- is.na(as.character(move$steps))
move_no_NA <- move[!NA_index,]
stepsinterval <- aggregate(move_no_NA$steps, by=list(interval=move_no_NA$interval), FUN=mean)
colnames(stepsinterval) <- c("interval", "average_steps")
plot(as.integer(levels(stepsinterval$interval)), stepsinterval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")
meantable <- data.frame(stepsinterval$average_steps, as.integer(levels(stepsinterval$interval)))
meantable[meantable$stepsinterval.average_steps==max(meantable$stepsinterval.average_steps),][2]
# 835 is the interval