# Load data
activity <- read.csv("activity.csv")
library(lubridate)
library(ggplot2)
library(plyr)


# ##What is mean total number of steps taken per day?
daily.steps <- aggregate(steps ~ date, data = activity, sum, na.action=na.pass, na.rm=TRUE)

ggplot(daily.steps, aes(steps)) + geom_histogram(colour = "navyblue", fill = "white", binwidth = 1000) + 
    ggtitle("Total Steps Taken by Day") 

mean(daily.steps$steps)
median(daily.steps$steps)


##What is the average daily activity pattern?
daily.average <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
colnames(daily.average) <- c("interval", "steps")
ggplot(daily.average, aes(interval, steps)) + geom_line(colour = "navyblue", size=1) + 
    ggtitle("Average number of steps per interval")

daily.average[which.max(daily.average$steps),1]


##Imputing missing values
sum(is.na(activity$steps))

impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
act.fixed <- ddply(activity, ~ interval, transform, steps = impute.mean(steps))
fixed.steps <- aggregate(steps ~ date, data = act.fixed, sum, na.rm = TRUE)

ggplot(fixed.steps, aes(steps)) + geom_histogram(colour = "firebrick", fill = "white", binwidth = 1000) +
    ggtitle("Total Steps Taken by Day") 

mean(fixed.steps$steps)
median(fixed.steps$steps)

##Are there differences in activity patterns between weekdays and weekends?
act.fixed$weekday <- ifelse(wday(act.fixed$date) >1 & wday(act.fixed$date) <7, "weekday", "weekend")
weekday.average <- aggregate(steps ~ interval + weekday, data=act.fixed, mean)

ggplot(weekday.average, aes(interval, steps)) + geom_line(colour="firebrick", size=1) + facet_grid(weekday~.)