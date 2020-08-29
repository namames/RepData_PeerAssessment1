

#Loading data and packages (1)
library(ggplot2)
unzip("activity.zip")
activity <- read.csv("activity.csv")


#Data without missing values
data <- activity[complete.cases(activity),]
dailysteps <- aggregate(steps ~ date, data, sum)


#Number of daily steps with NA values + histogram (2)
dailystepsNA <- with(activity, tapply(steps, as.factor(activity$date), sum, na.rm = T))
hist(dailystepsNA, main = "Daily Steps Histogram", xlab = "Steps per day", col = "red")


#Mean and Median number of steps (3)
summary(dailystepsNA)


#Time series plot (4)
intervalsxsteps <- aggregate (steps ~ interval, data, mean)
plot(intervalsxsteps$interval, intervalsxsteps$steps, type = "l", col = "blue",
     xlab = "Interval", ylab = "Average number of steps", main = "Average daily activity pattern")

#Maximum number of steps according to interval (5)
maxstepsperinterval <- which.max(intervalsxsteps$steps)
intervalsxsteps[maxstepsperinterval,]


#Missing values (6)
missing <- activity[!complete.cases(activity),]
nrow(missing)
#Total rows with NAs = 2304

newData <- activity

#replacing NAs with avg steps per interval
for (i in 1:nrow(activity)){
           if(is.na(activity$steps[i])){
                      sub <- intervalsxsteps$steps[which(intervalsxsteps$interval == activity$interval[i])]
                      newData$steps[i] <- sub
           }
}

#Histogram of newly created data + mean and median (7)
hist(aggregate(steps ~ date, newData, sum)$steps, xlab = "Steps per day", main = "Daily Steps Histogram")
summary(aggregate(steps ~ date, newData, sum)$steps)


#Weekdays and weekends
weekday <- function(dates) {
           day <- weekdays(as.Date(dates, '%Y-%m-%d'))
           if  (!(day == 'Saturday' || day == 'Sunday')) {
                      x <- 'Weekday'
           } else {
                      x <- 'Weekend'
           }
           x
}

#Adding new weekend/weekday column
newData$day_type <- as.factor(sapply(newData$date, weekday))

#Plotting according to weekdays and weekends
ggplot(aggregate(steps ~ interval+day_type, newData, mean), aes(interval, steps)) +
           facet_grid(day_type ~ ., scales = "fixed", space = "fixed") +
           geom_line(stat = "identity", aes(colour = day_type)) + 
           labs(title = "Number of steps per interval and day type", x = "Interval", y = "Steps")


