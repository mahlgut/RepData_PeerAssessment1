library(tidyverse)
library(lubridate)
library(knitr)
list.files()
unzip("./activity.zip")
activityfile <- read.csv("activity.csv")
activityfile$date <- as.Date(activityfile$date)

#Q1: plot histogram of total number of steps taken each day
stepsperdaysum <- aggregate(activityfile$steps, list(activityfile$date), sum)
names(stepsperdaysum) <- c("Date", "Steps.per.day.sum")

png(file = "./instructions_fig/stepsperdaysum.png", width = 480, height = 480)
ggplot(data = stepsperdaysum) +
  geom_histogram(aes(Steps.per.day.sum))
dev.off()

#Q2: calculate mean number of steps per day
averagesteps <- mean(activityfile$steps, na.rm = TRUE)
averagesteps

stepsperdaymean <- aggregate(activityfile$steps, list(activityfile$date), FUN = mean, na.rm = TRUE)
names(stepsperdaymean) <- c("Date", "Steps.per.day.mean")
stepsperdaymean$Date <- as.Date(stepsperdaymean$Date)
stepsperdaymean

#calculate median number of steps per day
stepsperdaymedian <- aggregate(activityfile$steps, list(activityfile$date), median)
names(stepsperdaymedian) <- c("Date", "Steps.per.day.median")

#Q3: plot time series of average steps per day

png(file = "./instructions_fig/timeseriesavg.png", width = 480, height = 480)
ggplot(data = stepsperdaymean, aes(x = Date, y = Steps.per.day.mean, group = 1)) +
  geom_point() + geom_line()
dev.off()

#Q4:The 5-minute interval that, on average, contains the maximum number of steps
activityfileomitted <- na.omit(activityfile)
activityfileomitted$interval[activityfileomitted$steps == as.integer(max(activityfileomitted$steps))]

#Q5: impute missing data
nafiles <- sum(is.na(activityfile$steps))
activityfile$steps[is.na(activityfile$steps)] <- mean(activityfile$steps[activityfile$date])

#Q6: Histogram of the total number of steps taken each day after missing values are imputed
stepsperdaysum <- aggregate(activityfile$steps, list(activityfile$date), sum)
names(stepsperdaysum) <- c("Date", "Steps.per.day.sum")

png(file = "./instructions_fig/stepsperdaysumimputed.png", width = 480, height = 480)
ggplot(data = stepsperdaysum) +
  geom_histogram(aes(Steps.per.day.sum))
dev.off()

#Q7:comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activityfile$weekday <- weekdays(activityfile$date)
activityfile$weekend <- "weekend"
activityfile$weekend[activityfile$weekday == "Montag" | activityfile$weekday == "Dienstag" | activityfile$weekday == "Mittwoch" | activityfile$weekday == "Donnerstag" | activityfile$weekday == "Freitag"] <- "weekdays"
stepsperintervalmean <- aggregate(activityfile$steps ~ activityfile$interval+activityfile$weekend, data = activityfile,FUN =  mean)
names(stepsperintervalmean) <- c("Interval", "Weekpart", "Steps")

png(file = "./instructions_fig/weekdayscomparison.png", width = 480, height = 480)
ggplot(stepsperintervalmean, aes(Interval, Steps)) + geom_point() +
  facet_grid(rows = vars(Weekpart))
dev.off()

