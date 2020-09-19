
ActivityData <- read.csv(file = "Activity.csv")
ActivityData$date <- as.Date(ActivityData$date,format = "%Y-%m-%d")
ActivityData$steps <- as.numeric(ActivityData$steps)

library(dplyr)
library(ggplot2)

dailyTotal <- ActivityData %>% group_by(date) %>% 
  summarise(steps = sum(steps,na.rm=TRUE))

##chart the histogram of daily total steps
hist(dailyTotal$steps, main = "Total Steps Taken a Day", xlab = "number of steps")

##calculating the mean
mean(dailyTotal$steps)

##calculating the median
median(dailyTotal$steps)

##calculate the average number of steps in each 5 min interval & removing NAs
FiveMinSummary <- ActivityData %>% group_by(interval) %>%
  summarise(steps = mean(steps, na.rm = TRUE))
plot(FiveMinSummary$interval, FiveMinSummary$steps, type = "l", 
     main = "Average number of steps in interval", 
     xlab = "Interval", ylab = "number of steps")

##calculate the interval with the most steps on average
maxSteps <- which.max(FiveMinSummary$steps)
FiveMinSummary$interval[maxSteps]

##Number of missing data entries
sum(is.na(ActivityData$steps))

##Merge the data to fill in the NAs with the average for that five min period
MergedData <- merge(ActivityData, FiveMinSummary, by.x = "interval", by.y = "interval", all=TRUE)
MergedData$steps.z <- coalesce(MergedData$steps.x, MergedData$steps.y)

##plot a histogram of the new data set with imputed values
dailyMergedTotal <- MergedData %>% group_by(date) %>% 
  summarise(steps.z = sum(steps.z,na.rm=TRUE))
hist(dailyMergedTotal$steps.z, main = "Total Steps Taken a Day", xlab = "number of steps")


##calculate updated mean & median
mean(dailyMergedTotal$steps.z)
median(dailyMergedTotal$steps.z)

##create weekend variable in dataset
MergedData$TypeOfDay <- weekdays(MergedData$date)
MergedData$TypeOfDay[MergedData$TypeOfDay  %in% c('Saturday','Sunday') ] <- "weekend"
MergedData$TypeOfDay[MergedData$TypeOfDay != "weekend"] <- "weekday"


NewFiveMinSummary <- MergedData %>% group_by(interval, TypeOfDay) %>%
  summarise(steps.z = mean(steps.z))

qplot(interval, 
      steps.z, 
      data = NewFiveMinSummary, 
      type = 'l',
      xlab = "Interval",
      ylab = "Total Steps",
      geom = c("line")) + 
      facet_wrap(~TypeOfDay, ncol = 1)

