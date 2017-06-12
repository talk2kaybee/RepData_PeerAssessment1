detach("package:plyr", unload=TRUE)
library(dplyr)
library(ggplot2)
library(timeDate)
activity <- read.csv("activity.csv")
activity <- tbl_df(activity)

##steps_per_day = summarize(by_date, totalSteps = sum(steps, na.rm = TRUE))
by_date = group_by(activity, date)
steps_per_day = summarize(by_date, totalSteps = sum(steps, na.rm = TRUE), avgSteps = mean(steps,na.rm = TRUE), mediSteps = median(steps, na.rm = TRUE))
steps_per_day
hist(steps_per_day$totalSteps)
select(steps_per_day,date,avgSteps)
ggplot(steps_per_day, aes(date, avgSteps)) + xlab("") + ylab("Avg Daily Steps")

by_interval = group_by(activity, interval)
##steps_per_day = summarize(by_date, totalSteps = sum(steps, na.rm = TRUE))
steps_per_interval = summarize(by_interval, totalSteps = sum(steps, na.rm = TRUE), avgSteps = mean(steps,na.rm = TRUE), mediSteps = median(steps, na.rm = TRUE))
steps_per_interval
select(filter(steps_per_interval, avgSteps == max(steps_per_interval$avgSteps)),interval,avgSteps)
ggplot(steps_per_interval, aes(interval,avgSteps))

count(activity[is.na(activity),])

##use the mean for each time interval to populate the missing values
activity2 = tbl_df(merge(activity,steps_per_interval, by = "interval", all.x =TRUE))
activity_imputed = tbl_df(transform(activity2, steps = ifelse(is.na(activity2$steps),activity2$avgSteps,activity2$steps)))
activity_imputed = select(activity_imputed, steps, date,interval)

activity_imputed

##steps_per_day2 = summarize(by_date, totalSteps = sum(steps, na.rm = TRUE))
by_date2 = group_by(activity_imputed, date)
steps_per_day2 = summarize(by_date2, totalSteps = sum(steps, na.rm = TRUE), avgSteps = mean(steps,na.rm = TRUE), mediSteps = median(steps, na.rm = TRUE))
steps_per_day2
hist(steps_per_day2$totalSteps)

activity_imputed = tbl_df(transform(activity_imputed, weekday = ifelse(isWeekday(activity_imputed$date), "weekday", "weekend")))

by_interval_weekday = group_by(activity_imputed, interval, weekday)

steps_by_interval_weekday = summarize(by_interval_weekday, avgSteps = mean(steps))

##panel_plot




