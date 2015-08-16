---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
detach("package:plyr", unload=TRUE)
```

```
## Error in detach("package:plyr", unload = TRUE): invalid 'name' argument
```

```r
library(dplyr)
activity <- read.csv("activity.csv")
activity <- tbl_df(activity)
by_date = group_by(activity, date)
steps_per_day = summarize(by_date, totalSteps = sum(steps, na.rm = TRUE))
steps_per_day
```

```
## Source: local data frame [61 x 2]
## 
##          date totalSteps
## 1  2012-10-01          0
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08          0
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## ..        ...        ...
```
## Histogram


```r
hist(steps_per_day$totalSteps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 
## What is mean total number of steps taken per day?



```r
summarise(steps_per_day, mean = mean(totalSteps), median = median(totalSteps))
```

```
## Source: local data frame [1 x 2]
## 
##      mean median
## 1 9354.23  10395
```
## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
