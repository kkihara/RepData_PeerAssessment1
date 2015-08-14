---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
dat <- read.csv("activity.csv")
df <- tbl_df(dat)
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
byDay <- group_by(df, date)
steps <- summarize(byDay, steps=sum(steps, na.rm=TRUE))
ggplot(steps, aes(x=steps)) + 
    ggtitle("Histogram of steps by day") +
    geom_histogram(binwidth = 2500)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 


3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(steps$steps)
```

```
## [1] 9354.23
```

```r
median(steps$steps)
```

```
## [1] 10395
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
byInterval <- group_by(df, interval)
intervals <- summarize(byInterval, steps=mean(steps, na.rm=TRUE))
plot(intervals$interval, intervals$steps, type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
filter(intervals, interval, steps == max(steps))
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(df))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
df_impute <- df
for (i in 1:dim(df_impute)[1]){
    if (is.na(df_impute$steps[i])) {
        df_impute$steps[i] <- intervals[intervals["interval"] == 
                                    df_impute[i,]$interval,]$steps
    }
}
df_impute
```

```
## Source: local data frame [17,568 x 3]
## 
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
## ..       ...        ...      ...
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
byDay_imp <- group_by(df_impute, date)
steps_imp <- summarize(byDay_imp, steps=sum(steps))
ggplot(steps_imp, aes(x=steps)) + 
    ggtitle("Histogram of steps by day with imputed data") +
    geom_histogram(binwidth = 2500)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
mean(steps_imp$steps)
```

```
## [1] 10766.19
```

```r
median(steps_imp$steps)
```

```
## [1] 10766.19
```
Imputing the average number of steps for the interval makes the mean equal the median. Also, it makes more of the days have the average number of total steps taken per day, making the graph more Gaussian shpaed.
## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
weekends <- c("Saturday", "Sunday")
df_impute <- mutate(df_impute, DayOfWeek=weekdays(as.Date(df_impute$date)))
df_impute$DayOfWeek <- factor((df_impute$DayOfWeek %in% weekends),
                              levels = c(FALSE, TRUE),
                              labels = c("weekday", "weekend"))
summary(df_impute$DayOfWeek)
```

```
## weekday weekend 
##   12960    4608
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
byIntAndDay <- group_by(df_impute, interval, DayOfWeek)
x <- summarize(byIntAndDay, steps=mean(steps))
xyplot(x$steps~x$interval|x$DayOfWeek, type="l",
       xlab="Interval", ylab="Number of steps",
       layout=c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
