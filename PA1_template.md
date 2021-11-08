---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: YES
  pdf_document: default
---

## Loading and preprocessing the data

```r
if (!file.exists("activity.csv")){
  url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
  download.file(url,destfile='repdata%2Fdata%2Factivity.zip', mode='wb')
  unzip('repdata%2Fdata%2Factivity.zip')
}
data <- read.csv("activity.csv")
```


```r
dim(data)
```

```
## [1] 17568     3
```


```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
StepsPerDay <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
```


```r
head(StepsPerDay)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

## What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day.

```r
hist(StepsPerDay$steps, main = paste("Total Number of Steps Taken per Day"), col="gray", xlab="Steps Number")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
Calculate and report the mean and median total number of steps taken per day.

```r
StepsMedian <- median(StepsPerDay$steps)
StepsMedian
```

```
## [1] 10765
```


```r
StepsMean <- mean(StepsPerDay$steps)
StepsMean
```

```
## [1] 10766.19
```
## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
StepsIntervals <- aggregate(steps ~ interval, data = data, FUN = mean)
```


```r
plot(StepsIntervals$interval, StepsIntervals$steps, 
     type="l", xlab="Interval", ylab="Steps Number",
     main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MostAvgInterval <- StepsIntervals[which.max(StepsIntervals$steps),]$interval
MostAvgInterval
```

```
## [1] 835
```
## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
NANumber <- sum(!complete.cases(data))
NANumber
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
StepsAvg <- aggregate(steps ~ interval, data = data, FUN = mean)
NAfilling <- numeric()
for (i in 1:nrow(data)) {
  remark <- data[i, ]
  if (is.na(remark$steps)) {
    steps <- subset(StepsAvg, interval == remark$interval)$steps
    } else {
      steps <- remark$steps
      }
  NAfilling <- c(NAfilling, steps)
}
```


```r
head(NAfilling)
```

```
## [1] 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
NewDataset <- data
head(NewDataset)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


```r
NewDataset$steps <- NAfilling
head(NewDataset)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
StepsSummarised <- aggregate(steps ~ date, data = NewDataset, FUN = sum, na.rm = TRUE)
hist(StepsSummarised$steps, col="gray",
     xlab = "Total Steps Per Day",
     main = "Total Number of Steps Taken Per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
StepMeanTotal <- mean(StepsSummarised$steps)
StepMeanTotal
```

```
## [1] 10766.19
```


```r
StepMedianTotal <- median(StepsSummarised$steps)
StepMedianTotal
```

```
## [1] 10766.19
```

* The mean and median of steps taken during the day are the same, they did not take place before the missing values were assigned

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
NewDataset$DayOfWeek = as.factor(ifelse(is.element(weekdays(as.Date(NewDataset$date)), weekdays), "Weekday", "Weekend"))

StepsAll <- aggregate(steps ~ interval + DayOfWeek, data = NewDataset, FUN = mean)
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
library(lattice)
xyplot(StepsAll$steps ~ StepsAll$interval|StepsAll$DayOfWeek, 
       main="Average Number of Steps per Day by Interval", xlab="Interval", ylab="Steps",
       layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
