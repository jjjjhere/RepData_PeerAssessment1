---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



### Loading and processing the data

1. Load the data
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data <- read.csv("//Users/yujiewang/Desktop/DS/RR/activity.csv")
data$date <- as.Date(data$date)
```

### What is the mean total number of steps taken per day ? 

1. Calculate the total number of steps taken per day.


```r
suppressMessages(library(dplyr))
data.clean <- data[!is.na(data$steps),]
dailySteps <- data.clean %>%
        group_by(date) %>% 
        summarise(steps = sum(steps))
head(dailySteps)
```

```
## # A tibble: 6 x 2
##         date steps
##       <date> <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
2. Make a histogram of the total number of steps taken each day.


```r
hist(dailySteps$steps)
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->


3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(dailySteps$steps)
```

```
## [1] 10766.19
```

```r
median(dailySteps$steps)
```

```
## [1] 10765
```

```r
options(scipen=1, digits=2)
```
- The mean total number of steps taken per day is 10766.19, and the median of the total number of steps per day is 10765.

### What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 


```r
library(ggplot2)
AverageSteps <-
        data.clean %>% group_by(interval) %>% summarise(steps = mean(steps))
ggplot(AverageSteps, aes(x = interval, y=steps)) + geom_line()
```

![](PA1_template_files/figure-html/pattern-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
AverageSteps$interval[which(AverageSteps$steps == max(AverageSteps$steps), arr.ind = TRUE)]
```

```
## [1] 835
```
- On average across all the days in the dataset, Interval <835> contains the maximum of steps. 

### Imputing missing values

1. Calculate and report the total number of missing values in the dataset.

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
- The total number of missing values is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
- I used the mean for that 5-minute interval to fill the missing data. 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
- The code is as follows: 

```r
data.tofill <- which(is.na(data$steps))
data.new <- data
for (i in data.tofill){
        data.new$steps[i] <- AverageSteps$steps[which(AverageSteps$interval == data.new$interval[i])]
}
head(data.new)
```

```
##   steps       date interval
## 1 1.717 2012-10-01        0
## 2 0.340 2012-10-01        5
## 3 0.132 2012-10-01       10
## 4 0.151 2012-10-01       15
## 5 0.075 2012-10-01       20
## 6 2.094 2012-10-01       25
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
dailySteps.new <- data.new %>%
        group_by(date) %>% 
        summarise(steps = sum(steps))
hist(dailySteps.new$steps)
```

![](PA1_template_files/figure-html/newHistogram-1.png)<!-- -->

```r
mean(dailySteps.new$steps)
```

```
## [1] 10766
```

```r
median(dailySteps.new$steps)
```

```
## [1] 10766
```

```r
AverageSteps.new <-
        data.new %>% group_by(interval) %>% summarise(steps = mean(steps))
```

- Using the new set of data, the mean and median total number of steps taken per day is 10766.19 and 10766.19 respectively. The means are the same for the two datasets, while the median differs slightly.   
- The total daily number of steps is 10766.19, which equals 10766.19 in the old data. 

### Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data.new$weekdays <- rep(NA, length(data.new$steps))
data.new[weekdays(data$date) %in% c("Saturday","Sunday"),"weekdays"] <- "weekend"
data.new[!weekdays(data$date) %in% c("Saturday","Sunday"),"weekdays"] <- "weekday"
data.new$weekdays <- factor(data.new$weekdays)
head(data.new)
```

```
##   steps       date interval weekdays
## 1 1.717 2012-10-01        0  weekday
## 2 0.340 2012-10-01        5  weekday
## 3 0.132 2012-10-01       10  weekday
## 4 0.151 2012-10-01       15  weekday
## 5 0.075 2012-10-01       20  weekday
## 6 2.094 2012-10-01       25  weekday
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
AverageSteps <-
        data.new %>% group_by(weekdays,interval) %>% summarise(steps = mean(steps))
ggplot(AverageSteps, aes(x = interval, y=steps, group=weekdays)) + geom_line() + facet_grid(weekdays~.)
```

![](PA1_template_files/figure-html/panelplot-1.png)<!-- -->
