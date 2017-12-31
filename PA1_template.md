---
title : "Reproducible Research: Peer Assessment 1"
author: "Nishanthan Kamaleson"
output: 
  html_document:
    toc: true  
    keep_md: true
---

## Loading required libraries

```r
# Extension of 'data.frame' that performs better with large data
library(data.table)

# A fast, consistent tool for working with data frame
library(dplyr)

# A system for 'declaratively' creating graphics, based on "The Grammar of Graphics"
library(ggplot2)
```



## Loading and preprocessing the data

```r
# Unzip the 'activity.zip' file, if 'activity.csv' does not exist 
if(!file.exists('activity.csv')) { unzip('activity.zip') }

# Read the csv file
activity <- fread('activity.csv', data.table = FALSE)

# Convert the date
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
# Select rows that does not contain any NA values
complete_activity <- activity[complete.cases(activity),]

# Calculate the total number of steps taken per day
steps_per_day <- complete_activity %>% 
                 group_by(date) %>% 
                 summarise(totalSteps=sum(steps))

# Make a histogram of the total number of steps taken each day
ggplot(steps_per_day, aes(totalSteps)) + 
    geom_histogram(bins = nclass.Sturges(steps_per_day$totalSteps)) +
    labs(x="Total steps taken per day", y="Frequency") +
    ggtitle('The total number of steps taken per day')
```

![](PA1_template_files/figure-html/step-2.1-1.png)<!-- -->


```r
# Calculate and report the mean and median of the total number of steps taken per day
mean_steps_per_day = mean(steps_per_day$totalSteps)
median_steps_per_day = median(steps_per_day$totalSteps)
```
The **mean** and **median** of the total number of steps taken per day are 10766.19 and 10765, respectively.

## What is the average daily activity pattern?

```r
# The average number of steps taken for each interval across all days
avg_steps_per_interval <- complete_activity %>% 
                          group_by(interval) %>%
                          summarise(averageSteps=mean(steps))

# Make a time series plot 
ggplot(avg_steps_per_interval, aes(interval, averageSteps)) +
    geom_line() + 
    labs(x="Interval (5 min)", y="Average number of steps") +
    ggtitle("Average number of steps taken per every 5 minute interval")
```

![](PA1_template_files/figure-html/step-3.1-1.png)<!-- -->


```r
# The interval which has the maximum average number of steps
max_interval <- with(avg_steps_per_interval, 
                     avg_steps_per_interval[averageSteps==max(averageSteps),])
```
The 5 minute interval at 
`835` has the **highest average steps**, 
`206.17`.

## Imputing missing values

```r
# Calculate and report the total number of missing values in the dataset 
total_missing_values <- sum(!complete.cases(activity))
```
The total number of rows with missing values is `2304`. However,
all of these missing values were from the ``steps`` column of the ``activity`` dataset.


```r
# View the first 6 rows that were missing values 
head(activity)
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
We can assume that the number of steps taken per day depends the time interval and the day
of the week as certain activities may repeatedly occur (e.g. every sunday going to 
church in the morning). Therefore, missing values were imputed with the median of
similar period (interval and day of the week). The mean is very sensitive to the outliers, 
thus, the median is chosen over the mean .


```r
# Imputing the 'steps' with the median of group (weekday and interval).
imputed_activity <- activity %>% 
                    group_by(weekday=wday(date), interval) %>%
                    mutate_at(vars(steps),funs(replace(., which(is.na(.)), median(., na.rm = TRUE))))
# View the first 6 rows that were missing values prior to the imputation
head(imputed_activity)
```

```
## # A tibble: 6 x 4
## # Groups:   weekday, interval [6]
##   steps       date interval weekday
##   <dbl>     <date>    <int>   <int>
## 1     0 2012-10-01        0       2
## 2     0 2012-10-01        5       2
## 3     0 2012-10-01       10       2
## 4     0 2012-10-01       15       2
## 5     0 2012-10-01       20       2
## 6     0 2012-10-01       25       2
```
During the first 30 minutes of the day subjects were deeply in sleep (0 steps were taken), therefore, we can safely say that the strategy seems to have imputed the values reasonably.


```r
# Calculate the total number of steps taken per day after imputation
imputed_steps_per_day <- imputed_activity %>% 
                          group_by(date) %>%
                          summarise(totalSteps=sum(steps))

ggplot(imputed_steps_per_day, aes(totalSteps)) + 
    geom_histogram(bins = nclass.Sturges(imputed_steps_per_day$totalSteps)) +
    labs(x="Total steps taken per day", y="Frequency") +
    ggtitle('The total number of steps taken per day (After Imputation)')
```

![](PA1_template_files/figure-html/step-4.4-1.png)<!-- -->

```r
imputed_mean_steps_per_day <- mean(imputed_steps_per_day$totalSteps)
imputed_median_steps_per_day <- median(imputed_steps_per_day$totalSteps)
```
The new mean is `9705.24` (old mean: `10766.19`) and the new median is `10395` (old median: `10765`). We can see that the imputation of new values has slightly decreased the mean and the median values of the distribution.

## Are there differences in activity patterns between weekdays and weekends?

```r
# Create a new factor variable in the dataset with two levels – 
# “weekday” and “weekend”
imputed_activity$dayType <- as.factor(if_else(imputed_activity$weekday %in% c(1,7), "weekend", "weekday"))

# Draw the grid
imputed_average_steps_per_interval <- imputed_activity %>% group_by(interval, dayType) %>% summarise(average_steps=mean(steps))
ggplot(imputed_average_steps_per_interval, aes(interval, average_steps)) + 
    geom_line() + 
    facet_grid(dayType ~ .) + 
    labs(x="Interval (5 min)", y="Average number of steps") +
    ggtitle("Average number of steps taken per every 5 minute interval")
```

![](PA1_template_files/figure-html/step-5.1-1.png)<!-- -->
