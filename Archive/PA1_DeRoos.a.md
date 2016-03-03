# Reproducible Research: Peer Assessment 1
Albert de Roos  

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data
The data file was unzipped using read.csv and the column with the dates was set to Date format. In order to get English-named weekdays, the locale on the machine was set to English_United States.1252


```r
myrepdata <- read.csv("activity.csv")
myrepdata$date <- as.Date(myrepdata$date)
str(myrepdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
In order to calculate the mean and median steps per day, first the data is grouped by the day and the sum per day is derived.


```r
stepsperday <- myrepdata %>% group_by(date) %>% summarize(sum = sum(steps))
```

This yields a new data frame with 61 consistent with the number of days in the 2 months of the data collection.


## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
