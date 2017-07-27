# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

Unzip and load the data from a .csv file.


```r
unzip("activity.zip")
activity.data <- read.csv("activity.csv")
```

Check a short summary of what have been read.

```r
str(activity.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


The date column will be transformed to Date object so that it can be treated correctly in plots.

```r
activity.data$date <- as.Date(activity.data$date, format = "%Y-%m-%d")
```



## What is mean total number of steps taken per day?
Use *tapply* to calculate the number of steps taken each day.

```r
activity.data.dailysums <- tapply(activity.data$steps, activity.data$date, sum, na.rm = TRUE)
head(activity.data.dailysums)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420
```

Plot a histogram with ggplot2 showing the distribution of daily step count.

```r
library(ggplot2)
qplot(activity.data.dailysums, binwidth=1000, xlab="Daily step count")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


Calculate the mean and median using the *summary* function.

```r
summary(activity.data.dailysums)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```




## What is the average daily activity pattern?

Use *ddply* function of the *plyr* library to calculate mean and median values of each interval across all days: split by interval, apply mean & median, and combine.


```r
library(plyr)

activity.data.daily.activity <- ddply(activity.data, c("interval"), summarize,
                                     mean = round(mean(steps, na.rm=TRUE), 2),
                                     median = round(median(steps, na.rm=TRUE), 2)
)
```

Plot the daily activity pattern.

![](PA1_template_files/figure-html/timeplot-1.png)<!-- -->

Calculate the maximum activity interval.


```r
activity.data.daily.activity[which.max(activity.data.daily.activity$mean), "interval"]
```

```
## [1] 835
```


## Imputing missing values
Calculate total number of missing values with *complete.cases* function that returns a binary vector of all the rows that have none NA's. Substract the length of that vector from the number of rows in the original data.


```r
nrow(activity.data) - sum(complete.cases(activity.data))
```

```
## [1] 2304
```

Filling missing values by filling the median of that interval and creating dataset

```r
  activity.data.complete <- ddply(activity.data, c("interval"), function(x) {
  missing <- is.na(x$steps)
  x[missing, "steps"] = mean(x[, "steps"], na.rm=TRUE)
  x
})
```



```r
activity.data.complete.dailysums <- tapply(activity.data.complete$steps, activity.data.complete$date, sum, na.rm = TRUE)
head(activity.data.complete.dailysums)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00
```

Then plot a histogram showing the distribution of daily step count.

```r
qplot(activity.data.complete.dailysums, binwidth=1000, xlab="Daily step count")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
summary(activity.data.complete.dailysums)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```


## Are there differences in activity patterns between weekdays and weekends?

```r
weeklookup <- cbind(
        c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend"))

activity.data.complete$part.of.the.week <- weeklookup[
  match(weekdays(activity.data.complete$date), weeklookup[,1])
  , 2]    


activity.data.complete.mean.intervals <- ddply(activity.data.complete, c( "interval", "part.of.the.week"),
           summarize,
           meansteps = mean(steps, na.rm = TRUE)
           )

ggplot(activity.data.complete.mean.intervals) + 
        geom_line(aes(y = meansteps, x = interval), stat="identity") +
        labs(title="Mean step count across intervals, weekdays vs. weekend", 
          x = "Interval", 
          y = "Mean step count") +
        facet_grid(facets = part.of.the.week ~ ., scales="free")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
