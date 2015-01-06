# Reproducible Research: Peer Assessment 1
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity <- read.csv(unz('activity.zip', 'activity.csv'), stringsAsFactors=FALSE)
activity <- transform(activity, date=as.Date(date, '%Y-%m-%d'))
```

## What is mean total number of steps taken per day?


```r
library(plyr)
library(ggplot2)

per_day <- ddply(activity, .(date), summarize, steps=sum(steps, na.rm=TRUE))
with(per_day, hist(steps, col='blue', breaks=20))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


```r
mean_per_day <- mean(per_day$steps, na.rm=TRUE)
median_per_day <- median(per_day$steps, na.rm=TRUE)
```

the **mean** total number of steps taken per day is 9354.2.

the **median** total number of steps taken per day is 10395.

## What is the average daily activity pattern?


```r
one_day <- ddply(activity, .(interval), summarize, steps=mean(steps, na.rm=TRUE))
with(one_day, plot(interval, steps, type='l'))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


```r
max_step <- max(one_day$steps)
max_step_time <- one_day$interval[which.max(one_day$steps)]
```

5-minute interval on average across all the days in the dataset,
the maximum number of steps is 206.2, the interval is 835.

## Imputing missing values


```r
total_na <- sum(is.na(activity$steps))
```

Total number of missing values in the dataset is 2304.

Use the mean for that 5-minute interval to fill the missing value.


```r
nomiss <- activity
for (interval in levels(factor(activity$interval))) {
	nomiss[nomiss$interval == interval & is.na(nomiss$steps),]$steps <- 
		mean(nomiss[nomiss$interval == interval,]$steps, na.rm=TRUE)
}
```


```r
nomiss_per_day <- ddply(nomiss, .(date), summarize, steps=sum(steps, na.rm=TRUE))
with(nomiss_per_day, hist(steps, col='blue', breaks=20))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


```r
nomiss_mean_per_day <- mean(nomiss_per_day$steps)
nomiss_median_per_day <- median(nomiss_per_day$steps)
```

the **mean** total number of steps taken per day is 10766.2.

the **median** total number of steps taken per day is 10766.2.


## Are there differences in activity patterns between weekdays and weekends?
