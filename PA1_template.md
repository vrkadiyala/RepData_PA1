# Reproducible Research : Peer Assessmet1

#### loading required libraries.

```r
library(plyr)
library(ggplot2)
```

#### downloading data and loading data for processing


```r
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "repdata-data-activity.zip", mode="wb")
unzip("repdata-data-activity.zip")
activity <- read.csv("activity.csv")
activity$date <- as.Date(activity$date, format = '%Y-%m-%d')
```

### plotting histogram of total number of steps taken each day


```r
steps_taken_per_day <- aggregate(steps~date, data =activity, sum )
head(steps_taken_per_day)
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

```r
names(steps_taken_per_day)
```

```
## [1] "date"  "steps"
```

```r
histogram<-barplot(steps_taken_per_day$steps, names.arg = steps_taken_per_day$date, xlab="Date", ylab="steps per day", main =" total no. of steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


### Finding the mean and median

```r
mean(steps_taken_per_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_taken_per_day$steps, na.rm=TRUE)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval <- aggregate(steps~interval, activity, mean)
names(steps_interval)
```

```
## [1] "interval" "steps"
```

```r
qplot(interval, steps, data = steps_interval, geom="line", xlab = "Interval", ylab ="average steps taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
steps_interval$interval[which.max(steps_interval$steps)]
```

```
## [1] 835
```

### Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(activity)-nrow(na.omit(activity))
```

```
## [1] 2304
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

* Merging original activity and average of steps by interval in to one data set
* transforming the data as follows. 

* after the above step checking for completeness

```r
activity_merge <- merge(activity, steps_interval, by="interval", suffixes =c("",".y"))
names(activity_merge)
```

```
## [1] "interval" "steps"    "date"     "steps.y"
```

```r
head(activity_merge)
```

```
##   interval steps       date  steps.y
## 1        0    NA 2012-10-01 1.716981
## 2        0     0 2012-11-23 1.716981
## 3        0     0 2012-10-28 1.716981
## 4        0     0 2012-11-06 1.716981
## 5        0     0 2012-11-24 1.716981
## 6        0     0 2012-11-15 1.716981
```

```r
activity_merge <- transform(activity_merge, steps=ifelse(is.na(steps),steps.y,steps))
sum(complete.cases(activity_merge))
```

```
## [1] 17568
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity <- activity_merge[,c(1:3)]
```
4.1 Make a histogram of the total number of steps taken each day 


```r
steps_taken_per_day <- aggregate(steps~date, data =activity, sum )
names(steps_taken_per_day)
```

```
## [1] "date"  "steps"
```

```r
head(steps_taken_per_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
barplot(steps_taken_per_day$steps, names.arg = steps_taken_per_day$date, xlab="Date", ylab="steps per day", main =" total no. of steps per day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

4.2 Calculate and report the mean and median total number of steps taken per day.

*Mean:

```r
mean(steps_taken_per_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

*Median:

```r
median(steps_taken_per_day$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```
4.3 Do these values differ from the estimates from the first part of the assignment? 

Mean is same before and after inputting missing values,however, there is very minute variance in median. 

What is the impact of imputing missing data on the estimates of the total daily number of steps?

One can say that mean and median became equal and there is no impact of inputting the missed values 

### Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and
"weekend" indicating whether a given date is a weekday or weekend day.

```r
activity$dayType <- ifelse(weekdays(activity$date) %in% c("Saturday","Sundat") ,"Weekend","Weekday")
names(activity)
```

```
## [1] "interval" "steps"    "date"     "dayType"
```

```r
head(activity)
```

```
##   interval    steps       date dayType
## 1        0 1.716981 2012-10-01 Weekday
## 2        0 0.000000 2012-11-23 Weekday
## 3        0 0.000000 2012-10-28 Weekday
## 4        0 0.000000 2012-11-06 Weekday
## 5        0 0.000000 2012-11-24 Weekend
## 6        0 0.000000 2012-11-15 Weekday
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
qplot(x=interval, y=steps,data=subset(activity, complete.cases(activity)),geom='smooth', stat='summary', fun.y=mean) + facet_grid(dayType~.) + facet_wrap(~dayType,nrow=2) + theme(strip.background = element_rect(fill="#ffe5cc")) + labs(title=' Average steps per days, analyzing weekdays and weekend patterns')
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
