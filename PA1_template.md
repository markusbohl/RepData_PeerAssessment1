# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

First, we are loading the activity.csv file into a data frame.


```r
activity <- read.csv("activity.csv")
```

Let's take a look at the data frame's structure and how the data might look like:

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Since `date` is currently regarded as a factor we change its type to `Date`:

```r
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```


## What is mean total number of steps taken per day?

In order to calculate the mean and median values we first calculate the total 
number of steps taken each day.


```r
totalStepsPerDay <- aggregate(steps ~ date, data = activity, FUN = "sum")
```

The data looks something like this:

```r
head(totalStepsPerDay)
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

Thereafter, mean and median values can easily be derived.

```r
totalStepsPerDay_mean <- mean(totalStepsPerDay$steps)
totalStepsPerDay_median <- median(totalStepsPerDay$steps)
```

Mean (10766 total steps per day) and median (10765 total steps per day) values lie rather close together.

In order to get mean and median values, the `summary()` function could also be helpful.

```r
summary(totalStepsPerDay)
```

```
##       date                steps      
##  Min.   :2012-10-02   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-29   Median :10765  
##  Mean   :2012-10-30   Mean   :10766  
##  3rd Qu.:2012-11-16   3rd Qu.:13294  
##  Max.   :2012-11-29   Max.   :21194
```

The following boxplot gives a good idea about the distribution of total number of steps per day:


```r
boxplot(totalStepsPerDay$steps, ylab = "total number of steps per day")
```

![plot of chunk unnamed-chunk-8](./PA1_template_files/figure-html/unnamed-chunk-8.png) 

## What is the average daily activity pattern?

In order to plot a line graph of the average number of steps taken (averaged accross all days) in each 5-minute interval, we first aggregate the activity data once more. This time, we calculate the average number of steps per interval:


```r
avgStepsPerInterval <- aggregate(steps ~ interval, data = activity, FUN = "mean")

# just to show the avg. number of steps across all intervals in the graph as well
avgStepsAcrossIntervals <- mean(avgStepsPerInterval$steps)
```

Next, the dataset-index with the maximum number of steps and the corresponding interval can be calculated by:

```r
maxIndex <- which.max(avgStepsPerInterval$steps)
avgStepsPerInterval$steps[maxIndex]
```

```
## [1] 206.2
```

```r
avgStepsPerInterval$interval[maxIndex]
```

```
## [1] 835
```
Thus, we can state that the maximum number of steps is 206.2 and happens to be in interval 835.


Finally, an according plot might be generated like this:

```r
library(ggplot2)

ggplot(data = avgStepsPerInterval, aes(x = interval, y = steps)) + 
    geom_line() + 
    geom_point(data = avgStepsPerInterval[maxIndex, ], 
               aes(x = interval, y = steps), col = "darkgreen") +
    geom_vline(aes(xintercept=avgStepsPerInterval$interval[maxIndex]), 
               col = "darkgreen", linetype="dashed") +
    geom_hline(aes(yintercept=avgStepsPerInterval$steps[maxIndex]), 
               col = "darkgreen", linetype="dashed") +
    annotate("text", x = 200, y = avgStepsPerInterval$steps[maxIndex], 
             label=sprintf("Max. of %.1f steps\nin interval: %.0f", 
                           avgStepsPerInterval$steps[maxIndex], 
                           avgStepsPerInterval$interval[maxIndex]), 
             family = "serif", fontface = "italic", col = "darkgreen", size = 4) +
    geom_hline(aes(yintercept=avgStepsAcrossIntervals), col = "orange") +
    annotate("text", x = 200, y = avgStepsAcrossIntervals, 
             label=sprintf("Avg. of %.1f steps\nacross all intervals", 
                           avgStepsAcrossIntervals), 
             family = "serif", fontface = "italic", col = "orange", size = 4) + 
    ggtitle("Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-11](./PA1_template_files/figure-html/unnamed-chunk-11.png) 


## Imputing missing values

First, let's get an idea of how many values are missing:

```r
totalNumberOfNAs <- sum(is.na(activity$steps))
totalNumberOfObservations <- length(activity$steps)
missingPercentage  <- totalNumberOfNAs/totalNumberOfObservations * 100
```

2304 out of 17568 values in the given activity dataset are missing which is a percentage of 13.1%.

Neither `date` nor `interval` column do have `NA` values:

```r
sum(is.na(activity$date))
```

```
## [1] 0
```

```r
sum(is.na(activity$interval))
```

```
## [1] 0
```

Strategy for filling NAs: Missing step values (`NAs`) should be filled with the mean value of their particular 5-minute interval.


```r
tempDf <- merge(x = activity, 
                y = avgStepsPerInterval, 
                by = "interval", 
                all.x = TRUE)

# fill NAs with mean of appropriate interval
tempDf[is.na(tempDf$steps.x),"steps.x"] <- tempDf[is.na(tempDf$steps.x),"steps.y"]

# and rename steps.x to steps
names(tempDf)[names(tempDf) == "steps.x"] <- "steps"

# finally create dataset equal to the original one but without NAs
activityWithoutNAs <- tempDf[order(tempDf$date, tempDf$interval), c("steps", "date", "interval")]
```


```r
str(activityWithoutNAs)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```




```r
withNAs <- activity
withNAs$origDataset <- "with NAs"
withoutNAs <- activityWithoutNAs
withoutNAs$origDataset <- "without NAs"

activities <- rbind(withNAs, withoutNAs)

activitiesPerDay <- aggregate(steps ~ origDataset + date, 
                              data = activities, FUN = "sum", na.rm = FALSE)

aggregate(steps ~ origDataset, data = activitiesPerDay, 
          FUN = "mean")
```

```
##   origDataset steps
## 1    with NAs 10766
## 2 without NAs 10766
```

```r
aggregate(steps ~ origDataset, data = activitiesPerDay, 
          FUN = "median")
```

```
##   origDataset steps
## 1    with NAs 10765
## 2 without NAs 10766
```



```r
ggplot(data = activitiesPerDay, aes(x=date, y=steps, fill=origDataset)) + 
    geom_bar(stat = "identity", position = "dodge")
```

![plot of chunk unnamed-chunk-17](./PA1_template_files/figure-html/unnamed-chunk-17.png) 



## Are there differences in activity patterns between weekdays and weekends?


```r
activitiesOnDays <- activityWithoutNAs

sundays <- format(activitiesOnDays$date, "%w") == 0 
saturdays <- format(activitiesOnDays$date, "%w") == 6

activitiesOnDays$day <- ifelse(sundays, "weekend", ifelse(saturdays, "weekend", "weekday"))

activitiesOnDays$day <- as.factor(activitiesOnDays$day)
```



```r
avgStepsPerIntervalAndDay <- aggregate(steps ~ day + interval, data = activitiesOnDays, FUN = "mean")
```


```r
ggplot(data = avgStepsPerIntervalAndDay, aes(x = interval, y = steps, col = day)) + 
    geom_line()
```

![plot of chunk unnamed-chunk-20](./PA1_template_files/figure-html/unnamed-chunk-20.png) 


```r
ggplot(data = avgStepsPerIntervalAndDay, aes(x = interval, y = steps)) + 
    geom_line() + facet_wrap(~ day, ncol = 1)
```

![plot of chunk unnamed-chunk-21](./PA1_template_files/figure-html/unnamed-chunk-21.png) 

