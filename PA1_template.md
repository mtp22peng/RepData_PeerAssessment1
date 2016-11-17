# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
d <- read.csv("activity.csv", colClasses = "character")

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
d$date <- as.Date(d$date, "%Y-%m-%d")

d$steps <- as.numeric(d$steps)

d$interval <- as.numeric(d$interval)
```

## What is mean total number of steps taken per day?



```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.5
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

```r
s <- d %>% group_by(date) %>% summarize( mean = mean(steps, na.rm = TRUE), median = median(steps, na.rm = TRUE), sum=  sum(steps, na.rm = TRUE))


s$Day <- as.Date(cut(s$date,
                     breaks = "day"))

ggplot(data = s,
      aes(Day, sum)) +geom_bar(stat = "identity") + ylab("Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
ggplot(data = s,
      aes(Day, mean)) +geom_bar(stat = "identity") + ylab("Mean step per day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
ggplot(data = s,
      aes(Day, median)) +geom_bar(stat = "identity") + ylab("Median step per day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-3.png)<!-- -->


## What is the average daily activity pattern?



```r
s2 <- d %>% group_by(interval) %>% summarize( mean = mean(steps, na.rm = TRUE))
with(s2, plot(interval, mean, type= "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is:



```r
s2$interval[which.max(s2$mean)]
```

```
## [1] 835
```
## Imputing missing values
 


The total number of missing values in the dataset (i.e. the total number of rows with NAs) is:



```r
sum(is.na(d$steps))
```

```
## [1] 2304
```

The strategy of imputing here is putting NA values by "interval" average of steps



```r
d1 <- merge(d, s2, by.x = "interval", by.y = "interval")


d1$newsteps <- apply(d1, 1, function(x) {
  if (is.na(x[2])){
    x[4]
  } else {
    x[2]
  }})
  
  
  
d1$newsteps <- as.numeric(d1$newsteps)



s3 <- d1 %>% group_by(date) %>% summarize( mean = mean(newsteps, na.rm = TRUE), median = median(newsteps, na.rm = TRUE), sum = sum(newsteps, na.rm = TRUE))


s3$Day <- as.Date(cut(s3$date,
                     breaks = "day"))

ggplot(data = s3,
       aes(Day, sum)) +geom_bar(stat = "identity")+ylab("Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggplot(data = s,
      aes(Day, mean)) +geom_bar(stat = "identity") + ylab("Mean step per day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
ggplot(data = s,
      aes(Day, median)) +geom_bar(stat = "identity") + ylab("Median step per day")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-6-3.png)<!-- -->



The difference between original steps taken in each day and the steps after imputing are calculated and is plotted. Since the imputing is made with interval average and the NA vlaues happen in all interval for each day containing NA values, the difference is the same for these days. 




```r
s3$diff <- s3$sum - s$sum
ggplot(data = s3,
       aes(Day, diff)) +geom_bar(stat = "identity")+ ylab("Total different steps per day after imputing")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Are there differences in activity patterns between weekdays and weekends?



```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

d1$daytype <-  factor((weekdays(d1$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday') )


s4 <- d1 %>% group_by(interval, daytype) %>% summarize( mean = mean(newsteps, na.rm = TRUE))


p <- ggplot(s4, aes(x = interval, y = mean)) + geom_line() +ylab ("Number of steps")
p + facet_grid( daytype ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


From the comparison of the number of steps taken in the weekday and weenkend, a remarkable difference is in more steps are taken during weekend between the intervals 1000 and 2000. But the steps taken are more in weekedays between the intervals 500 and 1000. 
