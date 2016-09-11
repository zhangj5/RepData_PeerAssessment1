# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
unzip("activity.zip")
activity<-read.csv("activity.csv")
activity<-activity[!is.na(activity$steps),]
```


## What is mean total number of steps taken per day?

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

```r
library(ggplot2)
library(timeDate)
total_steps_per_day=activity%>%group_by(date)%>%summarise(total_steps=sum(steps))
hist(total_steps_per_day$total_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean_total_steps_per_day=mean(total_steps_per_day$total_steps)
print(mean_total_steps_per_day)
```

```
## [1] 10766.19
```

```r
median_total_steps_per_day=median(total_steps_per_day$total_steps)
print(median_total_steps_per_day)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
mean_steps_per_interval=activity%>%group_by(interval)%>%summarise(mean_steps_per_interval=mean(steps))
g<-ggplot(mean_steps_per_interval,aes(x=interval,y=mean_steps_per_interval))
g+geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
ordered<-mean_steps_per_interval%>%arrange(desc(mean_steps_per_interval))
ordered$interval[1]
```

```
## [1] 835
```



## Imputing missing values

```r
activity_na<-read.csv("activity.csv")
row_is_na<-apply(activity_na, 1, function(x) any(is.na(x)))
print(sum(row_is_na))
```

```
## [1] 2304
```

```r
activity_new<-activity_na %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps),mean(steps,na.rm=T), steps))
total_steps_per_day=activity_new%>%group_by(date)%>%summarise(total_steps=sum(steps))
hist(total_steps_per_day$total_steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean_total_steps_per_day=mean(total_steps_per_day$total_steps)
print(mean_total_steps_per_day)
```

```
## [1] 10766.19
```

```r
median_total_steps_per_day=median(total_steps_per_day$total_steps)
print(median_total_steps_per_day)
```

```
## [1] 10766.19
```



## Are there differences in activity patterns between weekdays and weekends?

```r
activity_new_wkd<-activity_new%>%mutate(wkd=ifelse(isWeekend(date),"weekend","weekday"))
activity_new_wkd$wkd<-factor(activity_new_wkd$wkd)
activity_wkd_weekday=activity_new_wkd%>%group_by(wkd,interval)%>%summarise(mean_steps_per_interval=mean(steps))
g<-ggplot(activity_wkd_weekday,aes(x=interval,y=mean_steps_per_interval))
g+geom_line(col="blue")+facet_wrap(~wkd,ncol=1)+theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

