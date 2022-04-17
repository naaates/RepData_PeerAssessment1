---
title: "PA1_template"
author: "naaates"
date: "4/17/2022"
output:
  html_document: 
      keep_md: true
---

## About the Dataset:

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

- **Dataset**: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as \color{red}{\verb|NA|}NA)

- **date**: The date on which the measurement was taken in YYYY-MM-DD format

- **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and Preprocessing the data
  
Reading the Activity Data:

```r
activity = read.csv("activity.csv")
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

## What is mean total number of steps taken per day?
  
#### Calculating the total steps taken per day:

```r
TotalSteps = aggregate(activity$steps, by = list(activity$date), FUN = sum)
colnames(TotalSteps) = c("Date", "Total_Steps")
head(TotalSteps)
```

```
##         Date Total_Steps
## 1 2012-10-01          NA
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

#### Removing the NA rows:  


```r
TotalSteps = na.omit(TotalSteps)
```

#### Creating a histogram of the Total Steps Taken:  


```r
library(ggplot2)
x = qplot(TotalSteps$Total_Steps,
      geom="histogram",
      main = "Histogram for Steps per Day", 
      xlab = "Steps",  
      fill=I("pink"), 
      col=I("white"))
x
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
  
#### Mean and Median of the Total Steps taken per day:

```r
#Mean
mean(TotalSteps$Total_Steps)
```

```
## [1] 10766.19
```

```r
#Median
median(TotalSteps$Total_Steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

#### Calculating the average steps per interval:


```r
activity1 = na.omit(activity)
avgsteps = aggregate(activity1$steps, by = list(activity1$interval), FUN = mean)
colnames(avgsteps) = c("interval", "Avg_Steps")
head(avgsteps)
```

```
##   interval Avg_Steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

#### Time series plot of the 5-minute interval:

```r
p = ggplot(avgsteps, aes(x=interval, y=Avg_Steps)) + geom_line(color = "pink") +
   ggtitle("Average Steps taken per 5-Minute Interval")  +
   theme_minimal() + 
   theme(plot.title = element_text(hjust=0.5, size=20, face="bold")) +
   xlab('Interval') +
   ylab('Avgerage Steps')
p
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### Finding the Interval with the largest average steps:

```r
avgsteps[which.max(avgsteps$Avg_Steps),]
```

```
##     interval Avg_Steps
## 104      835  206.1698
```

## Imputing Missing Values:

#### Number of Missing Values for the column "steps"

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


#### Imputing the Missing Values with the Mean Steps per Interval:

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.1.2
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
#Add another column for the average steps per interval
df1 = activity
df2 = merge(x=df1,y=avgsteps,by="interval",all.x=TRUE)

#Replace NA values with the average steps per interval
df2$NewSteps = if_else(is.na(df2$steps),df2$Avg_Steps, as.numeric(df2$steps))

#Fix Sorting by Date then by Interval
df2 = df2[order( df2[,3], df2[,1] ),]
```


#### Calculating the Total Steps per day after replacing the missing values:

```r
## Updated histogram with the replaced values
updatedhist = aggregate(df2$NewSteps, by = list(df2$date), FUN = sum)
colnames(updatedhist) = c("Date", "Total_Steps")
```

#### Updated histogram for the total steps taken per day:

```r
library(ggplot2)
x = qplot(updatedhist$Total_Steps,
          geom="histogram",
          main = "Updated Histogram for Steps per Day", 
          xlab = "Steps",  
          fill=I("pink"), 
          col=I("white"))
x
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

#### Mean and Median Steps per day:


```r
#Mean
mean(updatedhist$Total_Steps)
```

```
## [1] 10766.19
```

```r
#Median
median(updatedhist$Total_Steps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

#### Creating a dayOftheWeek and Weekday column:


```r
df2$date = as.POSIXct(df2$date, format = "%Y-%m-%d")
df2$dayOftheWeek = weekdays(df2$date)
df2$Weekday = if_else(df2$dayOftheWeek == "Saturday" | df2$dayOftheWeek == "Sunday", "Weekend", "Weekday")
```


#### Creating Weekend and Weekday Subsets:

```r
Weekends = subset(df2, df2$Weekday == "Weekend")
Weekday = subset(df2, df2$Weekday == "Weekday")
```

#### Calculating the Average Steps Taken per Interval for both Subsets:

```r
avgWeekendSteps = aggregate(Weekends$NewSteps, by = list(Weekends$interval), FUN = mean)
colnames(avgWeekendSteps) = c("interval", "Avg_Steps")

avgWeekdaySteps = aggregate(Weekday$NewSteps, by = list(Weekday$interval), FUN = mean)
colnames(avgWeekdaySteps) = c("interval", "Avg_Steps")
```


#### Time Series Panel Plots to compare Weekday and Weekend Activity:

```r
ggplot() + 
   geom_line(data = avgWeekdaySteps, aes(x = interval, y = Avg_Steps), color = "green") +
   geom_line(data = avgWeekendSteps, aes(x = interval, y = Avg_Steps), color = "pink") +     
   ggtitle("Average Steps taken per 5-Minute Interval")  +
   theme_minimal() + 
   theme(plot.title = element_text(hjust=0.5, size=20, face="bold")) +
   xlab('Interval') +
   ylab('Avgerage Steps') + scale_color_manual(values = colors)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->





