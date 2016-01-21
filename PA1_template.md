# Reproducible Research: Peer Assessment 1


```r
## Loading and preprocessing the data
setwd("D:/Reproducible research/RepData_PeerAssessment1")
unzip("./activity.zip")
data <- read.csv("./activity.csv",stringsAsFactors = FALSE, na.strings = NA)
data$date <- as.Date(data$date, "%Y-%m-%d")
Sys.setlocale("LC_TIME","English")
```

```
## [1] "English_United States.1252"
```

Histogram of the total number of steps taken each day


```r
library(dplyr)
data <- tbl_df(data)
## What is mean total number of steps taken per day?
stepsxday <- data %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = T),
                                      mean_steps = mean(steps, na.rm = T))
library(ggplot2)
ggplot(stepsxday,aes(total_steps))+geom_histogram()+labs(x="total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Mean and median number of steps taken each day


```r
summary(stepsxday[,2:3])
```

```
##   total_steps      mean_steps     
##  Min.   :    0   Min.   : 0.1424  
##  1st Qu.: 6778   1st Qu.:30.6979  
##  Median :10395   Median :37.3785  
##  Mean   : 9354   Mean   :37.3826  
##  3rd Qu.:12811   3rd Qu.:46.1597  
##  Max.   :21194   Max.   :73.5903  
##                  NA's   :8
```

Time series plot of the average number of steps taken


```r
## What is the average daily activity pattern?
stepsxinterval <- data %>% group_by(interval) %>% 
        summarise(mean_steps = mean(steps, na.rm = T))

ggplot(stepsxinterval, aes(interval,mean_steps))+ geom_line()+ labs(x="5-minute interval", y = " average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The 5-minute interval that, on average, contains the maximum number of steps


```r
stepsxinterval$interval[stepsxinterval$mean_steps==max(stepsxinterval$mean_steps)]
```

```
## [1] 835
```

Code to describe and show a strategy for imputing missing data


```r
## Imputing missing values
any(is.na(data[,2:3])) # There is only NAs in the fist column
```

```
## [1] FALSE
```

```r
data1 <- data
for(i in seq_along(data1$steps)) {
        if(is.na(data1[i,1])) {
                row <- stepsxinterval$interval
                data1$steps[i] <- subset(stepsxinterval$mean_steps,row == data$interval[i])     }
}
```

Histogram of the total number of steps taken each day after missing values are imputed


```r
stepsxday1 <- data1 %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = T),
                                      mean_steps = mean(steps, na.rm = T))

ggplot(stepsxday1,aes(total_steps))+geom_histogram()+labs(x="total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
summary(stepsxday1[,2:3])
```

```
##   total_steps      mean_steps     
##  Min.   :   41   Min.   : 0.1424  
##  1st Qu.: 9819   1st Qu.:34.0938  
##  Median :10766   Median :37.3826  
##  Mean   :10766   Mean   :37.3826  
##  3rd Qu.:12811   3rd Qu.:44.4826  
##  Max.   :21194   Max.   :73.5903
```
                The mean and median of de total number of steps taken each day after missing values are imputed are similar because I use the mean for replace the missing values.
                
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
## Are there differences in activity patterns between weekdays and weekends?
data1$typeday <- ifelse(weekdays(data1$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

mean_wy <- data1 %>% filter(typeday == "weekday") %>% group_by(interval) %>%
        summarise(steps= mean(steps)) %>% mutate(typeday = "weekdays")

mean_wd <- data1 %>% filter(typeday == "weekend") %>% group_by(interval) %>%
        summarise(steps= mean(steps)) %>% mutate(typeday = "weekends")

mean_by_type <-bind_rows(mean_wy ,mean_wd)

ggplot(mean_by_type,aes(interval,steps))+geom_line()+ facet_grid(typeday~.)+ labs(x = "5-minute interval", y = "number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

There are differences in activity patterns between weekdays and weekends, in weekdays there are higher activity before the 10000th 5-minute interval, after these there are higher activity in weekends.
