---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---


## Loading and preprocessing the data

### Loading Libraries, defining variables and set location to US

```r
library(dplyr)

fileURL      <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
localZipFile <- "activity.zip"
localFile    <- "./data/activity.csv"

## TODO
## Sys.setlocale()
```

### Helper functions

```r
## Download and Extract Zip data file
downloadAndExtractZipFile <- function(){
  ## check if the data folder exists
  if(!file.exists("data")){
    dir.create("data")
  }
  
  ## check if local file already exists
  if(!file.exists(localFile)){
    
    ## check if zip file exists - if not download the zip file with the data
    if(!file.exists(localZipFile)){
      download.file(fileURL, destfile = localZipFile, method = "curl")
    }
    
    ## extract the zip file
    unzip(localZipFile, overwrite = TRUE, exdir = "./data")
  }
}

## Reads the data file
readCSVFile <- function(fileName, ...){
  if(! file.exists(fileName)){
    stop(paste("readDataFile: File ", fileName, " doesn't exist"))
  }
  
  print(paste("Reading file ", fileName))
  read.csv(fileName, ...)
}
```

### Load the data (unzip and download if needed)

```r
downloadAndExtractZipFile()

data <- readCSVFile(localFile)
```

```
## [1] "Reading file  ./data/activity.csv"
```

```r
## summary(data)
```



## What is mean total number of steps taken per day?

### A histogram of the total number of steps taken each day

```r
numSteps <- data %>%
   filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarise(totalSteps = sum(steps))

hist(x = numSteps$totalSteps, freq = TRUE,
     xlab = "Total number of steps",
     main = "")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

### Calculate and report the mean and median total number of steps taken per day
#### Mean of total number of steps taken per day

```r
mean(numSteps$totalSteps)
```

```
## [1] 10766.19
```
#### Median of total number of steps taken per day

```r
median(numSteps$totalSteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

### A time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
daily <- data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(av = mean(steps, rm.na = TRUE))

plot(x = daily$interval, y = daily$av, type = "l", main = "",
     xlab = "Interval (minutes)",
     ylab = "Average of steps taken")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, 
contains the maximum number of steps?

```r
daily$interval[daily$av == max(daily$av)]
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nas <- data %>%
  filter(is.na(steps))

nrow(nas)
```

```
## [1] 2304
```


2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
##Mean steps per day ignoring NAs
meanStepsDay <- data %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarise(mean = mean(steps))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newData <- data
newData$steps[is.na(newData$steps)] <- 
  meanStepsDay$mean[match(meanStepsDay$date, newData$date)]##[is.na(df$value)]
```

```
## Warning in newData$steps[is.na(newData$steps)] <-
## meanStepsDay$mean[match(meanStepsDay$date, : number of items to replace is
## not a multiple of replacement length
```

```r
##newData <- merge(y = newData, x = meanStepsDay)
##newData2 <- ddply(newData, .(date), transform, steps=ifelse(is.na(steps), meanStepsDay$mean[meanStepsDay$date == date], steps))

newData$steps[is.na(newData$steps)] <- meanStepsDay$steps[meanStepsDay$date == newData$date]
```

```
## Warning in is.na(e1) | is.na(e2): longer object length is not a multiple
## of shorter object length
```

```
## Warning in `==.default`(meanStepsDay$date, newData$date): longer object
## length is not a multiple of shorter object length
```

```
## Error in newData$steps[is.na(newData$steps)] <- meanStepsDay$steps[meanStepsDay$date == : replacement has length zero
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

