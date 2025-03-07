---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: true
---


``` r
knitr::opts_chunk$set(
    echo = TRUE,
    message = FALSE,
    warning = FALSE,
    results = TRUE,
    cache = TRUE
)
```

## Loading and preprocessing the data

First, we load the required libraries for the task.


``` r
library(tidyverse)
library(ggplot2)
```

Next, we load and preprocess the data.


``` r
data <- read.csv("activity.csv" , header = TRUE)
```

## What is mean total number of steps taken per day?

For the total steps taken, we first group our data by summing the total number of steps and grouping by each date


``` r
totalSteps <- aggregate(steps ~ date , data , FUN = sum)
```

Next, we plot the histogram for the no. of steps.


``` r
hist(totalSteps$steps,
     main = "Total steps per day", 
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/histogramNumStepsPerDay-1.png)<!-- -->

Finally, we calculate the mean and median, and report it in the data frame.


``` r
#mean steps per day 
meanSteps <- mean(totalSteps$steps , na.rm = TRUE)
print(meanSteps)
```

```
## [1] 10766.19
```

``` r
#median steps 

medianSteps <- median(totalSteps$steps, na.rm = TRUE)
print(medianSteps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

First, we take the mean of the # of steps taken during each interval and store it in a meanStepsWithInterval.


``` r
#Time series plot of the average number of steps taken

library(ggplot2)

meanStepsWithInterval<- aggregate(steps ~ interval, data, mean)
```

Next, we plot the time series plot for daily activity pattern.


``` r
ggplot(data = meanStepsWithInterval, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/timeSeriesPlot-1.png)<!-- -->

For the interval during which the highest number of average steps are taken, the following code snipped calculates that:


``` r
 # max interval

maximumInterval <- meanStepsWithInterval[which.max(meanStepsWithInterval$steps),]
print(maximumInterval)
```

```
##     interval    steps
## 104      835 206.1698
```
Which translates to 08:35 to 08:40.


## Imputing missing values

To calculate the total number of missing values:


``` r
missingValues <- is.na(data$steps)
```
To compensate for the missing values, we substitute the values for the mean we found during the specific interval and store this in the variable missingActivity


``` r
missingActivity <- transform(data , 
                             steps = ifelse(is.na(data$steps),
                                            meanStepsWithInterval$steps[match(data$interval,
                                                                              meanStepsWithInterval$interval)],
                                            data$steps))
```

After imputing the data set, the new dataset we aggregate the data and calculate the sum of the steps taken


``` r
missingActivityWorkable <- aggregate(steps ~ date , missingActivity, FUN = sum)
hist(missingActivityWorkable$steps,
     main = "Missing Value Imputed steps per day",
     xlab = "Number of steps")
```

![](PA1_template_files/figure-html/InvalidWorkable-1.png)<!-- -->

Now we create the histrogram of the new dataset of the workable data that we imputted (missingActivityWorkable).


``` r
hist(missingActivityWorkable$steps,
     main = "Missing Value Imputed steps per day",
     xlab = "Number of steps")
```

![](PA1_template_files/figure-html/stepsPerDay2-1.png)<!-- -->

From the updated data, we see that there is no difference in mean, because essentially, we have replaced the NA's with means. There is an increase in median, however.


## Are there differences in activity patterns between weekdays and weekends?

To look for difference in activity patterns, we must make a function called dayType to determine what day of the week that we are dealing with in the data set 


``` r
# Weekdays vs Weekends 

dayType <- function(date) {
  day <- weekdays(date)
  if( day %in% c('Monday' , 'Tuesday','Wednesday',"Thursday",'Friday'))
    return("Weekday")
  else if (day %in% c('Saturday','Sunday'))
    return("Weekend")
  else
    stop("Invalid Date Formatting")
}
```

Next, we run the as.Date function on our data  


``` r
missingActivity$date <- as.Date(missingActivity$date)
missingActivity$day <- sapply(missingActivity$date, FUN = dayType)
```

Now, we plot our data using ggplot


``` r
# Time series 


totalMeanSteps <- aggregate(steps ~ interval + day , missingActivity, mean)
ggplot(data = totalMeanSteps, aes(x=interval , y= steps ))+
  geom_line() + 
  facet_grid(day ~ .) + 
  ggtitle("Average Daily Activity Pattern") + 
  xlab("5 Minute Interval") + 
  ylab("Average # Steps ") 
```

![](PA1_template_files/figure-html/PlottingComparison-1.png)<!-- -->

From this activity you (user) should be able to see the difference in steps on weekdays and weekends 
