Loading and preprocessing the data
----------------------------------

    library(ggplot2)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(gridExtra)

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    dat<- read.csv("activity.csv")

What is mean total number of steps taken per day?
-------------------------------------------------

    # Calculate the total number of steps taken per day
    dailysteps<- aggregate(steps ~date,dat , sum, na.action = na.omit)

    # Make a histogram of the total number of steps taken each day
    qplot(dailysteps$steps,geom = "histogram", binwidth = 600, xlab = "Number of steps daily")

![](PA_Completed_files/figure-markdown_strict/mean%20steps-1.png)

    # Calculate and report the mean and median of the total number of steps taken per day
    meansteps<- mean(dailysteps$steps)
    medsteps<- median(dailysteps$steps)

Mean total steps per day is 1.076618910^{4}.

Median total steps per day is 10765.

What is the average daily activity pattern?
-------------------------------------------

    # Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    intvstep<- aggregate(steps ~interval, dat , mean, na.action = na.omit)
    g<- ggplot(data = intvstep, aes(x = interval, y = steps, group = 1)) +
        geom_line()
    print(g)

![](PA_Completed_files/figure-markdown_strict/avg%20daily%20pattern-1.png)

    maxstep<- intvstep$interval[which.max(intvstep$steps)]

The 5 min interval is 835.

Imputing missing values
-----------------------

    # Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ????????s)
    summary(dat)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    # Devise a strategy for filling in all of the missing values in the dataset.
    # Create a new dataset that is equal to the original dataset but with the missing data filled in.
    dat2<-dat
    for(n in 1:nrow(dat2)){
      if(is.na(dat2$steps[n])){
        dat2$steps[n] <- intvstep$steps[which(intvstep$interval == dat2$interval[n])]
      }
    }
    summary(dat2)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##                   (Other)   :15840

    # Make a histogram of the total number of steps taken each day 
    dailysteps2<- aggregate(steps ~date,dat2 , sum, na.action = na.omit)
    qplot(dailysteps2$steps,geom = "histogram", binwidth = 600, xlab = "Number of steps daily")

![](PA_Completed_files/figure-markdown_strict/input%20missing%20values-1.png)

    # Calculate and report the mean and median total number of steps taken per day. 
    meansteps2<- mean(dailysteps2$steps)
    medsteps2<- median(dailysteps2$steps)

    # Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Total number of missing values in the dataset = 2304

Mean total steps per day (missing values corrected) is 1.076618910^{4}.

Median total steps per day (missing values corrected) is
1.076618910^{4}.

The mean total steps per day is the same but the median total steps per
day has a slight shift (10765 (original) vs 10766.19(corrected)).

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

    # Create a new factor variable in the dataset with two levels - ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.
    dat2<- mutate(dat2, weekday =  weekdays(ymd(dat2$date)))
    weekdaydat<- filter(dat2, weekday == "Monday"| weekday == "Tuesday" |weekday == "Wednesday"|weekday == "Thursday"|weekday == "Friday")
    weekenddat<- filter(dat2, weekday == "Saturday"| weekday == "Sunday")

    # Make a panel plot containing a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
    intvstepweekday<- aggregate(steps ~interval, weekdaydat , mean, na.action = na.omit)
    intvstepweekend<- aggregate(steps ~interval, weekenddat , mean, na.action = na.omit)
    i<- ggplot(data = intvstepweekday, aes(x = interval, y = steps, group = 1)) +
        geom_line() +
        ggtitle("Weekday") 
    j<- ggplot(data = intvstepweekend, aes(x = interval, y = steps, group = 1)) +
        geom_line() +
        ggtitle("Weekend") 
    grid.arrange(i, j, ncol=1, nrow =2)

![](PA_Completed_files/figure-markdown_strict/activity%20patterns-1.png)
