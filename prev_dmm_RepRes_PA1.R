# dmm_RepRes_PA1.R

padleadingzeros = function (x){
    if (nchar(x)==1) (paste("000",x, sep =""))
    else if (nchar(x)==2) (paste("00",x, sep =""))
    else if (nchar(x)==3) (paste("0",x, sep =""))
    else x
}

setwd("~/Documents/DataScienceSpecialization/5_Reproducible_Research/Project_1")


## Loading and preprocessing the data

# 1. Load the data (i.e. read.csv())
actfile <- "./activity.csv"
act <- read.csv(actfile, header = TRUE, sep = "," ) #, 

# 2. Process/transform the data (if necessary) into a format suitable for your analysis


library(dplyr)
library(ggplot2)

totalbyday <- summarize(group_by(act, date), daytotal= sum(steps))
meanbyinterval <- 
    summarize(group_by(act, interval), intervalmean = mean(steps, na.rm=TRUE))


## What is mean total number of steps taken per day?

# 1. Make a histogram of the total number of steps taken each day
hist(totalbyday$daytotal, breaks = 10)

plot11 <- ggplot(totalbyday, aes(x=factor(date), y=daytotal)) +
    geom_bar(color="firebrick") +
    labs(x="Day", y="Total Number of Steps") +
    labs(title = "Total Number of Steps per Day")
plot11

# 2. Calculate and report the mean and median total number of steps taken per day
summary(totalbyday)
mean(totalbyday$daytotal, na.rm=TRUE)
median(totalbyday$daytotal, na.rm=TRUE)


## What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
with(meanbyinterval, plot(interval, intervalmean, type = "l"))


# 2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
meanbyinterval[which.max(meanbyinterval$intervalmean),]


#rownames(meanbyinterval)<-meanbyinterval$interval

# 1
# base1 <- (merge(lookup, largetable, by = 'HouseType'))    
# library(dplyr)


# 3. using the plyr package
#library(plyr)
#plyr1 <- join(largetable, lookup, by = "HouseType")

## Imputing missing values

# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
sum(is.na(act$steps))
dim(act)[1]

# 2. Devise a strategy for filling in all of the missing values in the dataset.
# The strategy does not need to be sophisticated. For example, 
# you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
actfilled <- merge(meanbyinterval, act, by = 'interval')    
actfilled <- arrange(actfilled, date, interval)
# 3. Create a new dataset that is equal to the original dataset 
# but with the missing data filled in.
actfilled[is.na(actfilled$steps), 'steps'] <- 
    actfilled[is.na(actfilled$steps), 'intervalmean']

# 4. Make a histogram of the total number of steps taken each day and 
# Calculate and report the mean and median total number of steps taken per day. 
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of 
# the total daily number of steps?

#groupedbydatefilled <- group_by(actfilled, date)
totalbydayfilled <- summarize(group_by(actfilled, date), daytotal= sum(steps))
hist(totalbydayfilled$daytotal, breaks=10)
summary(totalbydayfilled)
mean(totalbydayfilled$daytotal, na.rm=TRUE)
median(totalbydayfilled$daytotal, na.rm=TRUE)

## Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels -- "weekday" 
# and "weekend" indicating whether a given date is a weekday or weekend day.


actfilled <- mutate(actfilled, 
    dayofweek = weekdays(as.Date(date), abbreviate = TRUE))
actfilled <- mutate(actfilled, typeofday = as.factor( 
        ifelse(dayofweek == "Sat" | dayofweek == "Sun", "weekend", "weekday" )))

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of 
# the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis). 
# The plot should look something like the following, 
# which was created using simulated data:
    
meanbyintervalpertypeofday <-   
    summarize(group_by(actfilled,  typeofday,interval), 
              typeintervalmean = mean(steps, na.rm=TRUE))


g <- ggplot(meanbyintervalpertypeofday, aes(interval, typeintervalmean)) +
    geom_line(aes(color = "brickred"), show_guide=FALSE) + 
    facet_grid(typeofday ~ .) +
    labs(title = expression( "Comparison of Weekday and Weekend Activity")) +
    labs(x = "Interval number") + 
    labs(y = expression ("Average Number of Steps per Interval") ) 
print(g)
