

#Load Data 

data <- read.csv("activity.csv", header = TRUE)

#total steps per day 
totalSteps <- aggregate(steps ~ date , data , FUN = sum)


hist(totalSteps$steps,
     main = "Total steps per day",
     xlab = "Number of Steps")




#mean steps per day 

meanSteps <- mean(totalSteps$steps , na.rm = TRUE)
print(meanSteps)



#median steps 

medianSteps <- median(totalSteps$steps, na.rm = TRUE)
print(medianSteps)



#Time series plot of the average number of steps taken

library(ggplot2)


meanStepsWithInterval<- aggregate(steps ~ interval, data, mean)
ggplot(data = meanStepsWithInterval, aes(x = interval, y = steps)) +
  geom_line() +
  ggtitle("Average Daily Activity Pattern") +
  xlab("5-minute Interval") +
  ylab("Average Number of Steps") +
  theme(plot.title = element_text(hjust = 0.5))



    # max interval

maximumInterval <- meanStepsWithInterval[which.max(meanStepsWithInterval$steps),]
print(maximumInterval)





#replace missing (NA ) Data 


missingValues <- is.na(data$steps)

    # transform NA values to workable data 

missingActivity <- transform(data , 
                             steps = ifelse(is.na(data$steps),
                                            meanStepsWithInterval$steps[match(data$interval,
                                                                              meanStepsWithInterval$interval)],
                                            data$steps))


missingActivityWorkable <- aggregate(steps ~ date , missingActivity, FUN = sum)
hist(missingActivityWorkable$steps,
     main = "Missing Value Imputed steps per day",
     xlab = "Number of steps")








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



missingActivity$date <- as.Date(missingActivity$date)
missingActivity$day <- sapply(missingActivity$date, FUN = dayType)



# Time series 


totalMeanSteps <- aggregate(steps ~ interval + day , missingActivity, mean)
ggplot(data = totalMeanSteps, aes(x=interval , y= steps ))+
  geom_line() + 
  facet_grid(day ~ .) + 
  ggtitle("Average Daily Activity Pattern") + 
  xlab("5 Minute Interval") + 
  ylab("Average # Steps ") 
