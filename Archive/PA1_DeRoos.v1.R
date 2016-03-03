## Assignment PA1 for Coursera (RepData), Albert de Roos, 

library(dplyr)
library(ggplot2)

## First read in the data file

Sys.setlocale("LC_TIME", "English")
myrepdata <- read.csv("activity.csv")
myrepdata$date <- as.Date(myrepdata$date)


## Determine total steps per day and put in a histogram
stepsperday <- myrepdata %>%  group_by(date) %>%
summarize(sum = sum(steps))
hist(stepsperday$sum, breaks = 15)
summary(stepsperday$sum)
nrow(stepsperday)
hist(stepsperday$sum, breaks = 15, xlab = "total steps per day", main = "Steps per day distribution")

##average daily activity pattern?
stepsperinterval <- myrepdata %>%  group_by(interval) %>% summarize(avgsteps = mean(steps, na.rm = TRUE))
plot(stepsperinterval$interval, stepsperinterval$avgsteps, type = "l", main = "average daily pattern", xlab = "interval", ylab = "avg steps")
stepsperinterval[which.max(stepsperinterval$avgsteps), ]


##Imputing missing values
summary(myrepdata$steps)
myrepdata2 <- myrepdata[complete.cases(myrepdata),]
stepsperinterval2 <- myrepdata %>% group_by(interval) %>% summarize(count = n())
stepsperinterval3 <- myrepdata2 %>% group_by(interval) %>% summarize(count = n())
summary(stepsperinterval2)
summary(stepsperinterval3)


##create a new dataset with missing values filled in
     myrepdatafilled <- myrepdata
          for (n in 1:nrow(myrepdatafilled)){
               if (is.na(myrepdatafilled[n, "steps"])) {
                    interval = myrepdatafilled[n,3]
                    interval_average <- stepsperinterval[stepsperinterval$interval == interval, ]
                    myrepdatafilled[n, 1] <- interval_average$avgsteps[[1]]
               }
          }
 
          head(myrepdata, 5)
          head(myrepdatafilled, 5)
          head(stepsperinterval, 5)

##Compare the new filled histogram
          
          dev.off()
          stepsperday <- myrepdata %>% group_by(date) %>% summarize(sum = sum(steps))
          str(stepsperday)
          hist(stepsperday$sum, breaks = 15, xlab = "original total steps per day", main = "Original: Steps per day distribution")
          summary(stepsperday$sum)
          
          stepsperdayfilled <- myrepdatafilled %>% group_by(date) %>% summarize(sum = sum(steps))
          str(stepsperdayfilled)
          hist(stepsperdayfilled$sum, breaks = 15, xlab = "total steps per day", main = "Filled: Steps per day distribution")
          summary(stepsperdayfilled$sum)
          
## Compare weekdays versus weekend
     
     # add column with values
     myrepdataplus <- myrepdata
     weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
     myrepdataplus$weekday <- factor((weekdays(myrepdataplus$date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

     # group and summarize the data and make graph
     stepsperintervalplus <- myrepdataplus %>%  group_by(interval, weekday) %>% summarize(avgsteps = mean(steps, na.rm = TRUE))
     ggplot(data = stepsperintervalplus, aes(x = interval, y = avgsteps)) + 
          ggtitle("Average Daily Patterns Weekdays and Weekend") + ylab("Average Number of Steps") +
          facet_grid(weekday ~ .) +
          geom_line()

  
          