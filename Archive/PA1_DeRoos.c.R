## Assignment PA1 for Coursera (RepData), Albert de Roos, 

library(dplyr)

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


##
     #create vector with average values fo timeinterval
          
          myrepdatafilled <- myrepdata
          for (n in 1:nrow(myrepdatafilled)){
               if (is.na(myrepdatafilled[n, "steps"])) {
                    interval = myrepdatafilled[n,3]
                    interval_average <- stepsperinterval[stepsperinterval$interval == interval, ]
                    myrepdatafilled[n, 1] <- interval_average$avgsteps[[1]]
               }
          }
 


