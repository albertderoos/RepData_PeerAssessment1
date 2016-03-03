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
stepsperinterval <- myrepdata %>%  group_by(interval) %>% summarize(sum = sum(steps))