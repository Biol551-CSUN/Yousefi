### Lubridate Practice####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-02-24 ####################


#### Load Libraries ######
library(tidyverse)
library(here)
library(lubridate)

### Load data ######
datetimes<-c("02/24/2021 22:22:20",
             "02/25/2021 11:21:10",
             "02/26/2021 8:01:52")

datetimes <- mdy_hms(datetimes)

month(datetimes)
month(datetimes, label = TRUE)
month(datetimes, label = TRUE, abbr = FALSE) #Spell it out
day(datetimes) # extract day
wday(datetimes, label = TRUE) # extract day of week
hour(datetimes)
minute(datetimes)
second(datetimes)
datetimes + hours(4) # this adds 4 hours
datetimes + days(2) # this adds 2 days
round_date(datetimes, "minute") # round to nearest minute
round_date(datetimes, "5 mins") # round to nearest 5 minute

### Think, Pair, Share ##########

CondData <- read_csv(here("Week_5","Data", "CondData.csv"))

View(CondData)

CondData_datetime <- CondData %>% 
  mutate(DateTime = ymd_hms(date))

View(CondData_datetime)
