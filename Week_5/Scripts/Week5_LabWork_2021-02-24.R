### Week 5 Lab Work####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-02-24 ####################


#### Load Libraries ######
library(tidyverse)
library(here)
library(lubridate)
library(ggplot2)
library(scales)

### Load Data ##########
CondData <- read_csv(here("Week_5","Data", "CondData.csv"))

DepthData <-  read_csv(here("Week_5","Data", "DepthData.csv"))

View(CondData)
View(DepthData)

### Data and Graph ##########
CondData_s <- CondData %>%
  mutate(date = ymd_hms(CondData$date),
         date = round_date(CondData$date, "10 seconds")) #Round to nearest 10 seconds

DepthData <- DepthData %>% mutate(date = ymd_hms(date)) #Converting DepthData to HMS format

CondDepthData <- inner_join(CondData_s, DepthData) #Combining Cond Data and Depth Data 
  
CDDmeans <- Cond_Depth_Data %>%
  mutate(Hour = hour(date), # Adding a separate hour column
         Minute = minute(date)) %>% # Adding a separate minute column
  group_by(Hour, Minute) %>% # Grouping by the new columns
  summarize(mean_date=mean(date), # Getting mean of dates
            mean_depth=mean(Depth), # Getting mean of depth measurements
            mean_temp=mean(TempInSitu), # Getting mean of temperature measurements
            mean_salinity=mean(SalinityInSitu_1pCal)) %>% # Getting mean of salinity measurements
  pivot_longer(cols = c(4:6), 
               names_to = "variables", 
               values_to = "means") %>% # Pivoting to aforementioned columns
  mutate(Minute = sprintf("%02d", Minute)) %>% # Forcing the decimal place to be 2 to show accurate times
  unite(col = "Hr_Min", 
        c(Hour,Minute), 
        sep = ".",
        remove = FALSE) # Combining the Hour and minute columns to its own separate column while keeping the original

  ggplot(CDDmeans, aes(x=mean_date, y=means, color=factor(variables))) + # Setting up the axes and specifying variables
  geom_point() + # Setting up a scatter plot
  facet_wrap(variables~., nrow=3, ncol=1, scales="free", # Organizing the graphs so it looks clean
             labeller = labeller(variables = 
                                   c("mean_depth" = "Depth",
                                     "mean_temp" = "Temperature",
                                     "mean_salinity" = "Salinity in situ 1pCal"))) + # Labeling each specific graph
  theme_bw() + # Using the plain black/white theme
  theme(legend.position = "none", legend.text = element_text(size=9), # Removing legend 
        legend.title = element_text(size=9), legend.margin = margin(0), # Removing margin and changing text size
        strip.background = element_rect(fill = "white", color = "white"), # Filling in the background with white
        strip.text.x = element_text(size=10, hjust=0, face="bold")) + # Changing text size of title
  labs(y="Mean per Minute", x="Time", title="Means of Various Measurments per Minute") + # Labeling the axes and title
    ggsave(here("Week_5","Output","Week5_Homework_LubridateLab.png"), 
           width = 10, height = 8)  # Saving graph in the appropriate folder

