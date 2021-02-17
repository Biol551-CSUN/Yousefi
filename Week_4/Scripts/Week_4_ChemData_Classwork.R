### Today we are going to practice tidy with biogeochemistry data from Hawaii ####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-02-17 ####################


#### Load Libraries ######
library(tidyverse)
library(here)

### Load data ######
ChemData<-read_csv(here("Week_4","Data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% #filters out incomplete rows
  separate(col = Tide_time, # choose the Tide_Time column
           into = c("Tide","Time"), # separate it into two columns, Tide and Time
           sep = "_", # separate by _
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd, # pivoting columns from Temp_in to percent_sgd  
               names_to = "Variables", # names of the new columns with all the column names 
               values_to = "Values") %>% # names of the new column with all the values 
  group_by(Variables, Site, Time) %>% # selecting our groups to view
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>% # calculating mean
  pivot_wider(names_from = Variables, 
              values_from = mean_vals) %>% # notice it is now mean_vals as the col name
  write_csv(here("Week_4","Output","summary.csv"))  # export as a csv to the right folder
