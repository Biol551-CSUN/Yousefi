### Today we are going to practice joins with data from Becker and Silbiger (2020) ####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-02-22 ####################


#### Load Libraries ######
library(tidyverse)
library(here)


### Load data ######
# Environmental data from each site

EnviroData<-read_csv(here("Week_5","Data", "site.characteristics.data.csv"))

#Thermal performance data

TPCData<-read_csv(here("Week_5","Data","Topt_data.csv"))

glimpse(EnviroData)
glimpse(TPCData)

EnviroData_wide <- EnviroData %>% 
  pivot_wider(names_from = parameter.measured,
              values_from = values) %>%
  arrange(site.letter)

View(EnviroData_wide)


FullData_left<- left_join(TPCData, EnviroData_wide) %>% 
  relocate(where(is.numeric), .after = where(is.character))

## Joining, by = "site.letter"

View(FullData_left)

FullData_leftlonger <- FullData_left %>%
  pivot_longer(cols = c(5:23),
               names_to = "Variables",
               values_to = "Values") %>%
  group_by(site.letter, Variables) %>%
  summarise(Means = mean(Values, na.rm = TRUE),
            variations = var(Values, na.rm = TRUE))
  
  View(FullData_leftlonger)

  T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
               Temperature = c(14.1, 16.7, 15.3, 12.8))
  
  T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
              pH = c(7.3, 7.8, 8.1, 7.9))
  
  left_join(T1, T2)
  right_join(T1, T2)
  inner_join(T1, T2)
  full_join(T1,T2)
  semi_join(T1,T2)
  anti_join(T1,T2)  
  