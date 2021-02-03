# This is my first script, I am learning how to import data.
# Created by Nikolas Yousefi
# Created on 2021-02-03
#########################################


### Load libraries #####
library(tidyverse)
library(here)


### Read in data #####
WeightData <- read_csv(here("Week_2","data","weightdata.csv"))


### Data Analysis #####

head(WeightData) 
tail(WeightData)
View(WeightData)
