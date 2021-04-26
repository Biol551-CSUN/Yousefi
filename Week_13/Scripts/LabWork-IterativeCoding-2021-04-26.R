##### Week 13 - Lab Work: Iterative Coding ##############
##### Created by: Nikolas Yousefi#####
##### Updated: 2021-04-26 ############

##### Libraries ######
library(tidyverse)
library(here)

##### Using Looping ###########

hwpath <- here("Week_13", "Data", "homework") # Setting up the pathway

files <- dir(path = hwpath, pattern = ".csv") # Setting up directory

files

tide_data <- data.frame(matrix(nrow = length(files), ncol = 5)) # Making the empty table

colnames(tide_data)<-c("filename",
                       "mean_temp", 
                       "mean_intensity", 
                       "stdev_temp", 
                       "stdev_intensity") # Naming the columns

tide_data

for (i in 1:length(files)){
  raw_data <- read_csv(paste0(hwpath,"/",files[i]))
  tide_data$filename[i]<-files[i] # Filename
  tide_data$mean_temp[i]<-mean(raw_data$Temp.C, na.rm =TRUE) # Mean temp
  tide_data$mean_intensity[i]<-mean(raw_data$Intensity.lux, na.rm =TRUE) # Mean intensity
  tide_data$stdev_temp[i]<-sd(raw_data$Temp.C, na.rm =TRUE) # Standard dev. tamp
  tide_data$stdev_intensity[i]<-sd(raw_data$Intensity.lux, na.rm =TRUE) # Standard dev. intensity
} 

tide_data

##### Using Mapping ###########

hwpath2 <- here("Week_13", "Data", "homework") # Setting up pathway

tidepools <- dir(path = hwpath2, pattern = ".csv", full.names = TRUE) # Setting up directory

tidepools

tidemeans <- tidepools %>%
  set_names()%>% # Setting ID of each list to the file name
  map_df(read_csv,.id = "filename") %>% # Mapping everything to the dataframe; adding filename column
  group_by(filename) %>% # Grouping by filename
  summarise(mean_temp = mean(Temp.C, na.rm = TRUE), # Mean temp
            mean_intensity = mean(Intensity.lux, na.rm = TRUE), # Mean intensity
            stdev_temp = sd(Temp.C, na.rm = TRUE), # Standard dev. temp
            stdev_intensity = sd(Intensity.lux, na.rm = TRUE)) # Standard dev. intensity

tidemeans
