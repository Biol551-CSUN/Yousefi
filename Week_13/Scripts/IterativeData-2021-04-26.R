##### Week 13: Iterative Coding ##############
##### Created by: Nikolas Yousefi#####
##### Updated: 2021-04-26 ############

##### Libraries ##########
library(tidyverse)
library(here)

##### Data ###############
print(paste("The year is", 2000))

years<-c(2015:2021)

for (i in years){ # set up the for loop where i is the index
  print(paste("The year is", i)) # loop over i
}

#Pre-allocate space for the for loop

# empty matrix

year_data<-data.frame(matrix(ncol = 2, nrow = length(years)))

# add column names

colnames(year_data)<-c("year", "year_name")

for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over year name
  year_data$year[i]<-years[i] # loop over year
}

year_data

#### Multiple csv files looping ###########
testdata<-read.csv(here("Week_13", "Data", "cond_data","011521_CT316_1pcal.csv"))

glimpse(testdata)

# point to the location on the computer of the folder
CondPath<-here("Week_13", "Data", "cond_data")

# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename
# you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = CondPath,pattern = ".csv")

files

##### Calculating Temp and salinity ##########

# pre-allocate space
# make an empty dataframe that has one row for each file and 3 columns
cond_data<-data.frame(matrix(nrow = length(files), ncol = 3))

# give the dataframe column names
colnames(cond_data)<-c("filename","mean_temp", "mean_sal")

cond_data

raw_data<-read.csv(paste0(CondPath,"/",files[1])) # test by reading in the first file and see if it works

head(raw_data)

mean_temp<-mean(raw_data$Temperature, na.rm = TRUE) # calculate a mean

mean_temp

for (i in 1:length(files)){ # loop over 1:3 the number of files
  raw_data<-read.csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
  cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE)
  cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE)
} 

cond_data

#### Working with purr ###########

1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15) %>%  # calculate 15 random numbers based on a normal distribution in a list
  map_dbl(mean) 

1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)

1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)

# point to the location on the computer of the folder
CondPath<-here("Week_13", "data", "cond_data")
files <- dir(path = CondPath,pattern = ".csv")
files

files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE)
#save the entire path name
files

data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity,na.rm = TRUE))

data
