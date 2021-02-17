### Today we are going to do lab work using biogeochemistry data from Hawaii ####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-02-17 ####################


#### Load Libraries ######
library(tidyverse)
library(here)


### Load data ######
ChemData<-read_csv(here("Week_4","Data", "chemicaldata_maunalua.csv"))

ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% #filters out incomplete rows
  separate(col = Tide_time, # choose the Tide_Time column
           into = c("Tide","Time"), # separate it into two columns, Tide and Time
           sep = "_") %>% # separate by _
  filter(Time == "Day") %>%  # filter our to only show Day times
  select(-c("Time", "percent_sgd")) %>%  
  rename("Temp (C)" = Temp_in,
         "NN (umol/L)" = NN,
         "Phosphate (umol/L)" = Phosphate,
         "Silicate (umol/L)" = Salinity,
         "TA (umol/Kg)" = TA) %>%  #Renaming some of the graph titles %>% 
  pivot_longer(cols = c(8:14), # pivoting columns from Temp_in to TA
               names_to = "Variables", # names of the new columns with all the column names 
               values_to = "Values") %>% # names of the new column with all the values
  group_by(Season, Tide, Variables) %>% 
  summarise(Means = mean(Values, na.rm = TRUE),# calculating mean
            variations = var(Values, na.rm = TRUE), # calculating variations
            stdev = sd(Values, na.rm = TRUE)/sqrt(length(na.omit(Values)))) %>%   # calculating standard error
write_csv(here("Week_4","Output","Week_4_GroupLabWorkSummary_2021-02-17.csv"))  # export as a csv to the right folder

ggplot(ChemData_clean, aes(x = Season, y = Means, fill = Tide)) + # Labeling axes
  geom_bar(stat = 'identity', position = 'dodge') + # Bar graph getting cleaned up
  facet_wrap(~Variables, scales = "free") + # Setting scales
  theme_bw() +
  theme(strip.text = element_text(face = "bold")) + # Theme selction and text bolding
  labs(title = "Hawai'i Biogechemistry Data by Tide and Season")
ggsave(here("Week_4","Output","Week_4_BarGraph_ChemData_2021-02-17.png"), 
         width = 7, height = 5) # Saving the graph to my folder
