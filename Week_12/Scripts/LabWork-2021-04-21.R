##### Week 12 Lab Work ##############
##### Created by: Nikolas Yousefi#####
##### Updated: 2021-04-21 ############

##### Libraries ##########
library(tidyverse)
library(here)

##### Load Data ##########
intertidal <- read_csv(here("Week_12","Data", "intertidaldata.csv"))

latitude <- read_csv(here("Week_12","Data", "intertidaldata_latitude.csv"))

##### Cleaning Data ###########
tidalclean <- intertidal %>% select(c(1:3, 11:13)) %>% # Selecting columns of interest
  transform(Quadrat = str_replace_all(Quadrat, "\\.|[0-9]", "")) %>% # Removing any unnecessary punctuation and numbers
  mutate(Quadrat = str_trim(Quadrat), 
         Quadrat = factor(Quadrat, levels = c("Low", "Mid", "High"))) %>% # Setting levels for the Quadrat column
  rename("Whelks" = "Whelks..Counts.", 
         "Crabs" = "Crabs..Counts.", 
         "Sea Stars" = "Stars..Counts.") %>% # Renaming columns for easier parsing
  pivot_longer(cols = c(4:6), 
               names_to = "Invertebrates", 
               values_to = "Count") %>% # Pivoting to condense all vertebrates into one column
  filter(Invertebrates != "Whelks") # Removing whelks as they're skewing the data heavily

unique(tidalclean$Quadrat) # Checking the levels

tidedata = right_join(tidalclean, latitude) # Combining the two data sets

##### Graphing Data ##########
ggplot(tidedata, aes(x = fct_reorder(Site, Latitude),
                       y = Count,
                       fill = Invertebrates))+ # Setting up the basic plot and ordering site by ascending latitude
  geom_col()+ # Making a column graph
  theme_bw()+ # Basic black/white theme
  theme(plot.title=element_text(size = 18), # Plot title font size
        axis.title = element_text(size = 15), # Axis titles font size
        axis.text.x = element_text(angle = 50, size = 12, hjust = 1), # Angling x-axis text
        axis.text.y = element_text(size = 10), # Adjusting font sizes of various text elements
        legend.position = "bottom")+ # Moving legend to the bottom
  labs(x = "Site",
       title = "Non-Whelk Invertebrate Counts by Site",
       subtitle = "Sites ordered by ascending latitude")+ # Adding labels, plus title and subtitle
  ggsave(here("Week_12", "Output", "LabWorkPlot-2021-04-21.png"),
         width = 14, height = 7) # Saving in appropriate folder at desired size
