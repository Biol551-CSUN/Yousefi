### Today we are going to plot penguin data ####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-02-15 ####################


#### Load Libraries ######
library(palmerpenguins)
library(tidyverse)
library(here)


### Load data ######
# The data is part of the package and is called penguins

glimpse(penguins)


### Part 1 ######

penguins %>% # use penguin dataframe
  drop_na(sex) %>% # All Nas dropped
  group_by(species, island, sex) %>% # Grouping by specific criteria
  summarise(mean_mass = mean(body_mass_g, na.rm=TRUE), # Created separate column with mean body mass
            var_mass = var(body_mass_g, na.rm=TRUE)) # Create separate column for variance in body mass

### Part 2 ######

penguins %>%
  filter(sex == "Female") %>% # Filter out males
  mutate(log_mass = log(body_mass_g)) %>% # Adding new column named log_mass
  select(species, island, sex, log_mass) %>% # Selecting which specific rows to keep
ggplot(aes(x = island, y = log_mass, fill = sex)) + # Specifying the axes
geom_boxplot() + # Creating a boxplot
  theme_bw() + # Selecting a basic theme
theme(strip.text = element_text(face = "bold", size = 10), # Bolding text
      legend.position = "none", plot.title = element_text(face = "bold")) + # Removing unnecessary legend and bolding title 
facet_grid(species~.) + # Showcasing all graphs via species
labs(y="Log Body Mass (g)", x="Island", 
     title="Female Penguin Log Body Mass by Island") + # Labeling axes and adding title
ggsave(here("Week_4","Output","Week4_Homework_penguin_2021-02-15.png"), 
       width = 7, height = 5) # Saving the graph to our folder
