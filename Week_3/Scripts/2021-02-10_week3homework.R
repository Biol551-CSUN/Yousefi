### Today we are going to plot penguin data ####
### Created by Nikolas Yousefi and Group 4 #################
### Updated on 2021-02-10 ######################


### Load libraries ####
library(palmerpenguins)
library(tidyverse)
library(here)
library(hrbrthemes)
library(viridis)

### Load Data ####
glimpse(penguins) # View a summary of the data

penguins <- penguins %>% mutate(sex = fct_recode(sex, "Male" = "male", "Female" = "female"))
# Properly capitalize the sexes

penguins %>% filter(sex == "Male" | sex == "Female") %>% # Filter by sex
  filter(species == "Adelie") %>%  # Filter out other species, leaving only Adelie
  ggplot(aes(x=sex, y=body_mass_g, fill=sex)) + # Mapping our axes
  geom_boxplot() +  # Using a boxplot as the basis for our graph
  scale_fill_viridis(discrete = TRUE, alpha=0.6) + # Filled in boxplot with color-blind friendly colors
  geom_jitter(color="black", size=0.4, alpha=0.9) + # Adds each individual as a dot
  theme_bw() + # Black and white theme selection
  theme(strip.background = element_rect(fill = "white", color = "white"), # Forcing a white background
        strip.text = element_text(face = "bold", size = 10), # Bolding and increasing text of islands
        legend.position="none", # Removing the legend
        plot.title = element_text(size=20) # Increasing title font size
  ) +
  facet_grid(~island) + # Placing all the graphs in one row
  labs(y="Body Mass (g)", x="Sex", 
       title="Adelie Penguins Body Mass by Island") # Labeling the axes and title
ggsave(here("Week_3","Output","Week3Homework_groupplot.png"), 
       width = 7, height = 5) # Saving the graph to our folder
