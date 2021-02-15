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
  drop_na(sex) %>%
  group_by(species, island, sex) %>%
  summarise(mean_mass = mean(body_mass_g, na.rm=TRUE),
            var_mass = var(body_mass_g, na.rm=TRUE))

### Part 2 ######

penguins %>%
  filter(sex == "Female") %>%
  mutate(log_mass = log(body_mass_g)) %>%
  select(species, island, sex, log_mass) %>%
ggplot(aes(x = island, y = log_mass, fill = sex)) + 
geom_boxplot() + 
  theme_bw() + 
theme(strip.text = element_text(face = "bold", size = 10),
      legend.position = "none", plot.title = element_text(face = "bold")) + 
facet_grid(species~.) + 
labs(y="Log Body Mass (g)", x="Island", 
     title="Female Penguin Log Body Mass by Island") + 
ggsave(here("Week_4","Output","Week4_Homework_penguin_2021-02-15.png"), 
       width = 7, height = 5) # Saving the graph to our folder
