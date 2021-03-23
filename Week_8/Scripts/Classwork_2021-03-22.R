########## Week 8: Advanced Plotting ##########
##### Created by: Nikolas Yousefi #############
##### Created on: 2021-03-22 ###############

### Load libraries #####################
library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)
library(palmerpenguins)

### Load Data ##########################
View(penguins)

### Data Analysis ######################
p1 <- penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_length_mm, 
             color = species))+
  geom_point()

p2<-penguins %>%
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species))+
  geom_jitter(width = 0.2)

p1/p2+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A')

### Data Analysis 2 ###################
View(mtcars)

ggplot(mtcars, aes(x = wt, 
                  y = mpg, 
                  label = rownames(mtcars))) +
  geom_label_repel() + # creates a text label
  geom_point(color = 'red')

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1)+
  ggtitle('Year: {closest_state}')+
  anim_save(here("Week_8","Output","mypengiungif.gif"))

penguin<-image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")

penguin

penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  ggsave(here("Week_8","output","penguinplot.png"))

penplot<-image_read(here("Week_8","output","penguinplot.png"))

out <- image_composite(penplot, penguin, offset = "+70+30")

out
