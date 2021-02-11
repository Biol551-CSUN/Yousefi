### Today we are going to plot penguin data ####
### Created by Nikolas Yousefi #################
### Updated on 2021-02-10 ######################


### Load libraries ####
library(palmerpenguins)
library(tidyverse)
library(here)

### Load Data ####
glimpse(penguins)

plot1 <- ggplot(data=penguins,
  mapping = aes (x = bill_depth_mm,
                 y = bill_length_mm,
                 group = species,
                 color = species)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
    labs(x = "Bill depth (mm)",
         y = "Bill length (mm)"
         ) + 
  scale_color_manual(values = beyonce_palette(10)) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 20,
                                  color = "blue"),
        panel.background = element_rect(fill = "linen")) + 
  ggsave(here("Week_3","Output","penguin.png"), 
         width = 7, height = 5)
  