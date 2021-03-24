########## Week 8: Functional Programming ##########
##### Created by: Nikolas Yousefi #############
##### Created on: 2021-03-24 ###############

### Load libraries #####################
library(tidyverse)
library(palmerpenguins)
library(PNWColors)

### Data Analysis ######################
df <- tibble::tibble(
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10))

head(df)

df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

rescale01 <- function(x) {
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value)}

df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))


fahrenheit_to_celsius <- function(temp_F) { 
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
}

fahrenheit_to_celsius(32)

fahrenheit_to_celsius(212)

celsius_to_kelvin <- function(temp_C) { 
  temp_K <- (temp_C + 273.15) 
  return(temp_K)
}

celsius_to_kelvin(0)

myplot<-function(data = penguins, x, y, lines = TRUE){
pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 

if(lines==TRUE){
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}
else{
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}
}

myplot(data = penguins, x = body_mass_g, y = bill_length_mm)

myplot(data = penguins, x = body_mass_g, y = flipper_length_mm)

myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")

myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE)
