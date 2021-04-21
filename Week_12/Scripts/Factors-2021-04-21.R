##### Week 12 - Factors ##############
##### Created by: Nikolas Yousefi#####
##### Updated: 2021-04-21 ############

##### Libraries ##########
library(tidyverse)
library(here)

##### Load Data ##########
tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
income_mean<-tuesdata$income_mean

##### Data Analysis ######
fruits<-factor(c("Apple", "Grape", "Banana"))
fruits

test<-c("A", "1", "2")
as.numeric(test)

test<-factor(test) # covert to factor
as.numeric(test)

star_counts<-starwars %>%
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>%
  count(species)

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE), y = n))+ # reorder the factor of species by n
  geom_col() +
  labs(x = "Species")

glimpse(income_mean)

total_income<-income_mean %>%
  group_by(year, income_quintile)%>%
  summarise(income_dollars_sum = sum(income_dollars))%>%
  mutate(income_quintile = factor(income_quintile))

total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum)))+
  geom_line()+
  labs(color = "income quantile")

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() %>%  # drop extra levels
  mutate(species = fct_recode(species, "Humanoid" = "Human"))