### Today we are going to practice mapping ####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-03-08 ####################


#### Load Libraries ######
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

# Read in data on population in California by county
popdata<-read_csv(here("Week_7","Data","CApopdata.csv"))

#read in data on number of seastars at different field sites
stars<-read_csv(here("Week_7","Data","stars.csv"))

# get data for the entire world
world<-map_data("world")

head(world)

# get data for the USA
usa<-map_data("usa")

head(usa)

# get data for italy
italy<-map_data("italy")

head(italy)

# get data for states
states<-map_data("state")

head(states)

# get data for counties
counties<-map_data("county")

head(counties)

# plotting a map
ggplot()+
 geom_polygon(data = world, aes(x = long, 
                                 y = lat, 
                                 group = group,
                                 fill = region),
                                 color = "Black")+
  theme_minimal()+
  guides(fill = FALSE)+
  theme(panel.background = element_rect(fill = "lightblue"))+
  coord_map(projection = "mercator",
            xlim = c(-180,180))

# California map
CA_data<-states %>%
  filter(region == "california")
  
ggplot()+
  geom_polygon(data = CA_data, 
               aes(x = long, 
                   y = lat, 
                   group = group),
                   color = "black")+
  coord_map()+
  theme_void()

CApop_county<-popdata %>%
  select("subregion" = County, Population)  %>% # rename the county col
  inner_join(counties) %>%
  filter(region == "california")

ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),
               color = "black")+
  geom_point(data = stars,
             aes(x = long,
                 y = lat,
                 size = star_no))+
  coord_map()+
  theme_void()+
  scale_fill_gradient(trans = "log10")+
  labs(size = "# stars/m2")+
  ggsave(here("Week_7","Output","CApop.pdf"))
