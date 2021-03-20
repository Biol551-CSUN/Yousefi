### Today we are going to practice ggmaps ####
### Created by: Nikolas Yousefi #############
### Updated on: 2021-03-10 ####################

### Load libraries ##########
library(ggmap)
library(tidyverse)
library(here)
library(ggsn)

### Load Data ##########
ChemData<-read_csv(here("Week_7","Data","chemicaldata_maunalua.csv"))

glimpse(ChemData)

Oahu<-get_map("Oahu")

ggmap(Oahu)

### Data Analysis ##########

WP<-data.frame(lon = -157.7621, lat = 21.27427)

Map1<-get_map(WP, zoom = 17, maptype = "satellite")

ggmap(Map1)+
geom_point(data = ChemData,
           aes(x = Long, y = Lat, color = Salinity),
           size = 2) +
  scale_color_viridis_c()+
  scalebar( x.min = -157.766, x.max = -157.758,
            y.min = 21.2715, y.max = 21.2785,
            dist = 250, dist_unit = "m", model = "WGS84", 
            transform = TRUE, st.color = "white",
            box.fill = c("yellow", "white"))
