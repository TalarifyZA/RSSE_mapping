
library(tidyverse)
library(ggplot2)
library(leaflet)
library(dplyr)




rse_data <- read.csv("mapping_rse.csv")
head(rse_data)

mymap1 <- leaflet(data = rse_data) %>%
  setView(lat = 0, lng= 25, zoom = 3) %>%  # set map view
  addTiles() %>%                                       # Add default OpenStreetMap map background tiles
  addMarkers(lng = ~Long,lat = ~Lat, popup = ~as.character(Institution), label = ~as.character(Institution))

mymap1 # Print the map        


