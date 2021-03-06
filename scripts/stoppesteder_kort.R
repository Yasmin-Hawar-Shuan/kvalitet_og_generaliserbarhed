library(tidyverse)
library(leaflet)

df <- read.csv("Fynske_stoppesteder_rettet.csv")

m <- leaflet(df) %>% 
  addTiles() %>% 
  setView(lng = 10.3, lat = 55.2, zoom = 9) %>% 
  addCircles(
    lng = ~S_LONG,
    lat = ~S_LAT,
    label = ~Stopnavn,
    color = "coral",
    radius = 5,
    weight = 2)
m


