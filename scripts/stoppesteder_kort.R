library(tidyverse)
library(leaflet)
library(RJSONIO)
library(jsonify)

# stoppesteds data ----------------------------------------------------------------
df <- read.csv("Fynske_stoppesteder_rettet.csv")

# voi og tier data ----------------------------------------------------------------
json <- fromJSON("https://apps.odenserundt.dk/data/newtags/408-dk-light.json")
json_2 <- fromJSON("https://apps.odense.dk/data/newtags/409-dk-light.json")

lat <- vector(mode = "list", length = length(json$features))
long <- vector(mode = "list", length = length(json$features))
name <- vector(mode = "list", length = length(json$features))

lat_1 <- vector(mode = "list", length = length(json_2$features))
long_1 <-  vector(mode = "list", length = length(json_2$features))
name_1 <-  vector(mode = "list", length = length(json_2$features))

for (i in 1:length(json$features)) {
  lat[i] <- json$features[[i]]$geometry$coordinates[[1]][[1]][[2]]
  long[i] <- json$features[[i]]$geometry$coordinates[[1]][[1]][[1]]
  name[i] <- json$features[[i]]$name
}

for (i in 1:length(json_2$features)) {
  lat_1[i] <- json_2$features[[i]]$geometry$coordinates[[1]][[1]][[2]]
  long_1[i] <- json_2$features[[i]]$geometry$coordinates[[1]][[1]][[1]]
  name_1[i] <- json_2$features[[i]]$name
}


tier <- cbind(unlist(lat)) %>% 
  cbind(unlist(long)) %>% 
  cbind(unlist(name)) %>% 
  as.data.frame()

voi <- cbind(unlist(lat_1)) %>% 
  cbind(unlist(long_1)) %>% 
  cbind(unlist(name_1)) %>% 
  as.data.frame()

names(tier) <- c("lat", "long", "name")
names(voi) <- c("lat", "long", "name")

tier$lat <- as.numeric(tier$lat)
tier$long <- as.numeric(tier$long)

voi$lat <- as.numeric(voi$lat)
voi$long <- as.numeric(voi$long)

# Kort generering -----------------------------------------------------------------

m <- leaflet(df) %>% 
  addTiles() %>% 
  setView(lng = 10.39, lat = 55.4, zoom = 14) %>% 
  addCircles(
    lng = ~S_LONG,
    lat = ~S_LAT,
    label = ~Stopnavn,
    color = "#5f9713",
    radius = 10,
    weight = 3) %>% 
  addCircles(
    lng = ~tier$long,
    lat = ~tier$lat,
    label = ~tier$name,
    color = "#00708D",
    radius = 10,
    weight = 3) %>% 
  addCircles(
    lng = ~voi$long,
    lat = ~voi$lat,
    label = ~voi$name,
    color = "#E6177A",
    radius = 10,
    weight = 3)
m

