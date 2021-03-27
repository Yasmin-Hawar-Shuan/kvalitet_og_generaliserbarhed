library(tidyverse)
library(leaflet)
library(RJSONIO)
library(jsonify)

# stoppesteds data ----------------------------------------------------------------
stops <- read.csv("Fynske_stoppesteder_rettet.csv")

# voi og tier data ----------------------------------------------------------------
tier_json <- fromJSON("https://apps.odenserundt.dk/data/newtags/408-dk-light.json")
voi_json <- fromJSON("https://apps.odense.dk/data/newtags/409-dk-light.json")
cykl_json <- fromJSON("http://apps.odense.dk/data/newtags/418-dk-light.json")

tier_lat <- vector(mode = "list", length = length(tier_json$features))
tier_long <- vector(mode = "list", length = length(tier_json$features))
tier_name <- vector(mode = "list", length = length(tier_json$features))

voi_lat <- vector(mode = "list", length = length(voi_json$features))
voi_long <-  vector(mode = "list", length = length(voi_json$features))
voi_name <-  vector(mode = "list", length = length(voi_json$features))

cykl_lat <- vector(mode = "list", length = length(cykl_json$features))
cykl_long <- vector(mode = "list", length = length(cykl_json$features))
cykl_name <- vector(mode = "list", length = length(cykl_json$features))

for (i in 1:length(tier_json$features)) {
  tier_lat[i] <- tier_json$features[[i]]$geometry$coordinates[[1]][[1]][[2]]
  tier_long[i] <- tier_json$features[[i]]$geometry$coordinates[[1]][[1]][[1]]
  tier_name[i] <- tier_json$features[[i]]$name
}

for (i in 1:length(voi_json$features)) {
  voi_lat[i] <- voi_json$features[[i]]$geometry$coordinates[[1]][[1]][[2]]
  voi_long[i] <- voi_json$features[[i]]$geometry$coordinates[[1]][[1]][[1]]
  voi_name[i] <- voi_json$features[[i]]$name
}

for (i in 1:length(cykl_json$features)) {
  cykl_lat[i] <- cykl_json$features[[i]]$geometry$coordinates[[2]]
  cykl_long[i] <- cykl_json$features[[i]]$geometry$coordinates[[1]]
  cykl_name[i] <- cykl_json$features[[i]]$name
}


tier <- cbind(unlist(tier_lat)) %>% 
  cbind(unlist(tier_long)) %>% 
  cbind(unlist(tier_name)) %>% 
  as.data.frame()

voi <- cbind(unlist(voi_lat)) %>% 
  cbind(unlist(voi_long)) %>% 
  cbind(unlist(voi_name)) %>% 
  as.data.frame()

cykl <- cbind(unlist(cykl_lat)) %>% 
  cbind(unlist(cykl_long)) %>% 
  cbind(unlist(cykl_name)) %>% 
  as.data.frame()


names(tier) <- c("lat", "long", "name")
names(voi) <- c("lat", "long", "name")
names(cykl) <- c("lat", "long", "name")


tier$lat <- as.numeric(tier$lat)
tier$long <- as.numeric(tier$long)

voi$lat <- as.numeric(voi$lat)
voi$long <- as.numeric(voi$long)

cykl$lat <- as.numeric(cykl$lat)
cykl$long <- as.numeric(cykl$long)

# write.csv(voi, "../voi.csv")
# write.csv(tier, "../tier.csv")
# write.csv(cykl, "../lånecykler.csv")
# Kort generering -----------------------------------------------------------------

m <- leaflet(stops) %>% 
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
    weight = 3) %>% 
addCircles(
  lng = ~cykl$long,
  lat = ~cykl$lat,
  label = ~cykl$name,
  color = "#ECA400",
  radius = 10,
  weight = 3)
m

