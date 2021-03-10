library(tidyverse)

journey_det20 <- read.csv("../2020/Journey_Details_januar_2020.csv", sep = ";")
journey_det21 <- read.csv("Journey Details januar 2021.csv", sep = ";")

stoppesteder <- read.csv("../Fynske_stoppesteder_rettet.csv")

stop <- stoppesteder %>% 
  select(c(Stopnr_Rejseplan, Stopnavn, S_LONG, S_LAT))

stop$Stopnr_Rejseplan <- as.integer(stop$Stopnr_Rejseplan)

names(stop) <- c("station_stationExternalNumber", "Stopnavn", "long", "lat")

journey_det20 <- left_join(journey_det20, stop, by = "station_stationExternalNumber")

journey_det21 <- left_join(journey_det21, stop, by = "station_stationExternalNumber")
