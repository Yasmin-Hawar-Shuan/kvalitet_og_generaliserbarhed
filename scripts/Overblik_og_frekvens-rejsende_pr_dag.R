library(tidyverse)
library(lubridate)

helrejser <- read.csv("Helrejser_Rejsekortet_januar_2020.csv", sep = ";")
helrejser$RejseDato <- dmy(helrejser$RejseDato) #Laver RejseDato til Date format

tabel_helrejser <- helrejser %>% 
  subset(Personrejser != 0) %>% 
  count(RejseDato, Personrejser) %>% 
  group_by(RejseDato) %>% 
  mutate(total = n * Personrejser) # frekvenstabel over antal rejsende pr. rejse grupperet ved RejseDato

aggregate(Personrejser ~ RejseDato, helrejser, FUN = sum) # rejsende pr. dag

delrejser <- read.csv("Delrejser_Januar_2020.csv", sep = ";")
delrejser$RejseDato <- dmy(delrejser$RejseDato)

mennesker <- c(1, 2, 4, 5, 7, 8, 9, 10, 11, 12, 18) #Passagertyper der er mennesker (står i datadefinitioner).

delrejse_passagere <- delrejser %>% 
  subset(c(Passagertype1, Passagertype2, Passagertype3) %in% mennesker) %>% 
  transmute(RejseDato, Passagerantal = PassagerAntal1 + PassagerAntal2 + PassagerAntal3) %>% 
  count(RejseDato, Passagerantal) %>% 
  group_by(RejseDato) %>% 
  mutate(total = n * Passagerantal) # frekvenstabel over antal rejsende pr. rejse grupperet ved RejseDato

aggregate(total ~ RejseDato, delrejse_passagere, FUN = sum)
