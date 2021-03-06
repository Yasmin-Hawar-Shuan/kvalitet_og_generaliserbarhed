library(tidyverse)

df <- read.csv("Fynske_stoppesteder.csv", sep = ";")

df$S_LONG <- df$S_LONG %>% 
  str_remove_all(",") %>% # fjerner komma (,)
  str_replace("^9", "9.") %>% # bytter 9, hvis første karakter, ud med 9.
  str_replace("^10", "10.") # bytter 10, hvis første karakter, ud med 10.

df$S_LAT <- df$S_LAT %>%
  str_remove_all(",") %>% # fjerner kommer (,)
  str_replace("^55", "55.") %>% # bytter 55, hvis første karakterer, ud med 55.
  str_replace("^54", "54.") # bytter 54, hvis første karaterer, ud med 54.

df$S_LONG[ df$S_LONG == "0" | df$S_LONG == "NULL" | df$S_LONG == "10." | df$S_LONG == "11"] <- NA # laver 0, 11 og NULL til NA
df$S_LAT[ df$S_LAT == "0" | df$S_LAT == "NULL" | df$S_LAT == "734" | df$S_LAT == "55."] <- NA # laver 0, NULL, 55 og 734 til NA
missing_coord <- df$Stopnavn[is.na(df$S_LAT) & df$Stopnavn != "NULL"] # stoppesteder med manglende koordinater

write.csv(df, "Fynske_stoppesteder_rettet.csv")
