library(parallel) # library for parrallel processing

# Transformation af nedbørs data
files_ned <- list.files("../dmi_data/nedbør/") # finder filenavne
files_ned <- paste("..", "dmi_data", "nedbør", files_ned, sep = "/") # tilføjer sti til navne

lattitude <- c(55.3088, 55.4935, 55.0978, 55.4749, 55.3474, 54.9463, 54.7567, 55.5815, 55.0721) # latt koordinater for vejr stationer
longtitude <- c(10.4398, 9.8953, 10.1009, 10.3305, 10.7618, 10.7234, 10.7094, 10.393, 10.4249) # long koordinater for vejr stationer
by <- c("Årslev", "Båring", "Bøjden", "HCA Airport", "Juelsberg", "Rudkøbing", "Søndenbro", "Tørresø", "Ulbølle") # by navne

lodf <- mclapply(files_ned, read.csv, sep = ";", mc.cores = detectCores()) # loader files_ned som liste af dataframes
lodf <- lapply(1:length(lodf), function(i) cbind(lodf[[i]], lattitude[i], longtitude[i], by[i])) # tilføjer long og latt koordinater

df <- do.call(rbind, lodf) # sammebinder til et dataframe
write.csv(df, "../dmi_data/nedbør/Fyn_nedbør_januar.csv") # gemmer csv

# Transformation af solskins data
files_sol <- list.files("../dmi_data/solskin/") # finder filnavne
files_sol <- paste("..", "dmi_data", "solskin", files_sol, sep = "/") # tilføjer sti til navne
  
lattitude <- c(55.3088, 55.2444) # latt koordinater for vejr stationer
longtitude <- c(10.4398, 9.8882) # long koordinater for vejr stationer
by <- c("Årslev", "Assens/Torø") # by navne

lodf <- mclapply(files_sol, read.csv, sep = ";", mc.cores = detectCores()) # loader files_sol som liste af dataframes
lodf <- lapply(1:length(lodf), function(i) cbind(lodf[[i]], lattitude[i], longtitude[i], by[i])) # tilføjer long og latt koordinater

df <- do.call(rbind, lodf) # sammenbinder til et dataframe
write.csv(df, "../dmi_data/solskin/Fyn_solskin_januar.csv") # gemmer csv

# Transformation af temperatur data
files_temp <- list.files("../dmi_data/temperatur/") # finder filnavne
files_temp <- paste("..", "dmi_data", "temperatur", files_temp, sep = "/") # tilføjer sti til navne

lattitude <- c(55.3088, 55.2444, 55.0144) # latt koordinater for vejr stationer
longtitude <- c(10.4398, 9.8882, 10.5693) # long koordinater for vejr stationer
by <- c("Årslev", "Assens/Torø", "Sydfyns Flyveplads") # by navne

lodf <- mclapply(files_temp, read.csv, sep = ";", mc.cores = detectCores()) # loader files_sol som liste af dataframes
lodf <- lapply(1:length(lodf), function(i) cbind(lodf[[i]], lattitude[i], longtitude[i], by[i])) # tilføjer long og latt koordinater

df <- do.call(rbind, lodf) # sammenbinder til et dataframe
write.csv(df, "../dmi_data/temperatur/Fyn_temperatur_januar.csv") # gemmer csv
