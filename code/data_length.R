# Load CSV Files ---------------------------------------------------------------
sp.codes <- readr::read_csv(file = here::here("code", "species_lengths.csv")) %>% 
  janitor::clean_names()
race_l_data <- readr::read_csv(here::here("data", "race_l_data.csv"))
fma_l_data <- readr::read_csv(here::here("data", "fma_l_data.csv"))

# Creating Tables 

## RACE Create Length Table -----------------------------------------------------

race0 <-data.frame(
  race_code = 1.00,
  fma_code = 1.00,
  species_name = 1.00,
  r_count = 1.00,
  r_min = 1.00,
  r_max = 1.00,
  r_95_lower = 1.00,
  r_95_upper = 1.00,
  r_99_lower = 1.00,
  r_99_upper = 1.00)

for(i in 1:nrow(sp.codes)){
  RACE <- sp.codes$race_species_code[i]
  NAME <- sp.codes$species_name[i]
  FMA <- sp.codes$fma_species_code[i]
  
  l_fish <- race_l_data %>% 
    select(species_code, length, frequency) %>%
    filter(species_code == RACE) %>%
    na.omit(l_fish$length)
  
  # if(nrow(l_fish) > 0) {l_fish <- l_fish %>% group_by(length) %>% 
  #                   mutate(total_lengths = sum(frequency)) %>% # Create total_length column, and displays number of lengths for each length class
  #                   select(species_code, length, total_lengths)
  # l_fish <- l_fish[!duplicated(l_fish$length),] # Remove duplicates
  # } else {}
    
  if(nrow(l_fish)>0) { 
    l_count <- sum(l_fish$frequency)
    l_temp <- l_fish[rep(x = 1:nrow(l_fish), l_fish$frequency), ]
    l_min <- (min(l_fish$length) / 10)
    l_max <- (max(l_fish$length) / 10)
    l_95_lower <- (quantile(x = l_temp$length, probs = 0.025, na.rm = TRUE) / 10)
    l_95_upper <- (quantile(x = l_temp$length, probs = 0.975, na.rm = TRUE) / 10)
    l_99_lower <- (quantile(x = l_temp$length, probs = 0.005, na.rm = TRUE) / 10)
    l_99_upper <- (quantile(x = l_temp$length, probs = 0.995, na.rm = TRUE) / 10)
      
    race0[i,] <- c(RACE, FMA, NAME, l_count, l_min, l_max, l_95_lower, l_95_upper, l_99_lower, l_99_upper)
    }
  else 
    race0[i,] <- c(RACE, FMA, NAME, NA, NA, NA, NA, NA, NA, NA)
}


## FMA Create Length Table ------------------------------------------------------

fma0 <-data.frame(
  race_code = 1.00,
  fma_code = 1.00,
  species_name = 1.00,
  f_count = 1.00,
  f_min = 1.00,
  f_max = 1.00,
  f_95_lower = 1.00,
  f_95_upper = 1.00,
  f_99_lower = 1.00,
  f_99_upper = 1.00
  )

for(i in 1:nrow(sp.codes)){
  RACE <- sp.codes$race_species_code[i]
  NAME <- sp.codes$species_name[i]
  FMA <- sp.codes$fma_species_code[i]
  
  l_fish <- fma_l_data %>% 
    select(species, length, frequency) %>%
    filter(species == FMA) %>%
    na.omit(l_fish$length)
  
  if(nrow(l_fish)>0) { 
    l_count <- sum(l_fish$frequency)
    l_temp <- l_fish[rep(x = 1:nrow(l_fish), l_fish$frequency), ]
    l_min <- min(l_fish$length)
    l_max <- max(l_fish$length)
    l_95_lower <- quantile(x = l_fish$length, probs = 0.025, na.rm = TRUE)
    l_95_upper <- quantile(x = l_fish$length, probs = 0.975, na.rm = TRUE)
    l_99_lower <- quantile(x = l_fish$length, probs = 0.005, na.rm = TRUE)
    l_99_upper <- quantile(x = l_fish$length, probs = 0.995, na.rm = TRUE)
    
    fma0[i,] <- c(RACE, FMA, NAME, l_count, l_min, l_max, l_95_lower, l_95_upper, l_99_lower, l_99_upper)
  }
  else 
    fma0[i,] <- c(RACE, FMA, NAME, NA, NA, NA, NA, NA, NA, NA)
  
}

# Combine tables & add date ---------------------------------------------

last_update <- data.frame(last_update = Sys.time())
combined_l <- dplyr::left_join(x = race0, y = fma0)
norpac_table <- cbind(combined_l, last_update)

write.csv(norpac_table, here::here("output", "FMA_length_verification.csv"))


# Connect to Oracle ------------------------------------------------------------
source("https://raw.githubusercontent.com/afsc-gap-products/metadata/main/code/functions_oracle.R")
library(getPass)
library(RODBC)
library(magrittr)
library(dplyr)

# Connect to Database

get.connected <- function(schema='AFSC'){(echo=FALSE)
  username <- getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(paste(
    schema),paste(username),paste(password), believeNRows=FALSE)
}

channel <- get.connected()

# Upload data to oracle -------------------------------------------------------

file_paths <- here::here("output", "FMA_length_verification.csv")
table_metadata <- c(Sys.Date())

## Code from separate project for reference
# metadata_column = data.frame(
#   metadata_colname = "DUMMY", 
#   metadata_colname_long = "example dummy column", 
#   metadata_units = "text", 
#   metadata_datatype = "VARCHAR2(225 BYTE)", 
#   metadata_colname_desc = "dummy.")

oracle_upload( 
  file_path = file_paths,
  metadata_table = table_metadata,
  channel = channel, 
  #metadata_column = metadata_column,
  schema = "ANDERSONC")
