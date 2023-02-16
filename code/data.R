# Load CSV Files ---------------------------------------------------------------

specimen_data <- readr::read_csv(here::here("data", "specimen_data.csv")) #why use readr instead of the base R???
sp.codes <- readr::read_csv(file = here::here("code", "species_lengths.csv")) %>% 
  janitor::clean_names()
fma_l_data <- readr::read_csv(here::here("data", "fma_l_data.csv"))
fma_w_data <- readr::read_csv(here::here("data", "fma_w_data.csv"))

# 
# dat <- specimen_data %>% 
#   # TOLEDO!!! deal with freq
#   dplyr::group_by(SRVY, species_code) %>% 
#   dplyr::summarize(min = min(val, na.rm = TRUE), 
#                    q1 = quantile(val, .025, na.rm = TRUE)#, 
#                    # mean...
#                    #median...
#                    )

# RACE Create Length Table -----------------------------------------------------

test0 <-data.frame(
  race_code = 1.00,
  fma_code = 1.00,
  species_name = 1.00,
  r_l_count = 1.00,
  r_l_min = 1.00,
  r_l_max = 1.00,
  r_l_q2_5 = 1.00,
  r_l_q97_5 = 1.00)


for(i in 1:nrow(sp.codes)){
  RACE <- sp.codes$race_species_code[i]
  NAME <- sp.codes$species_name[i]
  FMA <- sp.codes$fma_species_code[i]
  
  l_fish <- specimen_data %>% 
    select(SRVY, year, species_code, frequency, var, val) %>%
    filter(var == "length", species_code == RACE) %>%
    na.omit(l_fish$val)
  
  if(nrow(l_fish)>0) { 
    l_count <- sum(l_fish$frequency)
    l_min <- (min(l_fish$val) / 10)
    l_max <- (max(l_fish$val) / 10)
    l_temp <- l_fish[rep(x = 1:nrow(l_fish), l_fish$frequency), ]
    l_q2_5 <- (quantile(x = l_temp$val, probs = 0.025, na.rm = TRUE) / 10)
    l_q97_5 <- (quantile(x = l_temp$val, probs = 0.975, na.rm = TRUE) / 10)
      
    test0[i,] <- c(RACE, FMA, NAME, l_count, l_min, l_max, l_q2_5, l_q97_5)
    }
  else 
    test0[i,] <- c(RACE, FMA, NAME, NA, NA, NA, NA, NA)
}



# RACE Create Weight Table -----------------------------------------------------

test1 <-data.frame(
  race_code = 1.00,
  fma_code = 1.00,
  species_name = 1.00,
  r_w_count = 1.00,
  r_w_min = 1.00,
  r_w_max = 1.00,
  r_w_q2_5 = 1.00,
  r_w_q97_5 = 1.00)

for(i in 1:nrow(sp.codes)){
  RACE <- sp.codes$race_species_code[i]
  NAME <- sp.codes$species_name[i]
  FMA <- sp.codes$fma_species_code[i]
  
  w_fish <- specimen_data %>% 
    select(SRVY, year, species_code, frequency, var, val) %>%
    filter(var == "weight", species_code == RACE) %>%
    na.omit(w_fish$val)
  
  if(nrow(w_fish)>0) {
    w_count <- sum(w_fish$frequency)
    w_min <- (min(w_fish$val) / 1000)
    w_max <- (max(w_fish$val) / 1000)
    w_temp <- w_fish[rep(x = 1:nrow(w_fish), w_fish$frequency), ]
    w_q2_5 <- (quantile(x = w_temp$val, probs = 0.025, na.rm = TRUE) / 1000)
    w_q97_5 <- (quantile(x = w_temp$val, probs = 0.975, na.rm = TRUE) / 1000)
    
    test1[i,] <- c(RACE, FMA, NAME, w_count, w_min, w_max, w_q2_5, w_q97_5)
  }
  else 
    test1[i,] <- c(RACE, FMA, NAME, NA, NA, NA, NA, NA)
}

# RACE Combine tables and add date ---------------------------------------------

# racecombined_l_w <- dplyr::left_join(x = test0, y = test1)
# last_update <- data.frame(last_update = Sys.time())
# norpac_table0 <- cbind(racecombined_l_w, last_update)
# 
# date <- Sys.Date()
# write.csv(norpac_table0, here::here("output", "RACE_final_table.csv"))

# FMA Create Length Table ------------------------------------------------------

test2 <-data.frame(
  race_code = 1.00,
  fma_code = 1.00,
  species_name = 1.00,
  f_l_count = 1.00,
  f_l_min = 1.00,
  f_l_max = 1.00,
  f_l_q2_5 = 1.00,
  f_l_q97_5 = 1.00)

for(i in 1:nrow(sp.codes)){
  RACE <- sp.codes$race_species_code[i]
  NAME <- sp.codes$species_name[i]
  FMA <- sp.codes$fma_species_code[i]
  
  l_fish <- fma_l_data %>% 
    select(species_code, length_size) %>%
    filter(species_code == FMA) %>%
    na.omit(l_fish$length_size)
  
  if(nrow(l_fish)>0) { 
    l_count <- nrow(l_fish)
    l_min <- min(l_fish$length_size)
    l_max <- max(l_fish$length_size)
    l_q2_5 <- quantile(x = l_fish$length_size, probs = 0.025, na.rm = TRUE)
    l_q97_5 <- quantile(x = l_fish$length_size, probs = 0.975, na.rm = TRUE)
    
    test2[i,] <- c(RACE, FMA, NAME, l_count, l_min, l_max, l_q2_5, l_q97_5)
  }
  else 
    test2[i,] <- c(RACE, FMA, NAME, NA, NA, NA, NA, NA)
}

# FMA Create Weight Table ------------------------------------------------------

test3 <-data.frame(
  race_code = 1.00,
  fma_code = 1.00,
  species_name = 1.00,
  f_w_avg_ext = 1.00,
  f_w_avg_raw = 1.00,
  f_w_count = 1.00,
  f_w_min = 1.00,
  f_w_max = 1.00,
  f_w_q2_5 = 1.00,
  f_w_q97_5 = 1.00)

for(i in 1:nrow(sp.codes)){
  RACE <- sp.codes$race_species_code[i]
  NAME <- sp.codes$species_name[i]
  FMA <- sp.codes$fma_species_code[i]
  
  fma_w_data$species_avg <- 
    (fma_w_data$species_weight / fma_w_data$species_number) # adds average weight column of catch
  
# get the average of the 'average weights' column.  
  w_fish_1 <- fma_w_data %>% 
    filter(species_code == FMA, species_number != 0) %>%
    na.omit(w_fish_1)
  w_fish_avg_ext <- mean(w_fish_1$species_weight)
   
# get all instances where species_number = 1 so all weights are for a specific fish    
  w_fish <- fma_w_data %>% 
    filter(species_code == FMA, species_number == 1) %>%
    na.omit(w_fish)
  
  if(nrow(w_fish)>0) {
    w_avg_raw <- mean(w_fish$species_weight)
    w_count <- nrow(w_fish)
    w_min <- min(w_fish$species_weight)
    w_max <- max(w_fish$species_weight)
    w_q2_5 <- quantile(x = w_fish$species_weight, probs = 0.025, na.rm = TRUE)
    w_q97_5 <- quantile(x = w_fish$species_weight, probs = 0.975, na.rm = TRUE)
    
    test3[i,] <- c(RACE, FMA, NAME, w_fish_avg_ext, w_avg_raw, w_count, w_min, w_max, w_q2_5, w_q97_5)
  }
  else 
    test3[i,] <- c(RACE, FMA, NAME, NA, NA, NA, NA, NA, NA, NA)
}

# FMA Combine tables and add date ----------------------------------------------

# fmacombined_l_w <- dplyr::left_join(x = test2, y = test3)
# last_update <- data.frame(last_update = Sys.time())
# norpac_table1 <- cbind(fmacombined_l_w, last_update)
# 
# date <- Sys.Date()
# write.csv(norpac_table1, here::here("output", "FMA_final_table.csv"))

# Combine all tables and add date ---------------------------------------------

racecombined_l_w <- dplyr::left_join(x = test0, y = test1)
fmacombined_l_w <- dplyr::left_join(x = test2, y = test3)
last_update <- data.frame(last_update = Sys.time())
combined_l_w <- dplyr::left_join(x = racecombined_l_w, y = fmacombined_l_w)
norpac_table <- cbind(combined_l_w, last_update)

date <- Sys.Date()
write.csv(norpac_table, here::here("output", "final_norpac_table.csv"))