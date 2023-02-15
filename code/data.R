
# Load in FMA species of interest ----------------------------------------------

sp.codes <- readr::read_csv(file = here::here("code", "species_lengths.csv")) %>% 
  janitor::clean_names()

# Download species codes -------------------------------------------------------

# spcodes  <-
#   sqlQuery(channel,paste("SELECT * FROM RACEBASE.SPECIES"))  %>% 
#   janitor::clean_names()


# Download cruise data ---------------------------------------------------------

surveys <- 
  data.frame(survey_definition_id = c(143, 98, 47, 52, 78), 
             SRVY = c("NBS", "EBS", "GOA", "AI", "BSS"), 
             SRVY_long = c("northern Bering Sea", 
                           "eastern Bering Sea", 
                           "Gulf of Alaska", 
                           "Aleutian Islands", 
                           "Bering Sea Slope") )

cruise  <-
  RODBC::sqlQuery(channel,query = 
                    paste0("SELECT CRUISE, YEAR, SURVEY_DEFINITION_ID FROM RACE_DATA.V_CRUISES 
                           WHERE SURVEY_DEFINITION_ID IN (", 
                           paste(surveys$survey_definition_id, collapse = ", "), ");")) %>% 
  janitor::clean_names() %>% 
  dplyr::left_join(x = ., 
                   y = surveys, 
                   by = "survey_definition_id")


# Download weight data ---------------------------------------------------------

RACE_rawweight  <-
  RODBC::sqlQuery(channel,query = 
             paste0("SELECT CRUISE, SPECIES_CODE, SEX, WEIGHT FROM RACEBASE.SPECIMEN 
                    WHERE SPECIES_CODE IN (",
                    paste(sp.codes$race_species_code, collapse = ", "),");")) %>% 
  janitor::clean_names() 

# Download length data ---------------------------------------------------------

# RACE.rawlength  <-
#   RODBC::sqlQuery(channel,query = 
#              paste0("SELECT * FROM RACE_DATA.V_EXTRACT_FINAL_LENGTHS A WHERE ((A.SPECIES_CODE IN (",
#                     paste(sp.codes$race_species_code, collapse = ", "),");")) %>% 
#   janitor::clean_names()

RACE_rawlength  <-
  RODBC::sqlQuery(channel,query = 
                    paste0("SELECT * FROM RACE_DATA.V_EXTRACT_FINAL_LENGTHS 
                    WHERE SPECIES_CODE IN (",
                           paste(sp.codes$race_species_code, collapse = ", "),");")) %>% 
  janitor::clean_names() 


# Join tables together ---------------------------------------------------------

specimen_data <- 
  dplyr::bind_rows(RACE_rawlength %>% 
                     dplyr::mutate(var = "length") %>% 
                     dplyr::rename(val = length), 
                   RACE_rawweight %>% 
                     dplyr::mutate(var = "weight") %>% 
                     dplyr::rename(val = weight) %>% 
                     dplyr::mutate(frequency = 1)) %>% 
  dplyr::select(cruise, species_code, sex, frequency, length, weight) %>% 
  # tidyr::pivot_longer(data = .,  cols = c("length", "weight"), names_to = "var", values_to = "val")
  dplyr::left_join(
  x = cruise, 
  y = ., 
  by = "cruise")

readr::write_csv(here::here())



