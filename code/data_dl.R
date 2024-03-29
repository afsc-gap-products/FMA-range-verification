# Load in FMA species of interest ----------------------------------------------

sp.codes <- readr::read_csv(file = here::here("code", "species_lengths.csv")) %>% 
  janitor::clean_names()

fma_date_start <- 2013
fma_date_end <- 2023

lengths_dl <- TRUE
weights_dl <- FALSE


# Download Center

## Download cruise data ---------------------------------------------------------

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
                    paste0("SELECT * FROM RACE_DATA.V_CRUISES 
                           WHERE SURVEY_DEFINITION_ID IN (", 
                           paste(surveys$survey_definition_id, collapse = ", "), ");")) %>% 
  janitor::clean_names() %>% 
  dplyr::left_join(x = ., 
                   y = surveys, 
                   by = "survey_definition_id") %>%
  dplyr::filter(!is.na(cruisejoin)) # remove NA in cruisejoin (uncompleted surveys like 2020, or current year)



## Download Weight data ---------------------------------------------------------

if (weights_dl == TRUE) {
  RACE_rawweight  <-
    RODBC::sqlQuery(channel,query = 
                      paste0("SELECT * FROM RACEBASE.SPECIMEN 
                      WHERE SPECIES_CODE IN (",
                             paste(sp.codes$race_species_code, collapse = ", "),");")) %>% 
    janitor::clean_names() 
  readr::write_csv(RACE_rawweight, here::here("data", "race_w_data.csv"))
  
  fma_rawweight  <-
    RODBC::sqlQuery(channel,query = 
                      paste0("SELECT SPECIES_CODE, SPECIES_NUMBER, SPECIES_WEIGHT FROM NORPAC.ATL_SPECIES_COMPOSITION 
                      WHERE SPECIES_CODE IN (",
                             paste(sp.codes$fma_species_code, collapse = ", "),");")) %>% 
    janitor::clean_names()
  
  readr::write_csv(fma_rawweight, here::here("data", "fma_w_data.csv"))
}



## Download length data ---------------------------------------------------------

if (lengths_dl == TRUE) {
  RACE_rawlength  <-
    RODBC::sqlQuery(channel,query = 
                      paste0("SELECT * FROM RACE_DATA.V_EXTRACT_FINAL_LENGTHS 
                      WHERE SPECIES_CODE IN (",
                             paste(sp.codes$race_species_code, collapse = ", "),");")) %>% 
    janitor::clean_names() %>%
    dplyr::left_join(x = ., 
                     y = cruise %>%
                       dplyr::select(cruisejoin, cruise, region, vessel_id), 
                     by = c("region", "cruise", "vessel" = "vessel_id"))
  readr::write_csv(RACE_rawlength, here::here("data", "race_l_data.csv"))
  
  
  fma_rawlength  <-
    RODBC::sqlQuery(channel,query =
                      paste0("SELECT * FROM OBSINT.debriefed_length WHERE year BETWEEN ",paste(fma_date_start, "AND", fma_date_end),"AND SPECIES IN (",
                             paste(sp.codes$fma_species_code, collapse = ", "),");")) %>%
    janitor::clean_names()

  readr::write_csv(fma_rawlength, here::here("data", "fma_l_data.csv"))
}


# Join RACE tables together ----------------------------------------------------

# specimen_data <- 
#   dplyr::bind_rows(RACE_rawlength %>% 
#                      dplyr::mutate(var = "length") %>% #adding var column and populating with length?
#                      dplyr::rename(val = length), #changing length column to val column?
#                    RACE_rawweight %>% 
#                      dplyr::mutate(var = "weight") %>% 
#                      dplyr::rename(val = weight) %>% 
#                      dplyr::mutate(frequency = 1)) %>% 
#   dplyr::select(cruisejoin, species_code, sex, frequency, var, val) %>%
#   # dplyr::filter(cruise %in% cruise$cruise) %>%
#   dplyr::full_join(
#     y = cruise, 
#     x = ., 
#     by = "cruisejoin")
# 
# readr::write_csv(specimen_data, here::here("data", "RACE_specimen_data.csv"))
