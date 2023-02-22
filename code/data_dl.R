# Load in FMA species of interest ----------------------------------------------

sp.codes <- readr::read_csv(file = here::here("code", "species_lengths.csv")) %>% 
  janitor::clean_names()



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
                    paste0("SELECT * FROM RACE_DATA.V_CRUISES 
                           WHERE SURVEY_DEFINITION_ID IN (", 
                           paste(surveys$survey_definition_id, collapse = ", "), ");")) %>% 
  janitor::clean_names() %>% 
  dplyr::left_join(x = ., 
                   y = surveys, 
                   by = "survey_definition_id") %>%
  dplyr::filter(!is.na(cruisejoin)) # remove NA in cruisejoin (uncompleted surveys like 2020, or current year)



# Download Length data ---------------------------------------------------------

RACE_rawweight  <-
  RODBC::sqlQuery(channel,query = 
                    paste0("SELECT * FROM RACEBASE.SPECIMEN 
                    WHERE SPECIES_CODE IN (",
                           paste(sp.codes$race_species_code, collapse = ", "),");")) %>% 
  janitor::clean_names() 

fma_rawweight  <-
  RODBC::sqlQuery(channel,query = 
                    paste0("SELECT SPECIES_CODE, SPECIES_NUMBER, SPECIES_WEIGHT FROM NORPAC.ATL_SPECIES_COMPOSITION 
                    WHERE SPECIES_CODE IN (",
                           paste(sp.codes$fma_species_code, collapse = ", "),");")) %>% 
  janitor::clean_names()

readr::write_csv(fma_rawweight, here::here("data", "fma_w_data.csv"))




# Download length data ---------------------------------------------------------

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


fma_rawlength  <-
  RODBC::sqlQuery(channel,query = 
                    paste0("SELECT * FROM NORPAC.ATL_LENGTH WHERE SPECIES_CODE IN (",
                           paste(sp.codes$fma_species_code, collapse = ", "),");")) %>% 
  janitor::clean_names()

readr::write_csv(fma_rawlength, here::here("data", "fma_l_data.csv"))



# Join RACE tables together ----------------------------------------------------

specimen_data <- 
  dplyr::bind_rows(RACE_rawlength %>% 
                     dplyr::mutate(var = "length") %>% #adding var column and populating with length?
                     dplyr::rename(val = length), #changing length column to val column?
                   RACE_rawweight %>% 
                     dplyr::mutate(var = "weight") %>% 
                     dplyr::rename(val = weight) %>% 
                     dplyr::mutate(frequency = 1)) %>% 
  dplyr::select(cruisejoin, species_code, sex, frequency, var, val) %>%
  # dplyr::filter(cruise %in% cruise$cruise) %>%
  dplyr::full_join(
    y = cruise, 
    x = ., 
    by = "cruisejoin")

readr::write_csv(specimen_data, here::here("data", "specimen_data.csv"))
