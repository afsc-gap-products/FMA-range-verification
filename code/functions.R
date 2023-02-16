# Load libraries and functions --------------------------------------------------

PKG <- c(
  "RODBC", 
  "getPass", 
  "plyr",
  "dplyr",
  "magrittr",
  "readr",
  "tidyr",
  "here", 
  "janitor",
  "stringr")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}

#Query to use raw length data from RACEBASE to inform FMA length ranges
get.connected <- function(schema='AFSC'){(echo=FALSE)
  username <- getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(paste(
    schema),paste(username),paste(password), believeNRows=FALSE)
}
