#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------
#' 2018.03.21.
#' 
#' Received raw data for two COG studies (AE23, AE24) from IARC as .dta 
#' files.
#' 
#' No accompanying data dictionaries.
#' 
#' AE23 TODO LIST
#' 
#' AE24 TODO LIST
#' TODO: resolve uncertainty in the units for formula_dur (says months,
#' likely weeks).
#' TODO: re-calculate formula_dur_exclu if necessary.
#' TODO: obtain or compute age at DX for controls. 
#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------



# Load in and save raw data -----------------------------------------------

require(readstata13)

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/COG/AE23 and AE24/')
ae23 <- read.dta13(file = 'Infant_brfeeding_AE23.dta')
ae24 <- read.dta13(file = 'Infant_brfeeding_AE24.dta')

save(ae23, file = './ae23.raw.data.rdata')
save(ae24, file = './ae24.raw.data.rdata')

rm(ae23, ae24); gc()



# Add missing standard variables ------------------------------------------

require(dplyr)

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/COG/AE23 and AE24/')
load('ae23.raw.data.rdata'); load('ae24.raw.data.rdata')

#' Assign study numbers.
ae23$study <- 12
ae24$study <- 13

