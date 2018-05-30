#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'
#'                                          CANADA
#' 
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------

#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' 2017.06.07.
#' 
#' Received Claire's dataset for the CLIc pooled analysis last week in the form of a 
#' .dta file.
#' 
#' This particular dataset was light on metadata concerning the sorts of subjects 
#' included, matching criteria etc.  See Infante-Rivard et. al. Children's Health 2005
#' for more details of recruitment and matching.
#' 
#' Cases were enrolled via Quebec-wide coverage of hospitals from 1980-2000.  
#' Only ALL cases were enrolled.  No AML in this study.
#' 
#' Controls were poulation-
#' based and were matched to csaes 1:1 on sex and age at date of diagnosis.  
#' 
#' What variables were supplied?  What is their format?
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------




# Prep environment --------------------------------------------------------

require(dplyr)
require(ggplot2)

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/Canada/')



# User-defined functions --------------------------------------------------

restring.columns <- function(dataframe){
  stringr::str_replace_all(tolower(colnames(dataframe)),'_','.')
}

print.if.summary <- function(dataframe){
  attach(dataframe)
  
  tmp <- subset(dataframe, bf.ever == 1)
  print(paste('Number and proportion of children ever breastfed', length(rownames(tmp)), ',', (length(rownames(tmp))/length(rownames(dataframe)))*100))
  print(paste('breastfeeding duration (months) (median, sd):', median(bf.dur, na.rm = TRUE), ',', sd(bf.dur, na.rm = TRUE)))
  
  tmp <- subset(dataframe, bf.ex.ever == 1)
  print(paste('Number and proportion of children exclusively breastfed', length(rownames(tmp)), ',', (length(rownames(tmp))/length(rownames(dataframe)))*100))
  print(paste('Exclusive breastfeeding duration (months) (median, sd):', median(bf.ex.dur, na.rm = TRUE), sd(bf.ex.dur, na.rm = TRUE)))
  
  tmp <- subset(dataframe, bf.when.solids == 1)
  print(paste('Number and proportion of children breastfeeding when solids were introduced', length(rownames(tmp)), ',', (length(rownames(tmp))/length(rownames(dataframe)))*100))
  
  tmp <- subset(dataframe, bf.ex.when.solids == 1)
  print(paste('Number and proportion of children exclusively breastfeeding when solids were introduced', length(rownames(tmp)), ',', (length(rownames(tmp))/length(rownames(dataframe)))*100))
  
  tmp <- subset(dataframe, ff.ever == 1)
  print(paste('Number and proportion of children ever formula fed', length(rownames(tmp)), ',', (length(rownames(tmp))/length(rownames(dataframe)))*100))
  print(paste('Formula feeding duration (months) (median, sd):', median(bf.dur, na.rm = TRUE), sd(bf.dur, na.rm = TRUE)))
  print(paste('Exclusive formula feeding duration (months) (median, sd):', median(ff.ex.dur, na.rm = TRUE), sd(ff.ex.dur, na.rm = TRUE)))
  
  print(paste('age at introduction to solids (median, sd)', median(age.solids.mos, na.rm = TRUE), ',', sd(age.solids.mos, na.rm = TRUE)))
  
  detach(dataframe)
}



# Load in and save raw data -----------------------------------------------

canada <- readstata13::read.dta13(file='./canada.raw.data.dta', convert.underscore = TRUE)

save(canada, 
     file = './canada.raw.data.rdata')



# Standardize variables ---------------------------------------------------

load('raw.data.canada.rdata')

#' Rename existing standard variables.
canada <- rename(canada, 
                 immphen = preimmuno, 
                 bf.ever = breast.feeding, 
                 bf.dur = breast.feeding.dur,
                 mat.edu = mat.education,
                 pat.edu = pat.education,
                 mat.race.eth = ethnicity.mother,
                 pat.race.eth = ethnicity.father,
                 birth.wt.gr = birth.weight,
                 gest.age = gestational.age)


#' Standardize existing variables.
canada$unique.id <- paste0('ca',canada$unique.id)
canada$case.control <- as.numeric(ifelse(canada$case.control == 'case', 1, 0))
canada$sex <- as.numeric(ifelse(canada$sex == 'FEM', 2, 1))
canada$bf.ever <- as.numeric(ifelse(canada$bf.ever == 'yes', 1, 0))
canada$bf.dur <- ifelse(canada$bf.dur == 888, NA, canada$bf.dur)


#' Generate missing standard variables.
canada$study <- 2
canada$histology <- as.character(NA)
canada$cyto <- as.character(NA)
canada$birth.year <- as.numeric(substr(canada$birth.date, 1, 4))
canada$bf.ex.dur <- as.numeric(NA)
canada$ff.ever <- as.numeric(NA)
canada$bf.ex.ever <- as.numeric(NA)
canada$ff.dur <- as.numeric(NA)
canada$ff.type <- as.character(NA)
canada$ff.ex.dur <- as.numeric(NA)
canada$age.solids.mos <- as.numeric(NA)
canada$bf.when.solids <- as.numeric(NA)
canada$bf.ex.when.solids <- as.numeric(NA)

print(ggplot(data = canada, aes(x = bf.dur)) + geom_histogram(fill = 'white', color = 'red'))

save(canada, file = 'canada.v20171120.1.rdata')



# Infant feeding summary --------------------------------------------------

load("canada.v20171120.1.rdata")

tmp <- filter(canada, case.control == 0)
print.if.summary(tmp)
