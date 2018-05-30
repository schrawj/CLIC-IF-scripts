#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------
#'
#'                                COG 
#' 
#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------


# Prep environment --------------------------------------------------------
require(dplyr)
require(stringr)

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/COG/')



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
  print(paste('Formula feeding duration (months) (median, sd):', median(ff.dur, na.rm = TRUE), sd(ff.dur, na.rm = TRUE)))
  print(paste('Exclusive formula feeding duration (months) (median, sd):', median(ff.ex.dur, na.rm = TRUE), sd(ff.ex.dur, na.rm = TRUE)))
  
  print(paste('age at introduction to solids (median, sd)', median(age.solids.mos, na.rm = TRUE), ',', sd(age.solids.mos, na.rm = TRUE)))
  
  detach(dataframe)
}



# Read in and save raw data -----------------------------------------------
cog.raw <- read.csv(file = 'Z:/Jeremy/CLIC pooling project/Datasets/Raw data/COG/infant_feeding_db.csv', header = TRUE, stringsAsFactors = FALSE)
save(cog.raw, file = 'Z:/Jeremy/CLIC pooling project/Datasets/Raw data/COG/raw.data.cog.rdata')



# Standardize variables and variable names --------------------------------

load("raw.data.cog.rdata")
cog <- cog.raw
rm(cog.raw)
colnames(cog) <- restring.columns(cog)

#' Rename variables
cog <- rename(cog, 
              case.control = cc, #' convert to 0/1.
              age.dx = dxage,
              pat.age = age.fath,
              mat.age = age.moth,
              birth.wt.gr = birth.wt,
              bf.ever = br.feed,
              birth.order = bthorder,
              child.race.eth = c.eth,
              pat.race.eth = f.eth,
              mat.race.eth = m.eth,
              mat.edu = medu,
              pat.edu = fedu,
              gest.age = gestage,
              unique.id = id,
              immphen = immphenotyp,
              dx = leuk.typ)

#' Fix one instance where DOB and DOI were likely reversed.
cog[138, 'dateinter'] <- as.character('21-Mar-2000')
cog[138, 'datebirth'] <- as.character('24-Oct-1991')


#' Standardize existing variables.
cog$case.control <- ifelse(cog$case.control == 2, 0, cog$case.control)
cog$sex <- ifelse(cog$sex == 1, 2, 
                  ifelse(cog$sex == 0, 1, 0))
cog$unique.id <- paste0('cog',cog$unique.id)


#' This two breastfeeding variables thing is strange, but as far as I can tell kids either have
#' one or the other complete and the second is NA.  Probably the result of joining the E14 and
#' E15 studies together?

cog$bf.dur <- ifelse(is.na(cog$brfd.dur2), cog$brfd.dur/4.25,
                           ifelse(!is.na(cog$brfd.dur2), cog$brfd.dur2/4.25, NA))


#' print(cog[1:100,c('br.feed','brfd.dur','brfd.dur2')])
#' print(cog[101:300,c('br.feed','brfd.dur','brfd.dur2')])

#' tmp <- cog[!is.na(cog$brfd.dur2),]
#' print(tmp[1:100,c('br.feed','brfd.dur','brfd.dur2')])
#' print(tmp[101:300,c('br.feed','brfd.dur','brfd.dur2')])

#' tmp <- tmp[!is.na(tmp$brfd.dur), ]

#' Generate missing standard variables.
cog$study <- 6 

pattern <- '[-]{1}[[:digit:]]{4}'
cog$birth.year <- as.numeric(str_extract(cog$datebirth, pattern))
cog$birth.year <- as.numeric(substr(as.character(cog$birth.year), 2, 5))

cog$bf.ex.ever <- as.numeric(NA)
cog$bf.ex.dur <- as.numeric(NA)

cog$dob <- as.Date(cog$datebirth, format = '%d-%b-%Y')
cog$doi <- as.Date(cog$dateinter, format = '%d-%b-%Y')
cog$age.at.interview.mos <- as.numeric((cog$doi - cog$dob)/30.42)

cog$bf.currently <- ifelse(cog$bf.ever == 0, 0,
                           ifelse(cog$bf.ever == 1 & (cog$bf.dur >= cog$age.at.interview.mos), 1, NA))

cog$ff.ever <- as.numeric(NA)
cog$ff.dur <- as.numeric(NA)
cog$ff.ex.dur <- as.numeric(NA)
cog$ff.type <- as.numeric(NA)
cog$age.solids.mos <- as.numeric(NA)
cog$bf.when.solids <- as.numeric(NA)
cog$bf.ex.when.solids <- as.numeric(NA)

tmp <- filter(cog, bf.currently == 1)
print(tmp[,c('unique.id','bf.ever','bf.dur',"age.at.interview.mos",'datebirth','dateinter')])

save(cog, file = 'cog.v20171121.1.rdata')



# Infant feeding summary --------------------------------------------------

load('cog.v20171121.1.rdata')

tmp <- filter(cog, case.control == 0)
print.if.summary(tmp)
