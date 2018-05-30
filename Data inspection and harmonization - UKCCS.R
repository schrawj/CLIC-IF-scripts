#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'
#'                                  UKCCS
#' 
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' 2017.09.14.
#' 
#' Received UKCCS dataset for the CLIc pooled analysis in the form of a .csv file.  It 
#' is accompanied by a data dictionary, CLICInfantFeeding_UKCCSVariables.  
#' 
#' Both files are saved in GenEpi2/Jeremy/CLIC pooling project/Datasets/Raw data/UKCCS/.
#' 
#' What variables were supplied?  What is their structure?
#' 
#' Prep environment.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------



# Prep environment --------------------------------------------------------

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/UKCCS/')
require(dplyr)



# User-defined functions --------------------------------------------------

restring.columns <- function(x){
  stringr::str_replace_all(tolower(colnames(x)),'_','.')
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




# Load in and save raw data -----------------------------------------------

ukccs.raw <- read.csv(file = './UKCCS_ALInfantFeedingData.csv', header = TRUE, stringsAsFactors = FALSE)
save(ukccs.raw, file = './ukccs.raw.data.rdata')



# Standardize variables ---------------------------------------------------

load('ukccs.raw.data.rdata')

uk <- ukccs.raw
rm(ukccs.raw)

#' Rename existing variables.
colnames(uk) <- restring.columns(uk)
uk <- rename(uk, 
             unique.id = study.id,
             case.control = ccstatus,
             match.set = matchedset.id,
             dx = diagnosis,
             histology = fabtype,
             immphen = immunophenotype,
             cyto = cytogenetics,
             birth.year = yob,
             maternal.age = motherageatbirth,
             paternal.age = fatherageatbirth,
             mat.edu = motherqualifications,
             pat.edu = fatherqualifications,
             child.race.eth = childethnicity,
             mat.race.eth = motherethnicity,
             pat.race.eth = fatherethnicity,
             birth.order = birthorder,
             gest.age = gestationage,
             birth.wt.gr = birthweight,
             bf.ever = breastfed,
             bf.dur = breastfedduration,
             ff.ever = formulafed,
             ff.dur = formuladuration,
             bf.ex.dur = exclusivebreastfedduration,
             ff.ex.dur = exclusiveformuladuration)

#' Standardize existing variables.
uk$unique.id <- paste0('uk', uk$unique.id)
uk$match.set <- paste0('uk', uk$match.set)
uk$case.control <- ifelse(uk$case.control == 'Ctrl', 0, 1)
uk$study <- 4
uk$dx <- ifelse(uk$dx == 'ALL', 1, 
                ifelse(uk$dx == 'AML', 2, 0)) 
uk$sex <- as.numeric(ifelse(uk$sex == 'F', 2, 1))

#' Compute missing standard variables.
uk$bf.dur <- uk$bf.dur/30.42
uk$bf.ex.dur <- uk$bf.ex.dur/30.42
uk$ff.dur <- uk$ff.dur/30.42
uk$ff.ex.dur <- uk$ff.ex.dur/30.42
uk$bf.currently <- as.numeric(NA)
uk$ff.type <- ifelse(uk$formulasoyabased == 0, 'Milk',
                     ifelse(uk$formulasoyabased == 1, 'Soy',
                            ifelse(uk$formulasoyabased == 9, 'Unknown', NA)))
uk$ff.type <- ifelse(uk$ff.ever == 0, NA, uk$ff.type)
uk$age.solids.mos <- uk$agedsolids/30.42
uk$bf.when.solids <- ifelse(uk$age.solids.mos <= uk$bf.dur, 1, 0)
uk$bf.ex.ever <- ifelse(uk$bf.ever == 1 & uk$ff.ever == 0, 1, 
                        ifelse(uk$bf.ever %in% c(0,9) | uk$ff.ever %in% c(1,9), 0, NA))

#' table(uk$bf.ever, uk$ff.ever, useNA = 'always')
#' table(uk$bf.ex.ever, useNA = 'always')



# Compute breastfeeding status at time solids introduced ------------------

#' Call a child exclusively breastfed when solids were introduced if: 
#' breastfeeding duration > age at introduction and they were never formula fed,
#' OR they were formula fed but age at introduction to solids is less than age at introduction 
#' to formula and less than duration of breastfeeding.
uk$bf.ex.when.solids <- ifelse(uk$bf.dur >= uk$age.solids.mos & uk$ff.ever == 0, 1,
                               ifelse((uk$bf.dur >= uk$age.solids.mos) & ((uk$agedformulafed)/30.42 > uk$age.solids.mos), 1, 0))

#' Inspect results.  Commented out.
#' tmp <- filter(uk, bf.ex.when.solids == 1)
#' tmp$agedformulafed <- tmp$agedformulafed/30.42
#' print(tmp[1:100,c(22,25,23,26,56)])



# Save processed data -----------------------------------------------------

save(uk, file = 'uk.v20171120.1.rdata')



# Infant feeding summary --------------------------------------------------
load('uk.v20171120.1.rdata')

tmp <- filter(uk, dx == 0)
print.if.summary(tmp)








#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' Exploration.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------

#' Study.id is study id, obviously.  There are no duplicates.
ukccs <- ukccs[!duplicated(ukccs$study.id),]

#' study.id has an interesting structure.  Three numbers separated by slashes.
#' This is [region]/[id for trio w/in region]/[id for subject w/in trio].
#' Within each trio, it looks like the case is always /00.
print(ukccs[1:100,1:6])
print(ukccs[1200:1300,1:6])
print(ukccs[2280:2380,1:6])
#' Full karyotype included.  Would be a (occasionally not so) fun regular expressions 
#' exercise to parse these.
tmp <- ukccs[!duplicated(ukccs$cytogenetics), ]
tmp <- c(tmp$cytogenetics)
head(tmp, 100)

#' Categories are given for maternal and paternal race/ethnicity.  It is stated that 
#' child's race/ethnicity is derived from these. Children of mixed-race couples always 
#' given as mixed. 
tmp <- ukccs[ukccs$motherethnicity != ukccs$fatherethnicity, ]
print(tmp[1:100,c(1,19:21)])

#' Paternal race-ethnicity is missing at ~8%.  This propagates to the child as well.
table(ukccs$fatherethnicity)
table(ukccs$childethnicity)



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' Begin computing some of our standard variables.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
ukccs <- ukccs.raw
rm(ukccs.raw)

colnames(ukccs) <- restring.columns(ukccs)

ukccs$study.number <- 4
ukccs$unique.id <- paste0('uk',ukccs$study.id)

save(ukccs, file = 'Z:/Jeremy/CLIC pooling project/Datasets/ukccs.v20170914.1.rdata')
