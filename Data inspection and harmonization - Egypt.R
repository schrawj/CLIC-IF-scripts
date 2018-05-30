#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'
#'                                          EGYPT
#' 
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------

#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' 2017.06.07.
#' 
#' Received Sameera's dataset for the CLIc pooled analysis this week in the form of an 
#' .xlsx file.
#' 
#' This dataset was pretty comprehensive as to the details of study design and matching.
#' There are 299 ALL cases, 0 AML cases and 351 controls.  Only Precursor B-lineage ALL
#' cases are included.  
#' 
#' Cases are diagnosed at Children's Cancer Hospital, Egypt.  
#' 
#' Controls are population based.  Controls are frequency matched to cases on 
#' age, sex and residency.
#' 
#' What variables were supplied?  What is their structure?
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------




# Prep environment --------------------------------------------------------

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/Egypt/')

require(dplyr)
require(xlsx)




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

# Load in and save raw data -----------------------------------------------

egypt <- read.xlsx(file='raw.data.egypt.xlsx', sheetName = 'infant feeding', rowIndex = 2:652,
                   stringsAsFactors = FALSE, header = TRUE)

colnames(egypt) <- restring.columns(egypt)

save(egypt, file = 'egypt.raw.data.rdata')





# Generate standard variables ---------------------------------------------

load('egypt.raw.data.rdata')

egypt <- rename(egypt,
                unique.id = subject.unique.identifier,
                case.control = case.vs..control.status,
                matching = matching.,
                dx = all.aml.others, # convert to numeric.
                immphen = all.subtype,
                tel.aml.status = tel.aml.cytogenetics., # Append this info to cyto if positive.
                breastfeeding.months = duration.of.breastfeeding..months.,
                age.at.formula.months = age.at.introduction.of.formula.months.,
                age.at.solids.months = age.at.introduction.to.solids..months.,
                type.of.solids.one = type.s..of.solids.provided.during.infancy..1.,
                type.of.solids.two = type.s..of.solids.provided.during.infancy..2.,
                breastfeeding.exclus.months = duration.of.exclusive.breastfeeding,
                formula.type = type.of.formula..cow.s.milk..soy..other.,
                age.months = age.in.months,
                age.years = age.years,
                dob.day = date.of.birth.day,
                dob.month = dateof.birth.months,
                dob.year = date.of.birth.year,
                date.dx.case = date.of.diagnosis.for.cases,
                year.interview.control = year.of.recruitment.for.control,
                maternal.age = age.of.mother.at.birth,
                paternal.age = age.of.father.at.birth,
                mat.race.eth = maternal.ethinicity,
                pat.race.eth = paternal.ethinicity.,
                ses = socieconomic.status,
                mat.edu = mother.education.categories,
                pat.edu = father.education.categories,
                child.race.eth = ethinicity.of.child,
                number.deliveries = birth.plurality..number.of.deliveries.,
                number.pregnancies = parity..no.of.pregnancies.,
                mother.smoke.dur = maternal.smoking.during.pregnancy,
                mother.smoke.pre = maternal.smoking.before.pregnancy,
                urban.rural.birth = urban.rural.statusof.the.placeof.residence.at.birth,
                urban.rural.dx = urban.rural.statusof.the.placeof.residence.at.diagnosis.reference.date,
                father.cigs.pre = father.smoking.cigarettes.one.year.before.pregnanacy.am99a1,
                father.shisha.pre = father.smoking.shisha.one.year.before.pregnanacy.am99a2,
                cohabit = mother.live.with.husband.during.pregnancy.m1,
                father.cigs.dur = father.smoking.during.pregnancy.11,
                father.cigs.near.mom = father.smoking.cigarettes.in.mothers.presence.am12a,
                father.shisha.dur = father.smoking.shisha.during.pregnancy.m13,
                father.shisha.near.mom = father.smoking.shisha.in.mothers.presence.am14a,
                gest.age = gestational.age, # Convert to numeric.
                ff.dur = duration.of.formula.feeding..months., # Convert to numeric.
                ff.ex.dur = duration.of.exclusive.formula.feeding, # convert to numeric.
                bf.dur = duration.of.breastfeeding..months., # convert to numeric.
                bf.ex.dur = duration.of.exclusive.breastfeeding,
                age.solids.mos = age.at.introduction.to.solids..months.) # convert to numeric.

#' Some variables are empty or non-informative.
egypt <- select(egypt, 
                -study.name, -study.design, -matching, -source.of.case.ascertainment, -source.of.data, -source.of.control, -country)

#' Standardize existing variables.
egypt$sex <- as.numeric(ifelse(egypt$sex == 'male', 1, 2))
egypt$case.control <- ifelse(egypt$case.control == 'Case', 1, 0)

#' Generate missing standard variables.
egypt$histology <- as.character(NA)
egypt$cyto <- as.character(NA)
egypt$cyto <- ifelse(egypt$tel.aml.status == 'Positive', 'ETV6-RUNX1',egypt$cyto)
egypt$bf.ex.ever <- ifelse(is.na(egypt$age.at.formula.months), 1, 0)
egypt$bf.ever <- ifelse(egypt$bf.dur == 0, 0, 1)
egypt$ff.ever <- ifelse(!is.na(egypt$age.at.formula.months), 1, 0) 
egypt$ff.dur <- as.numeric(NA)
egypt$ff.ex.dur <- as.numeric(NA)

for (i in c(8,10,13)){
  egypt[,i] <- ifelse(egypt[,i] %in% c('40','99',"Don't Know"), NA, egypt[,i])
}

#' Check out the data.  Think about rules for generating bf.when.solids and bf.ex.when.solids.
#' print(egypt[1:100,c('age.solids.mos','bf.dur', 'bf.ex.dur')])
egypt$bf.when.solids <- ifelse((as.numeric(egypt$age.solids.mos) <= as.numeric(egypt$bf.dur)) | egypt$bf.dur == 'still in breast feeding', 1, 0)
#' print(egypt[1:100,c('age.solids.mos','bf.dur', 'bf.ex.dur', 'bf.when.solids')])
#' print(egypt[401:500,c('age.solids.mos','bf.dur', 'bf.ex.dur', 'bf.when.solids')])

#' Commented out this code: checks that no durations of exclusive breastfeeding exceed durations of any breastfeeding.
#' egypt$bf.ex.dur.flag <- ifelse(as.numeric(egypt$bf.ex.dur) > as.numeric(egypt$bf.dur), 1, 0)
#' print(egypt[1:100,c('bf.dur', 'bf.ex.dur','bf.ex.dur.flag')])
#' print(egypt[501:600,c('bf.dur', 'bf.ex.dur','bf.ex.dur.flag')])
#' egypt <- select(egypt, -bf.ex.dur.flag)
egypt$bf.ex.when.solids <- ifelse((as.numeric(egypt$age.solids.mos) <= as.numeric(egypt$bf.ex.dur)) | egypt$bf.ex.dur == 'still in breast feeding', 1, 0)
#' print(egypt[1:100,c('age.solids.mos', 'bf.dur','bf.ex.dur', 'bf.ex.when.solids')])

#' Change duration variables to numeric.
#' Add a flag for whether child is currently breastfed.
egypt$bf.currently <- ifelse(egypt$bf.dur == 'still in breast feeding', 1, 0)
egypt$age.solids.mos <- as.numeric(egypt$age.solids.mos)
for (i in c(8,13)){egypt[,i] <- as.numeric(ifelse(egypt[,i] == 'still in breast feeding', egypt$age.months, egypt[,i]))}

save(egypt, file = 'egypt.v.20171120.1.rdata')



# Infant feeding summary --------------------------------------------------

load('egypt.v.20171120.1.rdata')

tmp <- filter(egypt, case.control == 0)
print.if.summary(tmp)











