#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------
#' 2018.01.22.
#' 
#' Received raw data for four French studies (ADELE, ELECTRE, ESCALE and
#' ESTELLE) from Laurent and Jacqueline in the form of SAS files.
#' 
#' Accompanying data dictionary as an Excel spreadsheet.
#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------

require(haven)

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/France/')

adele <- as.data.frame(read_sas('pool_lal_aml_adelefinal.sas7bdat'))
electre <- as.data.frame(read_sas('pool_lal_aml_electrefinal.sas7bdat'))
escale <- as.data.frame(read_sas('pool_lal_aml_escalefinal.sas7bdat'))
estelle <- as.data.frame(read_sas('pool_lal_aml_estellefinal.sas7bdat'))

save(adele, file = 'adele.raw.data.rdata')
save(electre, file = 'electre.raw.data.rdata')
save(escale, file = 'escale.raw.data.rdata')
save(estelle, file = 'estelle.raw.data.rdata')




# A refresher on referring to elements in a list --------------------------

for (i in 1:length(l)){print(table(l[[i]][,2]))}
for (i in 1:length(l)){print(table(l[[i]][,'typela']))}



# Standardize variable names ----------------------------------------------

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/France/')

load('adele.raw.data.rdata'); load('electre.raw.data.rdata'); load('escale.raw.data.rdata'); load('estelle.raw.data.rdata')

l <- list(adele = adele, electre = electre, escale = escale, estelle = estelle)

for (i in 1:length(l)){
  names(l[[i]]) <- tolower(names(l[[i]]))
}

#' A function to reassign standard names to variable within each element of the list.
#' Only applied to variables present in all 4 datasets.
f <- function(df){
  rename(df, 
         unique.id = idno,
         case.control = case_cont,
         dx = typela,
         immphen = all_type,
         bf.ever = br_feed,
         bf.dur = brfd_dur, 
         age.dx = ageent,
         dob = ddn, #' Not a standard variable, but I'll rename it something more recognizeable anyways.
         sex = sxe,
         maternal.age = age_moth,
         paternal.age = age_fath,
         mat.edu = dip_m, 
         pat.ede = dip_p,
         child.race.eth = ethn,
         birth.order = bthorder,
         birth.wt.gr = birth_wt,
         birth.plur = plural,
         gest.age = gestage) 
}

estelle$ethn <- as.numeric(NA)

l <- lapply(l, f)

#' Add missing standard variables.
for (i in 1:length(l)){
  l[[i]]$mat.race.eth <- as.character(NA)
  l[[i]]$pat.race.eth <- as.character(NA)
  l[[i]]$age.solids.mos <- as.numeric(NA)
  l[[i]]$type.solids <- as.character(NA)
  l[[i]]$ff.type <- as.character(NA)
}

#' Recode variables.
for (i in 1:length(l)){
  l[[i]]$unique.id <- paste0('fr.', substr(names(l[i]),1,3), l[[i]]$unique.id)
}




# Scratch paper -----------------------------------------------------------

name <- 'formula_feed'
var.class <- as.character(NA)

for (i in 1:length(l)){
  if (name %in% names(l[[i]])){
    next
  }
  else{
    l[[i]]$formula_feed <- as.character(NA)
  }
}

add.var <- function(name){
  for (i in 1:length(l)){
    if (name %in% names(l[[i]])){
      next
    }
    else{
      l[[i]][,name] <- as.character(NA)
    }
  }
}

l <- add.var('norm_kar')


lapply(l, add.var('norm_kar'))

add.var('norm_kar', var.class = as.numeric(NA))

name <- 'norm_kar'
l[[4]][,'norm_kar'] <- as.numeric(NA)
names(l[[4]])
