#'---------------------------------------------------------------------------------------
#'
#'                                 ITALY (SETIL)
#' 
#' TODO: Recalculate duration of bf.dur.  
#'       Should it be the greater of lat.s.fi and lat.m.fi?
#' TODO: clarify which AnLL cases are actually AMLs and compute the standard DX variable.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' 2017.09.21.
#' 
#' Received Corrado's dataset for the CLIc pooled analysis in the forms of sas file.  
#' It is accompanied by a data dictionary, 'CLIC_InfantFeeding_AR_CM_AR_ago2017.xlsx'. 
#' 
#' What variables were supplied?  What is their structure?
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------



# Prep environment --------------------------------------------------------
require(haven)
require(dplyr)

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/SETIL/')



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

ita <- data.frame(read_sas('CLIC_InfantFeeding_ago2017.sas7bdat'))
save(ita, file = 'italy.raw.data.rdata')



# Rename variables --------------------------------------------------------

load('italy.raw.data.rdata')
colnames(ita) <- restring.columns(ita)

ita <- rename(ita, 
              unique.id = n.id,
              immphen = feno.lal,
              histology = fab.rec,
              bf.ever = lat.s, #' Recode to 0 and 1.
              bf.age.start = lat.s.in,
              bf.age.stop = lat.s.fi,
              bf.ex.dur = dur.exc.bf,
              ff.ex.ever = lat.a, #' TENATIVE.  Confirm with Corrado.
              ff.ex.age.start = lat.a.in, #' TENATIVE.  Confirm with Corrado.
              ff.ex.age.stop = lat.a.fi, #' TENATIVE.  Confirm with Corrado.
              ff.ex.dur = dur.exc.ff,
              mat.edu = scol.m.c,
              pat.edu = scol.p.c,
              child.race.eth = paese.na,
              birth.order = ord.nas,
              birth.wt.gr = peso.n, 
              gest.age = sett.ges)

ita$plurality <- ifelse(ita$gemello.ind == 1, "Twin", "Singleton")

#' Fix a minor error in the lat.a variable.
ita[ita$unique.id == '03/0007', 'ff.ex.ever'] <- 1



# Standardize existing variables ------------------------------------------

ita$unique.id <- paste0('it',ita$unique.id)
ita$case.control <- as.numeric(ifelse(ita$caso == 4, 0, ita$caso))
ita$sex <- ifelse(ita$sesso == 'M', 1, 2)
ita$birth.wt.gr <- ita$birth.wt.gr*1000
ita$bf.ever <- ifelse(ita$bf.ever == 6, 0, ita$bf.ever)
ita$lat.m <- ifelse(ita$lat.m == 6, 0, ita$lat.m)
ita$ff.ex.ever <- ifelse(ita$ff.ex.ever == 6, 0, ita$ff.ex.ever)



# Create missing standard variables ---------------------------------------

ita$study.number <- 7
ita$birth.year <- as.numeric(as.character(substr(ita$data.nas, 1, 4)))
ita$age.dx <- as.numeric(ita$d.diagno - ita$data.nas)/365
#' Corrado states that there are no race-ethnicity variables for parents because they 
#' are all Caucasian.  We will assume none are Hispanic and code this variable as if 
#' they were all NHW.
ita$mat.race.eth <- 'NHW'
ita$pat.race.eth <- 'NHW'

ita$age.solids.mos <- as.numeric(NA)

ita$bf.dur <- ita$bf.age.stop
ita$bf.ex.ever <- ifelse(ita$bf.ever == 1 & ita$ff.ex.ever == 0 & ita$lat.m == 0, 1, 0)
ita$bf.when.solids <- as.numeric(NA)
ita$bf.ex.when.solids <- as.numeric(NA)
ita$bf.currently <- as.numeric(NA)

ita$ff.ever <- ifelse(ita$lat.m  == 1 | ita$ff.ex.ever == 1, 1, 0)

#' Populate cytogenetics variable according to existing ones.
ita.cyto <- filter(
            select(ita, unique.id, case.control, t.9.22, t.9.22.p, t.9.22.1, t.4.11, aml1.eto, t.8.21.q, 
            inv.16.p, t.16.16., t.9.11, riarr.11, t.15.17,cbfb.myh, pml.rara),
            case.control == 1)

ita.cyto$cyto <- as.character('')

for (j in 3:15){
  
  ro <- as.numeric(which(ita.cyto[,j] == 1))
  co <- names(ita.cyto[j])
  
  for (i in 1:683){
    ita.cyto[i,16] <- ifelse(as.numeric(rownames(ita.cyto[i,])) %in% ro, paste0(co, ', ', ita.cyto[i,16]), ita.cyto[i,16])
  }
  
}

rm(i, j, co, ro)

ita <- left_join(ita, select(ita.cyto, unique.id, cyto), by = 'unique.id')

save(ita, file ='italy.v20171127.1.rdata')



# Inspecting infant feeding variables -------------------------------------

#' Some of the infant feeding variables are unclear.  Figure out what they
#' represent and rename/recalculate as needed.
print(ita[1:50, c(65:70,48:50,142:144)])
#' Believe lat.s variables refer to exclusive breastfeeding.
#' Believe lat.m variables refer to mixed breast and formula feeding.  
#' Believe lat.a variabels refer to exclusive formula feeding.

#' Note that for children with lat.a == 1, the supplied duration of exclusive
#' formula feeding is ALWAYS equal to the difference of lat.a.in and lat.a.fi.
tmp <- filter(ita, lat.a == 1)
tmp$comupted.dur.ex.ff <- tmp$lat.a.fi - tmp$lat.a.in
identical(tmp$comupted.dur.ex.ff, tmp$ff.ex.dur)

#' Generally kids with lat.a == 6 have duration of exclusive formula feeding == 0.
#' There is one exception.  Probably an error.
table(ita$ff.ex.dur, ita$ff.ex.dur, useNA = 'ifany')




# Extract uncertain AnLL diagnoses for review -----------------------------

#' Corrado said he could review AnLL cases who don't appear to be AML.
tmp <- filter(ita, caco == 12)
vec <- c(22,10,11,28,29,12,13,14,27,15,24,25,16,17,19)
tmp <- tmp[!tmp$histology %in% vec, ]
tmp <- c(tmp$unique.id)
tmp <- str_replace(tmp, '^it','')
write.csv(tmp, file = 'anll.ids.csv', row.names = FALSE)


# Infant feeding summary --------------------------------------------------

load('italy.v20171127.1.rdata')

tmp <- filter(ita, case.control == 0)
tmp$bf.ex.dur <- ifelse(tmp$bf.ever == 0, 0, tmp$bf.ex.dur)
print.if.summary(tmp)





































# Scratch paper -----------------------------------------------------------

#' Compute standard DX variable.
#' A vector of the fab.rec variable values that code for M0 thru M7 (these kids are AML).
vec <- c(22,10,11,28,29,12,13,14,27,15,24,25,16,17)

#' For kids who marked as acute non-lymphoblastic leukemia, set DX equal to AML if the fab.rec variable
#' codes for an M0 through M7 FAB classification.  Otherwise, set DX to other.
ita$dx <- ifelse(ita$caco %in% 41:42, 0,
                 ifelse(ita$caco == 11, 1, 
                        ifelse(ita$caco == 12 & ita$fab.rec %in% vec, 2, 3)))

      #' Inspect.  
      #' table(ita$caco)
      #' table(ita$dx)

#' Maternal and paternal dates of birth are given instead of ages.  Compute these, in years, at child's DOB.
ita$maternal.age <- as.numeric(with(ita, (data.nas - d.nas.m)/365))
ita$paternal.age <- as.numeric(with(ita, (data.nas - d.nas.p)/365))

      #' Inspect.
      #' summary(ita$maternal.age)
      #' summary(ita$paternal.age)
      #' print(ita[1:100,c(13,18,19,148,149)])

#' Convert birth weight to grams.
ita$birth.wt.gr <- ita$peso.n*1000

#' Variable names are in Italian, so let's rename some of the important ones right off the bat.
ita <- rename(ita, case.control = caso, immphen = feno.lal, birth.date = data.nas, sex = sesso, date.dx = d.diagno,
              birth.order = ord.nas, mat.edu = scol.m.c, pat.edu = scol.p.c, gest.age = sett.ges, agedx = eta.dia)

#' Corrado says that all participants in the study are Caucasian.
ita$mat.race.eth <- 'NHW'
ita$pat.race.eth <- 'NHW'

#' One child is missing all the infant feeding data.
#' The study used individual matching, but I don't see a good reason to throw out the two controls.
ita <- filter(ita, n.id != '08/0031')

save(ita, file = './italy.v20170921.1.rdata')







#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' Looking through the data.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------

#' Duration of breastfeeding is spread across 9 variables, that aren't described in 
#' detail.  The first in each trio is yes/no.  The second two must be start and end times.
#' 
#' Comparing these with the calculated variables dur.exc.bf and dur.exc.ff, it looks like:
#' lat.a.in and lat.a.fi are start and end times of formula feeding. 
#' lat.s.in and lat.s.fi are the start and end times of exclusive breastfeeding.
#' lat.m.in and lat.m.fi are the start and end times of mixed breast and formula feeding.
print(ita[1:100,c(48:50,65:70,142:144)])

#' Assess how many kids have at least some infant feeding data.
#' All but 1.
tmp <- ita[is.na(ita$lat.a) & is.na(ita$lat.m) & is.na(ita$lat.s), ]
print(tmp[,c(48:50,65:70,142:144)])

#' Uncertain whether cases coded 12 are AML, or any non-ALL leukemia.
tmp <- filter(ita, caco == 12)
print(tmp[1:82,c(17,10,11)])

#' A vector of the fab.rec variable values that code for M0 thru M7 (these kids are AML).
vec <- c(22,10,11,28,29,12,13,14,27,15,24,25,16,17)

#' 11 observations not in the vector.
tmp2 <- tmp[tmp$fab.rec %in% vec, ]
tmp3 <- tmp[!(tmp$fab.rec %in% vec), ]

print(tmp3[,c(17,10,11)])

ids <- c(tmp$n.id)



