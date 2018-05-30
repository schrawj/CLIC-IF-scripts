#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------
#' 
#'                                 NZCCS
#' 
#' TODO: Resolve issues with discordant age at DX for IDs 1261, 1778, 1825.
#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------




# Prep environment --------------------------------------------------------
require(dplyr)
require(stringr)

setwd('Z:/Jeremy/CLIC Pooling Project/Datasets/Raw data/NZCCS/')



# Load user-defined functions ---------------------------------------------
restring.columns <- function(x){
  stringr::str_replace_all(tolower(colnames(x)),'_','.')
}



# Load in and save raw data -----------------------------------------------
load("raw.data.nz.rdata")
nz <- data.frame(nz.raw)
rm(nz.raw)

colnames(nz) <- restring.columns(nz)

save(nz, file = './new.zealand.v20171018.rdata')

#' Tiffany says this is the raw data she got from John.
nz.old <- xlsx::read.xlsx(file = './Raw data/NZCCS/Covariate data from Tiffany/Dockerty Raw Data.xlsx', header = TRUE, stringsAsFactors = FALSE, sheetIndex = 1)
save(nz.old, file = './Raw data/NZCCS/nz.covariate.data.from.Tiffany.rdata')



# Inspect the data --------------------------------------------------------

#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------
#' Some points to raise right off the bat.
#' 
#' - The dataset contains all childhood cancers.  We will need to extract
#'   the relevant diagnoses.
#'   
#' - John believes he supplied duplicated variables.
#'   E806 is the child's age.
#'   A302:A311 are the child's birth date and reference date fields.
#'   Confirm that there are other fields for these data.
#'   
#' - John did not provide a number of the requested covariates because he
#'   had already sent them as part of a previous pooling project.
#'   Obtain the other file(s) from Tiffany and verify that we have what
#'   we want.
#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------
load("./new.zealand.v20171018.rdata")

names(nz)

str(nz)

unique(nz$icdoclas)
#' Interesting representation of age at DX.
tmp <- c(nz$age.at.d)
#' Pull out years portion.
tmp <- as.numeric(substr(tmp, 1,2))

tmp2 <- c(nz$e806.481)
#' Not identical.  
identical(tmp, tmp2)
#' Where different?
tmp <- data.frame(id = nz$studyno, age.at.d = tmp, e806 = tmp2)
tmp$age.diff <- tmp$age.at.d - tmp$e806

table(tmp$age.diff)
tmp <- filter(tmp, age.diff != 0)
print(tmp)

ids <- c(NaN,1825)

#' There's one mismatch that don't appear to be due to rounding.
#' age.at.d looks to be correct in that case.
#' Can drop E806.481
tmp2 <- nz[nz$studyno %in% ids, ]
tmp2[, c(8,15,3,7)]

#' How about the A series variables?
head(nz$a302.31)
head(nz$dob)

#' A302.31 is the day portion of DOB. Can drop.
tmp <- c(nz$a302.31)
tmp2 <- c(as.numeric(substr(nz$dob, 4, 5)))
identical(tmp, tmp2)

#' 303 is month portion of DOB.
tmp <- c(nz$a303.31)
tmp2 <- c(as.numeric(substr(nz$dob, 1, 2)))
identical(tmp, tmp2)

#' Year portion.
head(nz$a304.31)

#' Same setup here, but for reference date.
head(nz$diagn.re)
head(nz$a309.31)
head(nz$a310.31)
head(nz$a311.31)



# Extract relevant diagnoses ----------------------------------------------
load("./new.zealand.v20171018.rdata")

#' TIL: erythroleukemia is a rare form of AML where the blasts are derived
#' from erythroid precursors.  The more you know...
#' Extract relevant diagnoses, plus controls (for whom tumor type is an empty string).
diagnoses <- c('Acute lymphoblastic leukaemia, B-cell','Acute myeloid leukaemia','Acute promyelocytic leukaemia',
               'Acute lymphoblastic leukaemia, T-cell','Acute leukaemia, biphenotypic/bilineal','Acute lymphoblastic leukaemia, unkn cell type',
               "ALL: Burkitt's cell leukaemia (B-cell)", 'Erythroleukaemia', " ")

nz <- nz[nz$icdoclas %in% diagnoses, ]

save(nz, file = './new.zealand.v20171019.1.rdata')



# Remove duplicated columns -----------------------------------------------
load('./new.zealand.v20171019.1.rdata')

nz <- select(nz, -a302.31, -a303.31, -a304.31, -a309.31, -a310.31, -a311.31, -e806.481)

save(nz, file = 'new.zealand.v20171019.2.rdata')




# Consolidate all data into a single object -------------------------------
load('/new.zealand.v20171019.2.rdata')
load("./Raw data/NZCCS/nz.covariate.data.from.Tiffany.rdata")

colnames(nz.old) <- restring.columns(nz.old)

nz <- rename(nz, unique.id = id, case.control = cc, birth.order = birthorder)
nz.old <- rename(nz.old, 
                 unique.id = idno,
                 dx = leuktype,
                 immphen = immuno,
                 mat.edu = motheduc,
                 pat.edu = fatheduc,
                 birth.wt.gr = birth.wt,
                 gest.age = gestage,
                 child.race.eth = ethnic)

tmp <- select(nz.old, 
              unique.id, dx, immphen, mat.edu, pat.edu, birth.wt.gr, gest.age, bth.type, pregsmok, dadpgsmk, age.moth, age.fath, child.race.eth, childday, plural)

nz <- left_join(nz, tmp, by = 'unique.id')

save(nz, file = './new.zealand.v20171019.3.rdata')

rm(nz.old, tmp)



# Standardize variables ---------------------------------------------------

load('new.zealand.v20171019.3.rdata')

nz <- rename(nz, 
             age.dx = age.at.d,
             bf.ever = e600.461, 
             bf.dur.days = e601.461, 
             bf.dur.weeks = e602.461,  
             bf.dur.months = e603.461, 
             ff.ever = e604.461, 
             ff.age.intro.days = e605.461, 
             ff.age.intro.weeks = e606.461, 
             cow.milk.ever = e609.461, 
             cow.milk.age = e610.461, 
             age.solids.mos = e611.461, 
             age.solids.weeks = e612.461) 

#' Changing NaN values to NA.  Not sure that matters, but whatever.
for (i in 21:33){
  nz[,i] <- ifelse(is.na(nz[,i]), NA, nz[,i])
}

#' Standardize existing variables.
nz$bf.ever <- ifelse(nz$bf.ever == 2, 0, nz$bf.ever)
nz$ff.ever <- ifelse(nz$ff.ever == 2, 0, nz$ff.ever)

nz$bf.dur <- ifelse(nz$bf.ever == 0, 0,
                    ifelse(nz$bf.ever == 1 & !is.na(nz$bf.dur.days), nz$bf.dur.days/30.42,
                          ifelse(nz$bf.ever == 1 & !is.na(nz$bf.dur.weeks), nz$bf.dur.weeks/4.25,
                                 ifelse(nz$bf.ever == 1 & !is.na(nz$bf.dur.months), nz$bf.dur.months, NA))))

#' print(nz[1:100,c(21:24,69)])

nz$ff.age.intro.months <- ifelse(nz$ff.ever == 0, NA, 
                                 ifelse(nz$ff.ever == 1 & !is.na(nz$ff.age.intro.days), nz$ff.age.intro.days/30.42,
                                        ifelse(nz$ff.ever == 1 & !is.na(nz$ff.age.intro.weeks), nz$ff.age.intro.weeks/4.25,NA)))

#' print(nz[1:100,c('ff.ever','ff.age.intro.days','ff.age.intro.weeks','ff.age.intro.months')])

nz$age.solids.mos <- ifelse(is.na(nz$age.solids.mos) & !is.na(nz$age.solids.weeks), nz$age.solids.weeks/4.25, nz$age.solids.mos)

#' print(nz[1:100,c('age.solids.weeks','age.solids.mos')])


#' Compute missing variables.
nz$birth.year <- as.numeric(paste0('19',substr(nz$dob, 7, 8)))
nz$study <- 8
nz$bf.currently <- as.numeric(NA)
nz$ff.dur <- as.numeric(NA)
nz$ff.ex.dur <- as.numeric(NA)
nz$ff.type <- as.character(NA)
nz$bf.ex.ever <- ifelse(nz$bf.ever == 1 & nz$ff.ever == 0, 1,
                        ifelse(nz$ff.ever == 1, 0, 
                               ifelse(is.na(nz$bf.ever) | is.na(nz$ff.ever), NA, NA)))

#' print(nz[1:100,c('ff.ever','bf.ever','bf.ex.ever')])

tmp1 <- filter(nz, bf.ever == 0)
tmp1$bf.ex.dur <- 0

tmp2 <- filter(nz, bf.ever == 1)
tmp2$bf.ex.dur <- 0 
for (i in 1:378){
  tmp2[i, 'bf.ex.dur'] <- min(tmp2[i, c('ff.age.intro.months','age.solids.mos','cow.milk.age')], na.rm = TRUE)
}
tmp2$bf.ex.dur <- ifelse(tmp2$bf.ex.dur == Inf, NA, tmp2$bf.ex.dur)

#' print(tmp2[1:100, c('ff.age.intro.months','age.solids.mos','cow.milk.age','bf.ex.dur')])

nz <- arrange(rbind(tmp1, tmp2), unique.id)
rm(tmp1, tmp2, i)

nz$bf.when.solids <- ifelse(nz$bf.ever == 0, 0,
                            ifelse(nz$bf.ever == 1 & (nz$bf.dur >= nz$age.solids.mos), 1, 
                                   ifelse(nz$bf.ever == 1 & (nz$bf.dur < nz$age.solids.mos), 0, NA)))

nz$bf.ex.when.solids <- ifelse(nz$bf.ever == 0, 0,
                               ifelse(nz$bf.ever == 1 & (nz$bf.ex.dur >= nz$age.solids.mos), 1, 
                                       ifelse(nz$bf.ever == 1 & (nz$bf.ex.dur < nz$age.solids.mos), 0, NA)))

#' print(nz[1:100, c('bf.ever','bf.dur','bf.ex.dur','age.solids.mos', 'bf.when.solids','bf.ex.when.solids')])

save(nz, file = 'nz.v20171121.1.rdata')



# Infant feeding summary --------------------------------------------------

load('nz.v20171121.1.rdata')

tmp <- filter(nz, case.control == 0)
print.if.summary(tmp)
