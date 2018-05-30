#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------
#'
#'                                GREECE 
#' 
#'-------------------------------------------------------------------------
#'-------------------------------------------------------------------------



# prep environment --------------------------------------------------------
require(dplyr)

#' For desktop:
setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/Greece/')



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



# Load in an save raw data ------------------------------------------------
gr.raw <- read.csv(file = './greece raw data.csv', header = TRUE, stringsAsFactors = FALSE)

colnames(gr.raw) <- restring.columns(gr.raw)

save(gr.raw, file = './raw.data.greece.rdata')



# Standardize variables and variable names --------------------------------

load("raw.data.greece.rdata")

gr <- gr.raw
rm(gr.raw)

#' Rename variables.
gr <- rename(gr,
             case.control = case,
             age.dx = agerow,
             birth.year = byer,
             maternal.age = bmage, 
             paternal.age = bfage,
             mat.edu = gmedu,
             pat.edu = gfedu,
             child.race.eth = nation,
             pat.race.eth = fethnik,
             mat.race.eth = methnik,
             birth.order = border,
             birth.wt.gr = bweig,
             gest.age = bornweek,
             bf.ever = lact,
             bf.dur = lactmon)

#' Standardize variables.
gr$case.control <- ifelse(gr$case.control == 1, 0, 1)
gr$birth.wt.gr <- ifelse(gr$birth.wt.gr == 9999, NA, gr$birth.wt.gr)
gr$gest.age <- ifelse(gr$gest.age == 99, NA, gr$gest.age)
gr$bf.ever <- ifelse(gr$bf.ever == 9, NA, gr$bf.ever)
gr$bf.dur <- ifelse(gr$bf.dur %in% c(88,99), NA, gr$bf.dur)
gr$bf.dur <- ifelse(gr$bf.ever == 0, 0, gr$bf.dur)

#' Compute missing standard variables.
gr$unique.id <- paste0('gr',gr$serialn1,gr$case)
gr$bf.ex.ever <- as.numeric(NA)
gr$bf.ex.dur <- as.numeric(NA)
gr$bf.currently <- as.numeric(NA)
gr$age.solids.mos <- as.numeric(NA)
gr$bf.when.solids <- as.numeric(NA)
gr$bf.ex.when.solids <- as.numeric(NA)

for (i in 37:59){
  gr[,i] <- ifelse(gr[,i] %in% c(88,99), NA, gr[,i]) 
}

#' Want to exclude cow, goat, soy and evaporated milk from calculations of age at intro to formula.
#' That's more comparable with what other studies did.
tmp <- gr

vec <- c(5, 20, 30, 49, 51, 91)
tmp$milkfrom <- ifelse(tmp$milkwhat %in% vec, NA, tmp$milkfrom)
tmp$milkto <- ifelse(tmp$milkwhat %in% vec, NA, tmp$milkto)

tmp$age.formula.start <- as.numeric(NA)
for (i in 1:2870){
  tmp[i, 'age.formula.start'] <- min(tmp[i, c(37,39,41,43,45,47,49,51,53,55,57)], na.rm = TRUE)
}
tmp$age.formula.start <- ifelse(tmp$age.formula.start == Inf, NA, tmp$age.formula.start)

tmp$age.formula.stop <- as.numeric(NA)
for (i in 1:2870){
  tmp[i, 'age.formula.stop'] <- max(tmp[i, c(38,40,42,44,46,48,50,52,54,56,58)], na.rm = TRUE)
}
tmp$age.formula.stop <- ifelse(tmp$age.formula.stop == -Inf, NA, tmp$age.formula.stop)

tmp <- select(tmp, unique.id, age.formula.start, age.formula.stop)

gr <- left_join(gr, tmp, by = 'unique.id')

rm(tmp, i, vec)

gr$ff.ever <- ifelse(is.na(gr$age.formula.start) & is.na(gr$age.formula.stop), 0, 1)
gr$ff.dur <- gr$age.formula.stop - gr$age.formula.start
gr$ff.ex.dur <- as.numeric(NA)

#' Now to figure out how to compute ff.type.
tmp <- gr
tmp$number <- 500
tmp$ff.type <- 'placeholder'

for (j in 37:59){
  for (i in 1:2870){
      if(!is.na(tmp[i,j]) & tmp[i,j] < tmp[i, 'number']){
        tmp[i, 'number'] <- tmp[i,j]
        tmp[i, 'ff.type'] <- names(tmp[j])
    }
  }
}

tmp$ff.type <- ifelse(tmp$ff.type == 'placeholder', NA, tmp$ff.type)

tmp <- select(tmp, unique.id, ff.type)
gr <- left_join(gr, tmp, by = 'unique.id')

rm(tmp, i, j)

save(gr, file = 'greece.v20171121.1.rdata')



# Infant feeding summary --------------------------------------------------

load('greece.v20171121.1.rdata')

tmp <- filter(gr, case.control == 0)
print.if.summary(tmp)




