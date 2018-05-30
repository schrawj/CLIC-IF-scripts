#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'
#'                                  WASHINGTON STATE
#' 
#' 2017.09.14.
#' 
#' Received Beth's dataset for the CLIc pooled analysis in the form of a .dta file.  
#' It is accompanied by a data dictionary, 
#' VarList-SurveillChildCancer-ScheurerSubFile-4-3-17.xlsx.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------



# Prep environment --------------------------------------------------------

setwd('Z:/Jeremy/CLIC pooling project/Datasets/Raw data/Washington/')

require(readstata13)
require(dplyr)



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

compute.cancer1 <- function(hist, site){
  ifelse(hist %in% cancer.codes$all.non.contingent.codes, 'all',
         ifelse(hist %in% cancer.codes$all.contingent.codes & site %in% cancer.codes$all.contingent.codes.sites, 'all', 
                ifelse(hist %in% cancer.codes$aml.codes, 'aml', 
                       ifelse(hist %in% cancer.codes$other.hem.codes, 'leu.other', 
                              ifelse(hist %in% cancer.codes$hl.codes, 'hl',
                                     ifelse(hist %in% cancer.codes$nhl.non.contingent.codes, 'nhl',
                                            ifelse(hist %in% cancer.codes$nhl.contingent.codes & site %in% cancer.codes$nhl.contingent.codes.sites, 'nhl',
                                                   ifelse(hist %in% cancer.codes$lym.other.codes, 'lym.other',
                                                          ifelse(hist %in% cancer.codes$ependymoma.codes, 'ependymoma',
                                                                 ifelse(hist %in% cancer.codes$astro.non.contingent.codes, 'astro',
                                                                        ifelse(hist %in% cancer.codes$astro.contingent.codes & site %in% cancer.codes$astro.contingent.codes.sites, 'astro',
                                                                               ifelse(hist %in% cancer.codes$medullo.codes, 'medullo',
                                                                                      ifelse(hist %in% cancer.codes$pnet.codes, 'pnet',
                                                                                             ifelse(hist %in% cancer.codes$cns.other.non.contingent.codes, 'cns.other',
                                                                                                    ifelse(hist %in% cancer.codes$cns.other.contingent.codes1 & site %in% cancer.codes$cns.other.contingent.codes1.sites, 'cns.other',
                                                                                                           ifelse(hist %in% cancer.codes$cns.other.contingent.codes2 & site %in% cancer.codes$cns.other.contingent.codes2.sites, 'cns.other',
                                                                                                                  ifelse(hist %in% cancer.codes$cns.other.contingent.codes3 & site %in% cancer.codes$cns.other.contingent.codes3.sites, 'cns.other',
                                                                                                                         ifelse(hist %in% cancer.codes$neuroblast.codes, 'neuro',
                                                                                                                                ifelse(hist %in% cancer.codes$pns.other.non.contingent.codes, 'pns.other',
                                                                                                                                       ifelse(hist %in% cancer.codes$pns.other.contingent.codes & site %in% cancer.codes$pns.other.contingent.codes.sites, 'pns.other',      
                                                                                                                                              ifelse(hist %in% cancer.codes$retino.codes, 'retino',
                                                                                                                                                     ifelse(hist %in% cancer.codes$nephro.codes, 'nephro',
                                                                                                                                                            ifelse(hist %in% cancer.codes$renal.other.non.contingent.codes, 'renal.other',
                                                                                                                                                                   ifelse(hist %in% cancer.codes$renal.other.contingent.codes & site %in% cancer.codes$renal.other.contingent.codes.sites, 'renal.other',
                                                                                                                                                                          ifelse(hist %in% cancer.codes$hepatoblast.codes, 'hepato',
                                                                                                                                                                                 ifelse(hist %in% cancer.codes$hepatic.other.non.contingent.codes, 'hepatic.other',
                                                                                                                                                                                        ifelse(hist %in% cancer.codes$hepatic.other.contingent.codes & site %in% cancer.codes$hepatic.other.contingent.codes.sites, 'hepatic.other',
                                                                                                                                                                                               ifelse(hist %in% cancer.codes$osteo.codes & site %in% cancer.codes$osteo.codes.sites, 'osteo',
                                                                                                                                                                                                      ifelse(hist %in% cancer.codes$ewing.contingent.codes1 & site %in% cancer.codes$ewing.contingent.codes1.sites, 'ewing',
                                                                                                                                                                                                             ifelse(hist %in% cancer.codes$ewing.contingent.codes2 & site %in% cancer.codes$ewing.contingent.codes2.sites, 'ewing', 'other.cancer'))))))))))))))))))))))))))))))
}
compute.cancer2 <- function(hist, site, newvar){
  ifelse(hist %in% cancer.codes$bone.other.non.contingent.codes, 'bone.other',
         ifelse(hist %in% cancer.codes$bone.other.contingent.codes1 & site %in% cancer.codes$bone.other.contingent.codes1.sites, 'bone.other',
                ifelse(hist %in% cancer.codes$bone.other.contingent.codes2 & site %in% cancer.codes$bone.other.contingent.codes2.sites, 'bone.other',  
                       ifelse(hist %in% cancer.codes$erms.codes, 'erms',
                              ifelse(hist %in% cancer.codes$arms.codes, 'arms',      
                                     ifelse(hist %in% cancer.codes$rms.other.codes, 'rms.other',
                                            ifelse(hist %in% cancer.codes$soft.other.non.contingent.codes, 'soft.other',
                                                   ifelse(hist %in% cancer.codes$soft.other.contingent.codes1 & site %in% cancer.codes$soft.other.contingent.codes1.sites, 'soft.other',
                                                          ifelse(hist %in% cancer.codes$soft.other.contingent.codes2 & site %in% cancer.codes$soft.other.contingent.codes2.sites, 'soft.other',
                                                                 ifelse(hist %in% cancer.codes$soft.other.contingent.codes3 & site %in% cancer.codes$soft.other.contingent.codes3.sites, 'soft.other',
                                                                        ifelse(hist %in% cancer.codes$soft.other.contingent.codes4 & site %in% cancer.codes$soft.other.contingent.codes4.sites, 'soft.other',
                                                                               ifelse(hist %in% cancer.codes$soft.other.contingent.codes5 & site %in% cancer.codes$soft.other.contingent.codes5.sites, 'soft.other',
                                                                                      ifelse(hist %in% cancer.codes$soft.other.contingent.codes6 & site %in% cancer.codes$soft.other.contingent.codes6.sites, 'soft.other',
                                                                                             ifelse(hist %in% cancer.codes$soft.other.contingent.codes7 & site %in% cancer.codes$soft.other.contingent.codes7.sites, 'soft.other',
                                                                                                    ifelse(hist %in% cancer.codes$soft.other.contingent.codes8 & site %in% cancer.codes$soft.other.contingent.codes8.sites, 'soft.other',
                                                                                                           ifelse(hist %in% cancer.codes$intra.gct.codes & site %in% cancer.codes$intra.gct.codes.sites, 'gct.intra',
                                                                                                                  ifelse(hist %in% cancer.codes$extra.gct.codes & site %in% cancer.codes$extra.gct.codes.sites, 'gct.extra',
                                                                                                                         ifelse(hist %in% cancer.codes$gonad.gct.codes & site %in% cancer.codes$gonad.gct.codes.sites, 'gct.gonad',
                                                                                                                                ifelse(hist %in% cancer.codes$other.unspec.non.contingent.codes, 'other.any',
                                                                                                                                       ifelse(hist %in% cancer.codes$other.unspec.contingent.codes1 & site %in% cancer.codes$other.unspec.contingent.codes1.sites, 'other.any',
                                                                                                                                              ifelse(hist %in% cancer.codes$other.unspec.contingent.codes2 & site %in% cancer.codes$other.unspec.contingent.codes2.sites, 'other.any',
                                                                                                                                                     ifelse(hist %in% cancer.codes$other.unspec.contingent.codes3 & site %in% cancer.codes$other.unspec.contingent.codes3.sites, 'other.any',
                                                                                                                                                            ifelse(hist %in% cancer.codes$other.unspec.contingent.codes4 & site %in% cancer.codes$other.unspec.contingent.codes4.sites, 'other.any',
                                                                                                                                                                   ifelse(hist %in% cancer.codes$epithe.non.contingent.codes, 'epithe',
                                                                                                                                                                          ifelse(hist %in% cancer.codes$epithe.contingent.codes1 & site %in% cancer.codes$epithe.contingent.codes1.sites, 'epithe',
                                                                                                                                                                                 ifelse(hist %in% cancer.codes$epithe.contingent.codes2 & site %in% cancer.codes$epithe.contingent.codes2.sites, 'epithe',
                                                                                                                                                                                        ifelse(hist %in% cancer.codes$epithe.contingent.codes3 & site %in% cancer.codes$epithe.contingent.codes3.sites, 'epithe',
                                                                                                                                                                                               ifelse(hist %in% cancer.codes$epithe.contingent.codes4 & site %in% cancer.codes$epithe.contingent.codes4.sites, 'epithe',
                                                                                                                                                                                                      ifelse(hist %in% cancer.codes$epithe.contingent.codes5 & site %in% cancer.codes$epithe.contingent.codes5.sites, 'epithe',
                                                                                                                                                                                                             ifelse(hist %in% cancer.codes$epithe.contingent.codes6 & site %in% cancer.codes$epithe.contingent.codes6.sites, 'epithe',
                                                                                                                                                                                                                    ifelse(is.na(hist), NA, newvar)))))))))))))))))))))))))))))))
}



# Load in and save raw data -----------------------------------------------

wa.raw <- read.dta13(file = './scheurer_leukemia_data_20170828.dta', convert.underscore = TRUE,
                     convert.factors = TRUE)

save(wa.raw, file = './washington.raw.data.rdata')



# Standardize variables ---------------------------------------------------

load("washington.raw.data.rdata")
load('Z:/Jeremy/CLIC pooling project/Datasets/iccc.codes.to.dx.mappings.v20171018.rdata')

wa <- wa.raw
rm(wa.raw)

wa <- rename(wa, 
             unique.id = bm.seq,
             case.control = case,
             bf.ever = breastfd,
             birth.year = birthyr,
             maternal.age = momage, 
             paternal.age = dadage,
             mat.edu = momedu,
             pat.edu = dadedu,
             birth.order = parity,
             birth.wt.gr = birgrams)

#' Standardize existing variables.
wa$unique.id <- paste0('wa',wa$unique.id)
wa$bf.ever <- as.numeric(ifelse(wa$bf.ever == "Y", 1,
                                ifelse(wa$bf.ever == "" | wa$bf.ever == 'U', NA, 0)))
wa$sex <- as.numeric(ifelse(wa$sex == 'Male', 1, 2))
wa$sex <- ifelse(is.na(wa$sex), 0, wa$sex)
wa$birth.order <- ifelse(wa$birth.order %in% 98:99, NA, wa$birth.order)
wa$case.control <- ifelse(wa$case.control == 'Case', 1, 0)

#' Generate missing standard variables.
wa$dx <- compute.cancer1(wa$hist4, wa$site)
wa$dx <- compute.cancer2(wa$hist4, wa$site, wa$dx)
wa$dx <- ifelse(is.na(wa$dx), 0, wa$dx)
wa$dx <- as.numeric(ifelse(wa$dx == 'all', 1, 
                           ifelse(wa$dx == 'aml', 2, 
                                  ifelse(wa$dx == 'leu.other', 3, 0))))
wa$histology <- as.character(NA)
wa$immphen <- as.character(NA)
wa$cyto <- as.character(NA)
wa$bf.ex.ever <- as.numeric(NA)
wa$bf.dur <- as.numeric(NA)
wa$bf.ex.dur <- as.numeric(NA)
wa$bf.currently <- as.numeric(NA)
wa$ff.ever <- as.numeric(NA)
wa$ff.dur <- as.numeric(NA)
wa$ff.ex.dur <- as.numeric(NA)
wa$ff.type <- as.character(NA)
wa$age.solids.mos <- as.numeric(NA)
wa$bf.when.solids <- as.numeric(NA)
wa$bf.ex.when.solids <- as.numeric(NA)

wa <- wa[!is.na(wa$bf.ever), ]

save(wa, file = 'wa.v20171120.1.rdata')



# Infant feeding summary --------------------------------------------------

load('wa.v20171120.1.rdata')

tmp <- filter(wa, case.control == 0)
print.if.summary(tmp)















