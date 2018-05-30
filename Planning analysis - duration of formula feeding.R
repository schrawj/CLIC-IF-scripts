#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' 2017.09.28.
#' 
#' Met with Austin today, originally to discuss propensity scores as a way of accounting
#' for the (expected) correlations between breastfeeding and formula feeding in the CLIC
#' data.
#' 
#' He suggested something else: compute OR for ALL stratified by breastfeeding status.
#' 
#' This is probably doable given our sample size.  This analysis would use TX, SETIL and
#' UKCCS data.  What are the distributions of breastfeeding durations in those studies? 
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
load("Z:/Jeremy/CLIC pooling project/Datasets/italy.v20170921.1.rdata")
load("Z:/Jeremy/CLIC pooling project/Datasets/ukccs.v20170914.1.rdata")
load("Z:/Jeremy/CLIC pooling project/Datasets/Raw data/TX/texas.raw.infant.feeding.data.v20170927.rdata")

require(dplyr)
require(gmodels)



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' Filter out children who were never formula-fed.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
ukccs <- filter(ukccs, formulafed == 1)
ita <- ita[!is.na(ita$age.in.formula), ]
tx <- filter(tx, mformula == 'Yes')



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' Look at distribution of duration of breastfeeding duration by DX.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
pattern <- '^9'
tx$dx <- ifelse(grepl(pattern, tx$studyid), 'case', 'control')
tx.crstab <- CrossTable(tx$feedmon, tx$dx, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

ukccs$diagnosis <- ifelse(ukccs$diagnosis == "","Control",ukccs$diagnosis)
ukccs$approx.mons.bf <- as.numeric(ukccs$breastfedduration)
ukccs$approx.mons.bf <- floor(ukccs$approx.mons.bf/30)
uk.crstab <- CrossTable(ukccs$approx.mons.bf, ukccs$diagnosis,  prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

ita$dx <- ifelse(ita$caco == 11, 'ALL',
                 ifelse(ita$caco == 12, 'AML', 'Control')) #' This is a rough estimate.  ~11 AnLL are MDS, CML etc.

ita$dur.any.bf <- apply(ita[,c(49,50,66,67)], 1, max, na.rm = TRUE)
ita$dur.any.bf <- ifelse(ita$lat.m ==6 & ita$lat.s == 6, NA, ita$dur.any.bf)
print(ita[1:100,c(1,48:50,65:67,143,153)])

it.crstab <- CrossTable(ita$dur.any.bf, ita$dx, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)

write.csv(it.crstab$t, file = 'C:/Users/schraw/Desktop/italy.bf.durations.csv')
write.csv(uk.crstab$t, file = 'C:/Users/schraw/Desktop/uk.bf.durations.csv')
write.csv(tx.crstab$t, file = 'C:/Users/schraw/Desktop/tx.bf.durations.csv')
