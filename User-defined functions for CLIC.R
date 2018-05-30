#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' This is a central list of all the user-defined functions I wrote during
#' data cleaning for CLIC, as well as a place to hold a TO-DO list for unresolved 
#' data cleaning concerns.
#' 
#' TODO: COG: Is the 3 level COG education variable already in the IARC standard format?
#' 
#' TODO: SETIL: What is the precise diagnosis of the 11 AnLL cases who do not have AML
#' FAB types?  Are these biphenotypic leukemias?  Corrado says there are no MDS cases in
#' the data.
#' TODO: SETIL: What are the meanings of the lat.a, lat.m and lat.s variables? Suspect 
#' they are exclusive formula feeding, mixed feeding and exclusive breastfeeding,
#' respectively.
#' TODO: SETIL: What are the cut offs for the gestational age variable?   
#' TODO: Recalculate duration of bf.dur.  
#'       Should it be the greater of lat.s.fi and lat.m.fi?
#' TODO: clarify which AnLL cases are actually AMLs and compute the standard DX variable.
#' 
#' TODO: Canada: What are the units of time on the breastfeeding duration variable?
#' 
#' TODO: Washington: Better to use LMP or clinical for gestational age?
#' TODO: Washington: Compute standard race-ethnicity variables.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------


 
# List of functions -------------------------------------------------------
restring.columns <- function(dataframe){
  stringr::str_replace_all(tolower(colnames(dataframe)),'_','.')
}

#' Generate summary statistics for infant feeding practices.
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