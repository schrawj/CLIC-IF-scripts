#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'
#'                                        TEXAS
#' 
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' 2017.09.27.
#' 
#' For purposes of estimating how many kids we had to contribute to the CLIC pooling 
#' project I asked Lauren to pull the infant feeding variables from the TOPNOC QX for
#' any children who had completed it.
#' 
#' Prep environment.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
require(xlsx)

#' Takes argument X: a data frame, and converts its column names to lower case while replacing underscores with periods.
restring.columns <- function(x){
  stringr::str_replace_all(tolower(colnames(x)),'_','.')
}

tx <- read.xlsx(file = 'Z:/Jeremy/CLIC pooling project/Datasets/Raw data/TX/Jeremy TOPNOC (69-71).xlsx',
                sheetIndex = 1, header = TRUE, stringsAsFactors = FALSE)

colnames(tx) <- restring.columns(tx)

save(tx, file = 'Z:/Jeremy/CLIC pooling project/Datasets/Raw data/TX/texas.raw.infant.feeding.data.v20170927.rdata')



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' Queries in advance of the CLIC meeting.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------

#' 39 cases and 12 controls have explicit data on age at intro to formula.
#' This doesn't include kids whose parents indicated they were never breastfed on the old
#' battery of questions (these might be assumed to have age at intro of 0).
tmp <- subset(tx, !(is.na(tx$daily.form)))
pattern <- '^9'
cases <- grepl(pattern, tmp$studyid)
table(cases)
