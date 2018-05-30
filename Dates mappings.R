#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' 2017.06.15.
#' 
#' For studies that have provided (or for which we have calculated) age at DX, we do not 
#' necessarily need a bunch of dates lying around in the dataset cluttering things up and
#' generally being difficult to convert and join.
#' 
#' Build a data frame, "dates" to hold each subject's unique id, date of birth and date
#' of diagnosis.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
require(dplyr)
setwd('Z:/Jeremy/CLIC pooling project/Datasets/')

#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'                                       CANADA
#'---------------------------------------------------------------------------------------
load("./canada.v20170609.1.rdata")

canada$calculated.age.dx <- (canada$diag.date - canada$birth.date)/365
print(canada[,c(1,7,31,8,10)])

#' Check for any cases where ages might differ meaningfully.
canada$difference.ages <- abs(canada$agediag - canada$calculated.age.dx)
#' 0 obs.  Canadian age data look good.
canada <- filter(canada, difference.ages > 0.19)

load("Y:/Jeremy Schraw/CLIC pooling project/Datasets/canada.v20170609.1.rdata")

dates.canada <- rename(
  
                  select(canada, unique.id, birth.date, diag.date),
                 
                 
                  date.birth = birth.date, 
                  date.dx = diag.date)

dates.canada$year.birth <- as.numeric(substr(dates.canada$date.birth, 1, 4))

#' Trim the dates variables from Canada and save.
canada <- select(canada, -birth.date, -diag.date)

save(canada, file = './canada.v20170615.1.rdata')

#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'                                       EGYPT
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
load("./egypt.v20170609.1.rdata")

#' A more complicated scenario...birth day, month and year are each in separate columns.
#' Also many missing values.
#' Format YYYY-MM-DD is desirable.

#' What to do in a perfect world...does not handle NAs.
#' Paste0 does not add white space between each element.  Useful in this scenario. 
compute.date.birth <- function(x, y, z){
 
  paste0(as.character(x),'-',as.character(y),'-',as.character(z))

                                       }

egypt$date.birth <- ifelse(is.na(egypt$dob.year), NA, 
                           ifelse(is.na(egypt$dob.month), NA,
                                  ifelse(is.na(egypt$dob.day),NA,
                                         compute.date.birth(egypt$dob.year, egypt$dob.month, egypt$dob.day))))

egypt$date.birth <- as.Date(egypt$date.birth)

#' Date of DX is only defined for cases, and in a separate column that date of interview, which is only defined for controls.
#' However, they're both in date format.  So I've got that going for me, which is nice.

#' One control is lowercase.  Clean that up, we'll need this variable in the next step.
egypt$case.control <- ifelse(egypt$case.control == 'control', 'Control', egypt$case.control)

#' Break cases and controls in two data frames each with three identical columns.
egypt.dates.cases <- select(egypt, unique.id, case.control, date.dx.case)
egypt.dates.cases <- filter(egypt.dates.cases, case.control == 'Case')
egypt.dates.cases <- rename(egypt.dates.cases, dx.date = date.dx.case)

#' Break cases and controls in two data frames each with three identical columns.
egypt.dates.controls <- select(egypt, unique.id, case.control, year.interview.control)
egypt.dates.controls <- filter(egypt.dates.controls, case.control == 'Control')
egypt.dates.controls <- rename(egypt.dates.controls, dx.date = year.interview.control)

#' rbind them together and you've got a perfectly good dx.date column in date format.
egypt.dates <- rbind(egypt.dates.cases, egypt.dates.controls)

egypt.dates <- left_join(egypt.dates, 
                         
                         select(egypt, unique.id, date.birth),
                         
                         by = 'unique.id')

#' Re-order everything to be consistent with Canada.
dates.egypt <- egypt.dates[,c(1,3,4)]
dates.egypt <- rename(dates.egypt, date.dx = dx.date)
dates.egypt$year.birth <- as.numeric(egypt$dob.year) 
dates.egypt <- dates.egypt[,c(1,3,2,4)]

dates <- rbind(dates.canada, dates.egypt)

save(dates, file = './dates.v20170615.rdata')

#' Check validity of calculated ages.
egypt <- left_join(egypt, 
                   
                   select(dates.egypt, unique.id, date.dx),
                   
                   by = 'unique.id')

#' Some of these actually do differ.
egypt$calculated.age <- as.numeric((egypt$date.dx - egypt$date.birth)/365)
print(egypt[1:100,c(1,22,50)])

egypt$age.difference <- abs(egypt$calculated.age - egypt$age.years)
different <- filter(egypt, egypt$age.difference > 0.2)

#' In this case, calculated age appears to be more accurate than the provided data.
print(different[1:50,c(1,22,53,52,23:25,51)])

#' Remove extraneous dates and ages variables from egypt.
egypt <- select(egypt, -dob.day, -dob.month, -dob.year, -date.dx, -age.years, -date.dx.case, -year.interview.control,
                -date.birth, -date.dx, -age.difference)
egypt <- rename(egypt, age.yrs.calculated = calculated.age)
save(egypt, file = './egypt.v20170615.1.rdata')



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'                                  WASHINGTON STATE
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
load("Z:/Jeremy/CLIC pooling project/Datasets/washington.v20170914.1.rdata")

tmp <- select(wa, unique.id, birthyr)
tmp$year.birth <- as.numeric(tmp$birthyr)
tmp$date.birth <- as.Date(NA)
tmp$date.dx <- as.Date(NA)
tmp <- tmp[, c(1,4,5,3)]

dates <- rbind(dates, tmp)

save(dates, file = './Expanded datasets/dates.v20170914.1.rdata')



#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#'                                  UKCCS
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
load("Z:/Jeremy/CLIC pooling project/Datasets/ukccs.v20170914.1.rdata")

tmp <- rename(select(ukccs, unique.id, yob), year.birth= yob)
tmp$year.birth <- as.numeric(tmp$year.birth)
tmp$date.birth <- as.Date(NA)
tmp$date.dx <- as.Date(NA)
tmp <- tmp[, c(1,3,4,2)]

dates <- rbind(dates, tmp)

save(dates, file = './Expanded datasets/dates.v20170914.2.rdata')
