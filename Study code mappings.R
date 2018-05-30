#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------
#' This data frame maps the study column in the CLIC pooled dataset to a unique 
#' study-country combination.
#' 
#' It also provides relevant details of the study design: where it was conducted, its
#' name, how matching was conducted, how cases and controls were ascertained, start and
#' end of recruitment, age ranges of cases and controls, type of interview and numbers
#' of ALLs, AMLs and controls.
#'---------------------------------------------------------------------------------------
#'---------------------------------------------------------------------------------------

studies <- data.frame(study.number = c(1,2,3,4),
                      
                      country = c('Canada','Egypt','USA','UK'),
                      
                      name.or.region = c('Quebec',
                                         'Risk factors for TEL/AML1 fusion gene and childhood ALL in Egypt',
                                         'WA State',
                                         'UKCCS'),
                      
                      matching.scheme = c('pair (1:1)','frequency','pair (10:1)', 'pair (2:1)'),
                      
                      matching.vars = c('sex, age at date of diagnosis',
                                        'age category, sex and area of residence', 
                                        'year of birth',
                                        'DOB, sex, region'),
                      
                      number.all = c(790, 299, 123,1462),
                      
                      number.aml = c(0, 0, 48,263),
                      
                      number.controls = c(790, 351, 3727,7615),
                      
                      source.cases = c('Hospitals in Quebec',
                                       "Children's Cancer Hospital, Egypt",
                                       'Birth certificates',
                                       'Hospital'),
                      
                      source.controls = c('population-based', 'population-based', 'population-based','population-based'),
                      
                      recruit.start = c(1980,2009,1974,1990),
                      
                      recruit.end = c(2000, 2012,2014,1997),
                      
                      age.yrs.lower = c(0.08, 0.33, 0, 0),
                      
                      age.yrs.upper = c(14.53, 14.50, 19, 14.90),
                      
                      interview.type = c('phone interview', 'in-person interview', 'no interview', 'in-person interview'))

save(studies,
     file = 'Z:/Jeremy/CLIC pooling project/Datasets/Expanded Datasets/studies.v20170914.2.rdata')
