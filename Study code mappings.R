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

studies <- data.frame(study.number = c(1,2,3,4,5,6),
                      
                      country = c('Canada',
                                  'Egypt',
                                  'USA',
                                  'UK',
                                  'Italy',
                                  'New Zealand'),
                      
                      name.or.region = c('Quebec',
                                         'Risk factors for TEL/AML1 fusion gene and childhood ALL in Egypt',
                                         'WA State',
                                         'UKCCS',
                                         'SETIL',
                                         'NZCCS'),
                      
                      matching.scheme = c('pair (1:1)',
                                          'frequency',
                                          'pair (10:1)', 
                                          'pair (2:1)',
                                          'pair (2:1)',
                                          'pair (2-3:1'),
                      
                      matching.vars = c('age, sex',
                                        'age category, area of residence, sex', 
                                        'year of birth',
                                        'DOB, region',
                                        'DOB, region, sex',
                                        'age, sex'),
                      
                      number.all = c(790, 
                                     299, 
                                     123,
                                     1462,
                                     601,
                                     98),
                      
                      number.aml = c(0, 
                                     0, 
                                     48,
                                     263,
                                     82,
                                     22),
                      
                      number.controls = c(790, 
                                          351, 
                                          3727,
                                          7615,
                                          1044,
                                          303),
                      
                      source.cases = c('Hospitals in Quebec',
                                       "Children's Cancer Hospital, Egypt",
                                       'Birth certificates',
                                       'Hospital',
                                       'National ped heme-onc registry',
                                       'National cancer registry'),
                      
                      source.controls = c('population-based', 
                                          'population-based', 
                                          'population-based',
                                          'population-based',
                                          'population-based',
                                          'birth records'),
                      
                      recruit.start = c(1980,
                                        2009,
                                        1974,
                                        1990,
                                        1999,
                                        1990),
                      
                      recruit.end = c(2000, 
                                      2012,
                                      2014,
                                      1997,
                                      2002,
                                      1993),
                      
                      age.yrs.lower = c(0.08, 
                                        0.33, 
                                        0, 
                                        0,
                                        0.02,
                                        0),
                      
                      age.yrs.upper = c(14.53, 
                                        14.50, 
                                        19, 
                                        14.90,
                                        10.97,
                                        14),
                      
                      interview.type = c('phone interview', 
                                         'in-person interview', 
                                         'no interview', 
                                         'in-person interview',
                                         'in-person interview',
                                         'in-person interview'))

save(studies,
     file = 'Z:/Jeremy/CLIC pooling project/Datasets/Expanded Datasets/studies.v20180530.1.rdata')
