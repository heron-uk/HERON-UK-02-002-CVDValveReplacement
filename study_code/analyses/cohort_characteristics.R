omopgenerics::logMessage(message = "Get cohort attrition")

results[["attrition"]] <- CohortCharacteristics::summariseCohortAttrition(cdm$study_cohorts) 

omopgenerics::logMessage(message = "Get cohort characteristics")

results[["characterisation"]] <- CohortCharacteristics::summariseCharacteristics(cdm$study_cohorts,
                                                                                 ageGroup = age_groups, 
                                                                                 otherVariables = "race") 

