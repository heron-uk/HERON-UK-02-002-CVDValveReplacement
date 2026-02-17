omopgenerics::logMessage(message = "Get cohort attrition")

results[["attrition"]] <- CohortCharacteristics::summariseCohortAttrition(cdm$study_cohorts) 

omopgenerics::logMessage(message = "Get cohort characteristics")

results[["characterisation"]] <- CohortCharacteristics::summariseCharacteristics(cdm$study_cohorts,
                                                                                 ageGroup = age_groups, 
                                                                                 otherVariables = c("race", "latest_townsend"))

omopgenerics::logMessage(message = "Get cohort overlap") 

results[["overlap"]] <- CohortCharacteristics::summariseCohortOverlap(cdm$study_cohorts)