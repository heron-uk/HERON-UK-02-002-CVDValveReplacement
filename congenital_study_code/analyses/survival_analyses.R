
omopgenerics::logMessage("Running survival analysis")
results[["survival_avr_intervention"]] <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  outcomeCohortTable = "outcome_cohorts",
  outcomeWashout = Inf,  
  followUpDays = followUpDays, 
  strata = list(c("age_group", "sex"))
)
omopgenerics::logMessage("Survival analysis complete")
