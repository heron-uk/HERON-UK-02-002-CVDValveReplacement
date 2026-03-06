omopgenerics::logMessage("Running surgical interventions survival analysis")

omopgenerics::logMessage("Analyzing AVR intervention survival")
results[["survival_avr_intervention"]] <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  targetCohortId = c(1, 2),
  outcomeCohortTable = "intervention_cohorts",
  outcomeCohortId = 1, 
  outcomeWashout = Inf,  
  followUpDays = 730  
)

omopgenerics::logMessage("Surgical interventions survival analysis complete")

omopgenerics::logMessage("Running mortality survival analysis")

omopgenerics::logMessage("Creating death cohort")
cdm$death_cohort <- CohortConstructor::deathCohort(cdm, name = "death_cohort")

# Estimate mortality survival
results[["survival_mortality"]] <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  targetCohortId = c(1, 2),  
  outcomeCohortTable = "death_cohort",
  outcomeWashout = Inf, 
  followUpDays = 730  
)

omopgenerics::logMessage("Mortality survival analysis complete")
