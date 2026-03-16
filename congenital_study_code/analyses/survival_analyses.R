omopgenerics::logMessage("Running surgical interventions survival analysis")

cdm$study_cohorts <- cdm$study_cohorts |> 
  addDemographics(ageGroup = list(
    c(0, 4),
    c(5, 9),
    c(10, 14),
    c(15, 17)
  ))

omopgenerics::logMessage("Analysing AVR intervention survival")
results[["survival_avr_intervention"]] <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  outcomeCohortTable = "intervention_cohorts",
  outcomeWashout = Inf,  
  followUpDays = followUpDays, 
  strata = list(c("age_group"), c("sex"))
)

omopgenerics::logMessage("Surgical interventions survival analysis complete")

omopgenerics::logMessage("Running mortality survival analysis")

omopgenerics::logMessage("Creating death cohort")
cdm$death_cohort <- CohortConstructor::deathCohort(cdm, name = "death_cohort")

# Estimate mortality survival
results[["survival_mortality"]] <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  outcomeCohortTable = "death_cohort",
  outcomeWashout = Inf, 
  followUpDays = followUpDays, 
  strata = list(c("age_group"), c("sex"))
)

omopgenerics::logMessage("Mortality survival analysis complete")
