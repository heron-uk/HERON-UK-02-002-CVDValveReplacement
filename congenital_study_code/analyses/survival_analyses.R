# Surgical interventions survival ----
omopgenerics::logMessage("Running surgical interventions survival analysis")

# AVR intervention survival (up to 2 years)
omopgenerics::logMessage("Analyzing AVR intervention survival")
results[["survival_avr_intervention"]] <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  targetCohortId = c(1, 2),  # congenital AS and AVD
  outcomeCohortTable = "intervention_cohorts",
  outcomeCohortId = 1,  # AVR intervention
  outcomeWashout = Inf,  # first event only
  followUpDays = 730  # 2 years
)

omopgenerics::logMessage("Surgical interventions survival analysis complete")

# Analysis 4: Mortality survival ----
omopgenerics::logMessage("Running mortality survival analysis")

# Create death cohort restricted to study cohorts
omopgenerics::logMessage("Creating death cohort")
cdm <- CohortConstructor::deathCohort(cdm, name = "death_cohort", subsetCohort = "study_cohorts")

# Estimate mortality survival
results[["survival_mortality"]] <- CohortSurvival::estimateSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  targetCohortId = c(1, 2),  # congenital AS and AVD
  outcomeCohortTable = "death_cohort",
  outcomeWashout = Inf,  # first death only
  followUpDays = 730  # 2 years
)

omopgenerics::logMessage("Mortality survival analysis complete")
