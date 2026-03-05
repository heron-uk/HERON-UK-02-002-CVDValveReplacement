# Surgical interventions survival ----
logMessage("Running surgical interventions survival analysis")

# AVR intervention survival (up to 2 years)
logMessage("Analyzing AVR intervention survival")
results[["survival_avr_intervention"]] <- summariseSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  targetCohortId = c(1, 2),  # congenital AS and AVD
  outcomeCohortTable = "intervention_cohorts",
  outcomeCohortId = 1,  # AVR intervention
  outcomeWashout = Inf,  # first event only
  followUpDays = 730,  # 2 years
  minCellCount = min_cell_count
)

logMessage("Surgical interventions survival analysis complete")

# Analysis 4: Mortality survival ----
logMessage("Running mortality survival analysis")

results[["survival_mortality"]] <- summariseSingleEventSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  targetCohortId = c(1, 2),  # congenital AS and AVD
  outcomeCohortTable = "death_cohort",
  outcomeCohortId = 1,  # death
  outcomeWashout = Inf,  # first death (should only be one anyway)
  followUpDays = 730,  # 2 years
  minCellCount = min_cell_count
)

logMessage("Mortality survival analysis complete")
