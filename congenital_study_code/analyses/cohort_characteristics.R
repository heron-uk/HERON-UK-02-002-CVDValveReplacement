# Baseline characteristics of congenital AS/AVD ----
omopgenerics::logMessage("Running baseline characteristics analysis")

results[["baseline_characteristics"]] <- CohortCharacteristics::summariseCharacteristics(
  cdm$study_cohorts,
  cohortId = c(1, 2),  # congenital AS and AVD only (not AVR procedure)
  ageGroup = list(
    c(0, 4),
    c(5, 9),
    c(10, 14),
    c(15, 17)
  ),
  tableIntersectCount = list(
    "Comorbidities" = list(
      tableName = "condition_occurrence",
      window = c(-Inf, -1)
    )
  )
)

omopgenerics::logMessage("Baseline characteristics analysis complete")

# Large scale characteristics of congenital AS/AVD ----
omopgenerics::logMessage("Running large scale characteristics analysis")

results[["large_scale_characteristics"]] <- CohortCharacteristics::summariseLargeScaleCharacteristics(
  cdm$study_cohorts,
  cohortId = c(1, 2),  # congenital AS and AVD only
  window = list(
    "any_time_prior" = c(-Inf, -1),
    "index_date" = c(0, 0),
    "one_year_after" = c(1, 365),
    "any_time_after" = c(1, Inf)
  ),
  eventInWindow = c(
    "condition_occurrence",
    "observation", 
    "procedure_occurrence",
    "device_exposure"
  ),
  episodeInWindow = "drug_exposure"
)

omopgenerics::logMessage("Large scale characteristics analysis complete")
