omopgenerics::logMessage("Running baseline characteristics analysis of congenital AS/AVD ")

results[["baseline_characteristics"]] <- CohortCharacteristics::summariseCharacteristics(
  cdm$study_cohorts,
  ageGroup = list(
    c(0, 4),
    c(5, 9),
    c(10, 14),
    c(15, 17)
  )
)

omopgenerics::logMessage("Baseline characteristics analysis complete")

omopgenerics::logMessage("Running large scale characteristics analysis of congenital AS/AVD ")
nameFollowUp <- glue::glue("day_post_to_{followUpDays}_days_after")
results[["large_scale_characteristics"]] <- CohortCharacteristics::summariseLargeScaleCharacteristics(
  cdm$study_cohorts,
  includeSource = TRUE,
  excludedCodes = NULL,
  window = rlang::list2(
    "any_time_prior_to_day_prior" = c(-Inf, -1),
    "on_index_date" = c(0, 0),
    !!nameFollowUp := c(1, followUpDays)
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
