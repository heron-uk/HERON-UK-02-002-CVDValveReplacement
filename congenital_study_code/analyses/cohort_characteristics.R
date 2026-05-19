omopgenerics::logMessage("Running baseline characteristics analysis")
results[["baseline_characteristics"]] <- CohortCharacteristics::summariseCharacteristics(
  cdm$characterisation_cohorts,
  strata = list(c("age_group"),
                c("sex"),
                c("age_group", "sex")),
  ageGroup = study_age_groups, 
  cohortIntersectFlag = list(targetCohortTable = "comorbidity_cohorts",
                             window = list(c(-Inf, 0)))
)
omopgenerics::logMessage("Baseline characteristics analysis complete")

omopgenerics::logMessage("Running outcome characteristics analysis")
results[["outcome_overlap"]] <- CohortCharacteristics::summariseCohortOverlap(cdm$characterisation_cohorts)
results[["outcome_timing"]] <- CohortCharacteristics::summariseCohortTiming(cdm$characterisation_cohorts)
results[["outcome_upset"]] <- cdm$study_cohorts |> 
  PatientProfiles::addCohortIntersectFlag("outcome_cohorts", 
                                          nameStyle = "{cohort_name}") |> 
  CohortCharacteristics::summariseCohortCount(
    strata = list(c("aortic_valve_repair",
                    "surgical_valvotomy",
                    "percutaneous_valvotomy",
                    "ross_procedure",
                    "aortic_valve_replacement")))
omopgenerics::logMessage("Outcome characteristics analysis complete")



omopgenerics::logMessage("Running large scale characteristics analysis")
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
    "procedure_occurrence"
  )
)
omopgenerics::logMessage("Large scale characteristics analysis complete")
