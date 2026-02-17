
omopgenerics::logMessage(message = "Create death cohort")

cdm$death_cohort <- CohortConstructor::deathCohort(cdm = cdm,
                                                   subsetCohort = "study_cohorts", 
                                                   #subsetCohortId = c("aortic_stenosis", "aortic_valve_disease"), 
                                                   name = "death_cohort")

omopgenerics::logMessage(message = "Competing risk analyisis")

results[["survival"]] <- CohortSurvival::estimateCompetingRiskSurvival(
  cdm = cdm,
  targetCohortTable = "study_cohorts",
  outcomeCohortTable = "study_cohorts",
  competingOutcomeCohortTable = "death_cohort",
  targetCohortId = c("aortic_stenosis", "aortic_valve_disease"),
  outcomeCohortId = "aortic_valve_replacement",
  outcomeWashout = Inf,
  competingOutcomeWashout = Inf,
  censorOnCohortExit = TRUE,
  censorOnDate = NULL,
  followUpDays = Inf,
  strata = list("sex", "age_group", "race", "latest_townsend"),
  eventGap = 30,
  estimateGap = 1,
  restrictedMeanFollowUp = NULL,
  minimumSurvivalDays = 1
)


survival_data <- results[["survival"]] |>
  CohortSurvival::asSurvivalResult()




