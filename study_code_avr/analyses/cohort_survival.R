# 
# omopgenerics::logMessage(message = "Create death cohort")
# 
# cdm$death_cohort <- CohortConstructor::deathCohort(cdm = cdm,
#                                                    subsetCohort = "study_cohorts", 
#                                                    #subsetCohortId = c("aortic_stenosis", "aortic_valve_disease"), 
#                                                    name = "death_cohort")

# omopgenerics::logMessage(message = "Competing risk analyisis")
# 
# results[["survival_as"]] <- CohortSurvival::estimateCompetingRiskSurvival(
#   cdm = cdm,
#   targetCohortTable = "study_cohorts",
#   outcomeCohortTable = "study_cohorts",
#   competingOutcomeCohortTable = "death_cohort",
#   targetCohortId = c("aortic_stenosis"),
#   outcomeCohortId = c("aortic_valve_replacement_after_as"),
#   outcomeWashout = Inf,
#   competingOutcomeWashout = Inf,
#   censorOnCohortExit = TRUE,
#   censorOnDate = NULL,
#   followUpDays = Inf,
#   strata = list("sex", "age_group", "ethnicity","ethnicity_group", "ses"),
#   eventGap = 30,
#   estimateGap = 1,
#   restrictedMeanFollowUp = NULL,
#   minimumSurvivalDays = 1
# )
# 
# results[["survival_avd"]] <- CohortSurvival::estimateCompetingRiskSurvival(
#   cdm = cdm,
#   targetCohortTable = "study_cohorts",
#   outcomeCohortTable = "study_cohorts",
#   competingOutcomeCohortTable = "death_cohort",
#   targetCohortId = c("aortic_valve_disease"),
#   outcomeCohortId = c("aortic_valve_replacement_after_avd"),
#   outcomeWashout = Inf,
#   competingOutcomeWashout = Inf,
#   censorOnCohortExit = TRUE,
#   censorOnDate = NULL,
#   followUpDays = Inf,
#   strata = list("sex", "age_group", "ethnicity", "ethnicity_group",  "ses"),
#   eventGap = 30,
#   estimateGap = 1,
#   restrictedMeanFollowUp = NULL,
#   minimumSurvivalDays = 1
# )



