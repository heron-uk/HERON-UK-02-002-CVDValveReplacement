source(here::here("analyses", "functions.R"))

omopgenerics::logMessage(message = "Importing codelists")

codelist <- omopgenerics::importCodelist(here::here("codelist"), "csv")


omopgenerics::logMessage(message = "Instastiating cohorts")

cdm$study_cohorts_inc <- CohortConstructor::conceptCohort(cdm = cdm, 
                                           conceptSet = c(codelist["aortic_stenosis"], codelist["aortic_valve_replacement"]), 
                                           name = "study_cohorts_inc", 
                                           exit = "event_start_date"
                                           ) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireConceptIntersect(conceptSet = codelist["aortic_valve_replacement"], 
                                             cohortId = "aortic_stenosis", 
                                             intersections = 0L, 
                                             window = c(-Inf, 0)) |>
  CohortConstructor::exitAtObservationEnd(cohortId = c("aortic_stenosis")) |>
  CohortConstructor::requireConceptIntersect(conceptSet = codelist["aortic_stenosis"], 
                                             cohortId = "aortic_valve_replacement", 
                                             intersections = c(1, Inf), 
                                             window = c(-Inf, 0)) 
  

cdm$study_cohorts <- cdm$study_cohorts_inc |>
  CohortConstructor::requireAge(ageRange = list(c(20, Inf)), name = "study_cohorts") |>
  CohortConstructor::requireInDateRange(dateRange = study_period) |>
  addEthnicity() |>
  addSES() |>
  PatientProfiles::addDemographics(sex = sex, 
                                   age = FALSE, 
                                   ageGroup = age_groups, 
                                   priorObservation = FALSE, 
                                   futureObservation = FALSE,
                                   name = "study_cohorts") 





