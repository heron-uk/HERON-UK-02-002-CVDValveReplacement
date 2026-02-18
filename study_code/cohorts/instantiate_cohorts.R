

omopgenerics::logMessage(message = "Importing codelists")

codelist <- omopgenerics::importCodelist(here::here("codelist"), "csv")


omopgenerics::logMessage(message = "Instastiating cohorts")

cdm$study_cohorts_inc <- CohortConstructor::conceptCohort(cdm = cdm, 
                                           conceptSet = codelist, 
                                           name = "study_cohorts_inc", 
                                           exit = "event_start_date"
                                           ) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd(cohortId = c("aortic_stenosis", "aortic_valve_disease")) 
  
cdm$study_cohort <- cdm$study_cohorts_inc |>
  CohortConstructor::requireAge(ageRange = list(c(20, Inf)), name = "study_cohort") |>
  CohortConstructor::requireInDateRange(dateRange = study_period) |>
  dplyr::left_join(cdm$person |> 
                     dplyr::select("person_id", "race_concept_id") |>
                     PatientProfiles::addConceptName(nameStyle = "race"),
                   by = c("subject_id" = "person_id")) |>
  dplyr::select(-"race_concept_id") |>
  dplyr::compute() |>
  PatientProfiles::addConceptIntersectField(conceptSet = list(townsend = 715996L), 
                                            field = "value_as_number", 
                                            window = list(c(-Inf, 0)), 
                                            order = "last", 
                                            nameStyle = "latest_townsend", 
                                            inObservation = FALSE,
                                            name = "study_cohorts") |>
  dplyr::mutate(latest_townsend = as.character(.data$latest_townsend), 
                latest_townsend = coalesce(.data$latest_townsend, "Missing")) |>
  dplyr::compute(name = "study_cohorts") |>
  PatientProfiles::addDemographics(sex = sex, 
                                   age = FALSE, 
                                   ageGroup = age_groups, 
                                   priorObservation = FALSE, 
                                   futureObservation = FALSE,
                                   name = "study_cohorts")
  
  