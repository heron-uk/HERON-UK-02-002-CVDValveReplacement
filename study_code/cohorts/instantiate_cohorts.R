

omopgenerics::logMessage(message = "Importing codelists")

codelist <- omopgenerics::importCodelist(here::here("codelist"), "csv")


omopgenerics::logMessage(message = "Instastiating cohorts")

cdm$study_cohorts <- CohortConstructor::conceptCohort(cdm = cdm, 
                                           conceptSet = codelist, 
                                           name = "study_cohorts", 
                                           exit = "event_start_date"
                                           ) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd(cohortId = c("aortic_stenosis", "aortic_valve_disease")) |>
  CohortConstructor::requireAge(ageRange = list(c(20, Inf)))

cdm$study_cohorts <- cdm$study_cohorts |> dplyr::left_join(cdm$person |> 
                                        dplyr::select("person_id", "race_concept_id") |>
                                        PatientProfiles::addConceptName(nameStyle = "race"), 
                                      by = c("subject_id" = "person_id")) |>
  dplyr::select(-"race_concept_id") |>
  dplyr::compute(name = "study_cohorts")
  
  