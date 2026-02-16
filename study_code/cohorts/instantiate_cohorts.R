

omopgenerics::logMessage(message = "Importing codelists")

codelist <- omopgenerics::importCodelist(here::here("codelist"), "csv")


omopgenerics::logMessage(message = "Instastiating cohorts for aortic steosis and aortic valve disease")

cdm$conditions <- CohortConstructor::conceptCohort(cdm = cdm, 
                                           conceptSet = list(codelist["aortic_stenosis"], 
                                                            codelist["aortic_valve_disease"]), 
                                           name = "conditions"
                                           ) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireAge(ageRange = list(c(20, Inf)))

omopgenerics::logMessage(message = "Instastiating cohorts for aortic valve replacement")
  
cdm$avr <- CohortConstructor::conceptCohort(cdm = cdm, 
                                            conceptSet = codelist["aortic_valve_replacement"], 
                                            exit = "event_start_date",
                                            name = "avr") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireAge(ageRange = list(c(20, Inf)))