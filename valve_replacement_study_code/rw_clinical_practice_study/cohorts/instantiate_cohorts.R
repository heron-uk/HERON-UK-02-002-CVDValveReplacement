omopgenerics::logMessage(message = "INSTANTIATING COHORTS")

omopgenerics::logMessage(message = "Import codelists")
codelist <- importCodelist(here::here("cohorts", "study_codelists"), type = "csv")

# Create  no restrictions codelists (objective 2) ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement (no restrictions)")
cdm[["aortic_valve_replacement_nr"]] <- conceptCohort(cdm = cdm, 
                                              conceptSet = c(codelist["aortic_valve_replacement"]), 
                                              name = "aortic_valve_replacement_nr", 
                                              exit = "event_start_date")

omopgenerics::logMessage(message = "Instantiate tavi & savi (no restrictions)")
cdm <- createProceduresCohorts(avrCohortName = "aortic_valve_replacement_nr", 
                               taviCohortName = "tavi_nr", 
                               saviCohortName = "savr_nr",
                               proceduresCohortName = "procedures_nr",
                               restrictions = FALSE) 

cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement_nr"]] |>
  copyCohorts(name = "aortic_valve_replacement")

omopgenerics::logMessage(message = "Anchor AVR to an aortic stenosis diagnosis")
cdm[["aortic_valve_replacement_nr"]] <- cdm[["aortic_valve_replacement_nr"]] |>
  requireConceptIntersect(conceptSet = codelist["aortic_stenosis_avr"], 
                          window = c(-365, 0), 
                          intersections = c(1,Inf))

# Create procedures objective one cohorts ----
omopgenerics::logMessage(message = "Add requirements to avr cohort")
cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement"]] |>
  requireIsFirstEntry() |>
  requireInDateRange(dateRange = study_period)

omopgenerics::logMessage(message = "Add requirements to savr and tavi cohorts")
cdm <- createProceduresCohorts(avrCohortName = "aortic_valve_replacement",
                               taviCohortName = "tavi", 
                               saviCohortName = "savr",
                               proceduresCohortName = "procedures",
                               restrictions = TRUE)

# Creating indications cohorts ----
omopgenerics::logMessage(message = "Instantiating aortic stenosis")
cdm[["aortic_stenosis"]] <- conceptCohort(cdm = cdm, 
                                          conceptSet = c(codelist["aortic_stenosis_avr"]), 
                                          name = "aortic_stenosis", 
                                          exit = "event_start_date") |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf)) |>
  requireIsLastEntry()

omopgenerics::logMessage(message = "Instantiating aortic insufficiency") 
cdm[["aortic_insufficiency"]] <- conceptCohort(cdm = cdm, 
                                               conceptSet = c(codelist["aortic_insufficiency_avr"]), 
                                               name = "aortic_insufficiency", 
                                               exit = "event_start_date") |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365)  |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf)) |>
  requireIsLastEntry()

omopgenerics::logMessage(message = "Instantiating aortic endocarditis")
cdm[["aortic_endocarditis"]] <- conceptCohort(cdm = cdm, 
                                              conceptSet = c(codelist["aortic_endocarditis_avr"]), 
                                              name = "aortic_endocarditis", 
                                              exit = "event_start_date") |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf)) |>
  requireIsLastEntry()

omopgenerics::logMessage(message = "FINISH INSTANTIATING COHORTS")



