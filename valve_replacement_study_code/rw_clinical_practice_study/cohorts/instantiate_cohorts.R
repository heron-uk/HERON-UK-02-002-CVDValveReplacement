omopgenerics::logMessage(message = "INSTANTIATING COHORTS")

# Aortic valve replacement ----
omopgenerics::logMessage(message = "Importing procedure codelists")
codelist <- importCodelist(here::here("cohorts", "study_codelists"), type = "csv")

cdm[["avr_no_restrictions"]] <- conceptCohort(cdm = cdm, 
                                              conceptSet = c(codelist["aortic_valve_replacement"]), 
                                              name = "avr_no_restrictions", 
                                              exit = "event_start_date")

cdm <- createProceduresCohorts(avrCohortName = "avr_no_restrictions", 
                               taviCohortName = "tavi_no_restrictions", 
                               saviCohortName = "savr_no_restrictions",
                               proceduresCohortName = "procedures_no_restrictions",
                               restrictions = FALSE) 

omopgenerics::logMessage(message = "Creating SAVI/TAVI - Objective 1")
cdm[["avr"]] <- cdm[["avr_no_restrictions"]] |>
  copyCohorts(name = "avr")

cdm[["avr"]] <- cdm[["avr"]] |>
  requireIsFirstEntry() |>
  requireInDateRange(dateRange = study_period)

cdm <- createProceduresCohorts(avrCohortName = "avr",
                               taviCohortName = "tavi", 
                               saviCohortName = "savr",
                               proceduresCohortName = "procedures_objective_one",
                               restrictions = TRUE) 

# Creating indications cohorts ----
omopgenerics::logMessage(message = "Instantiating aortic stenosis (no restrictions)")
cdm[["aortic_stenosis_no_restrictions"]] <- conceptCohort(cdm = cdm, 
                                                          conceptSet = c(codelist["aortic_stenosis_avr"]), 
                                                          name = "aortic_stenosis_no_restrictions", 
                                                          exit = "event_start_date") 

cdm[["aortic_stenosis"]] <- cdm[["aortic_stenosis_no_restrictions"]] |>
  copyCohorts(name = "aortic_stenosis")

omopgenerics::logMessage(message = "Instantiating aortic stenosis") 
cdm[["aortic_stenosis"]] <- cdm[["aortic_stenosis"]] |>
  requireIsFirstEntry() |>
  exitAtObservationEnd() |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365) 

omopgenerics::logMessage(message = "Instantiating aortic insufficiency") 
cdm[["aortic_insufficiency"]] <- conceptCohort(cdm = cdm, 
                                               conceptSet = c(codelist["aortic_insufficiency_avr"]), 
                                               name = "aortic_insufficiency", 
                                               exit = "event_start_date") |>
  requireIsFirstEntry() |>
  exitAtObservationEnd() |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365) 

omopgenerics::logMessage(message = "Instantiating aortic endocarditis")
cdm[["aortic_endocarditis"]] <- conceptCohort(cdm = cdm, 
                                              conceptSet = c(codelist["aortic_endocarditis_avr"]), 
                                              name = "aortic_endocarditis", 
                                              exit = "event_start_date") |>
  requireIsFirstEntry() |>
  exitAtObservationEnd() |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365) 

cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]], name = "indications")

# Anchor the indications to an AVR procedure ---
ids <- getCohortId(cdm[["procedures_objective_one"]], cohortName = "aortic_valve_replacement")
cdm[["indications"]] <- cdm[["indications"]] |>
  requireCohortIntersect(targetCohortTable = "procedures_objective_one", 
                         cohortId = ids,
                         window = c(0, Inf), 
                         intersections = c(1, Inf)) 

omopgenerics::logMessage(message = "FINISH INSTANTIATING COHORTS")



