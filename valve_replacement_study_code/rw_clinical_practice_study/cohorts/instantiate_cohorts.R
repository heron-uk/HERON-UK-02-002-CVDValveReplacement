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
cdm <- createProceduresCohorts(cdm, 
                               avrCohortName = "aortic_valve_replacement_nr", 
                               taviCohortName = "tavi_nr", 
                               saviCohortName = "savr_nr",
                               proceduresCohortName = "procedures_nr",
                               restrictions = FALSE) 

cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement_nr"]] |>
  copyCohorts(name = "aortic_valve_replacement") |>
  newCohortTable(cohortSetRef = settings(cdm[["aortic_valve_replacement_nr"]]),
                 cohortAttritionRef = attrition(cdm[["aortic_valve_replacement_nr"]]),
                 cohortCodelistRef = attr(cdm[["aortic_valve_replacement_nr"]], "cohort_codelist"))

omopgenerics::logMessage(message = "Anchor AVR to an aortic stenosis diagnosis")
cdm[["aortic_valve_replacement_nr"]] <- cdm[["aortic_valve_replacement_nr"]] |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireConceptIntersect(conceptSet = codelist["aortic_stenosis_avr"], 
                          window = c(-365, 0), 
                          intersections = c(1,Inf), 
                          name = "aortic_valve_replacement_nr")

# Create procedures objective one cohorts ----
omopgenerics::logMessage(message = "Add requirements to avr cohort")
cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement"]] |>
  requireIsFirstEntry() |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireInDateRange(dateRange = study_period)

omopgenerics::logMessage(message = "Add requirements to savr and tavi cohorts")
cdm <- createProceduresCohorts(cdm,
                               avrCohortName = "aortic_valve_replacement",
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

cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]],
            name = "indications")
omopgenerics::logMessage(message = "FINISH INSTANTIATING COHORTS")



