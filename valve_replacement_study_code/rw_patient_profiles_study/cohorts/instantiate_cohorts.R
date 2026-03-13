omopgenerics::logMessage(message = "INSTANTIATING COHORTS")

omopgenerics::logMessage(message = "Import codelists")
codelist <- importCodelist(here::here("cohorts", "study_codelists"), type = "csv")

# Create  no restrictions codelists (objective 2) ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement (no restrictions)")
cdm[["aortic_valve_replacement"]] <- conceptCohort(cdm = cdm, 
                                                      conceptSet = c(codelist["aortic_valve_replacement"]), 
                                                      name = "aortic_valve_replacement", 
                                                      exit = "event_start_date") |>
  requireConceptIntersect(conceptSet = codelist["aortic_stenosis_avr"], 
                          window = c(-365, 0), 
                          intersections = c(1,Inf), 
                          name = "aortic_valve_replacement")

omopgenerics::logMessage(message = "Add requirements to avr cohort")
cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement"]] |>
  requireIsFirstEntry() |>
  requireInDateRange(dateRange = study_period)

omopgenerics::logMessage(message = "Add requirements to savr and tavi cohorts")
cdm <- createProceduresCohorts(cdm,
                               avrCohortName = "aortic_valve_replacement",
                               taviCohortName = "tavi", 
                               saviCohortName = "savr",
                               proceduresCohortName = "procedures",
                               restrictions = TRUE)


omopgenerics::logMessage(message = "FINISH INSTANTIATING COHORTS")



