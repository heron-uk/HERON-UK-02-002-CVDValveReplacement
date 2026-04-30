omopgenerics::logMessage(message = "INSTANTIATING COHORTS")

omopgenerics::logMessage(message = "Import codelists")
codelist <- importCodelist(here::here("cohorts", "study_codelists"), type = "csv")

# Create restrictions code lists ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement")
cdm[["aortic_valve_replacement"]] <- conceptCohort(cdm = cdm, 
                                                   conceptSet = c(codelist["aortic_valve_replacement"]), 
                                                   name = "aortic_valve_replacement", 
                                                   exit = "event_start_date") 
cdm <- bind(cdm[["aortic_valve_replacement"]], name = "procedures_nr")

# Procedures with restrictions ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement - add inclusion criteria")
cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement"]] |>
  requireIsFirstEntry() |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireInDateRange(dateRange = study_period) 

# Create tavi additional ----
omopgenerics::logMessage(message = "Instantiating TAVI (additional) cohort")
cdm[["tavi_additional"]] <- cdm[["aortic_valve_replacement"]] |>
  requireConceptIntersect(conceptSet = codelist["aortic_valve_replacement_potential_tavi"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional") |>
  renameCohort(newCohortName = "tavi_additional") |>
  requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional")

omopgenerics::logMessage(message = "Instantiating TAVI (direct) cohort")
cdm[["tavi_direct"]] <- cdm[["aortic_valve_replacement"]] |>
  requireConceptIntersect(conceptSet = codelist["tavi"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_direct") |>
  renameCohort(newCohortName = "tavi_direct") 

omopgenerics::logMessage(message = "Instantiate TAVI cohorts")
cdm <- bind(cdm[["tavi_additional"]], cdm[["tavi_direct"]], name = "tavi")
cdm[["tavi"]] <- cdm[["tavi"]] |>
  unionCohorts(cohortId = c("tavi_additional", "tavi_direct"), 
               cohortName = "tavi",
               keepOriginalCohorts = FALSE) |>
  requireIsFirstEntry()

omopgenerics::logMessage(message = "Instantiate SAVR cohort")
cdm[["savr"]] <- cdm[["aortic_valve_replacement"]] |>
  requireCohortIntersect(targetCohortTable = "tavi",
                         window = c(0,0), 
                         intersections = 0, 
                         name = "savr") |>
  renameCohort(newCohortName = "savr")

cdm <- bind(cdm[["aortic_valve_replacement"]], cdm[["tavi"]], cdm[["savr"]], name = "procedures")

omopgenerics::logMessage(message = "Get cohort attritions")
results[["attrition_tavi_additional"]] <- summariseCohortAttrition(cdm[["tavi_additional"]])
results[["attrition_tavi_direct"]] <- summariseCohortAttrition(cdm[["tavi_direct"]])

# Create no restrictions code lists (Objective 2)----
# Procedures with restrictions ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement - add inclusion criteria")
# Create tavi additional ----
omopgenerics::logMessage(message = "Instantiating TAVI (additional) cohort")
cdm[["tavi_additional"]] <- cdm[["procedures_nr"]] |>
  requireConceptIntersect(conceptSet = codelist["aortic_valve_replacement_potential_tavi"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional") |>
  renameCohort(newCohortName = "tavi_additional") |>
  requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional")

omopgenerics::logMessage(message = "Instantiating TAVI (direct) cohort")
cdm[["tavi_direct"]] <- conceptCohort(cdm = cdm,
                                      name = "tavi_direct",
                                      conceptSet = codelist["tavi"],
                                      exit = "event_start_date") |>
  renameCohort(newCohortName = "tavi_direct")

omopgenerics::logMessage(message = "Instantiate TAVI cohorts")
cdm <- bind(cdm[["tavi_additional"]], cdm[["tavi_direct"]], name = "tavi")
cdm[["tavi"]] <- cdm[["tavi"]] |>
  unionCohorts(cohortId = c("tavi_additional", "tavi_direct"), 
               cohortName = "tavi",
               keepOriginalCohorts = FALSE)

omopgenerics::logMessage(message = "Instantiate SAVR cohort")
cdm[["savr"]] <- cdm[["procedures_nr"]] |>
  requireCohortIntersect(targetCohortTable = "tavi",
                         window = c(0,0), 
                         intersections = 0, 
                         name = "savr") |>
  renameCohort(newCohortName = "savr")

cdm <- bind(cdm[["procedures_nr"]], cdm[["tavi"]], cdm[["savr"]], name = "procedures_nr")

omopgenerics::logMessage(message = "Anchor to AS diagnosis during the previous year")
cdm[["procedures_nr"]] <- cdm[["procedures_nr"]] |>
  requireConceptIntersect(conceptSet = codelist["aortic_stenosis_avr"], 
                          window = c(-365, 0), 
                          intersections = c(1,Inf), 
                          name = "procedures_nr")

# Create Procedure Cohorts (Objective 1) ----
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



