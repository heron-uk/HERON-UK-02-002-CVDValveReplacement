logMessage(message = "STARTING OBJECTIVE 2")

logMessage(message = "Anchor to AS diagnosis during the previous year")
cdm[["procedures_nr"]] <- cdm[["procedures_nr"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_indication",
                         window = c(-365, 0),
                         intersections = c(1,Inf),
                         name = "procedures_nr")

logMessage(message = "Get denominator cohort")
cdm[["aortic_stenosis_denominator"]] <- cdm[["aortic_stenosis_denominator"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis_isolated", 
                         window = c(-Inf, Inf), 
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "avd_with_endocarditis_isolated", 
                         window = c(-Inf, Inf), 
                         intersections = 0) 

logMessage(message = "Get denominator cohort")
cdm <- generateTargetDenominatorCohortSet(cdm = cdm, 
                                   name = "denominator", 
                                   targetCohortTable = "aortic_stenosis_denominator",
                                   cohortDateRange = study_period,
                                   daysPriorObservation = 365,
                                   ageGroup = c(list(c(0, 150)), age_groups_extended),
                                   requirementInteractions = TRUE,
                                   requirementsAtEntry = TRUE,
                                   sex = c("Both", "Male", "Female"))

logMessage(message = "Estimate incidence")
results[["incidence"]] <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE)

logMessage(message = "Get denominator cohort")
cdm <- generateTargetDenominatorCohortSet(cdm = cdm, 
                                          name = "denominator", 
                                          targetCohortTable = "aortic_stenosis_denominator",
                                          cohortDateRange = study_period,
                                          daysPriorObservation = 365,
                                          ageGroup = c(list(c(0, 150))),
                                          requirementInteractions = TRUE,
                                          requirementsAtEntry = TRUE,
                                          sex = c("Both", "Male", "Female"))

logMessage(message = "Stratify by risk scores")
logMessage(message = "> Stratify by year")
cdm[["denominator"]] <- cdm[["denominator"]] |>
  yearCohorts(years = 2012:2025) 

logMessage(message = "> Add charlson")
cdm[["denominator"]] <- cdm[["denominator"]] |>
 addUpdatedCharlsonIndex(conceptSet = charlson_comorbidity_index_codelist,
                         ageAdjusted = FALSE,
                         nameStyle = "cci",
                         categories = list("low_risk" = c(0,2),
                                           "medium_risk" = c(3,4),
                                           "high_risk" = c(5, Inf)))

logMessage(message = "> Add efi")
cdm[["denominator"]] <- cdm[["denominator"]] |>
  addElectronicFrailtyIndex(conceptSet = electronic_frailty_index_codelist,
                          nameStyle = "efi",
                          categories = list("fit" = c(0, 0.12), 
                                            "mild" = c(0.12, 0.24), 
                                            "moderate" = c(0.24, 0.36),
                                            "severe" = c(0.36, 1)))

logMessage(message = "> Estimate incidence")
results[["incidence_per_groups"]] <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE, 
  strata = list("cci_categories", "efi_categories"),
  completeDatabaseIntervals = TRUE)

logMessage(message = "OBJECTIVE 2 FINISHED")
