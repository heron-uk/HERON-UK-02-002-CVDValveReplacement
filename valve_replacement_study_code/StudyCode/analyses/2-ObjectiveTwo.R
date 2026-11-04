logMessage(message = "STARTING OBJECTIVE 2 - INCIDENCE AMONG PEOPLE WITH AS or AS + INSUFFICIENCY")

logMessage(message = "> Removing pple with aortic endocarditis")
cdm[["aortic_stenosis_denominator"]]  <- cdm[["aortic_stenosis_denominator"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis", 
                         window = c(-Inf, Inf),
                         intersections = 0) |>
  exitAtObservationEnd()

logMessage(message = "Get denominator cohort")
cdm <- generateTargetDenominatorCohortSet(cdm = cdm, 
                                          name = "denominator_as", 
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
  denominatorTable = "denominator_as",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE)

logMessage(message = "> Create CCI denominator")
cdm[["denominator_as_cci"]] <- updatedCharlsonCohort(
  cdm = cdm,
  categories = list("low_risk" = c(0,2),
                    "medium_risk" = c(3,4),
                    "high_risk" = c(5, Inf)),
  name = "denominator_as_cci",
  ageAdjusted = FALSE,
  conceptSet = charlson_comorbidity_index_codelist,
  subsetCohort = "aortic_stenosis_denominator"
) 

logMessage(message = "> Add sex")
cdm[["denominator_as_cci"]]  <- cdm[["denominator_as_cci"]] |>
  addSex()

logMessage(message = "> Estimate incidence for CCI")
results[["incidence_cci"]] <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_as_cci",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE,
  strata = list("sex"),
  completeDatabaseIntervals = TRUE)

logMessage(message = "Create EFI denominator")
cdm[["denominator_as_efi"]] <- electronicFrailtyIndexCohort(
  cdm = cdm,
  categories = list("fit" = c(0, 0.12),
                    "mild" = c(0.12, 0.24),
                    "moderate" = c(0.24, 0.36),
                    "severe" = c(0.36, 1)),
  name = "denominator_as_efi",
  conceptSet = electronic_frailty_index_codelist,
  subsetCohort = "aortic_stenosis_denominator"
)

logMessage(message = "> Add sex")
cdm[["denominator_as_efi"]]  <- cdm[["denominator_as_efi"]] |>
  addSex()

logMessage(message = "> Estimate incidence for EFI")
results[["incidence_efi"]] <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_as_efi",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE,
  strata = list("sex"),
  completeDatabaseIntervals = TRUE)

# Stratify by year, CCI and EFI ----
# logMessage(message = "Get denominator cohort")
# cdm <- generateTargetDenominatorCohortSet(cdm = cdm, 
#                                           name = "denominator_as", 
#                                           targetCohortTable = "aortic_stenosis_denominator",
#                                           cohortDateRange = study_period,
#                                           daysPriorObservation = 365,
#                                           requirementsAtEntry = TRUE,
#                                           sex = c("Both", "Male", "Female"))
# 
# logMessage(message = "Stratify by risk scores")
# attr(cdm[["denominator_as"]], "cohort_set") <- cdm[["denominator_as"]] |>
#   settings() |>
#   select(-c("target_cohort_definition_id"))
# 
# logMessage(message = "> Stratify by year")
# cdm[["denominator_as"]] <- cdm[["denominator_as"]] |>
#   yearCohorts(years = 2012:2025)
# 
# logMessage(message = "> Add charlson")
# cdm[["denominator_as"]] <- cdm[["denominator_as"]] |>
#   addUpdatedCharlsonIndex(conceptSet = charlson_comorbidity_index_codelist,
#                           ageAdjusted = FALSE,
#                           nameStyle = "cci",
#                           categories = list("low_risk" = c(0,2),
#                                             "medium_risk" = c(3,4),
#                                             "high_risk" = c(5, Inf)))
# 
# logMessage(message = "> Add efi")
# cdm[["denominator_as"]] <- cdm[["denominator_as"]] |>
#   addElectronicFrailtyIndex(conceptSet = electronic_frailty_index_codelist,
#                             nameStyle = "efi",
#                             categories = list("fit" = c(0, 0.12), 
#                                               "mild" = c(0.12, 0.24), 
#                                               "moderate" = c(0.24, 0.36),
#                                               "severe" = c(0.36, 1)))
# 
# logMessage(message = "> Estimate incidence")
# results[["incidence_per_groups"]] <- estimateIncidence(
#   cdm = cdm,
#   denominatorTable = "denominator_as",
#   outcomeTable = "procedures_nr",
#   interval = c("years", "overall"),
#   repeatedEvents = FALSE, 
#   strata = list("cci_categories", "efi_categories"),
#   completeDatabaseIntervals = TRUE)
# 
# ----

logMessage(message = "STARTING OBJECTIVE 3 - INCIDENCE AMONG THE GENERAL POPULATION")
logMessage(message = "> Anchor to AS diagnosis during the previous year")
cdm[["procedures_nr"]] <- cdm[["procedures_nr"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_indication",
                         window = c(-365, 0),
                         intersections = c(1,Inf),
                         name = "procedures_nr")

logMessage(message = "Get denominator cohort")
cdm <- generateDenominatorCohortSet(cdm = cdm,
                                    name = "denominator",
                                    cohortDateRange = study_period,
                                    daysPriorObservation = 365,
                                    ageGroup = c(list(c(0, 150)), age_groups_extended),
                                    requirementInteractions = TRUE,
                                    sex = c("Both", "Male", "Female"))

logMessage(message = "Estimate incidence")
results[["incidence_general_pop"]] <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE)

logMessage(message = "OBJECTIVE 2 FINISHED")
