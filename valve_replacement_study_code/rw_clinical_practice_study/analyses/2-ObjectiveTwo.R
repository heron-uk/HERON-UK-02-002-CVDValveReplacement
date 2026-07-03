omopgenerics::logMessage(message = "STARTING OBJECTIVE 2")

omopgenerics::logMessage(message = "Anchor to AS diagnosis during the previous year")
cdm[["procedures_nr"]] <- cdm[["procedures_nr"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_indication", 
                         window = c(-365, 0),
                         intersections = c(1,Inf),
                         name = "procedures_nr")

omopgenerics::logMessage(message = "Get denominator cohort")
cdm <- generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator",
  cohortDateRange = study_period,
  daysPriorObservation = 365,
  ageGroup = c(list(c(0, 150)), age_groups_extended),
  requirementInteractions = TRUE,
  sex = c("Both", "Male", "Female")) 

omopgenerics::logMessage(message = "Estimate incidence")
results[["incidence"]] <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE)


omopgenerics::logMessage(message = "Estimate incidence - stratified by risk scores")
omopgenerics::logMessage(message = "Add HFRS - snomed")
cdm[["denominator"]] <- cdm[["denominator"]] |>
  addScores(mapping = "snomed", score = "hfrs") |>
  addScoresGrouping(mapping = "snomed", score = "hfrs")

omopgenerics::logMessage(message = "Add HFRS - icd10")
cdm[["denominator"]] <- cdm[["denominator"]] |>
  addScores(mapping = "icd", score = "hfrs") |>
  addScoresGrouping(mapping = "icd", score = "hfrs")

omopgenerics::logMessage(message = "Add CCI - snomed")
cdm[["denominator"]] <- cdm[["denominator"]] |>
  addScores(mapping = "snomed", score = "cci") |>
  addScoresGrouping(mapping = "snomed", score = "cci")

omopgenerics::logMessage(message = "Add CCI - icd10")
cdm[["denominator"]] <- cdm[["denominator"]] |>
  addScores(mapping = "icd", score = "cci") |>
  addScoresGrouping(mapping = "icd", score = "cci")

omopgenerics::logMessage(message = "Estimate incidence")
results[["incidence_per_groups"]] <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE, 
  strata = list("hfrs_snomed_groups", "hfrs_icd_groups", "cci_snomed_groups", "cci_icd_groups"),
  completeDatabaseIntervals = TRUE)

omopgenerics::logMessage(message = "OBJECTIVE 2 FINISHED")
