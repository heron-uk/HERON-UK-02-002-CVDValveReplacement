omopgenerics::logMessage(message = "STARTING OBJECTIVE 2")

omopgenerics::logMessage(message = "Anchor to AS diagnosis during the previous year")
cdm[["procedures_nr"]] <- cdm[["procedures_nr"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_indication", 
                         window = c(-365, 0),
                         intersections = c(1,Inf),
                         name = "procedures_nr")

omopgenerics::logMessage(message = "Get denominator cohort")
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator",
  cohortDateRange = study_period,
  daysPriorObservation = 365,
  ageGroup = c(list(c(0, 150)), age_groups_extended),
  requirementInteractions = TRUE,
  sex = c("Both", "Male", "Female")) 

omopgenerics::logMessage(message = "Estimate incidence")
results[["incidence"]] <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "procedures_nr",
  interval = c("years", "overall"),
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE
)

omopgenerics::logMessage(message = "OBJECTIVE 2 FINISHED")
