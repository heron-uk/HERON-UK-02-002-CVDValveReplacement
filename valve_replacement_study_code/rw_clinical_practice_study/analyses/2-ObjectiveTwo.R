omopgenerics::logMessage(message = "STARTING OBJECTIVE 2")

omopgenerics::logMessage(message = "Get denominator cohort")
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator",
  cohortDateRange = study_period,
  daysPriorObservation = 365,
  ageGroup = c(list(c(0, 150)), age_groups_extended),
  requirementInteractions = FALSE,
  sex = c("Both", "Male", "Female")) 

omopgenerics::logMessage(message = "Estimate incidence")
results[["incidence"]] <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "procedures_nr",
  interval = c("years"),
  repeatedEvents = FALSE,
  completeDatabaseIntervals = TRUE
) 

omopgenerics::logMessage(message = "OBJECTIVE 2 FINISHED")