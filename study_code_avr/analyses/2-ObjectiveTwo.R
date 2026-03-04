omopgenerics::logMessage(message = "STARTING OBJECTIVE 2")
omopgenerics::logMessage(message = "Get denominator cohort")

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator",
  cohortDateRange = study_period,
  ageGroup = c(list(c(0, 150)), age_groups),
  sex = c("Both", "Male", "Female")) 

omopgenerics::logMessage(message = "Estimate incidence")

results[["incidence"]] <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "proc",
  interval = c("years"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE
) 

omopgenerics::logMessage(message = "OBJECTIVE 2 FINISHED")