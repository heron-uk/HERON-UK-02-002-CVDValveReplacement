omopgenerics::logMessage(message = "Incidence prevalence")

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator",
  ageGroup = c(list(
    c(20, 150)), 
    age_groups),
  sex = c("Both", "Male", "Female"),
  daysPriorObservation = 365) 
cdm$denominator <- cdm$denominator |>
  dplyr::left_join(cdm$person |> 
                     dplyr::select("person_id", "race_concept_id") |>
                     PatientProfiles::addConceptName(nameStyle = "race"), 
                   by = c("subject_id" = "person_id")) |>
  dplyr::select(-"race_concept_id") |>
  dplyr::compute(name = "denominator")

results[["incidence"]] <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "study_cohorts",
  outcomeCohortId = c("aortic_stenosis", "aortic_valve_disease"),
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE, 
  strata = list("race")
)
