omopgenerics::logMessage(message = "Get denominator cohort")

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator",
  ageGroup = c(list(
    c(20, 150)), 
    age_groups),
  sex = c("Both", "Male", "Female"),
  daysPriorObservation = 365) 

omopgenerics::logMessage(message = "Add ethnicity and socioeconomic status")

cdm$denominator <- cdm$denominator |>
  dplyr::left_join(cdm$person |> 
                     dplyr::select("person_id", "race_concept_id") |>
                     PatientProfiles::addConceptName(nameStyle = "race"), 
                   by = c("subject_id" = "person_id")) |>
  dplyr::select(-"race_concept_id") |>
  dplyr::compute() |>
  PatientProfiles::addConceptIntersectField(conceptSet = list(townsend = 715996L), 
                                            field = "value_as_number", 
                                            window = list(c(-Inf, 0)), 
                                            order = "last", 
                                            nameStyle = "latest_townsend", 
                                            name = "denominator") |>
  dplyr::mutate(latest_townsend = as.character(.data$latest_townsend), 
                latest_townsend = coalesce(.data$latest_townsend, "Missing")) |>
  dplyr::compute(name = "denominator") 
  

omopgenerics::logMessage(message = "Estimate incidence")

results[["incidence"]] <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "study_cohorts",
  outcomeCohortId = c("aortic_stenosis", "aortic_valve_disease"),
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE, 
  strata = list("race", "latest_townsend")
)
