source(here::here("analyses", "functions.R"))

omopgenerics::logMessage(message = "Importing codelists")

codelist <- omopgenerics::importCodelist(here::here("codelist"), "csv")

# For testing uncomment the below to get counts
# codelist <- codelist |> CodelistGenerator::addConcepts(cdm,
#                                            concepts = c(316139L),
#                                            codelistName = "aortic_valve_replacement")

omopgenerics::logMessage(message = "Instastiating cohorts")
omopgenerics::logMessage(message = " -- AS and AVR concept cohorts")
cdm$study_cohorts_inc <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = c(
    codelist["aortic_stenosis"],
    codelist["aortic_valve_replacement"]
  ),
  name = "study_cohorts_inc",
  exit = "event_start_date"
)

omopgenerics::logMessage(message = " -- Study criteria")
cdm$study_cohorts <- cdm$study_cohorts_inc |>
  CohortConstructor::requireIsFirstEntry(name = "study_cohorts") |>
  CohortConstructor::exitAtObservationEnd(
    cohortId = c("aortic_stenosis"),
    name = "study_cohorts"
  )
cdm$study_cohorts <- cdm$study_cohorts |>
  # we allow AS and AVR to occur on the same day
  CohortConstructor::requireCohortIntersect(
    cohortId = "aortic_valve_replacement",
    targetCohortTable = "study_cohorts_inc",
    targetCohortId = "aortic_stenosis",
    intersections = c(1, Inf),
    window = c(-Inf, -1),
    name = "study_cohorts"
  ) |>
  CohortConstructor::requireCohortIntersect(
    cohortId = "aortic_stenosis",
    targetCohortTable = "study_cohorts_inc",
    targetCohortId = "aortic_valve_replacement",
    intersections = 0L,
    window = c(-Inf, -1),
    name = "study_cohorts"
  ) |>
  CohortConstructor::requireAge(
    ageRange = list(c(20, Inf)),
    name = "study_cohorts"
  ) |>
  CohortConstructor::requireInDateRange(
    dateRange = study_period,
    name = "study_cohorts"
  ) |>
  addEthnicity() |>
  addSES() |>
  PatientProfiles::addDemographics(
    sex = sex,
    age = FALSE,
    ageGroup = age_groups,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "study_cohorts"
  )

omopgenerics::logMessage(message = "Comorbidity cohorts")
cdm$comorbidities <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = codelist[stringr::str_detect(names(codelist),
                                "aortic_stenosis|aortic_valve_replacement",
                                negate = TRUE)],
  name = "comorbidities",
  exit = "event_start_date"
) |> 
  CohortConstructor::exitAtObservationEnd()

omopgenerics::logMessage(message = "Get denominator cohort")

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = study_period,
  ageGroup = c(
    list(
      c(20, 150)
    ),
    age_groups
  ),
  sex = c("Both", "Male", "Female"),
  daysPriorObservation = 365
)

omopgenerics::logMessage(message = "Add socioeconomic status")

cdm$denominator <- cdm$denominator |>
  addSES() |>
  dplyr::compute(name = "denominator")
