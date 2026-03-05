codes <- CodelistGenerator::importCodelist(here::here("cohorts", "codelists"), "csv")

# Create study cohorts directly - congenital AS and AVD (ages 0-17)
cdm$congenital_aortic_stenosis <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "congenital_aortic_stenosis",
  conceptSet = list(
    congenital_aortic_stenosis = codes$aortic_stenosis
  ),
  exit = "event_start_date"
) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireAge(ageRange = c(0, 17), name = "congenital_aortic_stenosis")

cdm$congenital_aortic_valve_disease <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "congenital_aortic_valve_disease",
  conceptSet = list(
    congenital_aortic_valve_disease = codes$aortic_valve_disease
  ),
  exit = "event_start_date"
) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireAge(ageRange = c(0, 17), name = "congenital_aortic_valve_disease")

# Bind the two cohorts into study_cohorts
cdm$study_cohorts <- omopgenerics::bind(
  cdm$congenital_aortic_stenosis,
  cdm$congenital_aortic_valve_disease,
  name = "study_cohorts"
)

# Intervention cohorts for survival analysis 
# AVR intervention (all ages, no restriction)
cdm$intervention_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "intervention_cohorts",
  conceptSet = list(
    avr = codes$aortic_valve_replacement
  ),
  exit = "event_start_date"
) |>
  CohortConstructor::exitAtObservationEnd()