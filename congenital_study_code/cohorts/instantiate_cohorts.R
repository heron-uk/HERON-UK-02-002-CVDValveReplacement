codes <- importCodelist(here::here("cohorts", "codelists"), "csv")

cdm$congenital_aortic_stenosis <- conceptCohort(
  cdm = cdm,
  name = "congenital_aortic_stenosis",
  conceptSet = list(
    congenital_aortic_stenosis = codes$aortic_stenosis
  ),
  exit = "event_start_date"
) |>
  requireIsFirstEntry() |>
  exitAtObservationEnd() |>
  requireAge(ageRange = c(0, 17), name = "congenital_aortic_stenosis")

cdm$congenital_aortic_valve_disease <- conceptCohort(
  cdm = cdm,
  name = "congenital_aortic_valve_disease",
  conceptSet = list(
    congenital_aortic_valve_disease = codes$aortic_valve_disease
  ),
  exit = "event_start_date"
) |>
  requireIsFirstEntry() |>
  exitAtObservationEnd() |>
  requireAge(ageRange = c(0, 17), name = "congenital_aortic_valve_disease")

# Bind the final two cohorts matching the specification
cdm$study_cohorts <- bind(
  cdm$congenital_aortic_stenosis,
  cdm$congenital_aortic_valve_disease,
  name = "study_cohorts"
)

# Intervention cohorts for survival analysis 
# AVR intervention (all ages, no restriction)
cdm$intervention_cohorts <- conceptCohort(
  cdm = cdm,
  name = "intervention_cohorts",
  conceptSet = list(
    avr = codes$aortic_valve_replacement
  ),
  exit = "event_start_date"
) |>
  exitAtObservationEnd()

# Death cohort for survival analysis 
cdm$death_cohort <- generateDeathCohortSet(
  cdm = cdm,
  name = "death_cohort"
)