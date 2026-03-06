codes <- CodelistGenerator::importCodelist(here::here("cohorts", "codelists"), "csv")

# Create study cohorts - congenital AS and AVD (ages 0-17)
# Create both in a single cohort table
cdm$study_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "study_cohorts",
  conceptSet = list(
    congenital_aortic_stenosis = codes$aortic_stenosis,
    congenital_aortic_valve_disease = codes$aortic_valve_disease
  ),
  exit = "event_start_date"
) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireAge(ageRange = c(0, 17))

# Intervention cohorts for survival analysis 
# AVR intervention (all ages, first ever, exit same day)
cdm$intervention_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "intervention_cohorts",
  conceptSet = list(
    avr = codes$aortic_valve_replacement
  ),
  exit = "event_start_date"  
) |>
  CohortConstructor::requireIsFirstEntry() 