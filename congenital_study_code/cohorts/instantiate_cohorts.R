index_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "index_codelists"), "csv")
procedure_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "procedure_codelists"), "csv")

# Create study cohorts - congenital AS and AVD (ages 0-17)
# Create both in a single cohort table
cdm$study_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "study_cohorts",
  conceptSet = index_codes,
  exit = "event_start_date", 
  useSourceFields = useSourceCodes
) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireAge(ageRange = c(0, 17)) |> 
  CohortConstructor::requireInDateRange(
    dateRange = study_period
  )

# Intervention cohorts for survival analysis 
# AVR intervention (all ages, first ever, exit same day)
cdm$intervention_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "intervention_cohorts",
  conceptSet = procedure_codelists,
  exit = "event_start_date", 
  useSourceFields = useSourceCodes
)
