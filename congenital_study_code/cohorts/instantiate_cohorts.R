index_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "index_codelists"), "csv")
procedure_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "procedure_codelists"), "csv")
comorbidity_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")


# Create study cohorts - congenital AS and AVD (ages 0-17)
# Create both in a single cohort table
cdm$study_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "study_cohorts",
  conceptSet = index_codes,
  exit = "event_start_date"
) |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireAge(ageRange = c(0, 17)) |> 
  CohortConstructor::requireInDateRange(
    dateRange = study_period
  )

cdm$study_cohorts <- cdm$study_cohorts |> 
  addDemographics(ageGroup = study_age_groups)

# Intervention cohorts for survival analysis 
# AVR intervention (all ages, first ever, exit same day)
cdm$intervention_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "intervention_cohorts",
  conceptSet = procedure_codes,
  exit = "event_start_date"
)

# Death cohort
omopgenerics::logMessage("Creating death cohort")
cdm$death_cohort <- CohortConstructor::deathCohort(cdm, name = "death_cohort")
cdm$death_cohort <- cdm$death_cohort |> renameCohort("death")

cdm <- bind(cdm$intervention_cohorts, 
            cdm$death_cohort, name = "outcome_cohorts")

# Comorbidity cohorts 
cdm$comorbidity_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "comorbidity_cohorts",
  conceptSet = comorbidity_codes,
  exit = "event_start_date"
) |>
  CohortConstructor::requireIsFirstEntry()



