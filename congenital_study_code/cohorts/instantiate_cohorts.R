index_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "index_codelists"), "csv")
procedure_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "procedure_codelists"), "csv")
comorbidity_codes <- CodelistGenerator::importCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")

# Comorbidity cohorts -----
cdm$comorbidity_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "comorbidity_cohorts",
  conceptSet = comorbidity_codes,
  exit = "event_start_date"
) |>
  CohortConstructor::requireIsFirstEntry()

# Outcome cohorts ----
# Intervention cohorts
cdm$intervention_cohorts <- CohortConstructor::conceptCohort(
  cdm = cdm,
  name = "intervention_cohorts",
  conceptSet = procedure_codes,
  exit = "event_start_date", 
  useSourceFields = TRUE
)

# Death cohort
omopgenerics::logMessage("Creating death cohort")
cdm$death_cohort <- CohortConstructor::deathCohort(cdm, name = "death_cohort")

cdm <- bind(cdm$intervention_cohorts, 
            cdm$death_cohort, name = "outcome_cohorts")

# Index cohorts -----
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

# no outcome on index date or before
cdm$study_cohorts <- cdm$study_cohorts |> 
  requireCohortIntersect("outcome_cohorts", 
                         window = c(-Inf, 0), 
                         intersections = c(0, 0), 
                         cohortCombinationCriteria = "all")

# no exlusion
cdm$study_cohorts_narrow <- cdm$study_cohorts |> 
  requireCohortIntersect("comorbidity_cohorts", 
                         targetCohortId = c("atrial_septal_defect",
                                            "congenital_anomaly_of_coronary_artery",
                                            "hypoplastic_left_heart_syndrome",
                                            "pulmonary_artery_disorder",
                                            "tricuspid_valve_disorder",
                                            "ventricular_septal_defect"),
                         window = c(-Inf, 0), 
                         intersections = c(0, 0), 
                         cohortCombinationCriteria = "all", 
                         name = "study_cohorts_narrow") |> 
  renameCohort("as_no_complex_chd")

# split isolated versus multi level left heart obstruction 
cdm$multi_level_as <- cdm$study_cohorts_narrow |> 
  requireCohortIntersect(
    "comorbidity_cohorts",
    window = c(-Inf, 0),
    intersections = c(1, Inf), 
    targetCohortId = c("subaortic_stenosis", "supravalvar_aortic_stenosis",
                       "mitral_valve_disorder", "coarctation_of_aorta"), 
    cohortCombinationCriteria = "any", 
    name = "multi_level_as") |> 
  renameCohort("multi_level_as")

cdm$isolated_as <- cdm$study_cohorts_narrow |> 
  requireCohortIntersect(
    "multi_level_as",
    window = c(-Inf, Inf),
    intersections = c(0, 0),  
    name = "isolated_as") |> 
  renameCohort("isolated_as")

cdm <- bind(cdm$study_cohorts,
            cdm$study_cohorts_narrow,
            cdm$multi_level_as,
            cdm$isolated_as, 
            name = "study_cohorts")

# add demographics
cdm$study_cohorts <- cdm$study_cohorts |> 
  addDemographics(ageGroup = study_age_groups)


# Outcome cohorts - characterisation ----
# those outcomes that will be outcomes in the study population (for the broadest cohort)
cdm$outcome_cohorts_characterisation <- cdm$outcome_cohorts |> 
  requireIsFirstEntry(name = "outcome_cohorts_characterisation") |> 
  requireCohortIntersect("study_cohorts", 
                         window = c(-followUpDays, 0), 
                         intersections = c(1, Inf), 
                         name = "outcome_cohorts_characterisation")

cdm$outcome_cohorts_characterisation <- cdm$outcome_cohorts_characterisation |> 
  addDemographics(ageGroup = study_age_groups)

cdm <- bind(cdm$study_cohorts,
            cdm$outcome_cohorts_characterisation,
            name = "characterisation_cohorts")





