# Instantiate HFRS (objective three) ----
omopgenerics::logMessage(message = "Define chronic")
x <- read_csv(here("cohorts", "study_codelists", "hospital_frailty_risk_score", "hfrs.csv")) |>
  mutate("cohort_name" = paste0("hfrs_", str_to_lower(cohort_name), "_snomed")) |>
  filter(chronic) |>
  distinct(cohort_name) |>
  pull("cohort_name")

omopgenerics::logMessage(message = "Instantiate HFRS - SNOMED")
cdm[["hfrs_snomed"]] <- conceptCohort(cdm,
                                      conceptSet = importCodelist(here("cohorts", "study_codelists", 
                                                                       "hospital_frailty_risk_score", "snomed_codelists"), 
                                                                  type = "csv"), 
                                      name = "hfrs_snomed",
                                      exit = "event_start_date")


cdm[["hfrs_snomed"]] <- cdm[["hfrs_snomed"]] |>
  exitAtObservationEnd(cohortId = x)

omopgenerics::logMessage(message = "Instantiate HFRS - Based on ICD10")
x <- read_csv(here("cohorts", "study_codelists", "hospital_frailty_risk_score", "hfrs.csv")) |>
  mutate("cohort_name" = paste0("hfrs_", str_to_lower(cohort_name), "_icd")) |>
  filter(chronic) |>
  distinct(cohort_name) |>
  pull("cohort_name")
cdm[["hfrs_icd"]] <- conceptCohort(cdm,
                                   conceptSet = importCodelist(here("cohorts", "study_codelists", 
                                                                    "hospital_frailty_risk_score", "icd10_codelists"), 
                                                               type = "csv"), 
                                   name = "hfrs_icd",
                                   useSourceFields = TRUE, 
                                   exit = "event_start_date")


cdm[["hfrs_icd"]] <- cdm[["hfrs_icd"]] |>
  exitAtObservationEnd(cohortId = x)

# Instantiate CCI (objective three) ----
omopgenerics::logMessage(message = "Instantiate CCI - SNOMED")
cdm[["cci_snomed"]] <- conceptCohort(cdm,
                                      conceptSet = importCodelist(here("cohorts", "study_codelists", 
                                                                       "charlson_comorbidity_index", "snomed_codelists"), 
                                                                  type = "csv"), 
                                      name = "cci_snomed",
                                      exit = "event_start_date")


cdm[["cci_snomed"]] <- cdm[["cci_snomed"]] |>
  exitAtObservationEnd()

omopgenerics::logMessage(message = "Instantiate CCI - Based on ICD10")
cdm[["cci_icd"]] <- conceptCohort(cdm,
                                   conceptSet = importCodelist(here("cohorts", "study_codelists", 
                                                                    "charlson_comorbidity_index", 
                                                                    "icd10_codelists"),
                                                                    type = "csv"), 
                                   name = "cci_icd",
                                   useSourceFields = TRUE, 
                                   exit = "event_start_date")


cdm[["cci_icd"]] <- cdm[["cci_icd"]] |>
  exitAtObservationEnd()

