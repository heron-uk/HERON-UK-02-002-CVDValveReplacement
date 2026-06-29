# Instantiate HFRS (objective three) ----
omopgenerics::logMessage(message = "Instantiate HFRS - SNOMED")
cdm[["hospital_frailty_risk_score"]] <- conceptCohort(cdm,
                                                      conceptSet = importCodelist(here("cohorts", "study_codelists", 
                                                                                       "hospital_frailty_risk_score", "snomed_codelists"), 
                                                                                  type = "csv"), 
                                                      name = "hospital_frailty_risk_score",
                                                      exit = "event_start_date")

omopgenerics::logMessage(message = "Define chronic")
def <- read_csv(here("cohorts", "study_codelists", "hospital_frailty_risk_score", 
                     "icd_mapping", "hospital_frailty_score.csv")) |>
  inner_join(read_csv(here("cohorts",  "study_codelists", "hospital_frailty_risk_score", 
                           "icd_mapping", "hfrs.csv"))) |>
  filter(chronic) |> 
  select(cohort_name_1, chronic) |>
  distinct() |>
  pull("cohort_name_1")

cdm[["hospital_frailty_risk_score"]] <- cdm[["hospital_frailty_risk_score"]] |>
  exitAtObservationEnd(cohortId = def)

omopgenerics::logMessage(message = "Instantiate HFRS - Based on ICD10")
x <- read_csv(here("cohorts", "study_codelists", "hospital_frailty_risk_score", 
                   "icd_mapping", "hospital_frailty_score.csv"))
x$icd10_code 
















cdm[["hospital_frailty_risk_score"]] <- conceptCohort(cdm,
                                                      conceptSet = importCodelist(here("cohorts", "study_codelists", "hospital_frailty_risk_score"), type = "csv"), 
                                                      name = "hospital_frailty_risk_score",
                                                      exit = "event_start_date")

omopgenerics::logMessage(message = "Define chronic")
def <- read_csv(here("cohorts", "study_codelists", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score.csv")) |>
  inner_join(read_csv(here("cohorts",  "study_codelists", "hospital_frailty_risk_score", "icd_mapping", "hfrs.csv"))) |>
  filter(chronic) |> 
  select(cohort_name_1, chronic) |>
  distinct() |>
  pull("cohort_name_1")

cdm[["hospital_frailty_risk_score"]] <- cdm[["hospital_frailty_risk_score"]] |>
  exitAtObservationEnd(cohortId = def)