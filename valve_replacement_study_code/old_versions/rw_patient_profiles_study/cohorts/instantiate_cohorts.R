omopgenerics::logMessage(message = "INSTANTIATING COHORTS")

omopgenerics::logMessage(message = "Import codelists")
codelist <- importCodelist(here::here("cohorts", "study_codelists"), type = "csv")

# Create  no restrictions codelists (objective 2) ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement (no restrictions)")
cdm[["aortic_valve_replacement"]] <- conceptCohort(cdm = cdm, 
                                                   conceptSet = c(codelist["aortic_valve_replacement"]), 
                                                   name = "aortic_valve_replacement", 
                                                   exit = "event_start_date") |>
  requireConceptIntersect(conceptSet = codelist["aortic_stenosis_avr"], 
                          window = c(-365, 0), 
                          intersections = c(1,Inf), 
                          name = "aortic_valve_replacement")

omopgenerics::logMessage(message = "Add requirements to avr cohort")
cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement"]] |>
  requireIsFirstEntry() |>
  requireInDateRange(dateRange = study_period)

omopgenerics::logMessage(message = "Add requirements to savr and tavi cohorts")
cdm <- createProceduresCohorts(cdm,
                               avrCohortName = "aortic_valve_replacement",
                               taviCohortName = "tavi", 
                               saviCohortName = "savr",
                               proceduresCohortName = "procedures",
                               restrictions = TRUE)

omopgenerics::logMessage(message = "Instantiate comorbidities")
cdm[["comorbidities"]] <- conceptCohort(cdm,
                                        conceptSet = importCodelist(here("cohorts", "comorbidity_codelists"), type = "csv"), 
                                        name = "comorbidities",
                                        exit = "event_start_date")

omopgenerics::logMessage(message = "Define chronic comorbidities")
cdm[["comorbidities"]] <- cdm[["comorbidities"]] |>
  exitAtObservationEnd(cohortId = c("aortic_calcification", "bicuspid_aortic_valve", "concomitant_valve_disorders_excluding_endocarditis",
                                     "dialysis", "left_bundle_branch_block", "mitral_regurgitation", "peripheral_arterial_disease", 
                                     "pre_existing_pacemaker_or_defibrillator", "right_bundle_branch_block", "unicuspid_aortic_valve",
                                     "atrial_fibrillation", "chronic_liver_disease", "copd", "coronary_artery_disease", "dementia", "disorders_of_lipid_metabolism", 
                                     "hypertension", "hypertrophic_cardiomyopathy", "left_bundle_branch_block", "type_2_diabetes", "heart_failure"))

omopgenerics::logMessage(message = "Instantiate hfrs")
cdm[["hospital_frailty_risk_score"]] <- conceptCohort(cdm,
                                        conceptSet = importCodelist(here("cohorts", "hospital_frailty_risk_score"), type = "csv"), 
                                        name = "hospital_frailty_risk_score",
                                        exit = "event_start_date")

omopgenerics::logMessage(message = "Define chronic")
def <- read_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score.csv")) |>
  inner_join(read_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hfrs.csv"))) |>
  filter(chronic) |> 
  select(cohort_name_1, chronic) |>
  distinct() |>
  pull("cohort_name_1")

cdm[["hospital_frailty_risk_score"]] <- cdm[["hospital_frailty_risk_score"]] |>
  exitAtObservationEnd(cohortId = def)

omopgenerics::logMessage(message = "FINISH INSTANTIATING COHORTS")



