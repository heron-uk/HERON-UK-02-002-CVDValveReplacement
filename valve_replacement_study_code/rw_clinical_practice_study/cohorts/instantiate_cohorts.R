omopgenerics::logMessage(message = "INSTANTIATING COHORTS")

omopgenerics::logMessage(message = "Import codelists")
codelist <- importCodelist(here::here("cohorts", "study_codelists"), type = "csv")

# Create restrictions code lists ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement")
cdm[["aortic_valve_replacement"]] <- conceptCohort(cdm = cdm, 
                                                   conceptSet = c(codelist["aortic_valve_replacement"]), 
                                                   name = "aortic_valve_replacement", 
                                                   exit = "event_start_date") 
cdm <- bind(cdm[["aortic_valve_replacement"]], name = "procedures_nr")

# Procedures with restrictions ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement - add inclusion criteria")
cdm[["aortic_valve_replacement"]] <- cdm[["aortic_valve_replacement"]] |>
  requireIsFirstEntry() |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireInDateRange(dateRange = study_period) 

# Create tavi additional ----
omopgenerics::logMessage(message = "Instantiating TAVI (additional) cohort")
cdm[["tavi_additional"]] <- cdm[["aortic_valve_replacement"]] |>
  requireConceptIntersect(conceptSet = codelist["aortic_valve_replacement_potential_tavi"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional") |>
  renameCohort(newCohortName = "tavi_additional") |>
  requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional")

omopgenerics::logMessage(message = "Instantiating TAVI (direct) cohort")
cdm[["tavi_direct"]] <- cdm[["aortic_valve_replacement"]] |>
  requireConceptIntersect(conceptSet = codelist["tavi"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_direct") |>
  renameCohort(newCohortName = "tavi_direct") 

omopgenerics::logMessage(message = "Instantiate TAVI cohorts")
cdm <- bind(cdm[["tavi_additional"]], cdm[["tavi_direct"]], name = "tavi")
cdm[["tavi"]] <- cdm[["tavi"]] |>
  unionCohorts(cohortId = c("tavi_additional", "tavi_direct"), 
               cohortName = "tavi",
               keepOriginalCohorts = FALSE) |>
  requireIsFirstEntry()

omopgenerics::logMessage(message = "Instantiate SAVR cohort")
cdm[["savr"]] <- cdm[["aortic_valve_replacement"]] |>
  requireCohortIntersect(targetCohortTable = "tavi",
                         window = c(0,0), 
                         intersections = 0, 
                         name = "savr") |>
  renameCohort(newCohortName = "savr")

cdm <- bind(cdm[["aortic_valve_replacement"]], cdm[["tavi"]], cdm[["savr"]], name = "procedures")

omopgenerics::logMessage(message = "Get cohort attritions")
results[["attrition_tavi_additional"]] <- summariseCohortAttrition(cdm[["tavi_additional"]])
results[["attrition_tavi_direct"]] <- summariseCohortAttrition(cdm[["tavi_direct"]])

# Create no restrictions code lists (Objective 2)----
# Procedures with restrictions ----
omopgenerics::logMessage(message = "Instantiate aortic valve replacement - add inclusion criteria")
# Create tavi additional ----
omopgenerics::logMessage(message = "Instantiating TAVI (additional) cohort")
cdm[["tavi_additional"]] <- cdm[["procedures_nr"]] |>
  requireConceptIntersect(conceptSet = codelist["aortic_valve_replacement_potential_tavi"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional") |>
  renameCohort(newCohortName = "tavi_additional") |>
  requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                          intersections = c(1,Inf), 
                          window = c(0,0), 
                          name = "tavi_additional")

omopgenerics::logMessage(message = "Instantiating TAVI (direct) cohort")
cdm[["tavi_direct"]] <- conceptCohort(cdm = cdm,
                                      name = "tavi_direct",
                                      conceptSet = codelist["tavi"],
                                      exit = "event_start_date") |>
  renameCohort(newCohortName = "tavi_direct")

omopgenerics::logMessage(message = "Instantiate TAVI cohorts")
cdm <- bind(cdm[["tavi_additional"]], cdm[["tavi_direct"]], name = "tavi")
cdm[["tavi"]] <- cdm[["tavi"]] |>
  unionCohorts(cohortId = c("tavi_additional", "tavi_direct"), 
               cohortName = "tavi",
               keepOriginalCohorts = FALSE)

omopgenerics::logMessage(message = "Instantiate SAVR cohort")
cdm[["savr"]] <- cdm[["procedures_nr"]] |>
  requireCohortIntersect(targetCohortTable = "tavi",
                         window = c(0,0), 
                         intersections = 0, 
                         name = "savr") |>
  renameCohort(newCohortName = "savr")

cdm <- bind(cdm[["procedures_nr"]], cdm[["tavi"]], cdm[["savr"]], name = "procedures_nr")

# Create Procedure Cohorts (Objective 1) ----
omopgenerics::logMessage(message = "Instantiating aortic stenosis")
cdm[["aortic_stenosis"]] <- conceptCohort(cdm = cdm, 
                                          conceptSet = c(codelist["aortic_stenosis_avr"]), 
                                          name = "aortic_stenosis", 
                                          exit = "event_start_date") |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf)) |>
  requireIsLastEntry()

omopgenerics::logMessage(message = "Instantiating aortic insufficiency") 
cdm[["aortic_insufficiency"]] <- conceptCohort(cdm = cdm, 
                                               conceptSet = c(codelist["aortic_insufficiency_avr"]), 
                                               name = "aortic_insufficiency", 
                                               exit = "event_start_date") |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365)  |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf)) |>
  requireIsLastEntry()

omopgenerics::logMessage(message = "Instantiating aortic endocarditis")
cdm[["aortic_endocarditis"]] <- conceptCohort(cdm = cdm, 
                                              conceptSet = c(codelist["aortic_endocarditis_avr"]), 
                                              name = "aortic_endocarditis", 
                                              exit = "event_start_date") |>
  requireInDateRange(dateRange = study_period) |>
  requirePriorObservation(minPriorObservation = 365) |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf)) |>
  requireIsLastEntry()

cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]],
            name = "indications")

# Instantiate HFRS (objective three) ----
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


omopgenerics::logMessage(message = "Comorbidities")
cdm[["comorbidities"]] <- conceptCohort(cdm,
                                        conceptSet = importCodelist(here("cohorts", "study_codelists","comorbidity"), 
                                                                    type = "csv"), 
                                        name = "comorbidities",
                                        exit = "event_start_date")


cdm[["comorbidities"]] <- cdm[["comorbidities"]] |> 
  exitAtObservationEnd(cohortId = c("chronic_liver_disease", "copd",  "dementia", "dialysis"))

omopgenerics::logMessage(message = "Aortic valve disease phenotype")
cdm[["aortic_valve_disease_phenotype"]] <- conceptCohort(cdm,
                                        conceptSet = importCodelist(here("cohorts", "study_codelists", "aortic_valve_disease_phenotype"), 
                                                                    type = "csv"), 
                                        name = "aortic_valve_disease_phenotype",
                                        exit = "event_start_date")

omopgenerics::logMessage(message = "- Define chronic aortic_valve_disease_phenotype")
cdm[["aortic_valve_disease_phenotype"]] <- cdm[["aortic_valve_disease_phenotype"]] |> 
  exitAtObservationEnd()

# Instantiate table 1 (objective three) ----
omopgenerics::logMessage(message = "Cardiovascular disease")
cdm[["cardiovascular_disease"]] <- conceptCohort(cdm,
                                                 conceptSet = importCodelist(here("cohorts", "study_codelists", "cardiovascular_disease"), 
                                                                             type = "csv"), 
                                                 name = "cardiovascular_disease",
                                                 exit = "event_start_date")

omopgenerics::logMessage(message = "- Define cardiovascular disease chronic")
cdm[["cardiovascular_disease"]] <- cdm[["cardiovascular_disease"]] |> 
  exitAtObservationEnd(cohortId = c("left_bundle_branch_block", "right_bundle_branch_block", 
                                    "atrial_fibrillation",  "coronary_artery_disease",
                                    "pre_existing_pacemaker_or_defibrillator", 
                                    "peripheral_arterial_disease", "heart_failure",
                                    "hypertrophic_cardiomyopathy", 
                                    "pulmonary_arterial_hypertension"))

omopgenerics::logMessage(message = "Instantiate cardiovascular_risk_factors")
cdm[["cardiovascular_risk_factors"]] <- conceptCohort(cdm,
                                                 conceptSet = importCodelist(here("cohorts", "study_codelists", "cardiovascular_risk_factors"), 
                                                                             type = "csv"), 
                                                 name = "cardiovascular_risk_factors",
                                                 exit = "event_start_date")

omopgenerics::logMessage(message = "- Define cardiovascular_risk_factors chronic")
cdm[["cardiovascular_risk_factors"]] <- cdm[["cardiovascular_risk_factors"]] |> 
  exitAtObservationEnd()

logMessage("Instantiate obesity")
obesity_diag <- list(obesity = c(
  604591, 4271317, 4171972,  4270189, 4079899,  4235799,
  4087487,  40481140, 36713437,  36678790,  45763687,  4097929,  4097996,  4182506,
  4100857,  4160821,  4029277,  4029276,  37166819,  4029900,  36717154,  4005991,
  4163032,  4185912,  4171147,  4177337,  4220527,  4203289,  35622038,  36674490,
  36674893,  4171317,  438731,  37208175,  37164247,  42872398,  4216214,  36716144,
  37110069,  434005,  37395980,  433736,  4212443,  4215969,  4189665,  36716555,
  36717199,  37204685,  37206117,  37397209,
  37162364,  36716151,  37204815,  37311904,  45757112,  4183240,
  4093860,  37163354, 36674827,  3199162,
  45771307,  36676689,  37204691,  37018860,  42539192,  37164244,
  4217557,  37166818,  4211019,  36714072, 36714548,  37165655
))
cdm$obesity <- conceptCohort(
  cdm = cdm, conceptSet = obesity_diag, exit = "event_start_date", name = "obesity"
)

cdm$obesity_bmi <- measurementCohort(
  cdm = cdm, 
  conceptSet = list("bmi_measurement" = c(3038553, 36304833)), 
  valueAsNumber = list("bmi_measurement" = list(c(30, 60))),   
  name = "obesity_bmi"
)
# body weight cohort
cdm$obesity_body_weight <- measurementCohort(
  cdm = cdm, conceptSet = list("body_weight"= c(3025315, 4099154, 3013762,
                                                3023166, 3027492)), 
  valueAsNumber = list("body_weight"= list("9529" = c(120, 200), 
                                           "3195625" = c(265, 440))),
  name = "obesity_body_weight"
)

# bind and union
cdm <- omopgenerics::bind(cdm$obesity, 
                          cdm$obesity_bmi, 
                          cdm$obesity_body_weight, 
                          name = "obesity")
cdm$obesity <- cdm$obesity |>
  unionCohorts(cohortName = "obesity") |> 
  exitAtObservationEnd()

cdm <- bind(cdm$obesity, cdm$cardiovascular_risk_factors, name = "cardiovascular_risk_factors")

logMessage("Instantiate CKD cohort")
## CKD stage from measurements
egfr_codes <- c(
  1619025,  1619026, 3029829,  3029859, 3030104,  3045262,
  3049187,  3053283, 3964988,  3965919, 4213477,  36031320,
  36031846,  36303797, 36304157,  36306178, 3630790,  36660257,
  37393690,  37399046,  40764999,  40769275,
  40771922,  42869913,  46236952,  4338520, 36303653,  37208635,
  37393011,  37393012,  40478895,  40478963, 40483219,  40485075,
  40490315,  44788275,  44790060,  44790183,  44806420,  44808279, 45766361
)
cdm$ckd_stage_meausurement <- measurementCohort(
  cdm = cdm,
  conceptSet = list("egfr" = egfr_codes),
  valueAsNumber = list("ckd_stage_1_meas" = list("8795" = c(90, 9999999),
                                                 "720870" = c(90, 9999999)),
                       "ckd_stage_2_meas" = list("8795" = c(60, 89.99999),
                                                 "720870" = c(60, 89.99999)),
                       "ckd_stage_3_meas" = list("8795" = c(30, 59.99999),
                                                 "720870" = c(30, 59.99999)),
                       "ckd_stage_4_meas" = list("8795" = c(15, 29.99999),
                                                 "720870" = c(15, 29.99999)),
                       "ckd_stage_5_meas" = list("8795" = c(0, 14.99999),
                                                 "720870" = c(0, 14.99999))
  ),
  name = "ckd_stage_meausurement"
) 
## CKD stage from diagnoses
ckd_diag_codes <- list(ckd_stage_1_diag = c(765535, 46284566, 46284567, 46284570, 443614, 46270354,
                                            601161, 44782703, 45773576, 43531559, 44792226, 44792227, 
                                            44784640, 43021853),
                       ckd_stage_2_diag = c(762000,	46284572, 46287169,46284575,
                                            443601,	46270355,	601162,	44782692,	45769901,	43531566,
                                            44792228,	44792229,	45757447,	43021836,	43021854),
                       ckd_stage_3_diag = c(37019193,	762001,	46284587,	46286992,
                                            46284588,	46284591,	46284592,	46284593,	443597,	46273636,
                                            601163,	44782691,	45771075,	43531653,
                                            44792230,	44792231,	45763854,
                                            44792232,	44792249,	45763855,	44792250,
                                            44792251,	45757446,	43021835,	43020456,	762033),
                       ckd_stage_4_diag = c(765536,	46284597,46284598,	46284599,
                                            443612,	46273514,	601164,	44782689,
                                            45769902,	43531577,	44792252,	44792253,
                                            45757445,	44784639,	43020457,	762034),
                       ckd_stage_5_diag = 	c(45768813,	760850,	46284600,	46284602,
                                             46284603,	443611, 46270356,
                                             601165,	44782690,	45769903,	43531562,
                                             37017813,	44792254,	37018761,	44792255,
                                             46273164,	37018886,	601166,	44782717,
                                             45769904,	45769906,	4030520,	4128200,
                                             4125970, 193782, 45772751,
                                             45757393,	45757392,	45757444,	762973,
                                             44784638,	43020437,	43020455,	43021864))

cdm$ckd_stage_diagnosis <- conceptCohort(cdm = cdm,
                                         ckd_diag_codes, 
                                         name = "ckd_stage_diagnosis",
                                         exit = "event_start_date")
## combine
cdm <- bind(cdm$ckd_stage_meausurement, 
            cdm$ckd_stage_diagnosis,  
            name = "ckd_stage")

cdm$ckd_stage <- cdm$ckd_stage |>
  unionCohorts(cohortId = c("ckd_stage_1_meas", "ckd_stage_1_diag"), 
               cohortName = "ckd_stage_1", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_2_meas", "ckd_stage_2_diag"), 
               cohortName = "ckd_stage_2", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_3_meas", "ckd_stage_3_diag"), 
               cohortName = "ckd_stage_3", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_4_meas", "ckd_stage_4_diag"), 
               cohortName = "ckd_stage_4", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_5_meas", "ckd_stage_5_diag"), 
               cohortName = "ckd_stage_5", name = "ckd_stage", 
               keepOriginalCohorts = TRUE) |> 
  subsetCohorts(cohortId = c("ckd_stage_1","ckd_stage_2","ckd_stage_3",
                             "ckd_stage_4","ckd_stage_5"), 
                name = "ckd_stage")

cdm <- bind(cdm$ckd_stage, cdm$comorbidities, name = "comorbidities")

omopgenerics::logMessage(message = "FINISH INSTANTIATING COHORTS")

