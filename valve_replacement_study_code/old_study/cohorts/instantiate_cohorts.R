omopgenerics::logMessage(message = "INSTANTIATING COHORTS")

# Procedures ---
omopgenerics::logMessage(message = "Importing procedure codelists")
codelist <- importCodelistWithDetails(here::here("codelist", "study_codelists"))

omopgenerics::logMessage(message = "Creating SAVI/TAVI no restrictions cohort")
cdm[["avr_no_restrictions"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                                 conceptSet = c(codelist["aortic_valve_replacement"]), 
                                                                 name = "avr_no_restrictions", 
                                                                 exit = "event_start_date")
cdm <- createProceduresCohorts(avrCohortName = "avr_no_restrictions", 
                               taviCohortName = "tavi_no_restrictions", 
                               saviCohortName = "savr_no_restrictions",
                               proceduresCohortName = "procedures_no_restrictions",
                               restrictions = FALSE) 

omopgenerics::logMessage(message = "Creating SAVI/TAVI - objective 1")
cdm[["avr"]] <- cdm[["avr_no_restrictions"]] |>
  copyCohorts(name = "avr")

cdm[["avr"]] <- cdm[["avr"]] |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

cdm <- createProceduresCohorts(avrCohortName = "avr",
                               taviCohortName = "tavi", 
                               saviCohortName = "savr",
                               proceduresCohortName = "procedures_objective_one",
                               restrictions = TRUE) 

omopgenerics::logMessage(message = "Creating SAVI/TAVI - restricting to those with AS diagnostic in the no restriction cohorts")
cdm[["aortic_stenosis_no_restrictions"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                             conceptSet = c(codelist["aortic_stenosis"]), 
                                                             name = "aortic_stenosis_no_restrictions", 
                                                             exit = "event_start_date") 
cdm[["procedures_no_restrictions"]] <- cdm[["procedures_no_restrictions"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_no_restrictions",
                         window = c(-Inf, 0),
                         intersections = c(1, Inf),
                         name = "procedures_no_restrictions")

omopgenerics::logMessage(message = "Creating SAVI/TAVI - restricting to those with AS diagnostic")
cdm[["aortic_stenosis"]] <- cdm[["aortic_stenosis_no_restrictions"]] |>
  copyCohorts(name = "aortic_stenosis")

cdm[["aortic_stenosis"]] <- cdm[["aortic_stenosis"]] |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireInDateRange(dateRange = study_period) |>
  CohortConstructor::requirePriorObservation(minPriorObservation = 365)

cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_objective_one"]] |>
  copyCohorts(name = "procedures_plus_aortic_stenosis")

cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_plus_aortic_stenosis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis",
                         window = c(-Inf, 0),
                         intersections = c(1, Inf),
                         name = "procedures_plus_aortic_stenosis")


# Other indications ----
omopgenerics::logMessage(message = "Instantiating indications")
omopgenerics::logMessage(message = "Instantiating aortic insufficiency")
cdm[["aortic_insufficiency"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                                  conceptSet = c(codelist["aortic_insufficiency"]), 
                                                                  name = "aortic_insufficiency", 
                                                                  exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

omopgenerics::logMessage(message = "Instantiating aortic endocarditis")
cdm[["aortic_endocarditis"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                                  conceptSet = c(codelist["aortic_endocarditis"]), 
                                                                  name = "aortic_endocarditis", 
                                                                  exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]], name = "indications")

# Hospital frailty risk score ----
omopgenerics::logMessage(message = "Instantiating hospital frailty risk score")
codelist <- importCodelistWithDetails(here::here("codelist", "hospital_frailty_risk_score"))

cdm[["hospital_frailty_risk_score"]] <- conceptCohort(cdm = cdm,
                                                      subsetCohort = "procedures_plus_aortic_stenosis",
                                                      name = "hospital_frailty_risk_score",
                                                      conceptSet = codelist,
                                                      exit = "event_start_date")
# Specify which ones are chronic!!!!!!!!!!!!!!

# Aortic valve disease phenotype -----
omopgenerics::logMessage(message = "Instantiating aortic valve disease phenotype")
codelist <- importCodelistWithDetails(here::here("codelist", "comorbidity_codelists", "aortic_valve_disease_phenotype"))

cdm[["aortic_valve_disease_phenotype"]] <- conceptCohort(cdm = cdm,
                                                      subsetCohort = "procedures_plus_aortic_stenosis",
                                                      name = "aortic_valve_disease_phenotype",
                                                      conceptSet = codelist,
                                                      exit = "event_start_date")
cdm$aortic_valve_disease_phenotype <- cdm$aortic_valve_disease_phenotype |> 
  exitAtObservationEnd()

# Previous comorbidities -----
omopgenerics::logMessage(message = "Instantiating previous comorbidities")
codelist <- importCodelistWithDetails(here::here("codelist", "comorbidity_codelists", "previous_comorbidities"))

cdm[["previous_comorbidities"]] <- conceptCohort(cdm = cdm,
                                                         subsetCohort = "procedures_plus_aortic_stenosis",
                                                         name = "previous_comorbidities",
                                                         conceptSet = codelist,
                                                         exit = "event_start_date")

cdm$previous_comorbidities <- cdm$previous_comorbidities |> 
  exitAtObservationEnd(cohortId = c("copd"))

# Cardiovascular risk factors -----
omopgenerics::logMessage(message = "Instantiating cardiovascular risk factors")
codelist <- importCodelistWithDetails(here::here("codelist", "comorbidity_codelists", "previous_comorbidities"))

cdm[["cardiovascular_risk_factors"]] <- conceptCohort(cdm = cdm,
                                                 subsetCohort = "procedures_plus_aortic_stenosis",
                                                 name = "cardiovascular_risk_factors",
                                                 conceptSet = codelist,
                                                 exit = "event_start_date")

cdm$cardiovascular_risk_factors <- cdm$cardiovascular_risk_factors |> 
  exitAtObservationEnd()

omopgenerics::logMessage(message = "Instantiating obesity")
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
cdm <- omopgenerics::bind(cdm$obesity,
                          cdm$cardiovascular_risk_factors, 
                          name = "cardiovascular_risk_factors")

# Cardiovascular disease -----
omopgenerics::logMessage(message = "Instantiating cardiovascular disease")
codelist <- importCodelistWithDetails(here::here("codelist", "comorbidity_codelists", "previous_comorbidities"))

cdm[["cardiovascular_disease"]] <- conceptCohort(cdm = cdm,
                                                      subsetCohort = "procedures_plus_aortic_stenosis",
                                                      name = "cardiovascular_disease",
                                                      conceptSet = codelist,
                                                      exit = "event_start_date")

cdm$cardiovascular_disease <- cdm$cardiovascular_disease |> 
  exitAtObservationEnd(cohortId = c("pre_existing_pacemaker_or_defibrillator"))

# Treatments
omopgenerics::logMessage(message = "Instantiating treatments")
codelist <- importCodelistWithDetails(here::here("codelist", "treatments_codelist"))
cdm[["treatments"]] <- cdm |>
  CohortConstructor::conceptCohort(
    subsetCohort = "procedures_plus_aortic_stenosis",
    conceptSet = codelist,
    exit = "event_start_date",
    name = "treatments")

omopgenerics::logMessage(message = "Finished instantiating cohorts")
