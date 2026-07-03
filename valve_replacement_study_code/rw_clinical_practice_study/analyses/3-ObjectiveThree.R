# Require an indication of aortic stenosis / aortic stenosis + regurgitation
# at least one year before the diagnostic
cdm[["procedures_as"]] <- cdm[["procedures"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_indication", 
                         window = c(-365,0), 
                         intersections = c(1,Inf), 
                         name = "procedures_as")

# Add scores ---
omopgenerics::logMessage(message = "Add HFRS - snomed")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addScores(mapping = "snomed", score = "hfrs") |>
  addScoresGrouping(mapping = "snomed", score = "hfrs")

omopgenerics::logMessage(message = "Add HFRS - icd10")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addScores(mapping = "icd", score = "hfrs") |>
  addScoresGrouping(mapping = "icd", score = "hfrs")

omopgenerics::logMessage(message = "Add CCI - snomed")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addScores(mapping = "snomed", score = "cci") |>
  addScoresGrouping(mapping = "snomed", score = "cci")

omopgenerics::logMessage(message = "Add CCI - icd10")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addScores(mapping = "icd", score = "cci") |>
  addScoresGrouping(mapping = "icd", score = "cci")

omopgenerics::logMessage(message = "Add age group extended")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addAge(ageGroup = age_groups_extended) 

omopgenerics::logMessage(message = "Population characteristics")
results[["table_one"]] <- summariseCharacteristics(cdm[["procedures_as"]], 
                                                   strata = list("calendar_year", 
                                                                 c("calendar_year", "sex"),
                                                                 c("calendar_year", "age_group"),
                                                                 c("calendar_year", "hfrs_snomed_groups"),
                                                                 c("calendar_year", "hfrs_icd_groups"),
                                                                 c("calendar_year", "cci_snomed_groups"),
                                                                 c("calendar_year", "cci_icd_groups")),
                                                   cohortIntersectFlag = list(
                                                     "Comorbidities" = list("targetCohortTable" = "comorbidities",
                                                                            "window" = c(-365, 0),
                                                                            "nameStyle" = "{cohort_name}"),
                                                     "Aortic valve disease phenotype" = list("targetCohortTable" = "aortic_valve_disease_phenotype",
                                                                                             "window" = c(-365, 0),
                                                                                             "nameStyle" = "{cohort_name}"),
                                                     "Cardiovascular disease" = list("targetCohortTable" = "cardiovascular_disease",
                                                                                     "window" = c(-365, 0),
                                                                                     "nameStyle" = "{cohort_name}"),
                                                     "Cardiovascular risk factors" = list("targetCohortTable" = "cardiovascular_risk_factors",
                                                                                          "window" = c(-365, 0),
                                                                                          "nameStyle" = "{cohort_name}")),
                                                   conceptIntersectFlag = list(
                                                     "Previous medications" = list("conceptSet" = importCodelist(here("cohorts", "study_codelists", "treatments"), type = "csv"),
                                                                                   "window" = c(-365, 0),
                                                                                   "nameStyle" = "{cohort_name}")),
                                                   otherVariables = c("hfrs_snomed", "hfrs_icd", "cci_snomed", "cci_icd", "hfrs_snomed_groups", "hfrs_icd_groups", "cci_snomed_groups", "cci_icd_groups"),
                                                   estimates = list("age" = c("density", 'min', 'q25', 'median', 'q75', 'max'),
                                                                    "hfrs_snomed" = c("density", 'min', 'q25', 'median', 'q75', 'max'),
                                                                    "hfrs_icd" = c("density", 'min', 'q25', 'median', 'q75', 'max'),
                                                                    "cci_snomed" = c("density", 'min', 'q25', 'median', 'q75', 'max'),
                                                                    "cci_icd" = c("density", 'min', 'q25', 'median', 'q75', 'max')))

omopgenerics::logMessage(message = "FINISH OBJECTIVE 3")
