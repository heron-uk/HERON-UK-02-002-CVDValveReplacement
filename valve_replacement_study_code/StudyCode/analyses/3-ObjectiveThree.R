# Require an indication of aortic stenosis / aortic stenosis + regurgitation
# at least one year before the diagnostic
cdm[["procedures_as"]] <- cdm[["procedures"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_indication", 
                         window = c(-365,0), 
                         intersections = c(1,Inf), 
                         name = "procedures_as")

# Add scores ---
omopgenerics::logMessage(message = "Add CCI")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addUpdatedCharlsonIndex(conceptSet = charlson_comorbidity_index_codelist,
                          ageAdjusted = FALSE,
                          nameStyle = "cci",
                          window = c(-Inf, 0),
                          categories = list("low_risk" = c(1,2),
                                            "medium_risk" = c(3,4),
                                            "high_risk" = c(5, Inf)))

omopgenerics::logMessage(message = "Add EFI")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addElectronicFrailtyIndex(conceptSet = electronic_frailty_index_codelist,
                            nameStyle = "efi",
                            window = c(-Inf, 0),
                            categories = list(fit = c(0, 0.12), 
                                              mild = c(0.12, 0.24), 
                                              moderate = c(0.24, 0.36),
                                              severe = c(0.36, 1)))

omopgenerics::logMessage(message = "Add age group extended")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  addAge(ageGroup = age_groups_extended) 

omopgenerics::logMessage(message = "Population characteristics")
results[["table_one"]] <- summariseCharacteristics(cdm[["procedures_as"]], 
                                                   strata = list("calendar_year", 
                                                                 c("calendar_year", "sex"),
                                                                 c("calendar_year", "age_group"),
                                                                 c("calendar_year", "efi_categories"),
                                                                 c("calendar_year", "cci_categories")),
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
                                                   otherVariables = c("efi", "cci", "efi_categories", "cci_categories"),
                                                   estimates = list("age" = c("density", 'min', 'q25', 'median', 'q75', 'max'),
                                                                    "efi" = c("density", 'min', 'q25', 'median', 'q75', 'max'),
                                                                    "cci" = c("density", 'min', 'q25', 'median', 'q75', 'max')))

omopgenerics::logMessage(message = "FINISH OBJECTIVE 3")
