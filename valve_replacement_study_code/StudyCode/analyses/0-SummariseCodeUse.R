logMessage(message = "Running cohort code use 1")
cdm <- bind(cdm[["indications"]], cdm[["procedures"]], cdm[["charlson_comorbidity_index"]], name = "code_use")

results[["code_use"]] <- codelistDiagnostics(cohort = cdm[["code_use"]], 
                                             achillesCodeUse = TRUE, 
                                             cohortCodeUse = FALSE, 
                                             orphanCodeUse = TRUE, 
                                             drugDiagnostics = FALSE, 
                                             measurementDiagnostics = FALSE)
logMessage(message = "Running cohort code use 2")
results[["aortic_stenosis_code_use"]] <- codelistDiagnostics(cohort = cdm[["aortic_stenosis_sa"]], 
                                                             achillesCodeUse = TRUE, 
                                                             cohortCodeUse = FALSE, 
                                                             orphanCodeUse = TRUE, 
                                                             drugDiagnostics = FALSE, 
                                                             measurementDiagnostics = FALSE)


logMessage(message = "Running cohort code use 3")
results[["code_use_efi"]] <- codelistDiagnostics(cohort = cdm[["electronic_frailty_index"]], 
                                                 achillesCodeUse = TRUE, 
                                                 cohortCodeUse = FALSE, 
                                                 orphanCodeUse = TRUE, 
                                                 drugDiagnostics = FALSE, 
                                                 measurementDiagnostics = FALSE)


logMessage(message = "Running cohort code use 4")
cdm <- bind(cdm[["comorbidities"]], cdm[["aortic_valve_disease_phenotype"]], cdm[["cardiovascular_disease"]],cdm[["cardiovascular_risk_factors"]], name = "tab_one")
results[["code_use_orphan"]] <- codelistDiagnostics(cohort = cdm[["tab_one"]], 
                                                    achillesCodeUse = TRUE, 
                                                    cohortCodeUse = FALSE, 
                                                    orphanCodeUse = TRUE, 
                                                    drugDiagnostics = FALSE, 
                                                    measurementDiagnostics = FALSE)
