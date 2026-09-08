logMessage(message = "Running cohort code use")
cdm <- bind(cdm[["indications"]], cdm[["procedures"]], cdm[["charlson_comorbidity_index"]], cdm[["electronic_frailty_index"]], name = "code_use")

results[["code_use"]] <- codelistDiagnostics(cohortTable = "code_use", 
                                             achillesCodeUse = FALSE, 
                                             cohortCodeUse = TRUE, 
                                             orphanCodeUse = FALSE, 
                                             drugDiagnostics = FALSE, 
                                             measurementDiagnostics = FALSE)

logMessage(message = "Running orphan codes & cohort code use")
cdm <- bind(cdm[["comorbidities"]], cdm[["aortic_valve_disease_phenotype"]], cdm[["cardiovascular_disease"]],cdm[["cardiovascular_risk_factors"]], name = "tab_one")
results[["code_use"]] <- codelistDiagnostics(cohortTable = "code_use", 
                                             achillesCodeUse = FALSE, 
                                             cohortCodeUse = TRUE, 
                                             orphanCodeUse = TRUE, 
                                             drugDiagnostics = FALSE, 
                                             measurementDiagnostics = FALSE)