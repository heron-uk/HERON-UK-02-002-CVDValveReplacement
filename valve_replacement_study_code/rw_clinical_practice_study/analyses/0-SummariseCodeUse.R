
cdm <- bind(cdm[["indications"]], cdm[["procedures"]], cdm[["hfrs_snomed"]],cdm[["hfrs_icd"]], cdm[["cci_snomed"]], cdm[["cci_icd"]], name = "code_use")

results[["code_use"]] <- codelistDiagnostics(cohortTable = "code_use", 
                                             achillesCodeUse = FALSE, 
                                             cohortCodeUse = TRUE, 
                                             orphanCodeUse = FALSE, 
                                             drugDiagnostics = FALSE, 
                                             measurementDiagnostics = FALSE)

cdm <- bind(cdm[["comorbidities"]], cdm[["aortic_valve_disease_phenotype"]], cdm[["cardiovascular_disease"]],cdm[["cardiovascular_risk_factors"]], name = "tab_one")
results[["code_use"]] <- codelistDiagnostics(cohortTable = "code_use", 
                                             achillesCodeUse = FALSE, 
                                             cohortCodeUse = TRUE, 
                                             orphanCodeUse = TRUE, 
                                             drugDiagnostics = FALSE, 
                                             measurementDiagnostics = FALSE)