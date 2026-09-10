logMessage(message = "START RISK SCORES ANALYSES")

# Measure each one of the concepts / year ----
logMessage(message = "Measure each risk score concept / year")
results[["scores"]] <- summariseCharacteristics(cdm[["procedures_as"]], 
                                                strata = list("calendar_year"),
                                                cohortIntersectFlag = list(
                                                  "EFI" = list("targetCohortTable" = "electronic_frailty_index",
                                                               "window" = c(-Inf, 0),
                                                               "nameStyle" = "{cohort_name}"),
                                                  "CCI" = list("targetCohortTable" = "charlson_comorbidity_index",
                                                               "window" = c(-Inf, 0),
                                                               "nameStyle" = "{cohort_name}")))

logMessage(message = "Measure each risk score concept / year")
cdm[["procedures_as_stratified"]] <- stratifyCohorts(cdm[["procedures_as"]],
                                                     strata = list("efi_categories", "cci_categories"),
                                                     name = "procedures_as_stratified")

results[["overlap_by_risk"]] <- summariseCohortOverlap(cdm[["procedures_as_stratified"]])

logMessage(message = "RISK SCORES ANALYSIS FINISHED")