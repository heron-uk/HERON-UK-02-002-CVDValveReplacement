omopgenerics::logMessage(message = "START RISK SCORES ANALYSES")

# Measure each one of the concepts / year ----
omopgenerics::logMessage(message = "Measure each risk score concept / year")
results[["scores"]] <- summariseCharacteristics(cdm[["procedures_as"]], 
                                                strata = list("calendar_year"),
                                                cohortIntersectFlag = list(
                                                  "EFI" = list("targetCohortTable" = "electronic_frailty_index",
                                                                       "window" = c(-Inf, 0),
                                                                       "nameStyle" = "{cohort_name}"),
                                                  "CCI" = list("targetCohortTable" = "charlson_comorbidity_index",
                                                                    "window" = c(-Inf, 0),
                                                                    "nameStyle" = "{cohort_name}")))

omopgenerics::logMessage(message = "Overlap between risk groups")
cdm[["procedures_as"]] <- cdm[["procedures_as"]] |>
  mutate("hfrs_snomed_groups" = paste0("hfrs_snomed_", .data$hfrs_snomed_groups)) |>
  mutate("hfrs_icd_groups" = paste0("hfrs_icd_", .data$hfrs_icd_groups)) |>
  mutate("cci_snomed_groups" = paste0("cci_snomed_", .data$cci_snomed_groups)) |>
  mutate("cci_icd_groups" = paste0("cci_icd_", .data$cci_icd_groups)) |>
  compute(temporary = FALSE, name = "procedures_as")

cdm[["procedures_as_stratified"]] <- stratifyCohorts(cdm[["procedures_as"]],
                                          strata = list("efi_categories", "cci_categories"),
                                          name = "procedures_as_stratified")

results[["overlap_by_risk"]] <- summariseCohortOverlap(cdm[["procedures_as_stratified"]])

omopgenerics::logMessage(message = "RISK SCORES ANALYSIS FINISHED")