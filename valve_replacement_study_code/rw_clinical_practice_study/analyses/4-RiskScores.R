omopgenerics::logMessage(message = "START RISK SCORES ANALYSES")
omopgenerics::logMessage(message = "Overlap among HFRS cohorts")
cdm <- bind(cdm[["hfrs_snomed"]], cdm[["hfrs_icd"]], name = "hfrs")
cdm[["hfrs"]] <- cdm[["hfrs"]] |>
  requireCohortIntersect(targetCohortTable = "procedures_as",
                         window = c(-Inf, Inf))

results[["hfrs_overlap"]] <- summariseCohortOverlap(cdm[["hfrs"]])

omopgenerics::logMessage(message = "Overlap among CCI cohorts")
cdm <- bind(cdm[["cci_icd"]], cdm[["cci_snomed"]], name = "cci")
cdm[["cci"]] <- cdm[["cci"]] |>
  requireCohortIntersect(targetCohortTable = "procedures_as",
                         window = c(-Inf, Inf))

results[["cci_overlap"]] <- summariseCohortOverlap(cdm[["cci"]])

# Measure each one of the concepts / year ----
omopgenerics::logMessage(message = "Measure each risk score concept / year")
results[["scores"]] <- summariseCharacteristics(cdm[["procedures_as"]], 
                                                strata = list("calendar_year"),
                                                cohortIntersectFlag = list(
                                                  "HFRS SNOMED" = list("targetCohortTable" = "hfrs_snomed",
                                                                       "window" = c(-365, 0),
                                                                       "nameStyle" = "{cohort_name}"),
                                                  "HFRS ICD" = list("targetCohortTable" = "hfrs_icd",
                                                                    "window" = c(-365, 0),
                                                                    "nameStyle" = "{cohort_name}"),
                                                  "CCI SNOMED" = list("targetCohortTable" = "cci_snomed",
                                                                      "window" = c(-Inf, 0),
                                                                      "nameStyle" = "{cohort_name}"),
                                                  "CCI ICD" = list("targetCohortTable" = "cci_icd",
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
                                          strata = list("hfrs_snomed_groups", "hfrs_icd_groups", "cci_snomed_groups", "cci_icd_groups"),
                                          name = "procedures_as_stratified")

results[["overlap_by_risk"]] <- summariseCohortOverlap(cdm[["procedures_as_stratified"]])

omopgenerics::logMessage(message = "RISK SCORES ANALYSIS FINISHED")