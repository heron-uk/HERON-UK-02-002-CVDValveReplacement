# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period"),
  orphan_code_use = list(result_type = "orphan_code_use"),
  cohort_code_use = list(result_type = "cohort_code_use"),
  summarise_cohort_overlap = list(result_type = "summarise_cohort_overlap"),
  summarise_cohort_attrition = list(result_type = "summarise_cohort_attrition"),
  summarise_characteristics = list(result_type = "summarise_characteristics"),
  incidence = list(result_type = "incidence"),
  incidence_attrition = list(result_type = "incidence_attrition")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(here::here("..", "StudyCode", "Results", "results_HERON_CDM_202509_2026_10_21.csv"))
result <- result |>
  dplyr::mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD Aurum", cdm_name)) 

data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

for (nm in names(values)[grepl("cohort_code_use_", names(values))]) {
  new_nm <- sub("cohort_code_use_", "cohort_code_use_cohorts_", nm)
  values[[new_nm]] <- values[[nm]]
}

values$cohort_code_use_cohorts_cohort_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Aortic valve replacement", "Indications")) |>
  splitGroup() |>
  distinct(cohort_name) |>
  pull(cohort_name)

values$cohort_code_use_cohorts_codelist_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Aortic valve replacement", "Indications")) |>
  splitGroup() |>
  distinct(codelist_name) |>
  pull(codelist_name)

for (nm in names(values)[grepl("cohort_code_use_", names(values))]) {
  new_nm <- sub("cohort_code_use_", "cohort_code_use_covariates_", nm)
  values[[new_nm]] <- values[[nm]]
}
values$cohort_code_use_covariates_cohort_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Covariates")) |>
  splitGroup() |>
  distinct(cohort_name) |>
  pull(cohort_name)

values$cohort_code_use_covariates_codelist_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Covariates")) |>
  splitGroup() |>
  distinct(codelist_name) |>
  pull(codelist_name)

for (nm in names(values)[grepl("orphan_code_use_", names(values))]) {
  new_nm <- sub("orphan_code_use_", "orphan_code_use_covariates_", nm)
  values[[new_nm]] <- values[[nm]]
}

values$orphan_code_use_covariates_codelist_name <- data[["orphan_code_use"]] |>
  filterAdditional(table_name %in% c("Covariates")) |>
  splitGroup() |>
  distinct(codelist_name) |>
  pull(codelist_name)

for (nm in names(values)[grepl("cohort_code_use_", names(values))]) {
  new_nm <- sub("cohort_code_use_", "cohort_code_use_cci_", nm)
  values[[new_nm]] <- values[[nm]]
}
values$cohort_code_use_cci_cohort_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Charlson comorbidity index")) |>
  splitGroup() |>
  distinct(cohort_name) |>
  pull(cohort_name)

values$cohort_code_use_cci_codelist_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Charlson comorbidity index")) |>
  splitGroup() |>
  distinct(codelist_name) |>
  pull(codelist_name)

for (nm in names(values)[grepl("orphan_code_use_", names(values))]) {
  new_nm <- sub("orphan_code_use_", "orphan_code_use_cci_", nm)
  values[[new_nm]] <- values[[nm]]
}
values$orphan_code_use_cci_codelist_name <- data[["orphan_code_use"]] |>
  filterAdditional(table_name %in% c("Charlson comorbidity index")) |>
  splitGroup() |>
  distinct(codelist_name) |>
  pull(codelist_name)

for (nm in names(values)[grepl("cohort_code_use_", names(values))]) {
  new_nm <- sub("cohort_code_use_", "cohort_code_use_efi_", nm)
  values[[new_nm]] <- values[[nm]]
}
values$cohort_code_use_efi_cohort_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Electronic frailty index")) |>
  splitGroup() |>
  distinct(cohort_name) |>
  pull(cohort_name)

values$cohort_code_use_efi_codelist_name <- data[["cohort_code_use"]] |>
  filterAdditional(table_name %in% c("Electronic frailty index")) |>
  splitGroup() |>
  distinct(codelist_name) |>
  pull(codelist_name)

for (nm in names(values)[grepl("orphan_code_use_", names(values))]) {
  new_nm <- sub("orphan_code_use_", "orphan_code_use_efi_", nm)
  values[[new_nm]] <- values[[nm]]
}
values$orphan_code_use_efi_codelist_name <- data[["orphan_code_use"]] |>
  filterAdditional(table_name %in% c("Electronic frailty index")) |>
  splitGroup() |>
  distinct(codelist_name) |>
  pull(codelist_name)

values$summarise_cohort_attrition_cohort_name <- c("aortic_valve_replacement", "aortic_stenosis_avr", "aortic_insufficiency_avr", "aortic_endocarditis_avr")

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

selected$summarise_cohort_attrition_cohort_name <- "aortic_valve_replacement"
selected$cohort_code_use_cohorts_cohort_name    <- "aortic_valve_replacement"
selected$cohort_code_use_covariates_cohort_name <- "aortic_calcification"
selected$orphan_code_use_covariates_codelist_name <- "aortic_calcification"
selected$cohort_code_use_cci_cohort_name <- "aortic_calcification"
selected$orphan_code_use_cci_codelist_name <- "aortic_calcification"
selected$cohort_code_use_efi_cohort_name <- "aortic_calcification"
selected$orphan_code_use_efi_codelist_name <- "aortic_calcification"

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)