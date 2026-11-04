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

result <- bind(
  omopgenerics::importSummarisedResult(here::here("..", "StudyCode", "Results", "results_HERON_CDM_202509_2026_10_21.csv")),
  omopgenerics::importSummarisedResult(here::here("..", "StudyCode", "Results", "results_CPRD GOLD_2026_11_02.csv")))
result <- result |>
  dplyr::mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD Aurum", cdm_name)) |>
  dplyr::mutate("group_level" = gsub("aortic_valve_replacement", "AVR", group_level),
                "group_level" = gsub("savr", "SAVR", group_level),
                "group_level" = gsub("tavi", "TAVI", group_level))

# Read data ----
data <- prepareResult(result, resultList)

data[["summarise_characteristics"]] <- bind(
  data[["summarise_characteristics"]], 
  
  data[["summarise_characteristics"]] |>
    dplyr::filter(variable_name == "Indications",
                  variable_level %in% c("Aortic stenosis avr",
                                        "Aortic insufficiency avr",
                                        "Aortic stenosis insufficiency",
                                        "Aortic endocarditis",
                                        "No indication identified")) |>
    omopgenerics::tidy() |>
    dplyr::summarise("count" = sum(count, na.rm = TRUE), 
                     "percentage" = sum(percentage, na.rm = TRUE),
                     .by = c(cdm_name, cohort_name, calendar_year, age_group, sex, variable_name)) |>
    dplyr::mutate("mixed_group_count" = c(100-percentage) * count/percentage) |>
    dplyr::mutate("mixed_group_percentage" = 100-percentage) |>
    dplyr::select(-c("count", "percentage")) |>
    dplyr::rename("count" = "mixed_group_count",
                  "percentage" = "mixed_group_percentage") |>
    dplyr::mutate('variable_level' = "Other identified indications") |>
    omopgenerics::transformToSummarisedResult(
      group = "cohort_name",
      strata = c("calendar_year", "age_group", "sex"),
      estimates = c("count", "percentage")
    ))

new_set <- data[["incidence"]] |>
  omopgenerics::splitGroup() |>
  omopgenerics::splitStrata() |>
  addSettings(settingsColumn = colnames(omopgenerics::settings(data[["incidence"]]))) |>
  dplyr::select(
    result_id,
    denominator_cohort_name,
    sex,
    dplyr::any_of(colnames(omopgenerics::settings(data[["incidence"]])))
  ) |>
  dplyr::distinct() |>
  dplyr::mutate("sex" = dplyr::if_else(sex == "overall", "Both", sex)) |> 
  dplyr::mutate("denominator_efi" = dplyr::if_else(denominator_cohort_name %in% c("fit", "mild", "moderate", "severe"), denominator_cohort_name, "overall")) |>
  dplyr::mutate("denominator_cci" = dplyr::if_else(denominator_cohort_name %in% c("low_risk", "medium_risk", "high_risk"), denominator_cohort_name, "overall")) |>
  dplyr::mutate("denominator_sex" = dplyr::if_else(denominator_sex == "-NA-", sex, denominator_sex)) |>
  dplyr::mutate("denominator_age_group" = dplyr::if_else(denominator_age_group == "-NA-", "0 to 150", denominator_age_group)) |>
  dplyr::mutate("new_result_id" = dplyr::row_number()) |> dplyr::mutate("new_result_id" = dplyr::row_number())  |>
  dplyr::mutate("denominator_target_cohort_name" = if_else(denominator_target_cohort_name == "-NA-", "aortic_stenosis_avr", denominator_target_cohort_name))

data[["incidence"]] <- data[["incidence"]] |>
  omopgenerics::splitGroup() |>
  omopgenerics::splitStrata() |>
  dplyr::mutate("sex" = dplyr::if_else(sex == "overall", "Both", sex)) |>
  dplyr::left_join(
    new_set |>
      dplyr::select("result_id", "denominator_cohort_name", "sex", "new_result_id"),
    by = c("result_id", "denominator_cohort_name", "sex")
  ) |>
  dplyr::select(-"result_id") |>
  dplyr::rename("result_id" = "new_result_id") |>
  omopgenerics::uniteGroup(cols = omopgenerics::groupColumns(data[["incidence"]])) |>
  dplyr::mutate("strata_name" = "overall",
                "strata_level" = "overall") |>
  dplyr::select(-"sex") |>
  newSummarisedResult(settings = new_set |>
                        dplyr::select(-"result_id") |>
                        dplyr::rename("result_id" = "new_result_id") |>
                        dplyr::select(-c("denominator_cohort_name", "sex")))

# Modify values ----
values <- getValues(result, resultList)

for (nm in names(values)[grepl("incidence_", names(values))]) {
  new_nm <- sub("incidence_", "incidence_general_", nm)
  values[[new_nm]] <- values[[nm]]
}

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

for (nm in names(values)[grepl("summarise_characteristics_", names(values))]) {
  new_nm <- sub("summarise_characteristics_", "summarise_characteristics_indications_", nm)
  values[[new_nm]] <- values[[nm]]
}

for (nm in names(values)[grepl("summarise_characteristics_", names(values))]) {
  new_nm <- sub("summarise_characteristics_", "summarise_characteristics_age_group_", nm)
  values[[new_nm]] <- values[[nm]]
}

for (nm in names(values)[grepl("summarise_characteristics_", names(values))]) {
  new_nm <- sub("summarise_characteristics_", "summarise_characteristics_cci_", nm)
  values[[new_nm]] <- values[[nm]]
}

for (nm in names(values)[grepl("summarise_characteristics_", names(values))]) {
  new_nm <- sub("summarise_characteristics_", "summarise_characteristics_efi_", nm)
  values[[new_nm]] <- values[[nm]]
}

for (nm in names(values)[grepl("summarise_characteristics_", names(values))]) {
  new_nm <- sub("summarise_characteristics_", "summarise_characteristics_sex_", nm)
  values[[new_nm]] <- values[[nm]]
}

for (nm in names(values)[grepl("summarise_characteristics_", names(values))]) {
  new_nm <- sub("summarise_characteristics_", "summarise_characteristics_overall_", nm)
  values[[new_nm]] <- values[[nm]]
}

values$summarise_characteristics_age_group_age_group <- c("0 to 39", "40 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 150")
values$summarise_characteristics_cci_cci_categories <- values$summarise_characteristics_cci_cci_categories[values$summarise_characteristics_cci_cci_categories != "overall"]
values$summarise_characteristics_efi_efi_categories <- values$summarise_characteristics_efi_efi_categories[values$summarise_characteristics_efi_efi_categories != "overall"]
values$summarise_characteristics_sex <- values$summarise_characteristics_sex[values$summarise_characteristics_sex != "overall"]

values$summarise_characteristics_indications_age_group <- c("overall", "0 to 64", "65 to 150")
values$summarise_characteristics_age_group_calendar_year <- values$summarise_characteristics_age_group_calendar_year[values$summarise_characteristics_age_group_calendar_year != "overall"]
values$summarise_characteristics_cci_calendar_year <- values$summarise_characteristics_age_group_calendar_year
values$summarise_characteristics_efi_calendar_year <- values$summarise_characteristics_age_group_calendar_year
values$summarise_characteristics_sex_calendar_year <- values$summarise_characteristics_age_group_calendar_year

values$summarise_cohort_attrition_cohort_name <- c("aortic_valve_replacement", "aortic_stenosis_avr", "aortic_insufficiency_avr", "aortic_endocarditis_avr")

values$incidence_cci_categories <- c("overall", "low_risk", "medium_risk", "high_risk")
values$incidence_efi_categories <- c("overall", "fit", "mild", "moderate", "severe")

values$incidence_denominator_age_group <- values$incidence_denominator_age_group[values$incidence_denominator_age_group != "-NA-"]
values$incidence_denominator_sex <- values$incidence_denominator_sex[values$incidence_denominator_sex != "-NA-"]
values$incidence_general_denominator_age_group <- values$incidence_denominator_age_group[values$incidence_denominator_age_group != "-NA-"]
values$incidence_general_denominator_sex <- values$incidence_denominator_sex[values$incidence_denominator_sex != "-NA-"]

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
selected$summarise_characteristis_indications_indications_window <- "365 days"

save(data, incidence_denominator_names, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

# rm(result, values, choices, selected, resultList, data)

