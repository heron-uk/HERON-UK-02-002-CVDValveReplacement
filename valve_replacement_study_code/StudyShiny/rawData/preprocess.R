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

result <- omopgenerics::importSummarisedResult(here::here("..")))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)




data[["orphan_code_use"]]  <- data[["orphan_code_use"]] |> 
  omopgenerics::splitAdditional() |>
  dplyr::mutate("table_name" = dplyr::case_when(
    group_level %in% c("tavi", "tavi_additional", "aortic_valve_replacement", "aortic_valve_replacement_potential_tavi") ~ "Aortic valve replacement",
    group_level %in% c("aortic_endocarditis_avr", "aortic_insufficiency_avr", "aortic_stenosis_avr") ~ "Indications",
    .default = table_name)) |>
  dplyr::mutate("table_name" = gsub("overall", "Charlson comorbidity index", table_name),
                "table_name" = gsub("efi", "Electronic frailty index", table_name),
                "table_name" = gsub("covariates", "Covariates", table_name)) |>
  omopgenerics::uniteAdditional("cols" = additionalColumns(data[["orphan_code_use"]])) |>
  splitStrata() |>
  select(-c("domain_id", "log_id"))  |>
  uniteStrata(c("domain")) |>
  bind()

values$orphan_code_use_table_name <- data[["orphan_code_use"]] |> splitAdditional() |> distinct(table_name) |> pull("table_name")

data[["cohort_code_use"]]  <- data[["cohort_code_use"]] |> 
  omopgenerics::splitAdditional() |>
  dplyr::mutate("table_name" = dplyr::case_when(
    group_level %in% c("tavi", "tavi_additional", "aortic_valve_replacement", "aortic_valve_replacement_potential_tavi") ~ "Aortic valve replacement",
    group_level %in% c("aortic_endocarditis_avr", "aortic_insufficiency_avr", "aortic_stenosis_avr") ~ "Indications",
    .default = table_name)) |>
  dplyr::mutate("table_name" = gsub("overall", "Charlson comorbidity index", table_name),
                "table_name" = gsub("efi", "Electronic frailty index", table_name),
                "table_name" = gsub("covariates", "Covariates", table_name)) |>
  omopgenerics::uniteAdditional("cols" = additionalColumns(data[["cohort_code_use"]])) |>
  bind()

values$cohort_code_use <- data[["cohort_code_use"]] |> splitAdditional() |> distinct(table_name) |> pull("table_name")
