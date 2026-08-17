# shiny is prepared to work with this resultList:
resultList <- list(
  orphan_code_use = list(result_type = "orphan_code_use"),
  cohort_code_use = list(result_type = "cohort_code_use")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))

data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

choices$cohort_code_use_variable_level_cci <- c("heart_failure", "dementia", "copd", "rheumatologic_disease",
                                                "mild_liver_disease", "diabetes_with_chronic_complications",
                                                "hemiplegia_or_paraplegia", "renal_disease",
                                                "any_malignancy_including_leukemia_and_lymphoma",
                                                "moderate_or_severe_liver_disease", "metastatic_solid_tumor_not_reviewed", "hiv")
selected$cohort_code_use_variable_level_cci <- c("heart_failure")

choices$cohort_code_use_variable_level_efi <- c("activity_limitation_not_reviewed","Anaemia and haematinic deficiency", 
                                                "arthritis_not_reviewed", "atrial_fibrilation", "cerebrovascular_disease_not_reviewed",
                                                "chronic_kidney_disease", "Diabetes", "dizziness_not_reviewed", 
                                                "dyspnea_not_reviewed", "falls_not_reviewed", "foot_problem_not_reviewed",
                                                "fractures", "hearing_impairment_not_reviewed", "heart_failure", 
                                                "heart_valve_disorder_not_reviewed", "housebound_not_reviewed", 
                                                "hypertension", "hypotension_not_reviewed", "ischaemic_heart_disease",
                                                "memory_and_cognitive_problems_not_reviewed", "mobility_and_transfer_problems_not_reviewed",
                                                "osteoporosis", "parkinson_not_reviewed", "peptic_ulcer_not_reviewed", "peripheral_vascular_disease_not_reviewed",
                                                "requirement_for_care_not_reviewed", "Respiratory disease", "skin_ulcer_not_reviewed",
                                                "sleep_disorder_not_reviewed", "social_vulnerability_not_reviewed", 
                                                "thyroid_disease_not_reviewed", "urinary_incontinence_not_reviewed",
                                                "urinary_system_disease_not_reviewed",
                                                "visual_impairment_not_reviewed", "weight_loss_and_anorexia_not_reviewed")

selected$cohort_code_use_variable_level_efi <- c("activity_limitation_not_reviewed")

choices$orphan_code_use_variable_level_cci <- choices$cohort_code_use_variable_level_cci
selected$orphan_code_use_variable_level_cci <- selected$cohort_code_use_variable_level_cci
choices$orphan_code_use_variable_level_efi <- choices$cohort_code_use_variable_level_efi
selected$orphan_code_use_variable_level_efi <- selected$cohort_code_use_variable_level_efi

choices$orphan_code_use_cdm_name_cci <- choices$orphan_code_use_cdm_name
selected$orphan_code_use_cdm_name_cci <- selected$orphan_code_use_cdm_name
choices$orphan_code_use_cdm_name_efi <- choices$orphan_code_use_cdm_name
selected$orphan_code_use_cdm_name_efi <- selected$orphan_code_use_cdm_name

choices$orphan_code_use_omop_table_cci <- choices$orphan_code_use_omop_table
selected$orphan_code_use_omop_table_cci <- selected$orphan_code_use_omop_table
choices$orphan_code_use_omop_table_efi <- choices$orphan_code_use_omop_table
selected$orphan_code_use_omop_table_efi <- selected$orphan_code_use_omop_table

choices$cohort_code_use_cdm_name_cci <- choices$cohort_code_use_cdm_name
selected$cohort_code_use_cdm_name_cci <- selected$cohort_code_use_cdm_name
choices$cohort_code_use_cdm_name_efi <- choices$cohort_code_use_cdm_name
selected$cohort_code_use_cdm_name_efi <- selected$cohort_code_use_cdm_name

choices$cohort_code_use_omop_table_cci <- choices$cohort_code_use_omop_table
selected$cohort_code_use_omop_table_cci <- selected$cohort_code_use_omop_table
choices$cohort_code_use_omop_table_efi <- choices$cohort_code_use_omop_table
selected$cohort_code_use_omop_table_efi <- selected$cohort_code_use_omop_table

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
