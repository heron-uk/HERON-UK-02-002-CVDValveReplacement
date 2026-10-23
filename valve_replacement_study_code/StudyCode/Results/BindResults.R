cdm_name <- "HERON_CDM_202509_2026_10_21"

# Db diagnostics
db_diagnostics <- omopgenerics::bind(
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_snapshot_", cdm_name,".csv"))),
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_obs_period_", cdm_name,".csv")))
)

# Codelist Diagnostics
x1 <- omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_code_use_efi_", cdm_name,".csv"))) 
x1 <- x1 |>
  omopgenerics::splitAdditional() |>
  dplyr::mutate("table_name" = "Electronic frailty index") |>
  omopgenerics::uniteAdditional(c(omopgenerics::additionalColumns(x1), "table_name")) |>
  omopgenerics::splitStrata() |>
  dplyr::rename("domain" = "domain_id") |>
  dplyr::select(-"log_id") |>
  omopgenerics::uniteStrata(c("domain"))

x2 <- omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_code_use_orphan_", cdm_name,".csv"))) 
x2 <- x2 |>
  omopgenerics::splitAdditional() |>
  dplyr::mutate("table_name" = "Covariates") |>
  omopgenerics::uniteAdditional(c(omopgenerics::additionalColumns(x2), "table_name")) |>
  omopgenerics::splitStrata() |>
  dplyr::rename("domain" = "domain_id") |>
  dplyr::select(-"log_id") |>
  omopgenerics::uniteStrata(c("domain"))

x3 <- omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_code_use_", cdm_name,".csv"))) 
x3 <- x3 |>
  omopgenerics::splitAdditional() |>
  dplyr::mutate("table_name" = dplyr::case_when(
    group_level %in% c("tavi", "tavi_additional", "aortic_valve_replacement", "aortic_valve_replacement_potential_tavi") ~ "Aortic valve replacement",
    group_level %in% c("aortic_endocarditis_avr", "aortic_insufficiency_avr", "aortic_stenosis_avr") ~ "Indications",
    .default = "Charlson comorbidity index")) |>
  omopgenerics::uniteAdditional(c(omopgenerics::additionalColumns(x3), "table_name")) |>
  omopgenerics::splitStrata() |>
  dplyr::rename("domain" = "domain_id") |>
  dplyr::select(-"log_id") |>
  omopgenerics::uniteStrata(c("domain"))

codelist_diagnostics <- omopgenerics::bind(x1, x2, x3)

# Attrition
attritions <- omopgenerics::bind(
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_attrition_tavi_additional_", cdm_name,".csv"))),
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_attrition_tavi_direct_", cdm_name,".csv"))),
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_objective_one_attrition_indications_", cdm_name,".csv"))),
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_objective_one_attrition_procedures_", cdm_name,".csv")))
)

# Indications
indications <- omopgenerics::bind(
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_objective_one_", cdm_name,".csv")))
)

# Incidence
incidence <- omopgenerics::bind(
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_incidence_", cdm_name,".csv"))),
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_incidence_per_groups_", cdm_name,".csv")))
)

# Table one
table_one <- omopgenerics::bind(
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_table_one_", cdm_name,".csv")))
)

# Scores
scores <- omopgenerics::bind(
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_scores_", cdm_name,".csv"))),
  omopgenerics::importSummarisedResult(path = here::here("Results", paste0("results_overlap_by_risk_", cdm_name,".csv")))
)

omopgenerics::bind(db_diagnostics,
                   codelist_diagnostics, 
                   attritions,
                   indications,
                   incidence,
                   table_one,
                   scores) |>
  omopgenerics::exportSummarisedResult(path = here::here("Results"), 
                                       fileName = paste0("results_", cdm_name, ".csv"))
