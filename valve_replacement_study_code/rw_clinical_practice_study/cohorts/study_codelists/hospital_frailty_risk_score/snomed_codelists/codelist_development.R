x <- read_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score.csv")) 

cdm_vocab_2025_08 <- insertTable(cdm_vocab_2025_08,
                                 name = "icd10_code",
                                 table = x)

cdm_vocab_2025_08[["icd10_code"]] <- cdm_vocab_2025_08[["icd10_code"]] |>
  select("concept_code" = "icd10_code") |>
  inner_join(
    cdm_vocab_2025_08[["concept"]] |>
      filter(vocabulary_id %in% c("ICD10")),
    by = "concept_code"
  ) |>
  select("concept_id_1" = "concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_code") |>
  compute(temporary = FALSE, name = "icd10_code")

# Map ancestors
ancestors <- cdm_vocab_2025_08[["icd10_code"]] |>
  inner_join(
    cdm_vocab_2025_08[["concept_relationship"]] |>
      filter(relationship_id %in% c("Maps to")),
    by = "concept_id_1"
  ) |>
  select("source_concept_id" = "concept_id_1", "concept_code",
         "cohort_name" = "concept_name",  "concept_id" = "concept_id_2") |>
  collect()

# Get descendants ----
descendants <- cdm_vocab_2025_08[["icd10_code"]] |>
  inner_join(
    cdm_vocab_2025_08[["concept_relationship"]] |>
      filter(relationship_id %in% c("Subsumes", "Is a")),
    by = "concept_id_1"
  ) |>
  select("cohort_name" = "concept_name", "concept_code",
         "concept_id_1" = "concept_id_2") |>
  inner_join(
    cdm_vocab_2025_08[["concept_relationship"]] |>
      filter(relationship_id %in% c("Maps to")),
    by = c("concept_id_1")
  ) |>
  select("cohort_name", "concept_id" = "concept_id_2", "concept_code",
         "source_concept_id" = "concept_id_1") |>
  collect()
hfrs <- bind_rows(ancestors, descendants)

hfrs <- hfrs |>
  mutate("cohort_name_1" = dense_rank(cohort_name)) |>
  mutate("cohort_name_1" = paste0("cohort_", as.character(cohort_name_1))) |>
  left_join(x, 
            by = c("concept_code" = "icd10_code"))
write_csv(hfrs, here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hfrs.csv"))

hfrs_list <- list()
for( nam in unique(hfrs$cohort_name_1) ) {
  hfrs_list[[nam]] <- hfrs |>
    filter(cohort_name_1 == nam) |>
    pull("concept_id") |>
    unique()
}

hfrs_list <- hfrs_list |>
  newCodelist()

exportCodelist(hfrs_list, path = here("cohorts", "hospital_frailty_risk_score"), type = "csv")
