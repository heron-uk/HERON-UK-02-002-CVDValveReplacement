# Get ICD codes -----
# icd <- read_csv(here("cohorts", "study_codelists", "hospital_frailty_risk_score", "hospital_frailty_score.csv"))
# x <- cdm_vocab_2025_08[["concept"]] |>
#   filter(vocabulary_id == "ICD10") |>
#   collect()
# 
# y <- icd$concept_code
# y <- append(y[y!="M48"], "M48.0$")
# 
# x |>
#   filter(grepl(paste0("^", y, collapse = "|"), concept_code)) |>
#   filter(!grepl("-", concept_code)) |>
#   select("concept_code",
#          "source_concept_name" = "concept_name",
#          "source_concept_id" = "concept_id") |>
#   mutate("cohort_name" = str_to_lower(gsub("\\..*", "", concept_code))) |>
#   left_join(
#     icd |>
#       mutate("cohort_name" = str_to_lower(concept_code)) |>
#       select(-"concept_code"),
#     by = c("cohort_name")
#   ) |>
#   write_csv(file = here("cohorts", "study_codelists", "hospital_frailty_risk_score", "hfrs.csv"))
# -----
x <- read_csv(file = here("cohorts", "study_codelists", "hospital_frailty_risk_score", "hfrs.csv"))
cdm_vocab_2025_08 <- insertTable(cdm_vocab_2025_08,
                                 name = "icd10_code",
                                 table = x)

# Map to snomed ----
cdm_vocab_2025_08[["icd10_code"]] <- cdm_vocab_2025_08[["icd10_code"]] |>
  inner_join(
    cdm_vocab_2025_08[["concept_relationship"]] |>
      filter(relationship_id %in% c("Maps to")) |>
      select("source_concept_id" = "concept_id_1",
             "standard_concept_id" = "concept_id_2"),
    by = c("source_concept_id")
  ) 

icd10_snomed <- cdm_vocab_2025_08[["icd10_code"]] |>
  left_join(cdm_vocab_2025_08[["concept"]] |>
              select("standard_concept_name" = "concept_name", "standard_concept_id" = "concept_id"),
            by = "standard_concept_id") |>
  collect()

icd10_snomed <- icd10_snomed |>
  mutate("cohort_name" = paste0("hfrs_",cohort_name, "_snomed"))

lapply(split(icd10_snomed$standard_concept_id, icd10_snomed$cohort_name),
       unique) |>
  newCodelist() |>
  exportCodelist(path = here("cohorts", "study_codelists", "hospital_frailty_risk_score", 
                             "snomed_codelists"),
                 type = "csv")

# codelist with ICD10 codes ----
icd10 <- cdm_vocab_2025_08[["icd10_code"]] |>
  select("source_concept_id", "cohort_name") |>
  distinct() |>
  mutate("cohort_name" = paste0("hfrs_", cohort_name, "_icd")) |>
  collect()

lapply(split(icd10$source_concept_id, icd10$cohort_name),
       unique) |>
  newCodelist() |>
  exportCodelist(path = here("cohorts", "study_codelists", "hospital_frailty_risk_score", 
                             "icd10_codelists"),
                 type = "csv")
