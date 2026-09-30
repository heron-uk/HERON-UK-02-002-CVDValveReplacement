# Get ICD codes -----
x <- cdm_vocab_2025_08[["concept"]] |>
  filter(vocabulary_id == "ICD10") |>
  collect()

rbind(
  x |>
    filter(grepl("^I09\\.9$|^I11\\.0$|^I13\\.0$|^I13\\.2$|^I25\\.5$|^I42\\.0$|^I42\\.[5-9]|^I43|^I50|^P29\\.0$", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "congestive_heart_failure",
           "weight" = 2),
  x |>
    filter(grepl("^F0[0-3]|^F05\\.1$|^G30|^G31\\.1$", concept_code)) |>
    filter(!grepl("F00-F09|G30-G32", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "dementia",
           "weight" = 2),
  x |>
    filter(grepl("^I27\\.8$|^I27\\.9$|^J4[0-7]|^J6[0-7]|^J68\\.4$|^J70\\.1$|^J70\\.3$", concept_code)) |>
    filter(!grepl("J60-J70", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "chronic_pulmonary_disease",
           "weight" = 1),
  x |>
    filter(grepl("^M05|^M06|^M31\\.5$|^M3[2-4]|^M35\\.1$|^M35\\.3$|^M36\\.0$", concept_code)) |>
    filter(!grepl("M05-M14", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "rheumatologic_disease",
           "weight" = 1),
  x |>
    filter(grepl("^B18|^K70\\.[0-3]|^K70\\.9$|^K71\\.[3-5]|^K71\\.7$|^K73|^K74|^K76\\.0$|^K76\\.[2-4]|^K76\\.8$|^K76\\.9$|^Z94\\.4$", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "mild_liver_disease",
           "weight" = 2),
  x |>
    filter(grepl("^E10\\.[2-5]|^E10\\.7$|^E11\\.[2-5]|^E11\\.7$|^E12\\.[2-5]|^E12\\.7$|^E13\\.[2-5]|^E13\\.7$|^E14\\.[2-5]|^E14\\.7$", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "diabetes_with_chronic_complications",
           "weight" = 1),
  x |>
    filter(grepl("^G04\\.1$|^G11\\.4$|^G80\\.1$|^G80\\.2$|^G81|^G82|^G83\\.[0-4]|^G83\\.9$", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "hemiplegia",
           "weight" = 2),
  x |>
    filter(grepl("^I12\\.0$|^I13\\.1$|^N03\\.[2-7]|^N05\\.[2-7]|^N18|^N19|^N25\\.0$|^Z49\\.[0-2]|^Z94\\.0$|^Z99\\.2", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "renal_disease",
           "weight" = 1),
  x |>
    filter(grepl("^C0[0-9]|^C1[0-9]|^C2[0-6]|^C3[0-4]|^C3[7-9]|^C4[0-1]|^C43|^C4[5-9]|^C5[0-8]|^C6[0-9]|^C7[0-6]|^C8[1-5]|^C88|^C9[0-7]", concept_code)) |>
    filter(!grepl("C00-C75|C00-C97|C43-C44|C76-C80|C81-C96", concept_code)) |>
        select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "any_malignancy",
           "weight" = 2),
  x |>
    filter(grepl("^I85\\.0$|^I85\\.9$|^I86\\.4$|^I98\\.2$|^K70\\.4$|^K71\\.1$|^K72\\.1$|^K72\\.9$|^K76\\.5$|^K76\\.6$|^K76\\.7", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "moderate_or_severe_liver_disease",
           "weight" = 4),
  x |>
    filter(grepl("^C7[7-9]|^C80", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "metastatic_solid_tumor",
           "weight" = 6),
  x |>
    filter(grepl("^B2[0-2]|^B24", concept_code)) |>
    filter(!grepl("B20-B24", concept_code)) |>
    select("source_concept_id" = "concept_id",
           "source_concept_name" = "concept_name",
           "concept_code") |>
    mutate("cohort_name" = "aids",
           "weight" = 4)
) |>
  write_csv(file = here("cohorts", "study_codelists", "charlson_comorbidity_index", "cci.csv"))
#  -----

x <- read_csv(here("cohorts", "study_codelists", "charlson_comorbidity_index", "cci.csv")) 

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
  mutate("cohort_name" = paste0("cci_",cohort_name, "_snomed"))

lapply(split(icd10_snomed$standard_concept_id, icd10_snomed$cohort_name),
       unique) |>
  newCodelist() |>
  exportCodelist(path = here("cohorts", "study_codelists", "charlson_comorbidity_index", 
                             "snomed_codelists"),
                 type = "csv")

# codelist with ICD10 codes ----
icd10 <- cdm_vocab_2025_08[["icd10_code"]] |>
  select("source_concept_id", "cohort_name") |>
  distinct() |>
  mutate("cohort_name" = paste0("cci_", cohort_name, "_icd")) |>
  collect()

lapply(split(icd10$source_concept_id, icd10$cohort_name),
       unique) |>
  newCodelist() |>
  exportCodelist(path = here("cohorts", "study_codelists", "charlson_comorbidity_index", 
                             "icd10_codelists"),
                 type = "csv")
