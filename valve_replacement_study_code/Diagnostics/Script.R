# Get concept counts ----
library(tidyr)
library(omopgenerics)
library(dplyr)
library(here)
library(CodelistGenerator)
cprd_aurum <- importSummarisedResult(here("data", "summarise_concept_id_counts_CPRD AURUM.csv")) |>
  tidy() |>
  rename("concept_name" = "variable_name", "concept_id" = "variable_level")

data_loch <- importSummarisedResult(here("data", "summarise_concept_id_counts_Data Loch.csv")) |>
  tidy() |>
  rename("concept_name" = "variable_name", "concept_id" = "variable_level")

concept_counts <- cprd_aurum |>
  rbind(data_loch)

# cohort code use ---
for(index in c("efi", "cci")) {
  x <- importCodelist(path = here("codelists", paste0(index, "_codelists"))) 
  cohort_code_counts <- list()
  orphan_code_counts <- list()
  cohort_code_counts_standard <- list()
  orphan_code_counts_standard <- list()
  
  for(i in c(1:length(names(x)))) {

    cohort_name <- case_when(
      names(x)[[i]] %in% c("systemic_lupus_erythematosus", "systemic_sclerosis_not_reviewed", 
                           "polymyositis_not_reviewed", "rheumatoid_arthritis", 
                           "rheumatoid_lung_disease_not_reviewed", "polymyalgia_rheumatica_not_reviewed") & index == "cci" ~ "rheumatologic_disease",
      names(x)[[i]] %in% c("cirrhosis_of_liver_not_reviewed", "disease_of_liver_not_reviewed", "chronic_liver_disease") & index == "cci"~ "mild_liver_disease",
      names(x)[[i]] %in% c("eye_disorder_due_to_dm_not_reviewed", "complications_due_to_dm_not_reviewed") & index == "cci"~ "diabetes_with_chronic_complications",
      names(x)[[i]] %in% c("hemiplegia_not_reviewed", "paraplegia_not_reviewed") & index == "cci"~ "hemiplegia_or_paraplegia",
      names(x)[[i]] %in% c("acute_kidney_injury", "chronic_kidney_disease") & index == "cci"~ "renal_disease",
      names(x)[[i]] %in% c("malignant_neoplastic_disease_not_reviewed") & index == "cci"~ "any_malignancy_including_leukemia_and_lymphoma",
      names(x)[[i]] %in% c("hepatic_failure_not_reviewed", "hepatic_encephalopathy_not_reviewed", "portal_hypertension_not_reviewed", "esophageal_varices_not_reviewed") & index == "cci" ~ "moderate_or_severe_liver_disease", 
      names(x)[[i]] %in% c("malignant_neoplastic_disease_not_reviewed") & index == "cci"~ "metastatic_solid_tumor",
      names(x)[[i]] %in% c("anemia_broad","anemia_nutritional") & index == "efi"~ "Anaemia and haematinic deficiency",
      names(x)[[i]] %in% c("t1dm","t2dm") & index == "efi"~ "Diabetes",
      names(x)[[i]] %in% c("copd","asthma") & index == "efi" ~ "Respiratory disease",
      .default = names(x)[[i]])
    
    cohort_code_counts[[i]] <- concept_counts |>
      filter(concept_id %in% x[[i]]) |>
      mutate("result_id" = 1L,
             "group_name"  = "omop_table &&& index",
             "group_level" = paste0(omop_table, " &&& ", index), 
             "variable_name" = "cohort_name",
             "variable_level" = cohort_name,
             "strata_name" = "codelist_name &&& concept_name &&& concept_id &&& source_concept_name &&& source_concept_id",
             "strata_level" = paste0(names(x)[[i]], " &&& ", concept_name, " &&& ", concept_id, " &&& ", source_concept_name, " &&& ", source_concept_id),
             "estimate_name" = "count",
             "estimate_type" = "integer",
             "estimate_value" = count_subjects,
             "additional_name" = "type",
             "additional_level" = "cohort_code_use") |>
      select(-c("omop_table", "concept_name", "concept_id", "source_concept_id", "source_concept_name", "count_subjects")) |>
      newSummarisedResult(
        settings = tibble("result_id" = 1,
                          "result_type" = "cohort_code_use")
      ) |>
      mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD AURUM", cdm_name))
    
    cohort_code_counts_standard[[i]] <- concept_counts |>
      filter(concept_id %in% x[[i]]) |>
      group_by(cdm_name, omop_table, concept_name, concept_id) |>
      summarise(estimate_value = sum(count_subjects, na.rm = TRUE), .groups = "drop") |>   
      mutate("result_id" = 2L,
             "group_name"  = "omop_table &&& index",
             "group_level" = paste0(omop_table, " &&& ", index), 
             "variable_name" = "cohort_name",
             "variable_level" = cohort_name,
             "strata_name" = "codelist_name &&& concept_name &&& concept_id",
             "strata_level" = paste0(names(x)[[i]], " &&& ", concept_name, " &&& ", concept_id),
             "estimate_name" = "count",
             "estimate_type" = "integer",
             "additional_name" = "type",
             "additional_level" = "cohort_code_use_standard") |>
      select(-c("omop_table", "concept_name", "concept_id")) |>
      newSummarisedResult(
        settings = tibble("result_id" = 2,
                          "result_type" = "cohort_code_use")
      ) |>
      mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD AURUM", cdm_name))
    
    # orphan codes 
    cdm_vocab_2025_08 <- insertTable(cdm_vocab_2025_08,
                                     name = "concepts",
                                     table = tibble("concept_id_1" = x[[1]]))
    
    cdm_vocab_2025_08[["relationships"]] <- cdm_vocab_2025_08$concept_relationship |>
      inner_join(cdm_vocab_2025_08$concepts, 
                 by = "concept_id_1") |>
      compute(temporary = FALSE, name = "relationships")
    
    orphan_code_counts[[i]] <- concept_counts |>
      select("cdm_name", "omop_table", "concept_name", "concept_id", "source_concept_id", "source_concept_name", "count_subjects") |>
      distinct() |>
      group_by(cdm_name, omop_table, concept_name, concept_id, source_concept_id, source_concept_name) |>
      summarise(
        count_subjects = sum(count_subjects),
        .groups = "drop"
      ) |>
      inner_join(
        cdm_vocab_2025_08$relationships |>
          select("concept_id_1", "concept_id_2", "relationship_id") |>
          select(concept_id = concept_id_2, relationship_id) |>
          mutate(concept_id = as.character(concept_id)) |>
          collect(),
        by = c("concept_id"),
        relationship = "many-to-many"
      ) |>
      select("cdm_name", "omop_table", "concept_name", "concept_id",  "source_concept_id", "source_concept_name", "relationship_id", "count_subjects") |>
      group_by(cdm_name, omop_table, concept_name, concept_id, count_subjects, source_concept_id, source_concept_name) |>
      summarise(
        relationship_id = paste(relationship_id, collapse = ", "),
        .groups = "drop"
      ) |>
      mutate("result_id" = 3L,
             "group_name"  = "omop_table &&& index",
             "group_level" = paste0(omop_table, " &&& ", index), 
             "variable_name" = "cohort_name",
             "variable_level" = cohort_name,
             "strata_name" = "codelist_name &&& concept_name &&& concept_id &&& source_concept_name &&& source_concept_id &&& relationship",
             "strata_level" = paste0(names(x)[[i]], " &&& ", concept_name, " &&& ", concept_id, " &&& ", source_concept_name, " &&& ", source_concept_id, " &&& ", relationship_id),
             "estimate_name" = "count",
             "estimate_type" = "integer",
             "estimate_value" = count_subjects,
             "additional_name" = "type",
             "additional_level" = "orphan_code_counts") |>
      select(-c("omop_table", "concept_name", "concept_id", "source_concept_id", "source_concept_name", "count_subjects",  "relationship_id")) |>
      newSummarisedResult(settings = tibble("result_id" = 3L,
                                            "result_type" = "orphan_code_use")) |>
      mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD AURUM", cdm_name))
    
    orphan_code_counts_standard[[i]] <- concept_counts |>
      select("cdm_name", "omop_table", "concept_name", "concept_id", "count_subjects") |>
      distinct() |>
      group_by(cdm_name, omop_table, concept_name, concept_id) |>
      summarise(
        count_subjects = sum(count_subjects),
        .groups = "drop"
      ) |>
      inner_join(
        cdm_vocab_2025_08$relationships |>
          select("concept_id_1", "concept_id_2", "relationship_id") |>
          select(concept_id = concept_id_2, relationship_id) |>
          mutate(concept_id = as.character(concept_id)) |>
          collect(),
        by = c("concept_id"),
        relationship = "many-to-many"
      ) |>
      select("cdm_name", "omop_table", "concept_name", "concept_id",  "relationship_id", "count_subjects") |>
      group_by(cdm_name, omop_table, concept_name, concept_id, count_subjects) |>
      summarise(
        relationship_id = paste(relationship_id, collapse = ", "),
        .groups = "drop"
      ) |>
      mutate("result_id" = 3L,
             "group_name"  = "omop_table &&& index",
             "group_level" = paste0(omop_table, " &&& ", index), 
             "variable_name" = "cohort_name",
             "variable_level" = cohort_name,
             "strata_name" = "codelist_name &&& concept_name &&& concept_id &&&  relationship",
             "strata_level" = paste0(names(x)[[i]], " &&& ", concept_name, " &&& ", concept_id, " &&& ",  relationship_id),
             "estimate_name" = "count",
             "estimate_type" = "integer",
             "estimate_value" = count_subjects,
             "additional_name" = "type",
             "additional_level" = "orphan_code_counts_standard") |>
      select(-c("omop_table", "concept_name", "concept_id", "count_subjects",  "relationship_id")) |>
      newSummarisedResult() |>
      mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD AURUM", cdm_name))
  }
  
  cohort_code_counts <- bind(cohort_code_counts) 
  cohort_code_counts_standard <-  bind(cohort_code_counts_standard)
  orphan_code_counts <-  bind(orphan_code_counts) 
  orphan_code_counts_standard <-  bind(orphan_code_counts_standard) 
  
  bind(cohort_code_counts, cohort_code_counts_standard, orphan_code_counts, orphan_code_counts_standard) |>
    exportSummarisedResult(fileName = paste0("results_", index, ".csv"))
}
