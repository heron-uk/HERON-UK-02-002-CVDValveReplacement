# Get concept counts ----
library(tidyr)
library(omopgenerics)
library(dplyr)
library(here)
cprd_aurum <- importSummarisedResult(here("summarise_concept_id_counts_CPRD AURUM.csv")) |>
  tidy() |>
  rename("concept_name" = "variable_name", "concept_id" = "variable_level")

data_loch <- importSummarisedResult(here("summarise_concept_id_counts_Data Loch.csv")) |>
  tidy() |>
  rename("concept_name" = "variable_name", "concept_id" = "variable_level")

concept_counts <- cprd_aurum |>
  rbind(data_loch)

# cohort code use ---
x <- importCodelist(path = here("reviewed_codelist")) 
cohort_code_counts <- list()
orphan_code_counts <- list()
cohort_code_counts_standard <- list()
orphan_code_counts_standard <- list()

for(i in c(1:length(names(x)))) {
  cohort_code_counts[[i]] <- concept_counts |>
    filter(concept_id %in% x[[i]]) |>
    mutate("result_id" = 1L,
           "group_name"  = "omop_table",
           "group_level" = omop_table, 
           "variable_name" = "concept_name &&& concept_id &&& source_concept_name &&& source_concept_id",
           "variable_level" = paste0(concept_name, " &&& ", concept_id, " &&& ", source_concept_name, " &&& ", source_concept_id),
           "strata_name" = "overall",
           "strata_level" = "overall",
           "estimate_name" = "count",
           "estimate_type" = "integer",
           "estimate_value" = count_subjects,
           "additional_name" = "type",
           "additional_level" = "cohort_code_use") |>
    select(-c("omop_table", "concept_name", "concept_id", "source_concept_id", "source_concept_name", "count_subjects")) |>
    newSummarisedResult() |>
    mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD AURUM", cdm_name))
  
  cohort_code_counts_standard[[i]] <- concept_counts |>
    filter(concept_id %in% x[[i]]) |>
    group_by(cdm_name, omop_table, concept_name, concept_id) |>
    summarise(estimate_value = sum(count_subjects, na.rm = TRUE), .groups = "drop") |>   
    mutate("result_id" = 2L,
           "group_name"  = "omop_table",
           "group_level" = omop_table, 
           "variable_name" = "concept_name &&& concept_id",
           "variable_level" = paste0(concept_name, " &&& ", concept_id),
           "strata_name" = "overall",
           "strata_level" = "overall",
           "estimate_name" = "count",
           "estimate_type" = "integer",
           "additional_name" = "type",
           "additional_level" = "cohort_code_use_standard") |>
    select(-c("omop_table", "concept_name", "concept_id")) |>
    newSummarisedResult() |>
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
           "group_name"  = "omop_table",
           "group_level" = omop_table, 
           "variable_name" = "concept_name &&& concept_id &&& source_concept_name &&& source_concept_id",
           "variable_level" = paste0(concept_name, " &&& ", concept_id, " &&& ", source_concept_name, " &&& ", source_concept_id, " &&& ", relationship_id),
           "strata_name" = "overall",
           "strata_level" = "overall",
           "estimate_name" = "count",
           "estimate_type" = "integer",
           "estimate_value" = count_subjects,
           "additional_name" = "type",
           "additional_level" = "orphan_code_counts") |>
    select(-c("omop_table", "concept_name", "concept_id", "source_concept_id", "source_concept_name", "count_subjects",  "relationship_id")) |>
    newSummarisedResult() |>
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
           "group_name"  = "omop_table",
           "group_level" = omop_table, 
           "variable_name" = "concept_name &&& concept_id",
           "variable_level" = paste0(concept_name, " &&& ", concept_id, " &&& ", relationship_id),
           "strata_name" = "overall",
           "strata_level" = "overall",
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
  exportSummarisedResult(fileName = "results.csv")

