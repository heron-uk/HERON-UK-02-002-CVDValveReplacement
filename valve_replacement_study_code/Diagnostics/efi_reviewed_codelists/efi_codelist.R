library(readr)
library(here)
x <- list("activity_limitation", "anaemia_and_haematinic_deficiency", "arthritis",
       "atrial_fibrillation", "cerebrovascular_disease", "chronic_kidney_disease",
       "diabetes", "dizziness", "dyspnoea", "falls", "foot_problems", "fragility_fracture",
       "hearing_impairment", "heart_failure", "heart_valve_disease", "housebound",
       "hypertension", "hypotension/syncope", "ischaemic_heart_disease", 
       "memory_and_cognitive_problems","mobility_and_transfer_problems","osteoporosis", 
       "parkinsonism_and_tremor", "peptic_ulcer", "peripheral_vascular_disease",
       "polypharmacy", "requirement_for_care", "respiratory_disease", "skin_ulcer", "sleep_disturbance",
       "social_vulnerability", "thyroid_disease", "urinary_incontinence", "urinary_system_disease",
       "visual_impairment", "weight_loss_and_anorexia")

names(x) <- list.dirs(path = here("efi_reviewed_codelists"), full.names = FALSE, recursive = FALSE)


for(i in names(x)){
  read_tsv(here("efi_reviewed_codelists", i, paste0(i,".txt")),
           col_types = cols(
             "MedCodeId" = col_character(),
             "SnomedCTConceptId" = col_character(),
             "SnomedCTDescriptionId" = col_character()
           )) |> 
    select("OriginalReadCode", "CleansedReadCode") |>
    mutate("cohort_name" = x[[i]])
}


read_tsv(here("efi_reviewed_codelists", "efi_04", "efi_04_clegg_add.txt"),
         col_types = cols(
           "medcodeid" = col_character(),
           "snomedctconceptid" = col_character(),
           "snomedctdescriptionid" = col_character()
         )) |> select(term)
