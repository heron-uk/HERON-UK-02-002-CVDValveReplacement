x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "aortic calcification")
newCodelist(list("aortic_calcification" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "bicuspid aortic valve")
newCodelist(list("bicuspid_aortic_valve" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "unicuspid aortic valve")
newCodelist(list("unicuspid_aortic_valve" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

as  <- importCodelist(path = here("cohorts", "study_codelists"), type = "csv")
com <- importCodelist(path = here("cohorts", "comorbidity_codelists"), type = "csv")

list("concomitant_valve_disorders_excluding_endocarditis" = tibble(concept_id = com[["concomitant_valve_disorders_excluding_endocarditis"]]) |>
  filter(!concept_id %in% as[["aortic_stenosis"]]) |>
  pull("concept_id")) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "mitral regurgitation")
newCodelist(list("mitral_regurgitation" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("pacemaker", "defibrillator"),
                       domains = c("Procedure", "Observation", "Condition"))
newCodelist(list("pre_existing_pacemaker_or_defibrillator" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("dialysis"),
                       domains = c("Procedure", "Observation", "Condition"))
newCodelist(list("dialysis" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

list("type_2_diabetes" = read_csv(file = here("cohorts", "comorbidity_codelists", "type_2_diabetes.csv")) |>
       pull("concept_id")) |>
  newCodelist() |>
  exportCodelist(path = here("cohorts", "comorbidity_codelists"), type = "csv")

list("disorders_of_lipid_metabolism" = read_csv(file = here("cohorts", "comorbidity_codelists", "disorders_of_lipid_metabolism.csv")) |>
       pull("concept_id")) |>
  newCodelist() |>
  exportCodelist(path = here("cohorts", "comorbidity_codelists"), type = "csv")

list("coronary_artery_disease" = read_csv(file = here("cohorts", "comorbidity_codelists", "coronary_artery_disease.csv")) |>
       pull("concept_id")) |>
  newCodelist() |>
  exportCodelist(path = here("cohorts", "comorbidity_codelists"), type = "csv")

# list("hypertrophic_cardiomyopathy" = read_csv(file = here("cohorts", "comorbidity_codelists", "HCM_CGsearch.csv"))  |>
#        pull("concept_id")) |>
#   newCodelist() |>
#   exportCodelist(path = here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("pacemaker", "defibrillator"),
                       domains = c("Procedure", "Observation", "Condition"))
newCodelist(list("pre_existing_pacemaker_or_defibrillator" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

# list("peripheral_arterial_disease" = read_csv(file = here("cohorts", "comorbidity_codelists", "peripheral_arterial_disease.csv"))  |>
#        filter(dani_PAD_broad == TRUE) |>
#        pull("concept_id")) |>
#   newCodelist() |>
#   exportCodelist(path = here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("left bundle branch block"),
                       domains = c("Condition"))
newCodelist(list("left_bundle_branch_block" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("right bundle branch block"),
                       domains = c("Condition"))
newCodelist(list("right_bundle_branch_block" = x$concept_id)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")
