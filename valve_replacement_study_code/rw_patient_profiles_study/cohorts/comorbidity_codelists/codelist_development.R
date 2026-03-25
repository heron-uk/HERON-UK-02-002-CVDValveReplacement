x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "aortic calcification")
newCodelist(list("aortic_calcification" = x$concept_id)) |>
  excludeConcepts(cdm, 4058384) |>
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
  newCodelist() |>
  excludeConcepts(cdm,
                  concepts = c(4188554, 4231452, 43020630, 437461, 4095624, 4353740,
                               45766082, 4354063, 4125005, 4353822, 45766103)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "mitral regurgitation")
newCodelist(list("mitral_regurgitation" = x$concept_id)) |>
  excludeConcepts(cdm, concepts = c(40487183,  606159, 45766214, 4353822, 4102992)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("pacemaker", "defibrillator"),
                       domains = c("Procedure", "Observation", "Condition"))
newCodelist(list("pre_existing_pacemaker_or_defibrillator" = x$concept_id)) |>
  excludeConcepts(cdm, concepts = c(4019140, 4019140, 4281670, 4281670, 40481373, 4209748,
                                    4270363, 4242529, 44511196, 4228672, 46272748, 4050575,
                                    4216485, 44511203, 4199753, 4263193, 4062879, 4062879,
                                    4249002, 4216602, 44783080, 44810459, 40486971, 4241102,
                                    44790597, 4049990,  4049402,35609511)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("dialysis"),
                       domains = c("Procedure", "Observation", "Condition"))
newCodelist(list("dialysis" = x$concept_id)) |>
  excludeConcepts(cdm, concepts = c(4161526, 4126449, 44809610, 44807877, 44807874,
                                    44811116, 44807875, 44807870, 4197217, 4183419, 42690435,
                                    4128367, 4051330, 4335329, 4338539, 4213996, 44516041,
                                    44516040, 44516042, 4140589, 439634, 44810123, 44810122,
                                    44810124, 44809906, 437268, 4229839, 4231951, 4105171,
                                    4049372, 4019622, 4080242, 40489914, 4031777, 4076662,
                                    4055766, 4259615, 35609512, 35609513, 4080968,  37206601,
                                    4153208)) |>
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

list("peripheral_arterial_disease" = read_csv(file = here("cohorts", "comorbidity_codelists", "peripheral_arterial_disease.csv")) |>
       pull("concept_id")) |> 
  newCodelist() |>
    excludeConcepts(cdm, concepts = c(4128218, 4127997, 439037, 4213638, 4006293,
                                      4144981, 4093607,  4212101, 194385, 36712809,
                                      194964, 4276505, 443622, 193793, 195834, 4119141,
                                      4139534, 35615054, 4108367, 4121627, 4179907,
                                      4108381, 4112161, 4108375, 4108380, 4110331,
                                      4110205, 4231816, 4173167, 4329498, 44808745, 4341646,
                                      4180025, 4110200, 4318843, 4030663, 4118795, 42535335,
                                      36712987, 4114011, 4175570, 4055025, 4025854, 4175579,
                                      605282, 4136335, 4348039, 4316367, 35615098,44813802)) |>
  exportCodelist(path = here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("left bundle branch block"),
                       domains = c("Condition"))
newCodelist(list("left_bundle_branch_block" = x$concept_id)) |>
  excludeConcepts(cdm, concepts = c(4088336, 4295336, 40482086, 313209, 4268046,
                                    40482505, 316432, 321590, 40482887, 40483359, 
                                    4138973, 4244693, 4217860, 321315)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = c("right bundle branch block"),
                       domains = c("Condition"))
newCodelist(list("right_bundle_branch_block" = x$concept_id)) |>
  excludeConcepts(cdm, concepts = c(321587, 37017193,4088338)) |>
  exportCodelist(here("cohorts", "comorbidity_codelists"), type = "csv")
