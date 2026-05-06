# list(mitral_valve_disorder = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                conceptId = 319843) |> 
#        dplyr::pull("concept_id")) |> 
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(tricuspid_valve_disorder = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                conceptId = 316993) |> 
#        dplyr::pull("concept_id")) |> 
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(pulmonary_artery_disorder = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                   conceptId = c(319845, 4317288)) |> 
#        dplyr::pull("concept_id")) |> 
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(cardiomegaly = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                   conceptId = 314658) |> 
#        dplyr::pull("concept_id")) |> 
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(coarctation_of_aorta = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                       conceptId = 321119) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(supravalvar_aortic_stenosis = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                               conceptId = 4147787) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(subaortic_stenosis = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                      conceptId = 259123) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(aortic_aneurysm = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                             conceptId = 317585) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(congenital_anomaly_of_coronary_artery = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                          conceptId = 321109) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(hypoplastic_left_heart_syndrome = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                                conceptId = 440207) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(atrial_septal_defect = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                          conceptId = 4289309) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(ventricular_septal_defect = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                               conceptId = 434462) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(secondary_pulmonary_hypertension = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                    conceptId = 4339214) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# list(pleural_effusion = CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08, 
#                                                                           conceptId = 254061) |> 
#        dplyr::pull("concept_id")) |>
#   newCodelist() |> 
#   exportCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")
# 
# list(aortic_valve_repair = c(CodelistGenerator::getDescendants(cdm = cdm_vocab_2025_08,
#                                                                           conceptId = c(4019027)) |>
#                                dplyr::filter(!is.na(concept_name)) |> 
#        dplyr::pull("concept_id"),
#        44511059)) |>
#   newCodelist() |>
#   exportCodelist(here::here("cohorts", "procedure_codelists"), "csv")
