mergeCodelists <- function(x, newCodelistName, keepOriginal, codelistsToJoin) {
  omopgenerics::assertLogical(keepOriginal, length = 1)
  omopgenerics::assertCharacter(newCodelistName, length = 1, null = TRUE)
  omopgenerics::assertChoice(codelistsToJoin, names(x), unique = TRUE)
  
  xNames <- codelistsToJoin
  
  allCodes <- purrr::list_c(x[codelistsToJoin]) |>
    unique()
  if (is.null(newCodelistName)) {
    newCodelistName <- paste0(xNames, collapse = "_")
  }
  
  newX <- list()
  newX[[newCodelistName]] <- allCodes
  
  if (isTRUE(keepOriginal)) {
    newX <- purrr::list_flatten(list(x[setdiff(names(x), xNames)], newX))
  }
  
  if (inherits(x, "codelist")) {
    newX <- newX |> omopgenerics::newCodelist()
  }
  if (inherits(x, "codelist_with_details")) {
    newX <- newX |> omopgenerics::newCodelistWithDetails()
  }
  if (inherits(x, "concept_set_expression")) {
    newX <- newX |> omopgenerics::newConceptSetExpression()
  }
  
  return(newX)
}

# Instantiate charlson comorbidity index
omopgenerics::logMessage(message = "Instantiate Charlson Comorbidity Index")
charlson_comorbidity_index_codelist <- importCodelist(here("cohorts", "study_codelists", "charlson_comorbidity_index"))

charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "connective_tissue_disease",
                 keepOriginal = TRUE, 
                 codelistsToJoin = c("systemic_lupus_erythematosus", "systemic_sclerosis",
                                     "polymyositis", "rheumatoid_arthritis", "rheumatoid_lung_disease",
                                     "polymyalgia_rheumatica")) |>
  mergeCodelists(newCodelistName = "mild_liver_disease",
                 keepOriginal = TRUE, 
                 codelistsToJoin = c("chronic_liver_disease", "cirrhosis_of_liver", "disease_of_liver")) |>
  mergeCodelists(newCodelistName = "diabetes_with_complication",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("complications_due_to_dm", "eye_disorder_due_to_dm")) |>
  mergeCodelists(newCodelistName = "hemiplegia",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("paraplegia", "hemiplegia")) |>
  mergeCodelists(newCodelistName = "moderate_or_severe_liver_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hepatic_failure", "hepatic_encephalopathy",
                                     "portal_hypertension", "esophageal_varices")) |>
  mergeCodelists(newCodelistName = "severe_chronic_kidney_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("chronic_kidney_disease", "acute_kidney_injury")) |>
  mergeCodelists(newCodelistName = "congestive_heart_failure",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("heart_failure")) |>
  mergeCodelists(newCodelistName = "chronic_pulmonary_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("copd")) |> 
  mergeCodelists(newCodelistName = "any_malignancy",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("malignant_neoplastic_disease")) |>
  mergeCodelists(newCodelistName = "aids",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hiv")) 
  
cdm[["charlson_comorbidity_index"]] <- conceptCohort(cdm,
                                                     conceptSet = charlson_comorbidity_index_codelist,
                                                     name = "charlson_comorbidity_index",
                                                     exit = "event_start_date")


# Instantiate electronic frailty index
omopgenerics::logMessage(message = "Instantiate Electronic Frailty Index")
electronic_frailty_index_codelist <- importCodelist(here("cohorts", "study_codelists", "electronic_frailty_index"))

electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "anemia",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("anemia_broad", "anemia_nutritional")) |>
  mergeCodelists(newCodelistName = "care_requirement",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("requirement_for_care")) |>
  mergeCodelists(newCodelistName = "diabetes",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("t1dm", "t2dm")) |>
  mergeCodelists(newCodelistName = "fragility_fracture",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("fractures")) |>
  mergeCodelists(newCodelistName = "hearing_impairment",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hearing_impairment_not_reviewed")) |>
  mergeCodelists(newCodelistName = "heart_valve_disorder",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("heart_valve_disorder_not_reviewed")) |>
  mergeCodelists(newCodelistName = "housebound",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("housebound_not_reviewed")) |>
  mergeCodelists(newCodelistName = "hypotension_syncope",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hypotension_not_reviewed")) |>
  mergeCodelists(newCodelistName = "memory_cognitive_disorder",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("memory_and_cognitive_problems_not_reviewed")) |>
  mergeCodelists(newCodelistName = "mobility_problems",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("mobility_and_transfer_problems_not_reviewed")) |>
  mergeCodelists(newCodelistName = "parkinsonism_tremor",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("parkinsonism_tremor_not_reviewed")) |>
  mergeCodelists(newCodelistName = "peptic_ulcer",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("peptic_ulcer_not_reviewed")) |>
  mergeCodelists(newCodelistName = "peripheral_vascular_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("peripheral_vascular_disease_not_reviewed")) |>
  mergeCodelists(newCodelistName = "respiratory_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("copd", "asthma")) |>
  mergeCodelists(newCodelistName = "skin_ulcer",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("skin_ulcer_not_reviewed")) |>
  mergeCodelists(newCodelistName = "sleep_disturbance",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("sleep_disorder_not_reviewed")) |>
  mergeCodelists(newCodelistName = "social_vulnerability",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("social_vulnerability_not_reviewed")) |>
  mergeCodelists(newCodelistName = "thyroid_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("thyroid_disease_not_reviewed")) |>
  mergeCodelists(newCodelistName = "urinary_incontinence",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("urinary_incontinence_not_reviewed")) |>
  mergeCodelists(newCodelistName = "urinary_system_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("urinary_system_disease_not_reviewed")) |>
  mergeCodelists(newCodelistName = "visual_impairment",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("visual_impairment_not_reviewed")) |>
  mergeCodelists(newCodelistName = "weight_loss_anorexia",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("weight_loss_and_anorexia_not_reviewed")) |>
  mergeCodelists(newCodelistName = "activity_limitation",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("activity_limitation_not_reviewed")) |>
  mergeCodelists(newCodelistName = "arthritis",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("arthritis_not_reviewed")) |>
  mergeCodelists(newCodelistName = "atrial_fibrillation",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("atrial_fibrilation")) |>
  mergeCodelists(newCodelistName = "cerebrovascular_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("cerebrovascular_disease_not_reviewed")) |>
  mergeCodelists(newCodelistName = "dizziness",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("dizziness")) 

cdm[["electronic_frailty_index"]] <- conceptCohort(cdm,
                                                   conceptSet = electronic_frailty_index_codelist,
                                                   name = "electronic_frailty_index",
                                                   exit = "event_start_date")


