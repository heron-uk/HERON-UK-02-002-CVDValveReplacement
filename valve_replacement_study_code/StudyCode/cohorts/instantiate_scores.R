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

omopgenerics::logMessage(message = " > Connective tissue disease")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "connective_tissue_disease",
                 keepOriginal = TRUE, 
                 codelistsToJoin = c("systemic_lupus_erythematosus", "systemic_sclerosis",
                                     "polymyositis", "rheumatoid_arthritis", "rheumatoid_lung_disease",
                                     "polymyalgia_rheumatica")) 

omopgenerics::logMessage(message = " > Mild liver disease")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "mild_liver_disease",
                 keepOriginal = TRUE, 
                 codelistsToJoin = c("chronic_liver_disease", "cirrhosis_of_liver", "disease_of_liver")) 

omopgenerics::logMessage(message = " > Diabetes with complication")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "diabetes_with_complication",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("complications_due_to_dm", "eye_disorder_due_to_dm"))

omopgenerics::logMessage(message = " > Hemiplegia")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "hemiplegia",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("paraplegia", "hemiplegia")) 

omopgenerics::logMessage(message = " > Moderate or severe liver disease")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "moderate_or_severe_liver_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hepatic_failure", "hepatic_encephalopathy",
                                     "portal_hypertension", "esophageal_varices"))

omopgenerics::logMessage(message = " > Severe chronic kidney disease")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "severe_chronic_kidney_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("chronic_kidney_disease", "acute_kidney_injury"))

omopgenerics::logMessage(message = " > Congestive heart failure")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "congestive_heart_failure",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("heart_failure")) 

omopgenerics::logMessage(message = " > Chronic pulmonary disease")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "chronic_pulmonary_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("copd")) 

omopgenerics::logMessage(message = " > Any malignancy")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "any_malignancy",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("malignant_neoplastic_disease"))

omopgenerics::logMessage(message = " > HIV")
charlson_comorbidity_index_codelist <- charlson_comorbidity_index_codelist |>
  mergeCodelists(newCodelistName = "aids",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hiv")) 
  
omopgenerics::logMessage(message = " > Instantiate cohort")
cdm[["charlson_comorbidity_index"]] <- conceptCohort(cdm,
                                                     conceptSet = charlson_comorbidity_index_codelist,
                                                     name = "charlson_comorbidity_index",
                                                     exit = "event_start_date")


# Instantiate electronic frailty index
omopgenerics::logMessage(message = "Instantiate Electronic Frailty Index")
electronic_frailty_index_codelist <- importCodelist(here("cohorts", "study_codelists", "electronic_frailty_index"))

omopgenerics::logMessage(message = "> Anemia")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "anemia",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("anemia_broad", "anemia_nutritional"))

omopgenerics::logMessage(message = "> Care requirement")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "care_requirement",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("requirement_for_care")) 

omopgenerics::logMessage(message = "> Diabetes")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "diabetes",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("t1dm", "t2dm"))

omopgenerics::logMessage(message = "> Fragility fracture")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "fragility_fracture",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("fractures"))

omopgenerics::logMessage(message = "> Hearing impairment")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "hearing_impairment",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hearing_impairment")) 

omopgenerics::logMessage(message = "> Heart valve disorder")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "heart_valve_disorder",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("heart_valve_disorder"))

omopgenerics::logMessage(message = "> Housebound")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "housebound",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("housebound"))

omopgenerics::logMessage(message = "> Hypotension syncope")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "hypotension_syncope",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("hypotension")) 

omopgenerics::logMessage(message = "> Memory cognitive disorder")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "memory_cognitive_disorder",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("memory_and_cognitive_problems")) 

omopgenerics::logMessage(message = "> Mobility and transfer problems")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "mobility_problems",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("mobility_and_transfer_problems"))

omopgenerics::logMessage(message = "> Parkinsonism")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "parkinsonism_tremor",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("parkinsonism_tremor"))

omopgenerics::logMessage(message = "> Peptic ulcer")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "peptic_ulcer",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("peptic_ulcer"))

omopgenerics::logMessage(message = "> Peripheral vascular disease")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "peripheral_vascular_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("peripheral_vascular_disease"))

omopgenerics::logMessage(message = "> Respiratory disease")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "respiratory_disease",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("copd", "asthma")) 

omopgenerics::logMessage(message = "> Sleep disturbance")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "sleep_disturbance",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("sleep_disorder"))

omopgenerics::logMessage(message = "> Weight loss anorexia")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "weight_loss_anorexia",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("weight_loss_and_anorexia"))

omopgenerics::logMessage(message = "> Atrial fibrillation")
electronic_frailty_index_codelist <- electronic_frailty_index_codelist |>
  mergeCodelists(newCodelistName = "atrial_fibrillation",
                 keepOriginal = TRUE,
                 codelistsToJoin = c("atrial_fibrilation")) 

omopgenerics::logMessage(message = "> Instantiate cohort")
cdm[["electronic_frailty_index"]] <- conceptCohort(cdm,
                                                   conceptSet = electronic_frailty_index_codelist,
                                                   name = "electronic_frailty_index",
                                                   exit = "event_start_date")


