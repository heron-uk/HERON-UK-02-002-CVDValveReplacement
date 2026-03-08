omopgenerics::logMessage(message = "STARTING OBJECTIVES 3 AND 4")

omopgenerics::logMessage(message = "Add hospital frailty risk score")
cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_plus_aortic_stenosis"]] |>
  addCohortIntersectFlag(targetCohortTable = "hospital_frailty_risk_score", 
                         window = c(-365,0), 
                         nameStyle = "{cohort_name}")  # Check window

scores <- read_csv(here("codelist", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score.csv")) |>
  mutate("icd_description" = toSnakeCase(icd_description)) |>
  mutate("cohort_name" = case_when(
    icd_description == "personal_history_of_risk_factors_not_elsewhere_classified" ~ "personal_history_of_risk_factors",
    icd_description == "other_disorders_of_urinary_system_includes_urinary_tract_infection_and_urinary_incontinence" ~ "other_disorders_of_urinary_system",
    icd_description == "problems_related_to_medical_facilities_and_other_health_care" ~ "problems_related_to_medical_facilities",
    icd_description == "other_symptoms_and_signs_involving_cognitive_functions_and_awareness" ~ "cognitive_functions_and_awareness",
    icd_description == "other_symptoms_and_signs_involving_the_nervous_and_musculoskeletal_systems_r29_6_tendency_to_fall" ~ "nervous_and_musculoskeletal_systems",
    icd_description == "other_degenerative_diseases_of_nervous_system_not_elsewhere_classified" ~ "degenerative_diseases_of_nervous_system",
    icd_description == "diarrhoea_and_gastroenteritis_of_presumed_infectious_origin" ~ "diarrhoea_and_gastroenteritis",
    icd_description == "other_disorders_of_fluid_electrolyte_and_acid_base_balance" ~ "fluid_electrolyte_and_acid_base_balance",
    icd_description == "other_disorders_of_kidney_and_ureter_not_elsewhere_classified" ~ "disorders_of_kidney_and_ureter",
    icd_description == "complications_of_genitourinary_prosthetic_devices_implants_and_grafts" ~ "prosthetic_devices_implants",
    icd_description == "delirium_not_induced_by_alcohol_and_other_psychoactive_substances" ~ "delirium",
    icd_description == "fall_on_same_level_from_slipping_tripping_and_stumbling" ~ "fall",
    icd_description == "mental_and_behavioural_disorders_due_to_use_of_alcohol" ~ "mental_and_behavioural_disorders",
    icd_description == "other_bacterial_agents_as_the_cause_of_diseases_classified_to_other_chapters_secondary_code" ~ "other_bacterial_agents",
    icd_description == "other_local_infections_of_skin_and_subcutaneous_tissue" ~ "infections_of_skin",
    icd_description == "other_medical_procedures_as_the_cause_of_abnormal_reaction_of_the_patient" ~ "other_medical_procedures",
    icd_description == "other_symptoms_and_signs_involving_general_sensations_and_perceptions" ~ "general_sensations_and_perceptions",
    icd_description == "streptococcus_and_staphylococcus_as_the_cause_of_diseases_classified_to_other_chapters" ~ "streptococcus_and_staphylococcus",
    icd_description == "transient_cerebral_ischaemic_attacks_and_related_syndromes" ~ "cerebral_ischaemic_attacks",
    .default = icd_description
  ))

for (i in scores$cohort_name ){
  points <- scores |>
    filter(cohort_name == i) |>
    pull("points")
  
  cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_plus_aortic_stenosis"]] |>
    mutate(!!i := .data[[i]]*points) 
}

cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_plus_aortic_stenosis"]] |>
  compute(temporary = FALSE, name = "procedures_plus_aortic_stenosis")

cols_to_exclude <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
cols_to_sum <- setdiff(colnames(cdm[["procedures_plus_aortic_stenosis"]] ), cols_to_exclude)

quoted <- DBI::dbQuoteIdentifier(db, cols_to_sum)
quoted_chr <- as.character(quoted)
expr_str <- paste0("(", paste0("COALESCE(", quoted_chr, ", 0)", collapse = " + "), ")")

cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_plus_aortic_stenosis"]] |>
  mutate("hospital_frailty_risk_score" = !!dbplyr::sql(expr_str)) |>
  select(all_of(cols_to_exclude), "hospital_frailty_risk_score") |>
  compute(temporary = FALSE, name = "hospital_frailty_risk_score")

omopgenerics::logMessage(message = "Add demographics")
cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_plus_aortic_stenosis"]] |>
  addDemographics(age = TRUE,
                  ageName = "age",
                  ageGroup = age_groups, 
                  sex = TRUE, 
                  priorObservation = FALSE, 
                  futureObservation = FALSE)


cdm[["procedures_plus_aortic_stenosis"]] <- cdm[["procedures_plus_aortic_stenosis"]] |>
  mutate("calendar_year" = get_year(cohort_start_date))

results[["objective_three_and_four"]] <- summariseCharacteristics(cdm[["procedures_plus_aortic_stenosis"]], 
                                                                  strata = list("sex", "age_group", "calendar_year", c("calendar_year", "sex"), c("calendar_year", "age_group")),
                                                                  cohortIntersectFlag = 
                                                                    list(
                                                                      "Aortic valve disease phenotype" = list(
                                                                        targetCohortTable = "aortic_valve_disease_phenotype",
                                                                        window = c(0, 0)
                                                                      ),
                                                                      "Cardiovascular disease (one year prior to index date)" = list(
                                                                        targetCohortTable = "cardiovascular_disease",
                                                                        window = c(-365, 0)
                                                                      ),
                                                                      "Cardiovascular risk factors" = list(
                                                                        targetCohortTable = "cardiovascular_disease",
                                                                        window = c(0, 0)
                                                                      ),
                                                                      "Previous comorbidities (one year prior to index date)" = list(
                                                                        targetCohortTable = "cardiovascular_disease",
                                                                        window = c(-365, 0)
                                                                      ),
                                                                      "Treatments (one month prior to index date)" = list(
                                                                        targetCohortTable = "treatments",
                                                                        window = c(-31, 0)
                                                                      )))

omopgenerics::logMessage(message = "OBJECTIVES 3 AND 4 FINISHED")
