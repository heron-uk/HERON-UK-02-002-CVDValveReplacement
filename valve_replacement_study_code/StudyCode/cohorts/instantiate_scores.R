# Instantiate charlson comorbidity index
omopgenerics::logMessage(message = "Instantiate Charlson Comorbidity Index")
charlson_comorbidity_index_codelist <- importCodelist(here("cohorts", "study_codelists", "charlson_comorbidity_index"))

cdm[["charlson_comorbidity_index"]] <- conceptCohort(cdm,
                                                     conceptSet = charlson_comorbidity_index_codelist,
                                                     name = "charlson_comorbidity_index",
                                                     exit = "event_start_date")


# Instantiate electronic frailty index
omopgenerics::logMessage(message = "Instantiate Electronic Frailty Index")
electronic_frailty_index_codelist <- importCodelist(here("cohorts", "study_codelists", "electronic_frailty_index"))

cdm[["electronic_frailty_index"]] <- conceptCohort(cdm,
                                                   conceptSet = electronic_frailty_index_codelist,
                                                   name = "electronic_frailty_index",
                                                   exit = "event_start_date")