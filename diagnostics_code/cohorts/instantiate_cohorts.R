
study_codes <- importCodelist(here::here("cohorts", "study_codelists"), "csv")
comorbidity_codes <- importCodelist(here::here("cohorts", "comorbidity_codelists"), "csv")

# procedures -----
cdm$aortic_valve_replacement <- conceptCohort(cdm = cdm,
                                              name = "aortic_valve_replacement",
                                              conceptSet = list(aortic_valve_replacement = study_codes$aortic_valve_replacement),
                                              exit = "event_start_date")

# tavi is either based on additional codes for a avr, or from tavi specific codes
cdm$aortic_valve_replacement_potential_tavi <- conceptCohort(cdm = cdm,
                                              name = "aortic_valve_replacement_potential_tavi",
                                              conceptSet = list(aortic_valve_replacement_potential_tavi =
                                                                  study_codes$aortic_valve_replacement_potential_tavi),
                                              exit = "event_start_date")

cdm$tavi_from_additional <- cdm$aortic_valve_replacement_potential_tavi |>
    requireConceptIntersect(conceptSet = list(tavi_additional = study_codes$tavi_additional),
                          window = c(0, 0),
                          name = "tavi_from_additional")
cdm$tavi_direct <- conceptCohort(cdm = cdm,
                                 name = "tavi_direct",
                                 conceptSet = list(tavi = study_codes$tavi),
                                 exit = "event_start_date")

cdm <- bind(cdm$tavi_from_additional,
                 cdm$tavi_direct,
                 name = "tavi")
cdm$tavi <- unionCohorts(cdm$tavi) |>
  renameCohort("tavi")


cdm$savr <- cdm$aortic_valve_replacement |>
  requireCohortIntersect(targetCohortTable = "tavi",
                         window = c(0, 0),
                         intersections = c(0, 0),
                         name = "savr") |>
  renameCohort("savr")

cdm$pediatric_aortic_valve_replacement <- cdm$aortic_valve_replacement |>
  requireAge(ageRange = c(0, 17),
             name = "pediatric_aortic_valve_replacement") |>
  renameCohort(newCohortName = "pediatric_aortic_valve_replacement")


# conditions -----
cdm$avd <- conceptCohort(cdm = cdm,
                    name = "avd",
                    conceptSet = list(aortic_stenosis = study_codes$aortic_stenosis,
                                      aortic_valve_disease = study_codes$aortic_valve_disease,
                                      aortic_insufficiency = study_codes$aortic_insufficiency,
                                      aortic_endocarditis = study_codes$aortic_endocarditis),
                    exit = "event_start_date") |>
  exitAtObservationEnd()

cdm$congenital_avd <- cdm$avd |>
  requireAge(ageRange = c(0, 17),
             name = "congenital_avd") |>
  renameCohort(newCohortName = "congenital_aortic_stenosis",
               cohortId = "aortic_stenosis") |>
  renameCohort(newCohortName = "congenital_aortic_valve_disease",
               cohortId = "aortic_valve_disease") |>
  renameCohort(newCohortName = "congenital_aortic_insufficiency",
               cohortId = "aortic_insufficiency") |>
  renameCohort(newCohortName = "congenital_aortic_endocarditis",
               cohortId = "aortic_endocarditis")

# comorbidities -----
cdm$comorbidities <- conceptCohort(cdm = cdm,
                         name = "comorbidities",
                         conceptSet = comorbidity_codes,
                         exit = "event_start_date") |>
  exitAtObservationEnd()

# bind -----
cdm <- bind(cdm$aortic_valve_replacement,
            cdm$tavi,
            cdm$savr,
            cdm$avd,
            cdm$congenital_avd,
            cdm$comorbidities,
            name = "study_cohorts")

