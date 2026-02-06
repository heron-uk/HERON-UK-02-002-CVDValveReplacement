
codes <- importCodelist(here::here("cohorts", "codelists"), "csv")

cdm$aortic_valve_replacement <- conceptCohort(cdm = cdm,
                                              name = "aortic_valve_replacement",
                                              conceptSet = list(aortic_valve_replacement = codes$aortic_valve_replacement),
                                              exit = "event_start_date")

cdm$tavi <- cdm$aortic_valve_replacement |>
  requireConceptIntersect(conceptSet = list(additional_tavi_route = codes$additional_tavi_route),
                          window = c(0, 0),
                          name = "tavi") |>
  requireConceptIntersect(conceptSet = list(additional_tavi_context = codes$additional_tavi_context),
                          window = c(0, 0),
                          name = "tavi") |>
  renameCohort("tavi")

cdm$savr <- cdm$aortic_valve_replacement |>
  requireCohortIntersect(targetCohortTable = "tavi",
                         window = c(0, 0),
                         intersections = c(0, 0),
                         name = "savr") |>
  renameCohort("savr")

cdm <- bind(cdm$aortic_valve_replacement,
            cdm$tavi,
            cdm$savr,
            name = "study_cohorts")
