
codes <- importCodelist(here::here("cohorts", "codelists"), "csv")
# procedures
cdm$aortic_valve_replacement <- conceptCohort(cdm = cdm,
                                              name = "aortic_valve_replacement",
                                              conceptSet = list(aortic_valve_replacement = codes$aortic_valve_replacement),
                                              exit = "event_start_date")

# tavi is either based on additional codes for a avr, or from tavi specific codes
cdm$tavi_from_additional <- cdm$aortic_valve_replacement |>
    requireConceptIntersect(conceptSet = list(tavi_additional = codes$tavi_additional),
                          window = c(0, 0),
                          name = "tavi_from_additional")
cdm$tavi_direct <- conceptCohort(cdm = cdm,
                                 name = "tavi_direct",
                                 conceptSet = list(tavi = codes$tavi),
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

# conditions
cdm$avd <- conceptCohort(cdm = cdm,
                    name = "avd",
                    conceptSet = list(aortic_stenosis = codes$aortic_stenosis,
                                      aortic_valve_disease = codes$aortic_valve_disease),
                    exit = "event_start_date") |>
  exitAtObservationEnd()

cdm$congenital_avd <- cdm$avd |>
  requireAge(ageRange = c(0, 17),
             name = "congenital_avd")
cdm$congenital_avd <- cdm$congenital_avd |>
  renameCohort(newCohortName = "congenital_aortic_stenosis",
               cohortId = "aortic_stenosis") |>
  renameCohort(newCohortName = "congenital_aortic_valve_disease",
               cohortId = "aortic_valve_disease")

cdm <- bind(cdm$aortic_valve_replacement,
            cdm$tavi,
            cdm$savr,
            cdm$avd,
            cdm$congenital_avd,
            name = "study_cohorts")
