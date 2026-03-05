codes <- importCodelist(here::here("cohorts", "codelists"), "csv")

# conditions: AS + AVD
cdm$avd <- conceptCohort(
  cdm = cdm,
  name = "avd",
  conceptSet = list(
    aortic_stenosis = codes$aortic_stenosis,
    aortic_valve_disease = codes$aortic_valve_disease
  ),
  exit = "event_start_date"
) |>
  exitAtObservationEnd()

#  congenital and pediatric subset
cdm$congenital_avd <- cdm$avd |>
  requireAge(ageRange = c(0, 17), name = "congenital_avd") |>
  renameCohort(newCohortName = "congenital_aortic_stenosis", cohortId = "aortic_stenosis") |>
  renameCohort(newCohortName = "congenital_aortic_valve_disease", cohortId = "aortic_valve_disease")

cdm <- bind(
  cdm$avd,
  cdm$congenital_avd,
  name = "study_cohorts"
)