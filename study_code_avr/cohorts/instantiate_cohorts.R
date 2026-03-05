omopgenerics::logMessage(message = "Importing codelists")
codelist <- omopgenerics::importCodelist(here::here("codelist", "indications"), "csv")

# Indications ----
omopgenerics::logMessage(message = "Instantiating indications")
cdm[["aortic_stenosis"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                             conceptSet = c(codelist["aortic_stenosis"]), 
                                                             name = "aortic_stenosis", 
                                                             exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

cdm[["aortic_insufficiency"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                                  conceptSet = c(codelist["aortic_insufficiency"]), 
                                                                  name = "aortic_insufficiency", 
                                                                  exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

cdm[["aortic_endocarditis"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                                  conceptSet = c(codelist["aortic_endocarditis"]), 
                                                                  name = "aortic_endocarditis", 
                                                                  exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]], name = "indications")

omopgenerics::logMessage(message = "Incidence cohorts") # ----
cdm[["incidence_aortic_stenosis"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                                       conceptSet = c(codelist["aortic_stenosis"]), 
                                                                       name = "incidence_aortic_stenosis", 
                                                                       exit = "event_start_date")

# Aortic valve replacement -----
omopgenerics::logMessage(message = "Instantiating AVR")

omopgenerics::logMessage(message = "Incidence cohorts") # ----
codelist <- importCodelist(path = here("codelist"), type = "csv")

cdm$incidence_avr <- CohortConstructor::conceptCohort(cdm = cdm, 
                                            conceptSet = c(codelist["aortic_valve_replacement"]), 
                                            name = "incidence_avr", 
                                            exit = "event_start_date") 

cdm$incidence_tavi_from_additional <- cdm$incidence_avr |>
  requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                          window = c(0, 0),
                          name = "incidence_tavi_from_additional")

cdm$incidence_tavi_direct <- conceptCohort(cdm = cdm,
                                 name = "incidence_tavi_direct",
                                 conceptSet = codelist["tavi"],
                                 exit = "event_start_date") 

cdm <- bind(cdm$incidence_tavi_from_additional,
            cdm$incidence_tavi_direct,
            name = "incidence_tavi")

cdm$incidence_tavi <- unionCohorts(cdm$incidence_tavi) |>
  renameCohort("incidence_tavi")

cdm$incidence_savr <- cdm$incidence_avr |>
  requireCohortIntersect(targetCohortTable = "incidence_tavi",
                         window = c(0, 0),
                         intersections = c(0, 0),
                         name = "incidence_savr") |>
  renameCohort("incidence_savr")

if (dbName == "CPRD GOLD") {
  cdm <- bind(cdm[["incidence_savr"]], cdm[["incidence_tavi"]], cdm[["incidence_avr"]], name = "incidence_procedures")
} else {
  cdm <- bind(cdm[["incidence_savr"]], cdm[["incidence_tavi"]], name = "incidence_procedures")
}

cdm$incidence_procedures <- cdm$incidence_procedures |>
  requireCohortIntersect(targetCohortTable = "incidence_aortic_stenosis",
                         window = c(-Inf, 0),
                         intersections = c(1, Inf),
                         name = "incidence_procedures") 

omopgenerics::logMessage(message = "Objective 1") # ----
# TAVI cohort
cdm$avr <- CohortConstructor::conceptCohort(cdm = cdm, 
                                            conceptSet = c(codelist["aortic_valve_replacement"]), 
                                            name = "avr", 
                                            exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

cdm$tavi_from_additional <- cdm$avr |>
  requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                          window = c(0, 0),
                          name = "tavi_from_additional")

cdm$tavi_direct <- conceptCohort(cdm = cdm,
                                 name = "tavi_direct",
                                 conceptSet = codelist["tavi"],
                                 exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

cdm <- bind(cdm$tavi_from_additional,
            cdm$tavi_direct,
            name = "tavi")

cdm$tavi <- unionCohorts(cdm$tavi) |>
  renameCohort("tavi")

cdm$savr <- cdm$avr |>
  requireCohortIntersect(targetCohortTable = "tavi",
                         window = c(0, 0),
                         intersections = c(0, 0),
                         name = "savr") |>
  renameCohort("savr")

if (dbName == "CPRD GOLD") {
  cdm <- bind(cdm[["savr"]], cdm[["tavi"]], cdm[["avr"]], name = "proc_obj_one")
} else {
  cdm <- bind(cdm[["savr"]], cdm[["tavi"]], name = "proc_obj_one")
}

omopgenerics::logMessage(message = "Other objectives")
cdm[["proc"]] <- cdm[["proc_obj_one"]] |>
  copyCohorts(name = "proc")

cdm[["proc"]] <- cdm[["proc"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis",
                         window = c(-Inf, 0),
                         intersections = c(1, Inf))

cdm[["proc"]] <- cdm[["proc"]] |>
  addDemographics(age = TRUE,
                  ageName = "age",
                  ageGroup = age_groups, 
                  sex = TRUE, 
                  priorObservation = FALSE, 
                  futureObservation = FALSE)

# Comorbidities
omopgenerics::logMessage(message = "Instantiating comorbidities")
codelist <- omopgenerics::importCodelist(here::here("codelist", "comorbidities"), "csv")

cdm$comorbidities <- conceptCohort(cdm = cdm,
                                   subsetCohort = "proc",
                                   name = "comorbidities",
                                   conceptSet = codelist,
                                   exit = "event_start_date")

cdm$comorbidities <- cdm$comorbidities |> 
  exitAtObservationEnd(cohortId = c("type_2_diabetes", "hypertension", "concomitant_valve_disorders_excluding_endocarditis"))

# Treatments
omopgenerics::logMessage(message = "Instantiating treatments")
codelist <- omopgenerics::importCodelist(here::here("codelist", "treatments"), "csv")
cdm[["treatments"]] <- cdm |>
  CohortConstructor::conceptCohort(
    subsetCohort = "proc",
    conceptSet = codelist,
    exit = "event_start_date",
    name = "treatments")

omopgenerics::logMessage(message = "Finished instantiating cohorts")
