source(here::here("analyses", "functions.R"))

omopgenerics::logMessage(message = "Importing codelists")
codelist <- omopgenerics::importCodelist(here::here("codelist"), "csv")

# Indications ----
omopgenerics::logMessage(message = "Instantiating indications")
cdm[["aortic_stenosis"]] <- CohortConstructor::conceptCohort(cdm = cdm, 
                                                             conceptSet = c(codelist["aortic_stenosis"]), 
                                                             name = "aortic_stenosis", 
                                                             exit = "event_start_date") |>
  CohortConstructor::requireIsFirstEntry() |>
  CohortConstructor::exitAtObservationEnd() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

# Aortic valve replacement -----
omopgenerics::logMessage(message = "Instantiating AVR")
omopgenerics::logMessage(message = "- Objective 1")
cdm$avr <- CohortConstructor::conceptCohort(cdm = cdm, 
                                            conceptSet = c(codelist["aortic_valve_replacement"]), 
                                            name = "avr", 
                                            exit = "event_start_date") |>
  CohortConstructor::requireIsLastEntry() |>
  CohortConstructor::requireInDateRange(dateRange = study_period)

# TAVI cohort
cdm$tavi_from_additional <- cdm$avr |>
  requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                          window = c(0, 0),
                          name = "tavi_from_additional")

cdm$tavi_direct <- conceptCohort(cdm = cdm,
                                 name = "tavi_direct",
                                 conceptSet = codelist["tavi"],
                                 exit = "event_start_date")

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

cdm <- bind(cdm[["savr"]], cdm[["tavi"]], cdm[["avr"]], name = "proc_obj_one")

omopgenerics::logMessage(message = "- Other objectives")
cdm[["proc"]] <- cdm[["proc_obj_one"]] |>
  copyCohorts(name = "proc")

cdm[["proc"]] <- cdm[["proc"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis",
                         window = c(-Inf, 0))

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