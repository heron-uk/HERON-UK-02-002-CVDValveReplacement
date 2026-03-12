omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

# Create no indication identified cohort ----
omopgenerics::logMessage(message = "Creating no indication identified cohort")
cdm[["no_indication_identified"]] <- copyCohorts(cdm[["aortic_valve_replacement"]], 
                                                 name = "no_indication_identified") |>
  newCohortTable(cohortSetRef = settings(cdm[["aortic_valve_replacement"]]),
                 cohortAttritionRef = attrition(cdm[["aortic_valve_replacement"]]))

cdm[["no_indication_identified"]] <- cdm[["no_indication_identified"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis",
                         window = c(-365, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency",
                         window = c(-365, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis",
                         window = c(-365, 0), 
                         intersections = c(0))

# Create combination cohorts ----
omopgenerics::logMessage(message = "Creating combination cohorts - stenosis + insufficiency + endocarditis")
cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]], name = "aortic_stenosis_insufficiency_endocarditis")
cdm[["aortic_stenosis_insufficiency_endocarditis"]] <- cdm[["aortic_stenosis_insufficiency_endocarditis"]] |>
  intersectCohorts(gap = 0)

omopgenerics::logMessage(message = "Creating combination cohorts - stenosis + insufficiency")
cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], name = "aortic_stenosis_insufficiency")
cdm[["aortic_stenosis_insufficiency"]] <- cdm[["aortic_stenosis_insufficiency"]] |>
  intersectCohorts(gap = 0) |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0))

omopgenerics::logMessage(message = "Creating combination cohorts - stenosis + endocarditis")
cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_endocarditis"]], name = "aortic_stenosis_endocarditis")
cdm[["aortic_stenosis_endocarditis"]] <- cdm[["aortic_stenosis_endocarditis"]] |>
  intersectCohorts(gap = 0) |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0))

omopgenerics::logMessage(message = "Creating combination cohorts - insufficiency + endocarditis")
cdm <- bind(cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]], name = "aortic_insufficiency_endocarditis")
cdm[["aortic_insufficiency_endocarditis"]] <- cdm[["aortic_insufficiency_endocarditis"]] |>
  intersectCohorts(gap = 0) |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0))

# Isolate cohorts (remove combinations from original cohorts) ----
omopgenerics::logMessage(message = "Creating stenosis isolated cohort")
cdm[["aortic_stenosis"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency"]], 
                         window = c(0,0), 
                         intersections = c(0)) |>
  requireCohortIntersect(cdm[["aortic_stenosis_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0)) |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0))

omopgenerics::logMessage(message = "Creating insufficiency isolated cohort")
cdm[["aortic_insufficiency"]] <- cdm[["aortic_insufficiency"]] |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency"]], 
                         window = c(0,0), 
                         intersections = c(0)) |>
  requireCohortIntersect(cdm[["aortic_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0)) |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0))

omopgenerics::logMessage(message = "Creating endocarditis isolated cohort")
cdm[["aortic_endocarditis"]] <- cdm[["aortic_endocarditis"]] |>
  requireCohortIntersect(cdm[["aortic_stenosis_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0)) |>
  requireCohortIntersect(cdm[["aortic_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0)) |>
  requireCohortIntersect(cdm[["aortic_stenosis_insufficiency_endocarditis"]], 
                         window = c(0,0), 
                         intersections = c(0))

cdm <- bind(cdm[["aortic_stenosis"]], cdm[["aortic_insufficiency"]], cdm[["aortic_endocarditis"]],
            cdm[["aortic_stenosis_insufficiency"]], cdm[["aortic_stenosis_endocarditis"]], cdm[["aortic_insufficiency_endocarditis"]], 
            cdm[["aortic_stenosis_insufficiency_endocarditis"]], 
            cdm[["no_indication_identified"]], 
            name = "indications")

# Get calendar year
omopgenerics::logMessage(message = "Add calendar year")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  mutate("calendar_year" = get_year(cohort_start_date)) |>
  compute(temporary = FALSE, name = "procedures")

omopgenerics::logMessage(message = "Add age and sex")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  addAge(ageGroup = age_groups) |>
  addSex()

omopgenerics::logMessage(message = "Summarise characteristics - on procedures")
results[["objective_one"]] <- summariseCharacteristics(cdm[["procedures"]], 
                                                       demographics = FALSE, 
                                                       strata = list("calendar_year", c("calendar_year", "age_group"), c("calendar_year", "sex")),
                                                       cohortIntersectFlag = list(
                                                         "Indications" = list("targetCohortTable" = "indications",
                                                                              "window" = c(-365, 0),
                                                                              "nameStyle" = "{cohort_name}")
                                                       ))

omopgenerics::logMessage(message = "Summarise characteristics - on indications")
results[["objective_one_aortic_stenosis"]] <- summariseCharacteristics(cdm[["indications"]], 
                                                                       demographics = FALSE, 
                                                                       strata = list("calendar_year", c("calendar_year", "age_group"), c("calendar_year", "sex")),
                                                                       cohortIntersectFlag = list(
                                                                         "Procedures" = list("targetCohortTable" = "procedures",
                                                                                             "window" = c(0, 365),
                                                                                             "nameStyle" = "{cohort_name}")
                                                                       ))

omopgenerics::logMessage(message = "Summarising cohort attrition")
results[["objective_one_attrition_indications"]] <- summariseCohortAttrition(cdm[["indications"]])
results[["objective_one_attrition_procedures"]]  <- summariseCohortAttrition(cdm[["procedures"]])

omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
