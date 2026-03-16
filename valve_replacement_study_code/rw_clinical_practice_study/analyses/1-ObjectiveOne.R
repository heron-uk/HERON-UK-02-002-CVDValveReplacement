omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

# Combination cohorts ----
omopgenerics::logMessage(message = "Stenosis + insufficiency")
cdm[["aortic_stenosis_insufficiency"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency", 
                         window = c(0,0),
                         intersections = 1,
                         name = "aortic_stenosis_insufficiency") |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis", 
                         window = c(0,0),
                         intersections = 0,
                         name = "aortic_stenosis_insufficiency")  |>
  renameCohort(newCohortName = "aortic_stenosis_insufficiency")

omopgenerics::logMessage(message = "Stenosis + endocarditis")
cdm[["aortic_stenosis_endocarditis"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency", 
                         window = c(0,0),
                         intersections = 0,
                         name = "aortic_stenosis_endocarditis") |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis", 
                         window = c(0,0),
                         intersections = 1,
                         name = "aortic_stenosis_endocarditis") |>
  renameCohort(newCohortName = "aortic_stenosis_endocarditis")

omopgenerics::logMessage(message = "Insufficiency + endocarditis")
cdm[["aortic_insufficiency_endocarditis"]] <- cdm[["aortic_insufficiency"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis", 
                         window = c(0,0),
                         intersections = 0,
                         name = "aortic_insufficiency_endocarditis") |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis", 
                         window = c(0,0),
                         intersections = 1,
                         name = "aortic_insufficiency_endocarditis") |>
  renameCohort(newCohortName = "aortic_insufficiency_endocarditis")

omopgenerics::logMessage(message = "Stenosis + insufficiency + endocarditis")
cdm[["aortic_stenosis_insufficiency_endocarditis"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency", 
                         window = c(0,0),
                         intersections = 1,
                         name = "aortic_stenosis_insufficiency_endocarditis") |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis", 
                         window = c(0,0),
                         intersections = 1,
                         name = "aortic_stenosis_insufficiency_endocarditis") |>
  renameCohort(newCohortName = "aortic_stenosis_insufficiency_endocarditis")

omopgenerics::logMessage(message = "Remove stenosis diagnosis that have other indications later on")
cdm[["aortic_stenosis"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency", 
                         window = c(0, 365),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis", 
                         window = c(0, 365),
                         intersections = 0) 

omopgenerics::logMessage(message = "Remove insufficiency diagnosis that have other indications later on")
cdm[["aortic_insufficiency"]] <- cdm[["aortic_insufficiency"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis", 
                         window = c(0, 365),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis", 
                         window = c(0, 365),
                         intersections = 0)

omopgenerics::logMessage(message = "Remove endocarditis diagnosis that have other indications later on")
cdm[["aortic_endocarditis"]] <- cdm[["aortic_endocarditis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis", 
                         window = c(0, 365),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency", 
                         window = c(0, 365),
                         intersections = 0)

# Create no indication identified cohort ----
omopgenerics::logMessage(message = "Creating no indication identified cohort")
cdm[["no_indication_identified"]] <- copyCohorts(cdm[["aortic_valve_replacement"]], 
                                                 name = "no_indication_identified") |>
  newCohortTable(cohortSetRef = settings(cdm[["aortic_valve_replacement"]]),
                 cohortAttritionRef = attrition(cdm[["aortic_valve_replacement"]])) |>
  renameCohort(newCohortName = "no_indication_identified")

cdm[["no_indication_identified"]] <- cdm[["no_indication_identified"]] |>
  renameCohort(newCohortName = "no_indication_identified") |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis",
                         window = c(-365, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency",
                         window = c(-365, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis",
                         window = c(-365, 0), 
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
                                                       strata = list("calendar_year", "age_group", "sex", c("calendar_year", "age_group"), c("calendar_year", "sex")),
                                                       cohortIntersectFlag = list(
                                                         "Indications" = list("targetCohortTable" = "indications",
                                                                              "window" = c(-365, 0),
                                                                              "nameStyle" = "{cohort_name}")
                                                       ))

omopgenerics::logMessage(message = "Summarising cohort attrition")
results[["objective_one_attrition_indications"]] <- summariseCohortAttrition(cdm[["indications"]])
results[["objective_one_attrition_procedures"]]  <- summariseCohortAttrition(cdm[["procedures"]])

omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
