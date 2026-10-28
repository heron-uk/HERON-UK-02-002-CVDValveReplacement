omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

# Combination cohorts ----
omopgenerics::logMessage(message = "Aortic stenosis + insufficiency")
cdm[["aortic_stenosis_insufficiency"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency", 
                         window = c(-Inf,Inf),
                         intersections = c(1, Inf),
                         name = "aortic_stenosis_insufficiency") 

cdm <- bind(cdm[["aortic_stenosis_insufficiency_codes"]], cdm[["aortic_stenosis_insufficiency"]], 
            name = "aortic_stenosis_insufficiency")
cdm[["aortic_stenosis_insufficiency"]] <- cdm[["aortic_stenosis_insufficiency"]] |>
  unionCohorts(cohortName = "aortic_stenosis_insufficiency")

omopgenerics::logMessage(message = "Aortic Stenosis isolated")
cdm[["aortic_stenosis_isolated"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(cdm[])
  copyCohorts(name = "aortic_stenosis_isolated")

cdm[["aortic_stenosis_isolated"]] <- cdm[["aortic_stenosis_isolated"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency",
                         window = c(-Inf,Inf),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_insufficiency",
                         window = c(-Inf,Inf),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis",
                         window = c(-Inf,Inf),
                         intersections = 0) 

omopgenerics::logMessage(message = "Aortic insufficiency isolated")
cdm[["aortic_insufficiency_isolated"]] <- cdm[["aortic_insufficiency"]] |>
  copyCohorts(name = "aortic_insufficiency_isolated")

cdm[["aortic_insufficiency_isolated"]] <- cdm[["aortic_insufficiency_isolated"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis",
                         window = c(-Inf,Inf),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_insufficiency",
                         window = c(-Inf,Inf),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis",
                         window = c(-Inf,Inf),
                         intersections = 0) 

omopgenerics::logMessage(message = "Aortic stenosis + insufficiency isolated")
cdm[["aortic_stenosis_insufficiency_isolated"]] <- cdm[["aortic_stenosis_insufficiency"]] |>
  copyCohorts(name = "aortic_stenosis_insufficiency_isolated")

cdm[["aortic_stenosis_insufficiency_isolated"]] <- cdm[["aortic_stenosis_insufficiency_isolated"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis",
                         window = c(-Inf,Inf),
                         intersections = 0) 

omopgenerics::logMessage(message = "Endocarditis isolated")
cdm[["aortic_endocarditis_isolated"]] <- cdm[["aortic_endocarditis"]] |>
  copyCohorts(name = "aortic_endocarditis_isolated")

cdm[["aortic_endocarditis_isolated"]] <- cdm[["aortic_endocarditis_isolated"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis",
                         window = c(-Inf,Inf),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_insufficiency",
                         window = c(-Inf,Inf),
                         intersections = 0) |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency",
                         window = c(-Inf,Inf),
                         intersections = 0) 

omopgenerics::logMessage(message = "Aortic valve disorders with endocarditis")
cdm[["avd_with_endocarditis_isolated"]] <- cdm[["aortic_endocarditis"]] |>
  copyCohorts(name = "avd_with_endocarditis_isolated")

cdm[["avd_with_endocarditis_isolated"]] <- cdm[["avd_with_endocarditis_isolated"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis_isolated",
                         window = c(-Inf, Inf),
                         intersections = 0) 

# Create no indication identified cohort ----
omopgenerics::logMessage(message = "Creating no indication identified cohort")
cdm[["no_indication_identified"]] <- copyCohorts(cdm[["aortic_valve_replacement"]], 
                                                 name = "no_indication_identified") |>
  renameCohort(newCohortName = "no_indication_identified")

cdm[["no_indication_identified"]] <- cdm[["no_indication_identified"]] |>
  renameCohort(newCohortName = "no_indication_identified") |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_isolated",
                         window = c(-Inf, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "aortic_insufficiency_isolated",
                         window = c(-Inf, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "aortic_endocarditis_isolated",
                         window = c(-Inf, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "aortic_stenosis_insufficiency_isolated",
                         window = c(-Inf, 0), 
                         intersections = c(0)) |>
  requireCohortIntersect(targetCohortTable = "avd_with_endocarditis_isolated",
                         window = c(-Inf, 0), 
                         intersections = c(0))

cdm <- bind(cdm[["aortic_stenosis_isolated"]], cdm[["aortic_insufficiency_isolated"]], cdm[["aortic_endocarditis_isolated"]],
            cdm[["aortic_stenosis_insufficiency_isolated"]], cdm[["avd_with_endocarditis_isolated"]], 
            cdm[["no_indication_identified"]], 
            name = "indications")

# Get calendar year
omopgenerics::logMessage(message = "Summarise characteristics - on procedures")
results[[paste0("objective_one_",window)]] <- summariseCharacteristics(cdm[["procedures"]], 
                                                                       demographics = TRUE, 
                                                                       ageGroup = age_groups,
                                                                       strata = list("calendar_year", "age_group", "sex", 
                                                                     c("calendar_year", "age_group"), 
                                                                     c("calendar_year", "sex")),
                                                       cohortIntersectFlag = list(
                                                         "Indications" = list("targetCohortTable" = "indications",
                                                                              "window" = c(-Inf, 0),
                                                                              "nameStyle" = "{cohort_name}")))

omopgenerics::logMessage(message = "Summarising cohort attrition")
results[[paste0("objective_one_attrition_indications_", window)]] <- summariseCohortAttrition(cdm[["indications"]])

omopgenerics::logMessage(message = "Create indication aortic stenosis or aortic stenosis+insuficiency cohort")
cdm <- bind(cdm[["aortic_stenosis_isolated"]], cdm[["aortic_stenosis_insufficiency_isolated"]], name = "aortic_stenosis_indication")
cdm[["aortic_stenosis_indication"]] <- cdm[["aortic_stenosis_indication"]] |>
  unionCohorts()
omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
