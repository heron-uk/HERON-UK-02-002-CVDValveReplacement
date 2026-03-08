omopgenerics::logMessage(message = "STARTING OBJECTIVE 1")

omopgenerics::logMessage(message = "Add indications flag")
cdm[["procedures_objective_one"]] <- cdm[["procedures_objective_one"]] |>
  addCohortIntersectFlag(targetCohortTable = "aortic_stenosis", 
                         window = c(-Inf, 0), 
                         nameStyle = "indication_{cohort_name}") |>
  addCohortIntersectFlag(targetCohortTable = "aortic_insufficiency", 
                         window = c(-Inf, 0), 
                         nameStyle = "indication_{cohort_name}") |>
  addCohortIntersectFlag(targetCohortTable = "aortic_endocarditis", 
                         window = c(-Inf, 0), 
                         nameStyle = "indication_{cohort_name}") |>
  mutate(across(starts_with("indication"), ~as.integer(.)))

omopgenerics::logMessage(message = "Add indications combinations flag")
cols <- colnames(cdm[["procedures_objective_one"]])[which(grepl("indication_", colnames(cdm[["procedures_objective_one"]])))]

cdm[["procedures_objective_one"]] <- cdm[["procedures_objective_one"]] |>
  addCombinations(name = "procedures_objective_one") |>
  mutate(across(starts_with("indication_"), ~as.character(.))) |>
  compute(temporary = FALSE, name = "procedures_objective_one")

omopgenerics::logMessage(message = "Add calendar year")
cdm[["procedures_objective_one"]] <- cdm[["procedures_objective_one"]] |>
  mutate("calendar_year" = get_year(cohort_start_date)) |>
  compute(temporary = FALSE, name = "procedures_objective_one")

omopgenerics::logMessage(message = "Summarise characteristics")

cols <- colnames(cdm[["procedures_objective_one"]] )[which(grepl("indication_", colnames(cdm[["procedures_objective_one"]])))]
results[["objective_one"]] <- summariseCharacteristics(cdm[["procedures_objective_one"]], 
                                                       demographics = FALSE, 
                                                       strata = "calendar_year",
                                                       otherVariables = cols)
omopgenerics::logMessage(message = "OBJECTIVE 1 FINISHED")