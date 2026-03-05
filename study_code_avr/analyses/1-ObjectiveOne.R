omopgenerics::logMessage(message = "STARTING OBJECTIVE 1")

omopgenerics::logMessage(message = "Add indications flag")
cdm[["proc_obj_one"]] <- cdm[["proc_obj_one"]] |>
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
cols <- colnames(cdm[["proc_obj_one"]])[which(grepl("indication_", colnames(cdm[["proc_obj_one"]])))]

cdm[["proc_obj_one"]] <- cdm[["proc_obj_one"]] |>
  addCombinations(name = "proc_obj_one") |>
  mutate(across(starts_with("indication_"), ~as.character(.))) |>
  compute(temporary = FALSE, name = "proc_obj_one")

omopgenerics::logMessage(message = "Add calendar year")
cdm[["proc_obj_one"]] <- cdm[["proc_obj_one"]] |>
  mutate("calendar_year" = get_year(cohort_start_date)) |>
  compute(temporary = FALSE, name = "proc_obj_one")

omopgenerics::logMessage(message = "Summarise characteristics")

cols <- colnames(cdm[["proc_obj_one"]] )[which(grepl("indication_", colnames(cdm[["proc_obj_one"]])))]
results[["objective_one"]] <- summariseCharacteristics(cdm[["proc_obj_one"]], 
                                                       demographics = FALSE,
                                                       ageGroup = list(c(0,64), c(65, 150)),
                                                       strata = "calendar_year",
                                                       otherVariables = cols)
omopgenerics::logMessage(message = "OBJECTIVE 1 FINISHED")