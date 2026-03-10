omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

# Create combinations ----
cdm[["aortic_stenosis_insufficiency"]] <- cdm[["indications"]] |> 
  intersectCohorts(cohortId = c("aortic_stenosis_avr", "aortic_insufficiency_avr"),
                   keepOriginalCohort = FALSE,
                   name =  "aortic_stenosis_insufficiency") 

cdm[["aortic_stenosis_endocarditis"]] <- cdm[["indications"]] |> 
  intersectCohorts(cohortId = c("aortic_stenosis_avr", "aortic_endocarditis_avr"),
                   keepOriginalCohort = FALSE,
                   name =  "aortic_stenosis_endocarditis") 

cdm[["aortic_insufficiency_endocarditis"]] <- cdm[["indications"]] |>
  intersectCohorts(cohortId = c("aortic_insufficiency_avr", "aortic_endocarditis_avr"),
                   keepOriginalCohort = FALSE,
                   name =  "aortic_insufficiency_endocarditis") 

cdm[["aortic_stenosis_insufficiency_endocarditis"]] <- cdm[["indications"]] |>
  intersectCohorts(cohortId = c("aortic_stenosis_avr", "aortic_insufficiency_avr", "aortic_endocarditis_avr"),
                   keepOriginalCohort = FALSE,
                   name =  "aortic_stenosis_insufficiency_endocarditis") 

cdm <- bind(cdm[["indications"]], cdm[["aortic_stenosis_insufficiency"]], cdm[["aortic_stenosis_endocarditis"]],
            cdm[["aortic_insufficiency_endocarditis"]], cdm[["aortic_stenosis_insufficiency_endocarditis"]],
            name = "indications")

# Isolate unique diagnostics
cdm[["indications"]] <- cdm[["indications"]] |>
  requireCohortIntersect(targetCohortTable = "indications", 
                         cohortId = "aortic_stenosis_avr", 
                         targetCohortId = "aortic_insufficiency_avr",
                         window = c(-Inf, Inf),
                         intersections = c(0,0)) |>
  requireCohortIntersect(targetCohortTable = "indications", 
                         cohortId = "aortic_stenosis_avr", 
                         targetCohortId = "aortic_endocarditis_avr",
                         window = c(-Inf, Inf),
                         intersections = c(0,0)) |>
  requireCohortIntersect(targetCohortTable = "indications", 
                         cohortId = "aortic_insufficiency_avr", 
                         targetCohortId = "aortic_endocarditis_avr",
                         window = c(-Inf, Inf),
                         intersections = c(0,0)) 

# Get calendar year
omopgenerics::logMessage(message = "Add calendar year")
cdm[["procedures_objective_one"]] <- cdm[["procedures_objective_one"]] |>
  mutate("calendar_year" = get_year(cohort_start_date)) |>
  compute(temporary = FALSE, name = "procedures_objective_one")

omopgenerics::logMessage(message = "Add age and sex")
cdm[["procedures_objective_one"]] <- cdm[["procedures_objective_one"]] |>
  addAge(ageGroup = age_groups) |>
  addSex()
  
omopgenerics::logMessage(message = "Summarise characteristics")
results[["objective_one"]] <- summariseCharacteristics(cdm[["procedures_objective_one"]], 
                                                       demographics = FALSE, 
                                                       strata = list("calendar_year", c("calendar_year", "age_group"), c("calendar_year", "sex")),
                                                       cohortIntersectFlag = list(
                                                         "Indications" = list("targetCohortTable" = "indications",
                                                                              "window" = c(-Inf, 0),
                                                                              "nameStyle" = "{cohort_name}")
                                                       ))

results[["objective_one_attrition_indications"]] <- summariseCohortAttrition(cdm[["indications"]])
results[["objective_one_attrition_procedures"]]  <- summariseCohortAttrition(cdm[["procedures_objective_one"]])

# Calculate no indication identified
groups <- results[["objective_one"]] |>
  select("cdm_name", "group_level", "strata_name", "strata_level", "variable_name", "variable_level", "estimate_name", "estimate_value") |>
  distinct()

for(i in seq_along(groups$cdm_name)){
  g <- results[["objective_one"]] |>
    filter(cdm_name == groups$cdm_name[[1]],
           group_level == groups$group_level[[1]],
           strata_name == groups$strata_name[[1]],
           strata_level == groups$strata_level[[1]])
  
  total_count <- g |>
    filter(variable_name == "Number subjects") |>
    pull("estimate_value") |>
    as.numeric()
  
  counts <- g |>
    filter(variable_name != "Number subjects", variable_name != "Number records", estimate_name == "count") |>
    pull("estimate_value") |>
    as.numeric()
  
  results[["objective_one"]] <- bind_rows(
    results[["objective_one"]],
    tibble(
      "result_id" = 1L,
      "cdm_name" = groups$cdm_name[[1]],
      "group_name" = "cohort_name",
      "group_level" = groups$group_level[[1]],
      "strata_name" = groups$strata_name[[1]],
      "strata_level" = groups$strata_level[[1]],
      "variable_name" = "Indications",
      "variable_level" = "No indication identified",
      "estimate_name" = c("count", "percentage"),
      "estimate_type" = c("integer", "percentage"),
      "estimate_value" = c(as.character(total_count - sum(counts)), 
                           as.character((total_count - sum(counts))/total_count*100)),
      "additional_name" = "overall",
      "additional_level" = "overall"
    )
  )
}

omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
