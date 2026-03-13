omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

# Add demographics ----
omopgenerics::logMessage(message = "Add demographics")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  addSex() |>
  addAge(ageGroup = age_groups) 

omopgenerics::logMessage(message = "Add hospital frailty risk score")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  addConceptIntersectFlag(targetCohortTable = "hospital_frailty_risk_score", 
                          window = c(-365,0), 
                          nameStyle = "{cohort_name}") 
scores <- read_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hfrs.csv"))

for (i in scores$cohort_name ){
  points <- scores |>
    filter(cohort_name == i) |>
    pull("points")
  
  cdm[["procedures"]] <- cdm[["procedures"]] |>
    mutate(!!i := .data[[i]]*points) 
}

omopgenerics::logMessage(message = "Add calendar year")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  mutate("calendar_year" = get_year(cohort_start_date)) |>
  compute(temporary = FALSE, name = "procedures")

omopgenerics::logMessage(message = "Summarise characteristics - on procedures")
results[["objective_one"]] <- summariseCharacteristics(cdm[["procedures"]], 
                                                       demographics = FALSE, 
                                                       strata = list("calendar_year", "age_group", "sex", c("calendar_year", "age_group"), c("calendar_year", "sex")),
                                                       cohortIntersectFlag = list(
                                                         "Prior comorbidities" = list("targetCohortTable" = "comorbidities",
                                                                                      "window" = c(-365, 0),
                                                                                      "nameStyle" = "{cohort_name}"),
                                                         "Previous treatments" = list("targetCohortTable" = "medications",
                                                                                      "window" = c(-365, 0),
                                                                                      "nameStyle" = "{cohort_name}"),
                                                       ))

omopgenerics::logMessage(message = "Summarising cohort attrition")
results[["objective_one_attrition_procedures"]]  <- summariseCohortAttrition(cdm[["procedures"]])

omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
