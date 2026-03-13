omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

omopgenerics::logMessage(message = "Add hospital frailty risk score")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  addCohortIntersectFlag(targetCohortTable = "hospital_frailty_risk_score",
                         window = c(-365,0), 
                         nameStyle = "{cohort_name}")
scores <- read_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hfrs.csv")) |>
  select("cohort_name_1", "points") |>
  distinct()
points_map <- setNames(scores$points, scores$cohort_name_1)
exprs <- imap(points_map, ~ expr(!!sym(.y) := !!sym(.y) * !!.x))

exprs <- imap(points_map, function(mult, col) {
  expr(!!sym(col) * !!mult)
})
names(exprs) <- names(points_map)

cdm[["procedures"]] <- cdm[["procedures"]] |>
  mutate(!!!exprs) |>
  compute(temporary = FALSE, name = "procedures")

omopgenerics::logMessage(message = "Create HFRS")
cols_to_exclude <- c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date")
cols_to_sum <- setdiff(colnames(cdm[["procedures"]] ), cols_to_exclude)
quoted <- DBI::dbQuoteIdentifier(db, cols_to_sum)
quoted_chr <- as.character(quoted)
expr_str <- paste0("(", paste0("COALESCE(", quoted_chr, ", 0)", collapse = " + "), ")")

cdm[["procedures"]] <- cdm[["procedures"]] |>
  mutate("hospital_frailty_risk_score" = !!dbplyr::sql(expr_str)) |>
  select(all_of(cols_to_exclude), "hospital_frailty_risk_score") |>
  compute(temporary = FALSE, name = "procedures")

omopgenerics::logMessage(message = "Create hfrs groups")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  mutate("hfrs_group" = case_when(
    hospital_frailty_risk_score  < 5 ~ "Low risk",
    hospital_frailty_risk_score  >= 5 & hospital_frailty_risk_score  < 15 ~ "Intermediate risk",
    hospital_frailty_risk_score  >= 15 ~ "High risk"
  )) |>
  compute(temporary = FALSE, name = "procedures")

omopgenerics::logMessage(message = "Add demographics")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  addSex() |>
  addAge(ageGroup = age_groups) 

omopgenerics::logMessage(message = "Add calendar year")
cdm[["procedures"]] <- cdm[["procedures"]] |>
  mutate("calendar_year" = get_year(cohort_start_date)) |>
  compute(temporary = FALSE, name = "procedures")

omopgenerics::logMessage(message = "Summarise characteristics - on procedures")
results[["objective_one"]] <- summariseCharacteristics(cdm[["procedures"]], 
                                                       demographics = FALSE, 
                                                       strata = list("calendar_year", "age_group", "sex", "hfrs_group", 
                                                                     c("calendar_year", "age_group"), 
                                                                     c("calendar_year", "sex"),
                                                                     c("calendar_year", "age_group", "hfrs_group"), 
                                                                     c("calendar_year", "sex", "hfrs_group")),
                                                       cohortIntersectFlag = list(
                                                         "Prior comorbidities" = list("targetCohortTable" = "comorbidities",
                                                                                      "window" = c(-365, 0),
                                                                                      "nameStyle" = "{cohort_name}")),
                                                       conceptIntersectFlag = list(
                                                         "Previous medications" = list("conceptSet" = importCodelist(here("cohorts", "treatments_codelists"), type = "csv"),
                                                                                       "window" = c(-365, 0),
                                                                                       "nameStyle" = "{cohort_name}")),
                                                       otherVariables = c("hospital_frailty_risk_score", "hfrs_group")
)

omopgenerics::logMessage(message = "Summarising cohort attrition")
results[["objective_one_attrition_procedures"]]  <- summariseCohortAttrition(cdm[["procedures"]])

omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
