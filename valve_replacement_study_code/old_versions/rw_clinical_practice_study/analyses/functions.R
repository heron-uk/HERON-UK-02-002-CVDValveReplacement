createProceduresCohorts <- function(cdm, avrCohortName, taviCohortName, saviCohortName, proceduresCohortName, restrictions) {
  
  # 1. TAVI based on AVR (potential tavr) + transcatheter procedure
  cdm[[paste0(taviCohortName, "_from_additional")]] <- cdm[[avrCohortName]] |>
    requireConceptIntersect(conceptSet = codelist["aortic_valve_replacement_potential_tavi"],
                            window = c(0,0),
                            name = paste0(taviCohortName, "_from_additional")) |>
    requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                            window = c(0, 0),
                            name = paste0(taviCohortName, "_from_additional"))
  
  # 2. TAVI based on TAVI SNOMED code
  cdm[[paste0(taviCohortName, "_direct")]] <- conceptCohort(cdm = cdm,
                                                            name = paste0(taviCohortName, "_direct"),
                                                            conceptSet = codelist["tavi"],
                                                            exit = "event_start_date")
  
  if(isTRUE(restrictions)) {
    cdm[[paste0(taviCohortName, "_direct")]] <- cdm[[paste0(taviCohortName, "_direct")]] |>
      CohortConstructor::requireIsFirstEntry() |>
      CohortConstructor::requireInDateRange(dateRange = study_period)
  }
  
  cdm <- bind(cdm[[paste0(taviCohortName, "_from_additional")]],
              cdm[[paste0(taviCohortName, "_direct")]],
              name = taviCohortName)
  
  cdm[[taviCohortName]] <- unionCohorts(cdm[[taviCohortName]]) |>
    renameCohort("tavi")
  
  cdm[[saviCohortName]] <- cdm[[avrCohortName]] |>
    requireCohortIntersect(targetCohortTable = taviCohortName,
                           window = c(0, 0),
                           intersections = c(0, 0),
                           name = saviCohortName) |>
    renameCohort("savr")
  
  cdm <- bind(cdm[[avrCohortName]], cdm[[taviCohortName]], cdm[[saviCohortName]], name = proceduresCohortName)
  
  cdm <- dropSourceTable(cdm, name = c(paste0(taviCohortName, "_from_additional"),
                                       paste0(taviCohortName, "_direct"),
                                       saviCohortName,
                                       taviCohortName)
  )
  
  return(cdm)
}

addScores <- function(cohort, mapping, score) {
  omopgenerics::assertChoice(mapping, length = 1, choices = c("icd", "snomed"))
  omopgenerics::assertChoice(score, length = 1, choices = c("hfrs", "cci"))
  
  cohort <- omopgenerics::validateCohortArgument(cohort = cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  tableName <- omopgenerics::tableName(cohort)
  
  cols_to_exclude <- colnames(cdm[[tableName]])
  if(score == "hfrs") {
    x <- read_csv(here("cohorts", "study_codelists", "hospital_frailty_risk_score", "hfrs.csv"))
    w <- c(-365,0)
  } else {
    x <- read_csv(here("cohorts", "study_codelists", "charlson_comorbidity_index", "cci.csv")) 
    w <- c(-Inf, 0)
  }
  
  cdm[[tableName]] <- cdm[[tableName]] |>
    addCohortIntersectFlag(targetCohortTable = paste0(score,"_",mapping),
                           window = w, 
                           nameStyle = "{cohort_name}")

  if(score == "cci") {
    col_name_sev <- paste0("cci_metastatic_solid_tumor_", mapping)
    col_name <- paste0("cci_any_malignancy_", mapping)
    cdm[[tableName]] <- cdm[[tableName]] |>
      mutate(!!rlang::sym(col_name) := if_else(!!rlang::sym(col_name_sev) == 1, 0, !!rlang::sym(col_name)))
    
    col_name_sev <- paste0("cci_moderate_or_severe_liver_disease_", mapping)
    col_name <- paste0("cci_mild_liver_disease_", mapping)
    cdm[[tableName]] <- cdm[[tableName]] |>
      mutate(!!rlang::sym(col_name) := if_else(!!rlang::sym(col_name_sev) == 1, 0, !!rlang::sym(col_name)))
    cdm[[tableName]] <- cdm[[tableName]] |>
      mutate(!!rlang::sym(col_name) := if_else(!!rlang::sym(col_name_sev) == 1, 0, !!rlang::sym(col_name)))
  }
  
  scores <- x |>
    mutate("cohort_name" = paste0(score, "_", cohort_name, "_", mapping)) |>
    select("cohort_name", "points") |>
    distinct()
  points_map <- setNames(scores$points, scores$cohort_name)
  exprs <- imap(points_map, ~ expr(!!sym(.y) := !!sym(.y) * !!.x))
  exprs <- imap(points_map, function(mult, col) {
    expr(!!sym(col) * !!mult)
  })
  names(exprs) <- names(points_map)
  
  cdm[[tableName]] <- cdm[[tableName]] |>
    mutate(!!!exprs) |>
    compute(temporary = FALSE, name = tableName)
  
  cols_to_sum <- setdiff(colnames(cdm[[tableName]]), c(cols_to_exclude, 
                                                       "hfrs_snomed", "hfrs_snomed_groups",
                                                       "hfrs_icd", "hfrs_icd_groups", 
                                                       "cci_snomed", "cci_snomed_groups", 
                                                       "cci_icd", "cci_icd_groups"))
  
  quoted <- DBI::dbQuoteIdentifier(db, cols_to_sum)
  quoted_chr <- as.character(quoted)
  expr_str <- paste0("(", paste0("COALESCE(", quoted_chr, ", 0)", collapse = " + "), ")")
  
  new_name <- paste0(score, "_", mapping)
  cdm[[tableName]] <- cdm[[tableName]] |>
    mutate(!!sym(new_name) := !!dbplyr::sql(expr_str)) |>
    select(all_of(cols_to_exclude), any_of(c("hfrs_snomed", "hfrs_snomed_groups", 
                                             "hfrs_icd", "hfrs_icd_groups",
                                             "cci_snomed", "cci_snomed_groups",
                                             "cci_icd", "cci_icd_groups"))) |>
    compute(temporary = FALSE, name = tableName)
  
  return(cdm[[tableName]])
}

addScoresGrouping <- function(cohort,
                              mapping,
                              score) {
  
  cohort <- omopgenerics::validateCohortArgument(cohort = cohort)
  cdm <- omopgenerics::cdmReference(cohort)
  tableName <- omopgenerics::tableName(cohort)
  
  if(score == "hfrs") {
    limits <- list("Low risk" = c(0, 4),
                   "Intermediate risk" = c(5, 14),
                   "High risk" = c(15, Inf))
  } else {
    limits <- list("Low risk" = c(0, 2),
                   "Intermediate risk" = c(3, 4),
                   "High risk" = c(5, Inf))
  }
  
  low_max  <- as.numeric(limits[["Low risk"]][2])
  int_min  <- as.numeric(limits[["Intermediate risk"]][1])
  int_max  <- as.numeric(limits[["Intermediate risk"]][2])
  high_min <- as.numeric(limits[["High risk"]][1])
  
  score_col  <- rlang::sym(paste0(score, "_", mapping))
  groups_col <- paste0(score, "_", mapping, "_groups")
  
  cdm[[tableName]] <- cdm[[tableName]] |>
    mutate(
      !!groups_col := case_when(
        !!score_col < low_max+1 ~ "Low risk",
        !!score_col >= int_min & !!score_col < int_max+1 ~ "Intermediate risk",
        !!score_col >= high_min ~ "High risk",
        TRUE ~ NA_character_
      )
    ) |>
    compute(temporary = FALSE, name = tableName)
  
  return(cdm[[tableName]])
}
