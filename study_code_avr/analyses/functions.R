

addSES <- function(cohort, date_name = "cohort_start_date"){
  n_imd <- cdm$observation |> dplyr::filter(.data$observation_source_concept_id == 35812882L) |> dplyr::tally() |> dplyr::pull()
  n_townsend <- cdm$measurement |> dplyr::filter(.data$measurement_concept_id == 715996L) |> dplyr::tally() |> dplyr::pull() 
  if (n_imd > 0){
    cohort |> dplyr::left_join(cdm$observation |> 
                                 dplyr::filter(.data$observation_source_concept_id == 35812882L) |> 
                                 dplyr::select("person_id", "ses" = "value_as_number"),
                               by = c("subject_id" = "person_id")) |>
      
     dplyr::mutate(ses = as.character(.data$ses),
                   ses = coalesce(.data$ses, "Missing"))
      
                               
  }else if (n_townsend>0) {
    cohort |> PatientProfiles::addConceptIntersectField(conceptSet = list(townsend = 715996L), 
                                                      indexDate = "cohort_start_date",
                                                      field = "value_as_number", 
                                                      window = list(c(-Inf, Inf)), 
                                                      order = "last", 
                                                      nameStyle = "ses", 
                                                      inObservation = FALSE) |>
    dplyr::mutate(
      ses = dplyr::case_when(
        ses %in% c(1, 2)  ~ 1L,
        ses %in% c(3, 4)  ~ 2L,
        ses %in% c(5, 6)  ~ 3L,
        ses %in% c(7, 8)  ~ 4L,
        ses %in% c(9, 10) ~ 5L,
        TRUE ~ NA_real_
      ),
      ses = as.character(.data$ses),
      ses = dplyr::coalesce(ses, "Missing")
    )
  }else {
    cohort
  }
  
}

addAge <- function(cohort, date_name = "cohort_start_date", col_name = "age"){

  cohort |> PatientProfiles::addAge(indexDate = date_name, ageName = col_name)
    
}

addEthnicity <- function(cohort) {
  cohort |> dplyr::left_join(cdm$person |> 
                     dplyr::select("person_id", "race_source_value"),
                     by = c("subject_id" = "person_id") ) |>
    dplyr::mutate(ethnicity_group = dplyr::case_when(
      .data$race_source_value %in% c("9", "10", "11", "12", "13") ~ "Asian",
      .data$race_source_value %in% c("14", "15", "16")          ~ "Black",
      .data$race_source_value %in% c("1", "2", "3", "4", "20")      ~ "White",
      .data$race_source_value %in% c("5", "6", "7", "8")          ~ "Mix",
      .data$race_source_value %in% c("17", "18")              ~ "Other",
      #is.na(race_source_value)                      ~ "Unknown",
      TRUE                                          ~ "Unknown"
    ),
    ethnicity = dplyr::case_when(
      race_source_value == "1"  ~ "WHITE: British/N. Irish",
      race_source_value == "2"  ~ "WHITE: Irish",
      race_source_value == "3"  ~ "WHITE: Gypsy/Irish Traveller",
      race_source_value == "4"  ~ "WHITE: Other white",
      race_source_value == "5"  ~ "MIXED/MULTIPLE: White/Black Caribbean",
      race_source_value == "6"  ~ "MIXED/MULTIPLE: White/Black African",
      race_source_value == "7"  ~ "MIXED/MULTIPLE: White/Asian",
      race_source_value == "8"  ~ "MIXED/MULTIPLE: Other mixed/multiple",
      race_source_value == "9"  ~ "ASIAN: Indian",
      race_source_value == "10" ~ "ASIAN: Pakistani",
      race_source_value == "11" ~ "ASIAN: Bangladeshi",
      race_source_value == "12" ~ "ASIAN: Chinese",
      race_source_value == "13" ~ "ASIAN: Other Asian",
      race_source_value == "14" ~ "BLACK: African",
      race_source_value == "15" ~ "BLACK: Caribbean",
      race_source_value == "16" ~ "BLACK: Other black",
      race_source_value == "17" ~ "OTHER: Arab",
      race_source_value == "18" ~ "OTHER: Other ethnicity",
      race_source_value == "19" ~ "UNKNOWN",
      race_source_value == "20" ~ "WHITE: Roma",
      TRUE ~ "Missing"
    )) |> 
    dplyr::select(-"race_source_value")
}


hr_summary <- function(summary, transition, model_name) {
  
  coef_tbl <- summary$coefficients |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::select(
      variable,
      coef,
      "hazard_ratio" = `exp(coef)`,
      se_coef = `se(coef)`,
      z,
      p = `Pr(>|z|)`
    )
  
  conf_tbl <- summary$conf.int |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::select(
      variable,
      lower_hr = `lower .95`,
      upper_hr = `upper .95`
    )
  
  data <- coef_tbl |>
    dplyr::left_join(conf_tbl, by = "variable") 
  
  data |>
    dplyr::mutate(

    # remove the trailing .number from variable
    variable_name = dplyr::if_else(stringr::str_detect(variable, "\\.[0-9]+$"),
                      stringr::str_remove(variable, "\\.[0-9]+$"),
                      variable)
  ) |> 
    clean_variables(var_col = "variable_name") |>
    dplyr::mutate(
      transition = transition, 
      model_name = model_name,
    result_type = "hr_summary", 
    package_name = "HERON-UK-02-002-CVDValveReplacement"
    ) |> 
    omopgenerics::transformToSummarisedResult(group = "transition", 
                                              estimates = c("coef", "hazard_ratio", "se_coef", "z", "p", "lower_hr", "upper_hr"), 
                                              additional = "model_name",
                                              settings = c("result_type", "package_name")) |>
    dplyr::mutate(cdm_name = dbName)
}




clean_variables <- function(df, var_col = "variable_name") {
  df |>
    dplyr::mutate(
      # ensure character
      tmp_var = as.character(.data[[var_col]]),
      
      loc = stringr::str_locate(tmp_var, "[A-Z0-9]")[,1],
      
      base = dplyr::if_else(is.na(loc), tmp_var, substr(tmp_var, 1, loc - 1)),
      
      level_raw = dplyr::if_else(is.na(loc), NA_character_, substr(tmp_var, loc, nchar(tmp_var))),
      
      variable_level = level_raw |>
        stringr::str_replace_all("^\\s*[\\._-]+\\s*", "") |>  # leading separators
        stringr::str_trim() |>
        dplyr::na_if("") 
    ) %>%
    dplyr::mutate(
      # final variable name = base (if exists) otherwise original
      !!var_col := dplyr::if_else(!is.na(base) & base != "", base, tmp_var),
      variable_level = dplyr::if_else(is.na(variable_level) | variable_level == "", NA_character_, variable_level),
      variable_level = gsub("\\.", " ", variable_level)
    ) %>%
    dplyr::select(-tmp_var, -loc, -base, -level_raw)
}

