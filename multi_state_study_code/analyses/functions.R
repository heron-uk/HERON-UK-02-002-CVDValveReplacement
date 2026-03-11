addSES <- function(cohort, date_name = "cohort_start_date") {
  n_imd <- cdm$observation |>
    dplyr::filter(.data$observation_source_concept_id == 35812882L) |>
    dplyr::tally() |>
    dplyr::pull()
  n_townsend <- cdm$measurement |>
    dplyr::filter(.data$measurement_concept_id == 715996L) |>
    dplyr::tally() |>
    dplyr::pull()
  if (n_imd > 0) {
    cohort |>
      dplyr::left_join(
        cdm$observation |>
          dplyr::filter(.data$observation_source_concept_id == 35812882L) |>
          dplyr::select("person_id", "ses" = "value_as_number"),
        by = c("subject_id" = "person_id")
      ) |>
      dplyr::mutate(
        ses = as.character(.data$ses),
        ses = coalesce(.data$ses, "Missing")
      )
  } else if (n_townsend > 0) {
    cohort |>
      PatientProfiles::addConceptIntersectField(
        conceptSet = list(townsend = 715996L),
        indexDate = "cohort_start_date",
        field = "value_as_number",
        window = list(c(-Inf, Inf)),
        order = "last",
        nameStyle = "ses",
        inObservation = FALSE
      ) |>
      dplyr::mutate(
        ses = dplyr::case_when(
          ses %in% c(1, 2)  ~ 5L,
          ses %in% c(3, 4)  ~ 4L,
          ses %in% c(5, 6)  ~ 3L,
          ses %in% c(7, 8)  ~ 2L,
          ses %in% c(9, 10) ~ 1L,
          TRUE ~ NA_real_
        ),
        ses = as.character(.data$ses),
        ses = dplyr::coalesce(ses, "Missing")
      )
  } else {
    cohort
  }
}

addAge <- function(cohort, date_name = "cohort_start_date", col_name = "age") {
  cohort |> PatientProfiles::addAge(indexDate = date_name, ageName = col_name)
}

addEthnicity <- function(cohort) {
  if (grepl("dataloch", tolower(cdmName(cdm)))) {
    cohort |>
      dplyr::left_join(
        cdm$person |>
          dplyr::select("person_id", "race_source_value"),
        by = c("subject_id" = "person_id")
      ) |>
      dplyr::mutate(
        ethnicity_group = dplyr::case_when(
          .data$race_source_value %in% c("Bangladeshi", "Chinese", "Indian", "Other Asian", "Pakistani") ~ "Asian",
          .data$race_source_value %in% c("African", "Caribbean", "Other Black") ~ "Black",
          .data$race_source_value %in% c("Gypsy/Traveller", "Irish", "Other British", "Other White", "Polish", "Scottish") ~ "White",
          .data$race_source_value %in% c("Other Mixed", "White and Asian", "White and Black African", "White and Black Caribbean") ~ "Mix",
          .data$race_source_value %in% c("Arab", "Other ethnic group") ~ "Other",
          TRUE ~ "Unknown"
        ),
        ethnicity = dplyr::coalesce(.data$race_source_value, "Missing")
      ) |>
      dplyr::select(-"race_source_value")
  } else {
    cohort |>
      dplyr::left_join(
        cdm$person |>
          dplyr::select("person_id", "race_source_value"),
        by = c("subject_id" = "person_id")
      ) |>
      dplyr::mutate(
        ethnicity_group = dplyr::case_when(
          .data$race_source_value %in% c("9", "10", "11", "12", "13") ~ "Asian",
          .data$race_source_value %in% c("14", "15", "16") ~ "Black",
          .data$race_source_value %in% c("1", "2", "3", "4", "20") ~ "White",
          .data$race_source_value %in% c("5", "6", "7", "8") ~ "Mix",
          .data$race_source_value %in% c("17", "18") ~ "Other",
          # is.na(race_source_value)                      ~ "Unknown",
          TRUE ~ "Unknown"
        ),
        ethnicity = dplyr::case_when(
          race_source_value == "1" ~ "WHITE: British/N. Irish",
          race_source_value == "2" ~ "WHITE: Irish",
          race_source_value == "3" ~ "WHITE: Gypsy/Irish Traveller",
          race_source_value == "4" ~ "WHITE: Other white",
          race_source_value == "5" ~ "MIXED/MULTIPLE: White/Black Caribbean",
          race_source_value == "6" ~ "MIXED/MULTIPLE: White/Black African",
          race_source_value == "7" ~ "MIXED/MULTIPLE: White/Asian",
          race_source_value == "8" ~ "MIXED/MULTIPLE: Other mixed/multiple",
          race_source_value == "9" ~ "ASIAN: Indian",
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
        )
      ) |>
      dplyr::select(-"race_source_value")
  }
}

addCKDStage <- function(cohort) {
  name <- tableName(cohort)
  cdm[[name]] |>
    addCohortIntersectDays(
      targetCohortTable = "ckd_stage",
      order = "last",
      window = c(-Inf, 0),
      nameStyle = "{cohort_name}",
      name = name
    ) |>
    mutate(
      ckd_stage_1 = abs(coalesce(ckd_stage_1, 999)),
      ckd_stage_2 = abs(coalesce(ckd_stage_2, 999)),
      ckd_stage_3 = abs(coalesce(ckd_stage_3, 999)),
      ckd_stage_4 = abs(coalesce(ckd_stage_4, 999)),
      ckd_stage_5 = abs(coalesce(ckd_stage_5, 999))
    ) |>
    mutate(
      ckd_stage = case_when(
        ckd_stage_1 == 999 & ckd_stage_2 == 999 & ckd_stage_3 == 999 & ckd_stage_4 == 999 & ckd_stage_5 == 999 ~ "Missing",
        ckd_stage_1 <= ckd_stage_2 & ckd_stage_1 <= ckd_stage_3 & ckd_stage_1 <= ckd_stage_4 & ckd_stage_1 <= ckd_stage_5 ~ "Stage 1",
        ckd_stage_2 <= ckd_stage_3 & ckd_stage_2 <= ckd_stage_4 & ckd_stage_2 <= ckd_stage_5 ~ "Stage 2",
        ckd_stage_3 <= ckd_stage_4 & ckd_stage_3 <= ckd_stage_5 ~ "Stage 3",
        ckd_stage_4 <= ckd_stage_5 ~ "Stage 4",
        .default = "Stage 5"
      )
    ) |>
    select(!all_of(c(paste0("ckd_stage_", 1:5)))) |>
    compute(name = name, temporary = FALSE)
}

hr_summary <- function(model, transition, model_name) {
  
  p_res <- as.data.frame(stats::anova(model)) |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::select("variable", "p_value" = "P")
  tibble::tibble(
    "variable" = names(coef(model)),
    "hazard_ratio" = exp(coef(model)),
    "lower_hr" = exp(confint(model)[,1]),
    "upper_hr" = exp(confint(model)[,2]),
    "se_coef" = sqrt(diag(stats::vcov(model)))
  ) |>
    dplyr::mutate(
      variable_name = dplyr::if_else(
        stringr::str_detect(variable, "\\.[0-9]+$"),
        stringr::str_remove(variable, "\\.[0-9]+$"),
        variable
      )
    ) |>
    clean_variables(var_col = "variable_name") |>
    dplyr::inner_join(p_res, by = c("variable_name" = "variable")) |>
    dplyr::mutate(
      transition = transition,
      model_name = model_name,
      result_type = "hr_summary",
      package_name = "HERON-UK-02-002-CVDValveReplacement"
    ) |>
    dplyr::mutate(cdm_name = cdmName(cdm)) |>
    dplyr::select(!any_of(c("rel_age", "variable"))) |>
    omopgenerics::transformToSummarisedResult(
      group = "transition",
      estimates = c("hazard_ratio", "se_coef", "lower_hr", "upper_hr", "p_value"),
      additional = "model_name",
      settings = c("result_type", "package_name")
    )
}


# improved clean_variables
clean_variables <- function(df, var_col = "variable_name") {
  df |>
    dplyr::mutate(
      tmp_var = as.character(.data[[var_col]]),
      
      # First handle "=" explicitly
      has_equal = stringr::str_detect(tmp_var, "="),
      
      base_raw = dplyr::if_else(
        has_equal,
        stringr::str_replace(tmp_var, "^(.*)=(.*)$", "\\1"),
        tmp_var
      ),
      
      level_raw = dplyr::if_else(
        has_equal,
        stringr::str_replace(tmp_var, "^(.*)=(.*)$", "\\2"),
        NA_character_
      ),
      
      # If no "=", fallback to uppercase/digit split
      loc = stringr::str_locate(base_raw, "[A-Z0-9]")[, 1],
      
      base_raw = dplyr::if_else(
        is.na(level_raw) & !is.na(loc),
        substr(base_raw, 1, loc - 1),
        base_raw
      ),
      
      level_raw = dplyr::if_else(
        is.na(level_raw) & !is.na(loc),
        substr(tmp_var, loc, nchar(tmp_var)),
        level_raw
      ),
      
      # Clean base
      base = base_raw |>
        stringr::str_replace_all("^\\s*[\\._\\-=]+\\s*", "") |>
        stringr::str_replace_all("[\\._\\-=\\s]+$", "") |>
        stringr::str_trim() |>
        dplyr::na_if(""),
      
      # Clean level
      variable_level = level_raw |>
        stringr::str_replace_all("^\\s*[\\._\\-=]+\\s*", "") |>
        stringr::str_trim() |>
        dplyr::na_if("")
    ) |>
    dplyr::mutate(
      !!var_col := dplyr::if_else(!is.na(base), base, tmp_var),
      
      # Replace dots with space
      variable_level = ifelse(
        is.na(variable_level),
        NA_character_,
        gsub("\\.", " ", variable_level)
      )
    ) |>
    dplyr::select(-tmp_var, -has_equal, -loc, -base_raw, -level_raw, -base)
}
hr_summary_age_model <- function(model,
                                 transition,
                                 model_name,
                                 reference_age = 70, # reference age
                                 comparison_age = seq(20, 100, 1)) { # comparison ages
  res <- list()

  for (i in 1:length(comparison_age)) {
    working_summary <- head(as.data.frame(summary({{ model }},
      age = c({{ reference_age }}, {{ comparison_age[i] }}), antilog = FALSE
    )), 1) # 1st- age
    working_summary <- working_summary %>%
      dplyr::mutate(
        hazard_ratio = exp(.data$Effect),
        lower_hr = exp(`Lower 0.95`),
        upper_hr = exp(`Upper 0.95`),
      ) %>%
      dplyr::mutate(
        ref_age = as.character(reference_age),
        rel_age = comparison_age[i]
      ) %>%
      dplyr::select("hazard_ratio", "se_coef" = "S.E.", "lower_hr", "upper_hr", "ref_age", "rel_age")
    working_summary$aic <- stats::AIC({{ model }})
    working_summary$bic <- stats::BIC({{ model }})
    res[[i]] <- working_summary
  }

  res |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      variable_name = "rel_age",
      variable_level = as.character(.data$rel_age)
    ) |>
    dplyr::mutate(
      transition = transition,
      model_name = model_name,
      result_type = "hr_summary",
      package_name = "HERON-UK-02-002-CVDValveReplacement",
      package_version = "1.0"
    ) |>
    dplyr::mutate(cdm_name = cdmName(cdm)) |>
    dplyr::select(!any_of(c("rel_age", "variable"))) |>
    omopgenerics::transformToSummarisedResult(
      group = "transition",
      estimates = c("hazard_ratio", "lower_hr", "upper_hr", "aic", "bic", "se_coef"),
      additional = "model_name", "ref_age",
      settings = c(
        "result_type", "package_name",
        "package_version"
      )
    )
}
