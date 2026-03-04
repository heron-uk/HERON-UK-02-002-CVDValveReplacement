source(here::here("analyses", "functions.R"))


omopgenerics::logMessage(message = "Estimate incidence")

results[["incidence"]] <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "study_cohorts_inc",
  outcomeCohortId = c("aortic_stenosis", "aortic_valve_replacement"),
  interval = c("years", "overall"),
  outcomeWashout = Inf,
  repeatedEvents = FALSE, 
  strata = list("ses")
) 

set <- omopgenerics::settings(results[["incidence"]]) |>
  dplyr::mutate(standardised_incidence = "FALSE")

results[["incidence"]]  <- omopgenerics::newSummarisedResult(results[["incidence"]], settings = set)

omopgenerics::logMessage(message = "Estimate age standardised incidence")

new_groups <- omopgenerics::validateAgeGroupArgument(age_groups)$age_group |>
  names()

incidence_to_standardise <- results[["incidence"]] |>
  omopgenerics::filterSettings(.data$denominator_age_group %in% new_groups, 
                               result_type == "incidence")

standard_pop <- EpiStandard::standardPopulation(region = "Europe") |>
  EpiStandard::mergeAgeGroups(newGroups = new_groups) |>
  dplyr::rename("denominator_age_group" = "age_group")

res_standardised_incidence <- EpiStandard::directlyStandardiseRates(
  data = incidence_to_standardise |> IncidencePrevalence::asIncidenceResult(),
  refdata = standard_pop,
  event = "outcome_count",
  denominator = "person_years",
  age = "denominator_age_group",
  pop = "pop",
  strata = c(
    "cdm_name", "outcome_cohort_name",
    "incidence_start_date", "incidence_end_date", "analysis_interval",
    "analysis_censor_cohort_name", "analysis_complete_database_intervals",
    "analysis_outcome_washout", "analysis_repeated_events",            
    "denominator_days_prior_observation", 
    "denominator_end_date", "denominator_requirements_at_entry", 
    "denominator_sex", "denominator_start_date", 
    "denominator_target_cohort_name", "denominator_time_at_risk", 
    "ses"
  ))

results[["standardised_incidence"]] <- res_standardised_incidence |>
  dplyr::rename(
    "incidence_100000_pys" = "standardised_rate",
    "incidence_100000_pys_95CI_lower" = "standardised_rate_95CI_lower",
    "incidence_100000_pys_95CI_upper" = "standardised_rate_95CI_upper"
  ) |>
  dplyr::select(!c("crude_rate", "crude_rate_95CI_lower", "crude_rate_95CI_upper")) |>
  dplyr::mutate(
    result_type = "incidence", 
    standardised_incidence = "TRUE",
    denominator_age_group = "20 to 150",
    denominator_cohort_name = "denominator_standardised_population"
  ) |>
  omopgenerics::transformToSummarisedResult(
    group = incidence_to_standardise|> omopgenerics::groupColumns(),
    strata = incidence_to_standardise |> omopgenerics::strataColumns(),
    additional = incidence_to_standardise |> omopgenerics::additionalColumns(),
    estimates = c("outcome_count", "person_years", "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper"),
    settings = c(
      "analysis_censor_cohort_name", "analysis_complete_database_intervals",
      "analysis_outcome_washout", "analysis_repeated_events",            
      "denominator_days_prior_observation", "standardised_incidence",
      "denominator_end_date", "denominator_requirements_at_entry", 
      "denominator_sex", "denominator_start_date", "result_type",
      "denominator_target_cohort_name", "denominator_time_at_risk",
      "denominator_age_group"
    )
  ) |>
  dplyr::mutate(
    variable_name = dplyr::if_else(.data$estimate_name == "person_years", "Denominator", "Outcome"),
    variable_level = NA_character_
  ) 
