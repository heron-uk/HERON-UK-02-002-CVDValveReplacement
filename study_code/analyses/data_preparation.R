source(here::here("analyses", "functions.R"))


cdm$healthy <- CohortConstructor::demographicsCohort(cdm = cdm, ageRange = list(c(20, 150)), sex = "Both", name = "healthy") |>
  CohortConstructor::trimToDateRange(dateRange = c(as.Date("2011-12-31"), as.Date(NA))) |>
  CohortConstructor::requirePriorObservation(minPriorObservation = 365L) |>
  CohortConstructor::requireConceptIntersect(conceptSet = list(as_or_avr = codelist |> unlist() |> unname()), 
                                             window = c(-Inf, 0),
                                             intersections = c(0, 0) )|>
  CohortConstructor::requireFutureObservation(minFutureObservation = 1L) |>
  PatientProfiles::addSex() |>
  addEthnicity() |>
  addAge(col_name = "age_start") |>
  addSES() |>
  
  PatientProfiles::addDeathDate(deathDateName = "death_date") |>
  addAge(col_name = "age_death", date_name = "death_date") |>
  
  dplyr::mutate(
    status_death = dplyr::if_else(!is.na(.data$death_date) & .data$death_date <= .data$cohort_end_date, 1L, 0L), 
    death_date = dplyr::coalesce(.data$death_date, .data$cohort_end_date),
    
    t_censor = clock::date_count_between(.data$cohort_start_date, .data$cohort_end_date, precision = "day"),
    t_death = clock::date_count_between(.data$cohort_start_date, .data$death_date, precision = "day"), 

  ) |>

  dplyr::compute(name = "healthy") 
### AS ----
cdm$multi_state_as <- cdm$healthy |> 
  
  PatientProfiles::addCohortIntersectDate(targetCohortTable = "study_cohorts", targetCohortId = "aortic_valve_replacement", nameStyle = "avr_date", order = "first") |>

  addAge(col_name = "age_avr", date_name = "avr_date") |>
  
  PatientProfiles::addCohortIntersectDate(targetCohortTable = "study_cohorts", targetCohortId = "aortic_stenosis", nameStyle = "as_date", order = "first") |>
  
  addAge(col_name = "age_as", date_name = "as_date")  |> 
  
  dplyr::mutate(
    status_as = dplyr::if_else(!is.na(.data$as_date) & .data$as_date <= .data$cohort_end_date, 1L, 0L),
    status_avr = dplyr::if_else(!is.na(.data$avr_date) & .data$avr_date <= .data$cohort_end_date, 1L, 0L),

    as_date = dplyr::coalesce(.data$as_date, .data$cohort_end_date),
    avr_date = dplyr::coalesce(.data$avr_date, .data$cohort_end_date),

    t_as = clock::date_count_between(.data$cohort_start_date, .data$as_date, precision = "day"), 
    t_avr = clock::date_count_between(.data$cohort_start_date, .data$avr_date, precision = "day"), 

    t_death = dplyr::if_else(.data$status_avr == 1 & .data$status_death == 1 & .data$t_death == .data$t_avr, .data$t_death + 0.5, .data$t_death),
    
  ) |>
  dplyr::filter(t_as > 0 & t_death > 0 & t_avr > 0 ) |>
  dplyr::filter(!(.data$status_as == 1 & .data$status_avr == 1 & .data$t_avr == .data$t_as)) |>
  dplyr::compute(name = "multi_state_as")

df_as <- cdm$multi_state_as |>
  dplyr::collect() |>
  as.data.frame()

tmat_as <- mstate::transMat(list(c(2,4), c(3,4), c(4), c()), names = c("Healthy", "AS","AVR","Death"))

msdata_as <- mstate::msprep(
  data = df_as,
  trans = tmat_as,
  time = c(NA,"t_as", "t_avr", "t_death"),
  status = c(NA, "status_as", "status_avr", "status_death"),
  keep = c( "cohort_definition_id", "cohort_start_date", "cohort_end_date", "sex", "ethnicity","ethnicity_group", "age_start", "age_as", "age_avr", "ses"),
  id = "subject_id"
)

data_as <- msdata_as |>
  dplyr::mutate(age = dplyr::case_when(
    to == 1 ~ age_start,
    
    from == 2 & status == 1L ~ age_as,
    
    
    from == 3 & status == 1L ~ age_avr,
    
    TRUE ~ NA_real_
  ) )|>
  tibble::as_tibble()|>
  dplyr::mutate(transition = dplyr::case_when(
    .data$from == 1 & .data$to == 2  ~ "Healthy to AS", 
    .data$from == 1 & .data$to == 4  ~ "Healthy to Death", 
    .data$from == 2 & .data$to == 3  ~ "AS to AVR", 
    .data$from == 2 & .data$to == 4  ~ "AS to Death", 
    .data$from == 3 & .data$to == 4  ~ "AVR to Death", 
  ), 
  subject_id = bit64::as.integer64(as.character(subject_id)))|>
  dplyr::rename("t_start" = "Tstart", "t_stop" = "Tstop")|>
  dplyr::select(-c("age_start", "age_as",  "age_avr")) |>
  dplyr::select("cohort_definition_id",  "subject_id", "cohort_start_date", "cohort_end_date", dplyr::everything())


cdm <- omopgenerics::insertTable(cdm = cdm, name = "multi_state_as_clean", table = data_as)

transitions_as <- data_as$transition |> unique()

for (transition in transitions_as){
  nm <- snakecase::to_snake_case(transition)
  cdm[[nm]] <- cdm$multi_state_as_clean |>
    dplyr::filter(.data$transition == .env$transition) |>
    dplyr::compute(name = nm) |>
    omopgenerics::newCohortTable(cohortSetRef = tibble::tibble(cohort_definition_id = 1L, cohort_name = nm))
}


cdm <- omopgenerics::bind(cdm$healthy_to_as, cdm$as_to_avr, cdm$healthy_to_death, cdm$as_to_death, cdm$avr_to_death, name = "multi_state_as")

### characterisation ----
results[["characterisation_multi_state_as"]] <- CohortCharacteristics::summariseCharacteristics(cdm$multi_state_as,
                                                                                 strata =  list("status", "ses"),
                                                                                 demographics = TRUE,
                                                                                 ageGroup = age_groups, 
                                                                                 otherVariables = c("t_start", "time", "ethnicity_group", "ethnicity"))

