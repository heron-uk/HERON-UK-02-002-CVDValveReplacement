
omopgenerics::logMessage(message = "Fitting multi state models")

cohort_names <- cdm$multi_state_as |> omopgenerics::getCohortName()
cdm$multi_state_as <- PatientProfiles::addCohortName(cdm$multi_state_as)

model_specs <- list(
  age_sex = c("age", "sex"),
  sex = c("sex"),
  ses_age_sex = c("age", "sex", "ses"),
  ses = c("ses")
)
age_limits <- list("overall" = c(0,150), 
  "below_80" = c(0, 79),
  "over_80" = c(80, 150))
res <- list()

for (cohort_name in cohort_names) {
  for (age_limit in names(age_limits)) {
  omopgenerics::logMessage(message = glue::glue(" -- modelling {cohort_name} for {age_limit}"))
  
  lower_limit <- age_limits[[age_limit]][1]
  upper_limit <- age_limits[[age_limit]][2]
  
  data_age <- cdm$multi_state_as |>
    dplyr::filter(.data$cohort_name == .env$cohort_name) |>
    dplyr::filter(age <= upper_limit & age >= lower_limit) |>
    dplyr::select(!c(
      "cohort_definition_id",
      "from",
      "to",
      "trans",
      "transition",
      "ethnicity",
      "ethnicity_group",
      "cohort_name"
    )) |>
    dplyr::collect()
  
  omopgenerics::logMessage(message = glue::glue(" -- dataset rows: {nrow(data_age)}"))
  
  n_events <- data_age |>
    filter(status == 1) |>
    nrow()
  
  cli::cli_inform(glue::glue(" -- n events: {n_events}"))
  
  data_age <- data_age |> 
    dplyr::mutate(
      ses = factor(ses),
      sex = factor(sex)
    ) |>
   dplyr:: mutate(
      ses  = forcats::fct_relevel(ses, "5"),        
      sex  = forcats::fct_relevel(sex, "Female"),   
    )

  if (n_events < 10) {
    omopgenerics::logMessage(message = " -- skipping due to low event count")
  } else {
    dd <- rms::datadist(data_age)
    options(datadist = "dd")
    flin <- rms::Surv(t_start, t_stop, status) ~ age
    f_rcs3 <- rms::Surv(t_start, t_stop, status) ~ rms::rcs(age, 3)
    f_rcs4 <- rms::Surv(t_start, t_stop, status) ~ rms::rcs(age, 4)
    fit <- list()
    omopgenerics::logMessage(message = " -- fitting linear age model")
    fit[["linear"]] <- rms::cph(
      flin,
      data = data_age,
      x = TRUE,
      y = TRUE,
      surv = TRUE
    )
    omopgenerics::logMessage(message = " -- fitting rcs3 age model")
    fit[["rcs3"]] <- rms::cph(
      f_rcs3,
      data = data_age,
      x = TRUE,
      y = TRUE,
      surv = TRUE
    )
    omopgenerics::logMessage(message = " -- fitting rcs4 age model")
    fit[["rcs4"]] <- rms::cph(
      f_rcs4,
      data = data_age,
      x = TRUE,
      y = TRUE,
      surv = TRUE
    )
    crit <- c(
      linear = stats::AIC(fit[["linear"]]),
      rcs3 = stats::AIC(fit[["rcs3"]]),
      rcs4 = stats::AIC(fit[["rcs4"]])
    )
    chosen_age <- names(which.min(crit))
    chosen_age_term <- switch(chosen_age,
      linear = "age",
      rcs3   = "rms::rcs(age, 3)",
      rcs4   = "rms::rcs(age, 4)"
    )

    res[[paste(cohort_name,"age_limit",age_limit, "age", chosen_age, sep = "_")]] <- hr_summary_age_model(fit[[chosen_age]],
      transition = cohort_name,
      model_name = paste0("age_", chosen_age), 
      age_limit = age_limit
    )


    for (nm in names(model_specs)) {
      omopgenerics::logMessage(message = glue::glue(" -- fitting {nm} model"))
      covs <- model_specs[[nm]]
      rhs_parts <- vapply(covs, function(v) if (v == "age") chosen_age_term else v, character(1))
      rhs <- paste(rhs_parts, collapse = " + ")


      form_base <- stats::as.formula(
        paste0("rms::Surv(t_start, t_stop, status) ~ ", rhs)
      )
    
      model <- rms::cph(
        form_base,
        data = data_age,
        x = TRUE,
        y = TRUE,
        surv = TRUE
      )
      res[[paste(cohort_name, age_limit, nm, sep = "_")]] <- hr_summary(model, transition = cohort_name, model_name = nm, age_limit = age_limit)
    }
    
    # smoothed hazard
    min_events_ses <- data_age |>
      filter(status == 1) |>
      group_by(ses) |> 
      tally() |> 
      summarise(min=min(n))
    
    if(min_events_ses <10 ){
      omopgenerics::logMessage(message = " -- skipping smoothed hazards as not all strata have 10 events")
    } else { 
    bs_hazards <- data_age |> 
      group_by(ses) |> 
      do(as.data.frame(bshazard::bshazard(Surv(t_start, t_stop, status)~1, 
                                data=., 
                                verbose=FALSE))) |>
      ungroup()
    res[[paste("smoothed_haz", cohort_name, 
               age_limit, nm, sep = "_")]] <- bshaz_summarised_result(bs_hazards,
                                                            transition = cohort_name,
                                                            age_limit = age_limit)
    
    
    }
    
  }
  }
}




results[["multi_state_model"]] <- omopgenerics::bind(res)


results[["multi_state_model"]] |> omopgenerics::addSettings("age_limit") |>
  dplyr::group_by(age_limit)

