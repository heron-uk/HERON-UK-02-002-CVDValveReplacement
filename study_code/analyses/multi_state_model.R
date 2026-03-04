source(here::here("analyses", "functions.R"))

omopgenerics::logMessage(message = "Fit multi state model")

cohort_names <- cdm$multi_state_as |> omopgenerics::getCohortName() 
cdm$multi_state_as <- PatientProfiles::addCohortName(cdm$multi_state_as)


model_specs <- list(
  age_sex = c("age", "sex"),
  sex = c("sex"),
  ses_age_sex = c("age", "sex", "ses"),
  ses =  c("ses")
)

res <- list()


for (cohort_name in cohort_names) {
  cli::cli_inform(glue::glue(" -- modelling {cohort_name}"))
  data <- cdm$multi_state_as |>
    dplyr::filter(.data$cohort_name == .env$cohort_name) |> 
    dplyr::collect() |> 
    select(!c("cohort_definition_id",
              "from",
              "to",
              "trans"))
  cli::cli_inform(glue::glue(" -- dataset rows: {nrow(data)}"))
  
  n_events <- data |> 
    filter(status == 1) |> 
    nrow()
  
  cli::cli_inform(glue::glue(" -- n events: {n_events}"))
  
  
  if(n_events < 10) {
    cli::cli_warn("skipping due to low event count")
  } else {
  
  dd <- rms::datadist(data)
  options(datadist = "dd")
  flin  <- rms::Surv(t_start, t_stop, status) ~ age
  f_rcs3 <- rms::Surv(t_start, t_stop, status) ~ rms::rcs(age, 3)
  f_rcs4 <-rms::Surv(t_start, t_stop, status) ~ rms::rcs(age, 4) 
  fit <- list()
  fit[["linear"]] <- rms::cph(
    flin,
    data = data,
    x = TRUE,
    y = TRUE,
    surv = TRUE
  )
  fit[["rcs3"]] <- rms::cph(
    f_rcs3,
    data = data,
    x = TRUE,
    y = TRUE,
    surv = TRUE
  )
  fit[["rcs4"]] <-rms::cph(
    f_rcs4,
    data = data,
    x = TRUE,
    y = TRUE,
    surv = TRUE
  )
  crit <- c(linear = stats::AIC(fit[["linear"]]), 
            rcs3 = stats::AIC(fit[["rcs3"]]), 
            rcs4 = stats::AIC(fit[["rcs4"]]))
  chosen_age <- names(which.min(crit)) 
  chosen_age_term <- switch(chosen_age,
                            linear = "age",
                            rcs3   = "rms::rcs(age, 3)",
                            rcs4   = "rms::rcs(age, 4)")
  
  
    
  res[[paste(cohort_name,"age",chosen_age, sep = "_")]] <- hr_summary_age_model(fit[[chosen_age]], 
                                                                                transition = cohort_name, 
                                                                                model_name = paste0("age_",chosen_age))
  
  
  for (nm in names(model_specs)) {
 
    covs <- model_specs[[nm]]
    rhs_parts <- vapply(covs, function(v) if (v == "age") chosen_age_term else v, character(1))
    rhs <- paste(rhs_parts, collapse = " + ")
    
    
    form_base <- stats::as.formula(
      paste0("rms::Surv(t_start, t_stop, status) ~ ", rhs)
    )
    
  model <- rms::cph(
     form_base,
     data = data,
     x = TRUE,
     y = TRUE,
     surv = TRUE
   )
   res[[paste(cohort_name,nm, sep = "_")]] <- hr_summary(model, transition = cohort_name, model_name = nm)
    }
  }
}

results[["multi_state_model"]] <- omopgenerics::bind(res)
