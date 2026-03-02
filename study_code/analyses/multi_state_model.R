source(here::here("analyses", "functions.R"))


cohort_names <- cdm$multi_state_as |> omopgenerics::getCohortName() 
cdm$multi_state_as <- PatientProfiles::addCohortName(cdm$multi_state_as)


model_specs <- list(
  age_sex = c("age", "sex"),
  ses_age_sex = c("age", "sex", "ses")
  # eth_age_sex = c("ethnicity_group", "age_group", "sex")
)

res <- list()
for (cohort_name in cohort_names) {
  data <- cdm$multi_state_as |> dplyr::filter(.data$cohort_name == cohort_name) |> dplyr::collect()
  f_lin  <- survival::Surv(t_start, t_stop, status) ~ age+ strata(trans)
  f_rcs3 <- survival::Surv(t_start, t_stop, status) ~ rms::rcs(age, 3) + strata(trans)
  f_rcs4 <- survival::Surv(t_start, t_stop, status) ~ rms::rcs(age, 4) + strata(trans)
  fit <- list()
  fit[["linear"]] <- survival::coxph(f_lin,  data = data, cluster = data$subject_id)
  fit[["rcs3"]] <- survival::coxph(f_rcs3, data = data, cluster = data$subject_id)
  fit[["rcs4"]] <- survival::coxph(f_rcs4, data = data, cluster = data$subject_id)
  crit <- c(linear = stats::AIC(fit_lin), rcs3 = stats::AIC(fit_rcs3), rcs4 = stats::AIC(fit_rcs4))
  chosen_age <- names(which.min(crit)) 
  chosen_age_term <- switch(chosen_age,
                            linear = "age",
                            rcs3   = "rms::rcs(age, 3)",
                            rcs4   = "rms::rcs(age, 4)")
  
  
    
  res[[paste(cohort_name,"age",chosen_age, sep = "_")]] <- hr_summary(summary(fit[[chosen_age]]), transition = cohort_name, model_name = paste0("age_",chosen_age))
  
  
  for (nm in names(model_specs)) {
 
    covs <- model_specs[[nm]]
    rhs_parts <- vapply(covs, function(v) if (v == "age") chosen_age_term else v, character(1))
    rhs <- paste(rhs_parts, collapse = " + ")
    
    
    form_base <- stats::as.formula(
      paste0("survival::Surv(t_start, t_stop, status) ~ ",
             rhs,
             " + strata(trans)")
    )
    
   cox_model <- survival::coxph(
      form_base,
      data = data,
      cluster = subject_id
    )
   res[[paste(cohort_name,nm, sep = "_")]] <- hr_summary(summary(cox_model), transition = cohort_name, model_name = nm)
    }

}

results[["multi_state_model"]] <- omopgenerics::bind(res)
