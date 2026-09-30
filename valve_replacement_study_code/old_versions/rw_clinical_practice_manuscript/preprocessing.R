x2 <- result |>
  filterSettings(result_type == "summarise_characteristics") |>
  filter(strata_name == "overall" |
         variable_name != "Indications") 
  
x1 <- result |>
  filterSettings(result_type == "summarise_characteristics") |>
  # filterGroup(cohort_name == !!cohort_name) |>
  # filterStrata(age_group == !!age_group,
  #              sex == !!sex) |>
  filter(strata_name != "overall",
         variable_name == "Indications") |>
  filterStrata(calendar_year != "overall") |>
  mutate("variable_level" = gsub("Aortic valve replacement", "No indication identified", variable_level)) |>
  mutate("variable_level" = gsub(" avr", "", variable_level)) |>
  mutate("variable_level" = factor(variable_level, 
                                   levels = c("Aortic stenosis", "Aortic insufficiency", "Aortic endocarditis",
                                              "Aortic stenosis insufficiency",
                                              "No indication identified"))) |>
  arrange(variable_level) |>
  filter(!is.na(variable_level)) |>
  mutate("estimate_value" = if_else(estimate_value == "-", "0", estimate_value))

x1 <- rbind(x1, 
      x1 |>
        filter(estimate_name == "percentage") |>
        summarise("estimate_value" = as.character(100-sum(as.numeric(estimate_value), na.rm = TRUE)),
                  .by = c(result_id, cdm_name, group_name, group_level, strata_name, strata_level, 
                         variable_name, estimate_name, estimate_type,
                         additional_name, additional_level)) |>
        mutate("variable_level" = "Other mixed aortic valve disease") |>
        ungroup()
)

result <- result |>
  filterSettings(result_type != "summarise_characteristics") |>
  bind(x1) |>
  bind(x2)

