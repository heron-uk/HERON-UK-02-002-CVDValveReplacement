getStackedPlot <- function(result) {
  result |>
    visOmopResults::barPlot(x = "calendar_year", 
                            y = "percentage", 
                            position = "stack", 
                            facet = c("cdm_name", "cohort_name"),
                            colour = "variable_level") +
    geom_hline(yintercept = 100,  
               linetype = "dashed") +
    labs(fill = "", colour = "", y = "Percentage (%)") +
    theme(panel.grid.major = element_line(linewidth = 0.5, colour = "#c1c1c1"),
          panel.spacing = unit(0.75, "cm"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          legend.position = "top") +
    scale_color_manual(values = c(
      "#B01513",  "#FFAD0AFF", "#EA6312","#2A9D8F", "#264653",  "grey")) +
    scale_fill_manual(values = c(
      "#B01513",  "#FFAD0AFF", "#EA6312", "#2A9D8F", "#264653", "grey")) 
}

getIncidenceTable <- function(result, outcome_cohort_name, age_group, sex) {
  result |>
    filterSettings(result_type == "incidence",
                   denominator_age_group %in% !!age_group,
                   denominator_sex %in% !!sex) |>
    filterGroup(outcome_cohort_name == !!outcome_cohort_name) |>
    splitGroup() |>
    mutate("outcome_cohort_name" = str_to_sentence(gsub("_", " ", outcome_cohort_name))) |>
    mutate("outcome_cohort_name" = case_when(
      outcome_cohort_name == "Savr" ~ "Surgical aortic valve replacement (SAVR)",
      outcome_cohort_name == "Tavi" ~ "Transcatheter aortic valve replacement (TAVI)",
      .default = outcome_cohort_name
    )) |>
    uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name")) |>
    tableIncidence(type = "flextable", 
                   header = c("cdm_name", "estimate_name"),
                   groupColumn = c("denominator_age_group", "denominator_sex"),
                   hide = c("outcome_cohort_name", "denominator_cohort_name", "analysis_interval"))
}

getIncidencePlot <- function(result, facet) {

  facet <- c("cdm_name", facet)
  
  p <- result |>
    mutate("outcome_cohort_name" = str_to_sentence(gsub("_", " ", outcome_cohort_name))) |>
    mutate("outcome_cohort_name" = case_when(
      outcome_cohort_name == "Avr" ~ "AVR",
      outcome_cohort_name == "Savr" ~ "SAVR",
      outcome_cohort_name == "Tavi" ~ "TAVI",
      .default = outcome_cohort_name
    )) |>
    mutate("incidence_start_date" = as.integer(gsub("-01-01", "", incidence_start_date))) |>
    visOmopResults::scatterPlot(x = "incidence_start_date",
                                y = "incidence_100000_pys",
                                line = TRUE, 
                                point = TRUE, 
                                ribbon = TRUE, 
                                ymin = "incidence_100000_pys_95CI_lower",
                                ymax = "incidence_100000_pys_95CI_upper",
                                facet = facet,
                                "outcome_cohort_name") +
    theme(
      panel.grid.major = element_line(linewidth = 0.5, colour = "#e1e1e1"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.75, "cm"),
      strip.background = element_rect(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      legend.position = "top"
    ) +
    labs(fill = "", colour = "", 
         y = "Incidence (100,000 person-years)", 
         x = "Calendar year") +
    geom_vline(
      xintercept = c(2012, 2017, 2021, 2025),  
      inherit.aes = FALSE,
      colour = "#B03030",
      linetype = "dashed",
      size = 0.8
    ) +
    scale_colour_manual(values = c("#264653",  "#FFAD0AFF", "#EA6312")) +
    scale_fill_manual(values = c("#264653",  "#FFAD0AFF", "#EA6312")) + 
    scale_x_continuous(breaks = seq(2010, 2025, by = 1))
  
  return(p)
}

combineReasons <- function(x, res_id, new_reason = NULL){
  merge <- x |>
    splitAll() |>
    filter(reason_id %in% !!res_id) 
  rid <- max(res_id, na.rm = TRUE)
  
    if(is.null(new_reason)){
    reason <- merge |>
      select("result_id", "cdm_name", "cohort_name", "reason") |>
      distinct() |>
      group_by(result_id, cdm_name, cohort_name) |>
      summarise("reason" = paste(reason, collapse = ", "), .groups = "drop_last")
  } else {
    reason <- merge |>
      select(result_id, cdm_name, cohort_name) |>
      distinct() |>
      mutate("reason" = new_reason)
  }
  
  number <- merge |>
    filter(variable_name %in% c("number_records", "number_subjects")) |>
    filter(reason_id == !!rid) 
  ids_sr <- x |> 
    pull(additional_level) |>
    unique() |>
    as.numeric() |>
    sort() 
  
  ids <- which(ids_sr %in% res_id)
  ids <- c(min(ids)-1, ids)
  ids <- ids_sr[ids]
  
  initial <- x |>
    splitAll() |>
    filter(variable_name %in% c("number_records", "number_subjects")) |>
    filter(reason_id == !!min(ids)) |>
    rename("estimate_value_initial" = "estimate_value") |>
    select(-c("reason_id", "reason"))
  
  excluded <- number |>
    left_join(initial,
              by = c("result_id", "cdm_name", "cohort_name", "variable_name",
                     "variable_level", "estimate_name", "estimate_type")) |>
    mutate("estimate_value" = as.character(as.numeric(estimate_value_initial) - as.numeric(estimate_value)),
           "variable_name" = gsub("number", "excluded", variable_name)) |>
    select(-"estimate_value_initial")

  x |>
    filterAdditional(!reason_id %in% res_id) |>
    bind_rows(
      number |>
        bind_rows(excluded) |>
        select(-"reason") |>
        left_join(
          reason,
          by = c("result_id", "cdm_name", "cohort_name"),
          relationship = "many-to-many"
        ) |>
        mutate("group_name"  = "cohort_name",
               "group_level" = cohort_name,
               "strata_name" = "reason",
               "strata_level" = reason,
               "additional_name" = "reason_id",
               "additional_level" = reason_id) |>
        select("result_id", "cdm_name", "group_name", "group_level", "strata_name", "strata_level",
               "variable_name", "variable_level", "estimate_name", "estimate_type", "estimate_value",
               "additional_name", "additional_level") |>
        distinct()
    )
}

plotVariable <- function(data, variable, strata = NULL) {
  
  if(variable == "Number subjects") {
    x <- data |>
      filter(variable_name == "Number subjects") |>
      filterStrata(calendar_year != "overall")
    
    facet <- c("cdm_name", strata)
    
    avr <- x |>
      filterGroup(cohort_name == "AVR")
    x <- x |>
      filterGroup(cohort_name != "AVR") |>
      left_join(
        avr |>
          select(-"group_level", -"result_id") |>
          rename("denominator" = "estimate_value"),
        by = c("cdm_name", "group_name", "strata_name", "strata_level", "variable_name", "variable_level", "estimate_name", "estimate_type",
               "additional_name", "additional_level")
      ) |>
      tidy() |>
      mutate("percentage" = count/as.numeric(denominator)*100) |>
      visOmopResults::barPlot(x = "calendar_year",
                              y = "percentage", 
                              facet =  facet,
                              colour = "cohort_name",
                              position = "stack") +
      labs(y = "Number subjects (%)") 
  }
  
  if(variable %in% c("Sex", "Age group", "Cci categories", "Efi categories")){
    x <- data |>
      filter(variable_name %in% !!variable) |>
      filterStrata(calendar_year != "overall") 
    
    facet <- c("cdm_name", "cohort_name", strata)
    
    x <- x |>
      tidy() |>
      visOmopResults::barPlot(x = "calendar_year",
                              y = "percentage", 
                              facet = facet, 
                              colour = "variable_level",
                              position = "stack") +
      labs(y = "Number subjects (%)", fill = variable, colour = variable) 
  } 
  
  if(variable %in% c("Cci", "Efi", "Age")){
    facet <- c("cdm_name", strata)
    
    x <- data |>
      filter(variable_name %in% !!variable) |>
      filter(!grepl("Density", variable_level)) |>
      filterStrata(calendar_year != "overall") 
    
    x <- x |>
      visOmopResults::boxPlot(x = "calendar_year",
                              facet = facet,
                              colour = "cohort_name")
  } 
  
  if(variable %in% c("Aortic valve disease phenotype", "Cardiovascular disease", "Cardiovascular risk factors", 
                     "Comorbidities", "Previous medications")){
    x <- data |>
      filter(variable_name %in% !!variable) |>
      filterStrata(calendar_year != "overall") 
    
    facet <- c("cdm_name", "variable_level", strata)
    
    x <- x |>
      visOmopResults::scatterPlot(x = "calendar_year",
                                  y = "percentage", 
                                  line = TRUE, 
                                  point = TRUE, 
                                  ribbon = FALSE,
                                  group = "variable_level",
                                  colour = "cohort_name")  +
      facet_wrap(
        facet,
        scales = "free_y"
      )
  } 
  
  return(x)
}

factorColumns <- function(x) {
  x |>
    dplyr::mutate("variable_level" = gsub("Ckd", "CKD", variable_level),
                  "variable_level" = gsub("Copd", "COPD", variable_level),
                  "variable_level" = gsub("Acei arbs", "ACE inhibitors and ARBs", variable_level),
                  "variable_level" = gsub("Isglt2", "ISGLT2", variable_level)) |>
    dplyr::mutate("calendar_year" = factor(calendar_year, levels = c("overall", as.character(seq(2012, 2025)))),
                  "age_group" = factor(age_group, levels = c("overall", "0 to 39", "40 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 150")),
                  "sex" = factor(sex, levels = c("overall", "Female", "Male")),
                  "efi_categories" = factor(efi_categories, levels = c("overall", "fit", "mild", "moderate", "severe")),
                  "cci_categories" = factor(cci_categories, levels = c("overall", "low_risk", "medium_risk", "high_risk")),
                  "variable_name" = factor(variable_name, levels = c("Number subjects", "Sex", "Efi categories", "Cci categories",
                                                                      "Aortic valve disease phenotype", "Cardiovascular disease",
                                                                      "Cardiovascular risk factors", "Comorbidities", "Previous medications"))) 
}