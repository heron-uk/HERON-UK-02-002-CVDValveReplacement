getStackedPlot <- function(result, cohort_name, age_group, sex, title) {

  x1 <- result |>
    filterSettings(result_type == "summarise_characteristics") |>
    filterGroup(cohort_name == !!cohort_name) |>
    filterStrata(age_group == !!age_group,
                 sex == !!sex) |>
    filter(strata_name != "overall",
           variable_name == "Indications") |>
    filterStrata(calendar_year != "overall") |>
    mutate("variable_level" = gsub("Aortic valve replacement", "No indication identified", variable_level)) |>
    mutate("variable_level" = gsub(" avr", "", variable_level)) |>
    mutate("variable_level" = factor(variable_level, 
                                     levels = c("Aortic stenosis", "Aortic insufficiency", "Aortic endocarditis",
                                                "Aortic stenosis insufficiency", "Aortic stenosis endocarditis",
                                                "Aortic insufficiency endocarditis", "Aortic stenosis insufficiency endocarditis",
                                                "No indication identified"))) |>
    arrange(variable_level) |>
    visOmopResults::barPlot(x = "calendar_year", 
                            y = "percentage", 
                            position = "stack", 
                            facet = "cdm_name",
                            colour = "variable_level") +
    geom_hline(yintercept = 100, 
               linetype = "dashed") +
    labs(fill = "", colour = "") +
    theme(panel.grid.major = element_line(linewidth = 0.5, colour = "#c1c1c1"),
          panel.spacing = unit(0.75, "cm"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          legend.position = "bottom") +
    scale_color_manual(values = c(
      "#B01513",  "#FFAD0AFF", "#EA6312", "#859B6CFF", "#2A9D8F", "#264653", "#6C6FB5", "grey")) +
    scale_fill_manual(values = c(
      "#B01513",  "#FFAD0AFF", "#EA6312", "#859B6CFF", "#2A9D8F", "#264653", "#6C6FB5", "grey")) +
    ggtitle(title) 
  
 
  data2 <- result |>
    filterSettings(result_type == "summarise_characteristics") |>
    filterGroup(cohort_name == !!cohort_name) |>
    filterStrata(age_group == !!age_group,
                 sex == !!sex) |>
    filter(strata_name != "overall", 
           variable_name == "Number subjects") |>
    filterStrata(calendar_year != "overall") 
  
  scale_factor <- max(as.integer(data2$estimate_value), na.rm = TRUE) / 100
  
  x1 +
    geom_line(
      data = data2,
      aes(x = strata_level, y = as.numeric(estimate_value) / scale_factor, group = cdm_name),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.8
    ) +
    geom_point(
      data = data2,
      aes(x = strata_level, y = as.numeric(estimate_value) / scale_factor, group = cdm_name),
      inherit.aes = FALSE,
      colour = "black",
      size = 2
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.1)),
      name = "Percentage",
      sec.axis = sec_axis(~ . * scale_factor, name = "Counts")
    )
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

getIncidencePlot <- function(result, age_group, sex) {
  
  if(sex == "Both") {
    facet <- c("cdm_name", "denominator_age_group")
  } else {
    facet <- c("cdm_name", "denominator_sex")
  }
  
  p <- result |>
    filterSettings(result_type == "incidence",
                   denominator_age_group %in% !!age_group,
                   denominator_sex %in% !!sex) |>
    splitGroup() |>
    mutate("outcome_cohort_name" = str_to_sentence(gsub("_", " ", outcome_cohort_name))) |>
    mutate("outcome_cohort_name" = case_when(
      outcome_cohort_name == "Savr" ~ "Surgical aortic valve replacement (SAVR)",
      outcome_cohort_name == "Tavi" ~ "Transcatheter aortic valve replacement (TAVI)",
      .default = outcome_cohort_name
    )) |>
    uniteGroup(cols = c("denominator_cohort_name", "outcome_cohort_name")) |>
    plotIncidence(facet = facet,
                  colour = "outcome_cohort_name",
                  ribbon = TRUE) +
    theme(
      panel.grid.major = element_line(linewidth = 0.5, colour = "#e1e1e1"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.75, "cm"),
      strip.background = element_rect(colour = "black"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      legend.position = "top"
    ) +
    labs(fill = "", colour = "") +
    geom_vline(
      xintercept = c(2012, 2017, 2021, 2025),  
      inherit.aes = FALSE,
      colour = "#B03030",
      linetype = "dashed",
      size = 0.8
    ) +
    scale_colour_manual(values = c("#FFAD0AFF", "#2A9D8F", "#8DA0CBFF")) +
    scale_fill_manual(values = c("#FFAD0AFF", "#2A9D8F", "#8DA0CBFF")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
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
