library(dplyr)
library(omopgenerics)
library(here)
library(CodelistGenerator)
library(readr)
library(visOmopResults)
library(stringr)
library(CohortCharacteristics)
library(tidyr)
library(ggplot2)
library(patchwork)
library(rsvg)
library(DiagrammeRsvg)
library(IncidencePrevalence)
source(here("functions.R"))

style <- here("style.yml")
tableType <- "flextable"
plotType  <- "ggplot"
setGlobalPlotOptions(style = style, type = plotType) 
setGlobalTableOptions(style = style, type = tableType)

load(here("..", "StudyShiny", "data", "studyData.RData"))

# Indications -----
data[["summarise_characteristics"]] |>
  filter(variable_name == "Indications") |>
  filterStrata(age_group == "overall", sex == "overall") |>
  dplyr::mutate("variable_level" = gsub(" avr", "", .data$variable_level),
                "variable_level" = gsub("Aortic stenosis insufficiency", 
                                        "Aortic stenosis and insufficiency", 
                                        .data$variable_level),
                "variable_level" = gsub("Avd", "Aortic valve disease", .data$variable_level)) |>
  dplyr::mutate("variable_level" = factor(.data$variable_level, 
                                          levels = c("Aortic stenosis", 
                                                     "Aortic insufficiency", 
                                                     "Aortic stenosis and insufficiency",
                                                     "Aortic endocarditis",
                                                     "Aortic valve disease with endocarditis",
                                                     "No indication identified"))) |>
  dplyr::filter(!is.na(variable_level)) |>
  dplyr::arrange(cdm_name, group_level, strata_level, variable_level)  |>
  filterStrata(calendar_year != "overall") |>
  getStackedPlot() 

# Incidence by age groups ----
data[["incidence"]] |>
  filterSettings(denominator_target_cohort_name == "aortic_stenosis_denominator") |>
  omopgenerics::filterAdditional(analysis_interval != "overall") |> 
  omopgenerics::filterSettings(!denominator_age_group  %in% c("0 to 150", "0 to 39"),
                               denominator_sex == "Both") |>
  tidy() |>
  dplyr::mutate("denominator_cohort_name" = gsub("low_risk", "CCI: Low", denominator_cohort_name),
                "denominator_cohort_name" = gsub("medium_risk", "CCI: Medium", denominator_cohort_name),
                "denominator_cohort_name" = gsub("high_risk", "CCI: High", denominator_cohort_name),
                "denominator_cohort_name" = gsub("fit", "EFI: Fit", denominator_cohort_name),
                "denominator_cohort_name" = gsub("mild", "EFI: Mild", denominator_cohort_name),
                "denominator_cohort_name" = gsub("moderate", "EFI: Moderate", denominator_cohort_name),
                "denominator_cohort_name" = gsub("severe", "EFI: Severe", denominator_cohort_name)) |>
  getIncidencePlot(facet = "denominator_age_group") +
  facet_wrap(
    vars(cdm_name, denominator_age_group),
    ncol = 2
  )
  # facet_grid(rows = vars(cdm_name), col = vars(denominator_age_group), ) +
  # theme(strip.text.y.right = element_text(angle = 270))

# Incidence by sex ----
data[["incidence"]] |>
  filterSettings(denominator_target_cohort_name == "aortic_stenosis_denominator") |>
  omopgenerics::filterAdditional(analysis_interval != "overall") |> 
  omopgenerics::filterSettings(denominator_age_group == "0 to 150",
                               denominator_sex != "Both") |>
  tidy() |>
  dplyr::mutate("denominator_cohort_name" = gsub("low_risk", "CCI: Low", denominator_cohort_name),
                "denominator_cohort_name" = gsub("medium_risk", "CCI: Medium", denominator_cohort_name),
                "denominator_cohort_name" = gsub("high_risk", "CCI: High", denominator_cohort_name),
                "denominator_cohort_name" = gsub("fit", "EFI: Fit", denominator_cohort_name),
                "denominator_cohort_name" = gsub("mild", "EFI: Mild", denominator_cohort_name),
                "denominator_cohort_name" = gsub("moderate", "EFI: Moderate", denominator_cohort_name),
                "denominator_cohort_name" = gsub("severe", "EFI: Severe", denominator_cohort_name)) |>
  getIncidencePlot(facet = "denominator_sex")

# Incidence by CCI ----
data[["incidence"]] |>
  filterSettings(denominator_target_cohort_name == "aortic_stenosis_avr") |>
  omopgenerics::filterAdditional(analysis_interval != "overall") |> 
  omopgenerics::filterSettings(denominator_age_group == "0 to 150",
                               denominator_sex == "Both",
                               denominator_cci != "overall",
                               denominator_efi == "overall") |>

  tidy() |>
  dplyr::mutate(
    "denominator_cohort_name" = gsub("low_risk", "CCI: Low", denominator_cohort_name),
    "denominator_cohort_name" = gsub("medium_risk", "CCI: Medium", denominator_cohort_name),
    "denominator_cohort_name" = gsub("high_risk", "CCI: High", denominator_cohort_name),
    "denominator_cohort_name" = factor(denominator_cohort_name, levels = c("CCI: Low", "CCI: Medium", "CCI: High")) 
  ) |>
  dplyr::mutate("outcome_cohort_name" = factor(outcome_cohort_name, levels = c("AVR", "SAVR", "TAVI"))) |>
  arrange(cdm_name, outcome_cohort_name, denominator_cohort_name) |>
  getIncidencePlot(facet = "denominator_cohort_name")

# Incidence by EFI ----
data[["incidence"]] |>
  filterSettings(denominator_target_cohort_name == "aortic_stenosis_avr") |>
  omopgenerics::filterAdditional(analysis_interval != "overall") |> 
  omopgenerics::filterSettings(denominator_age_group == "0 to 150",
                               denominator_sex == "Both",
                               denominator_cci == "overall",
                               denominator_efi != "overall") |>
  
  tidy() |>
  dplyr::mutate("denominator_cohort_name" = gsub("fit", "EFI: Fit", denominator_cohort_name),
                "denominator_cohort_name" = gsub("mild", "EFI: Mild", denominator_cohort_name),
                "denominator_cohort_name" = gsub("moderate", "EFI: Moderate", denominator_cohort_name),
                "denominator_cohort_name" = gsub("severe", "EFI: Severe", denominator_cohort_name),
                "denominator_cohort_name" = factor(denominator_cohort_name, levels = c("EFI: Fit", "EFI: Mild", "EFI: Moderate", "EFI: Severe"))) |>
  dplyr::mutate("outcome_cohort_name" = factor(outcome_cohort_name, levels = c("AVR", "SAVR", "TAVI"))) |>
  arrange(cdm_name, outcome_cohort_name, denominator_cohort_name) |>
  getIncidencePlot(facet = "denominator_cohort_name")

# Patient profiles ----
data[["summarise_characteristics"]] |>
  omopgenerics::filterAdditional(.data$indications_window == "overall") |>
  omopgenerics::filterSettings(.data$strata == "sex &&& age_group &&& efi_categories &&& cci_categories &&& calendar_year") |>
  omopgenerics::filterStrata(
    .data$sex %in% "overall",
    .data$age_group %in% "overall",
    .data$efi_categories %in% "overall",
    .data$cci_categories %in% "overall") |>
  
  
  
# Table 1 ----
data[["summarise_characteristics"]] |>
  omopgenerics::filterAdditional(.data$indications_window == "overall") |>
  omopgenerics::filterSettings(.data$strata == "sex &&& age_group &&& efi_categories &&& cci_categories &&& calendar_year") |>
  filterStrata(calendar_year == "overall",
               age_group == "overall",
               sex == "overall") |>
  plotVariable(variable = "Efi categories", 
               strata = NULL)
  visOmopResults::visTable(
    groupColumn = c(),
    header = c("cdm_name", "cohort_name"),
    estimateName = c("N (%)" = "<count> (<percentage>)",
                     "N" = "<count>"),
    hide = c("efi_categories", "cci_categories", "age_group", "sex", "estimate_type", "result_id"))



