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

result <- importSummarisedResult(path = here("Results")) |>
  mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD Aurum", cdm_name)) |>
  filter(!(cdm_name == "CPRD Aurum" & strata_level == "2025"))

# Attrition ----
x <- result |>
  filterSettings(result_type == "summarise_cohort_attrition") |>
  filter(group_level %in% c("aortic_valve_replacement", "tavi", "tavi_additional", "tavi_direct", "savr")) |>
  arrange(group_level)

x |> plotCohortAttrition()

# Table 1 ----
result |>
  filterStrata(calendar_year == "overall",
               age_group == "overall",
               sex == "overall") |>
  filter(variable_name %in% c("Number subjects", "Cohort start date", 
                              "Age", "Age group", "Sex")) |>
  mutate(group_level = gsub("aortic_valve_replacement", "Aortic valve replacement", group_level)) |>
  mutate(group_level = gsub("tavi", "TAVI", group_level)) |>
  mutate(group_level = gsub("savr", "SAVR", group_level)) |>
  mutate(variable_name = gsub("Cohort start date", "Index date", variable_name)) |>
  filter(!estimate_name %in% c("mean", "sd", "min", "max")) |>
  tableCharacteristics(hide = c("calendar_year", "age_group", "sex", "table_name"))

# Indications ----
p1 <- getStackedPlot(result, 
                     cohort_name = "aortic_valve_replacement",
                     age_group = "overall", 
                     sex = "overall", 
                     title = "A) Aortic valve replacement")

p2 <- getStackedPlot(result, 
                     cohort_name = "tavi",
                     age_group = "overall", 
                     sex = "overall", 
                     title = "B) Transcatheter aortic valve replacement (TAVI)")

p3 <- getStackedPlot(result, 
                     cohort_name = "savr",
                     age_group = "overall", 
                     sex = "overall", 
                     title = "C) Surgical aortic valve replacement (SAVR)")

p <- (p1 + p2 + p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

ggsave(filename = "indications.png", plot = p, path = here("Figures"), height = 12, width = 18)

# Incidence
p1 <- getIncidencePlot(result, age_group = c("40 to 64", "65 to 69"), sex = "Both")
p2 <- getIncidencePlot(result, age_group = c("70 to 74", "75 to 79"), sex = "Both")
p3 <- getIncidencePlot(result, age_group = c("80 to 84", "85 to 150"), sex = "Both")

p <- (p1/p2/p3) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "top") 

ggsave(filename = "incidence.png", plot = p, path = here("Report", "Figures"))



