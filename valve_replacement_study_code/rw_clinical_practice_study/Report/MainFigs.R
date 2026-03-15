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
source(here("Report", "functions.R"))

result <- importSummarisedResult(path = here("Results")) |>
  mutate("cdm_name" = gsub("HERON_CDM_202509", "CPRD Aurum", cdm_name)) 

# Attrition ----
x <- result |>
  filterSettings(result_type == "summarise_cohort_attrition") |> 
  filterGroup(cohort_name %in% c("aortic_stenosis", "aortic_insufficiency", "aortic_endocarditis")) |>
  mutate("group_level" = str_to_sentence(gsub("_", " ", group_level))) |>
  mutate("group_level" = factor(group_level, levels = c("Aortic stenosis", "Aortic insufficiency", "Aortic endocarditis"))) |> 
  arrange(group_level)

x <- combineReasons(x, res_id = c(2,3,4))
x <- combineReasons(x, res_id = c(5), new_reason = "Restrict to study period (2012-01-01 onwards)")
x <- combineReasons(x, res_id = c(7,8), new_reason = "Require an aortic valve replacement within 0-365 days following the diagnosis and restrict to the most recent diagnosis before the replacement")
x <- combineReasons(x, res_id = c(9,10,11), new_reason = "Restrict to diagnosis with no other indication diagnosis recorded on the same day")

g <- plotCohortAttrition(x) 
svg <- export_svg(g)

rsvg_png(
  charToRaw(svg),
  file = here("Report", "Figures", "cohort_attrition.png"),
  width = 6000,
  height = 3000
)

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

p <- (p1 / p2 / p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

ggsave(filename = "indications.png", plot = p, path = here("Report", "Figures"))

# Incidence
p1 <- getIncidencePlot(result, age_group = c("40 to 64", "65 to 69"), sex = "Both")
p2 <- getIncidencePlot(result, age_group = c("70 to 74", "75 to 79"), sex = "Both")
p3 <- getIncidencePlot(result, age_group = c("80 to 84", "85 to 150"), sex = "Both")

p <- (p1/p2/p3) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "top") 

ggsave(filename = "incidence.png", plot = p, path = here("Report", "Figures"))



