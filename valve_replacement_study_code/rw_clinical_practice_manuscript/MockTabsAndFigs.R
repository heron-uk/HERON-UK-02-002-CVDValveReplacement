library(CohortConstructor)
library(omock)
library(dplyr)
library(CohortCharacteristics)
library(PatientProfiles)
library(visOmopResults)
library(ggplot2)

cdm <- mockCohortConstructor()
result <- list()

cdm <- cdm |>
  mockCohort(name = "avr", 
             numberCohorts = 2, 
             cohortName = c("savr", "tavi"))

cdm[["avr"]] <- cdm[["avr"]] |>
  unionCohorts(keepOriginalCohorts = TRUE) |>
  renameCohort(cohortId = 3, newCohortName = "avr")


# Study code starts ------
cdm[["avr"]] <- cdm[["avr"]] |>
  mutate("frailty_risk_score" = sample(1:100, nrow(cdm[["avr"]]), replace = TRUE)) |>
  mutate("year" = clock::get_year(cohort_start_date)) |>
  addCohortName() |>
  # mutate("hfrs_group" = case_when(
  #   frailty_risk_score < 5 ~ "low",
  #   frailty_risk_score >=5 & frailty_risk_score <= 15 ~ "intermediate",
  #   frailty_risk_score > 15 ~ "high"
  # )) |>
  mutate("hfrs_group" = case_when(
    frailty_risk_score < 25 ~ "low",
    frailty_risk_score >=25 & frailty_risk_score <= 60 ~ "intermediate",
    frailty_risk_score > 60 ~ "high"
  )) |>
  requireInDateRange(dateRange = as.Date(c("2012-01-01", "2025-31-12"))) 

result[["frailty_risk_score_sc"]] <- cdm[["avr"]] |>
  summariseResult(group = "cohort_name", 
                  variables = "frailty_risk_score", 
                  strata = c("year"), 
                  includeOverallGroup = FALSE, 
                  includeOverallStrata = FALSE)

# Study code finishes ------
result[["frailty_risk_score_sc"]]  |>
  mutate(cdm_name = "CPRD AURUM") |>
  rbind(
    result[["frailty_risk_score_sc"]]  |>
      mutate(cdm_name = "DataLock") 
  ) |>
  visOmopResults::scatterPlot(x = "year", 
                              y = "median", 
                              ymin = "q25",
                              ymax = "q75",
                              point = TRUE, 
                              line = TRUE, 
                              ribbon = TRUE, 
                              facet = "cdm_name",
                              colour = "cohort_name") 

# Study code starts ------
result[["summarise_characteristics"]] <- cdm[["avr"]] |>
  summariseCharacteristics(strata = list(c("hfrs_group"), c("year"), 
                                         c("hfrs_group", "year")), 
                           ageGroup = list(c(0,39),c(40,64), c(65, 69), c(70, 74), c(75, 79),
                                           c(80, 84), c(85, 150)))


# Study code ends------
p <- result[["summarise_characteristics"]] |>
  mutate(cdm_name = "CPRD AURUM") |>
  rbind(
    result[["summarise_characteristics"]] |>
      mutate(cdm_name = "DataLoch")
  ) |>
  filterStrata(year != "overall") |>
  filter(variable_name == "Sex",
         estimate_name == "percentage",
         variable_level == "Female")  |>
  visOmopResults::scatterPlot(x = "year", 
                              y = "percentage", 
                              point = TRUE, 
                              line = TRUE, 
                              ribbon = FALSE, 
                              facet = c("cdm_name", "hfrs_group"),
                              colour = "cohort_name") 
p +
  facet_wrap(
    vars(cdm_name, hfrs_group),
    ncol = 4,
    nrow = 2
  )


p <- result[["summarise_characteristics"]] |>
  mutate(cdm_name = "CPRD AURUM") |>
  rbind(
    result[["summarise_characteristics"]] |>
      mutate(cdm_name = "DataLoch")
  ) |>
  filter(variable_name == "Age group") |>
  filterGroup(cohort_name == "avr") |>
  filterStrata(year != "overall") |>
  visOmopResults::barPlot(x = "year", 
                          y = "percentage", 
                          position = "stack", 
                          colour = "variable_level",
                          facet = c("cdm_name", "hfrs_group"))
p +
  facet_wrap(
    vars(cdm_name, hfrs_group),
    ncol = 4,
    nrow = 2
  )
