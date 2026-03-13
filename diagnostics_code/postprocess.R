
library(omopgenerics)
library(stringr)
library(dplyr)

res_files <- list.files(here::here("results"), pattern = ".csv")
res_study <- importSummarisedResult(here::here("results",
                                         res_files[str_detect(res_files,
                                                              "study_results")]))
res_cmorb <- importSummarisedResult(here::here("results",
                                               res_files[str_detect(res_files,
                                                                    "comorbidities")]))
res_cmorb <- res_cmorb |>
  filterSettings(result_type != "summarise_log_file")


res<- bind(res_study, res_cmorb)

# remove date of birth density
res <- res |>
  filterSettings(result_type != "summarise_dob_density")

# remove observation period density
res <- res |>
  filterSettings(result_type != "summarise_obs_density")

# remove age density
res <- res |>
  filterSettings(result_type != "summarise_obs_density")

# remove min and max for age
res <- res |>
  filter_out(estimate_name == "min",
             variable_name == "Age")

# remove day of birth
res <- res |>
  filter_out(variable_name == "Day of birth")

# remove month of birth
res <- res |>
  filter_out(variable_name == "Month of birth")

# remove year of birth
res <- res |>
  filter_out(variable_name == "Year of birth")

# export trimmed results
omopgenerics::exportSummarisedResult(
  res,
  fileName = here::here(
    "results",
    "trimmed_results_{cdm_name}_{date}.csv"),
  minCellCount = NULL)
