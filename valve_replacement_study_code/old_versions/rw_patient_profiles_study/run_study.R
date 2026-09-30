# Check code_to_run inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(minCellCount)

# Create a log file ----
omopgenerics::createLogFile(logFile = here("Results", "log_{date}_{time}"))
logMessage(message = "LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date("2012-01-01"), as.Date(NA))
sex <- TRUE
age_groups <- list(c(0, 64), c(65, 150))
source(here("analyses", "functions.R"))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----
# CDM summary -----
results[["snapshot"]] <- OmopSketch::summariseOmopSnapshot(cdm)
results[["obs_period"]] <- OmopSketch::summariseObservationPeriod(cdm)

# Instantiate study cohorts ----
omopgenerics::logMessage(message = "Instantiating study cohorts")
source(here::here("cohorts", "instantiate_cohorts.R"))
omopgenerics::logMessage(message = "Study cohorts instantiated")

# Run analyses ----
omopgenerics::logMessage(message = "Run study analyses")
results[["code_use_hfrs"]] <- summariseCohortCodeUse(cdm,
                                                     cohortTable = "hospital_frailty_risk_score")
results[["code_use_procedures"]]  <- summariseCohortCodeUse(cdm, 
                                                            cohortTable = "procedures")
results[["comorbidities"]]  <- summariseCohortCodeUse(cdm, 
                                                      cohortTable = "comorbidities")

source(here::here("analyses", "1-ObjectiveOne.R"))
omopgenerics::logMessage("Analyses finished")

# Finish ----
results <- results |>
  omopgenerics::bind()
omopgenerics::exportSummarisedResult(results,
                                     minCellCount = min_cell_count,
                                     fileName = "results_{cdm_name}_{date}.csv",
                                     path = here("Results"))

cli::cli_alert_success("Study finished")
