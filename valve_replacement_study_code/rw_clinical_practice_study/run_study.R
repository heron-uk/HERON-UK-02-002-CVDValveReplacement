# Check code_to_run inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(min_cell_count)

source(here("analyses", "functions.R"))

# Create a log file ----
omopgenerics::createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
logMessage(message = "LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date("2012-01-01"), as.Date(NA))
sex <- TRUE
age_groups <- list(c(0, 64), c(65, 150))
age_groups_extended <- list(c(0, 39), c(40, 64), c(65, 69), c(70, 74), c(75,79), c(80, 84), c(85, 150))
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
source(here::here("cohorts", "instantiate_scores.R"))
omopgenerics::logMessage(message = "Study cohorts instantiated")

# Run analyses ----
omopgenerics::logMessage(message = "Run study analyses")
source(here::here("analyses", "0-SummariseCodeUse.R"))
source(here::here("analyses", "1-ObjectiveOne.R"))
source(here::here("analyses", "2-ObjectiveTwo.R"))
source(here::here("analyses", "3-ObjectiveThree.R"))
source(here::here("analyses", "4-RiskScores.R"))
omopgenerics::logMessage("Analyses finished")

# Finish ----
result <- bind(results)
omopgenerics::exportSummarisedResult(result,
                                     minCellCount = min_cell_count,
                                     fileName = "results_{cdm_name}_{date}.csv",
                                     path = here("Results"))

cli::cli_alert_success("Study finished")
