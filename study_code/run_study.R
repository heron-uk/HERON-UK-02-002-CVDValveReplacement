
# Check code_to_run inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
omopgenerics::createLogFile(logFile = here::here("Results", "log_{date}_{time}"))
logMessage(message = "LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date("2012-01-01"), as.Date(NA))
sex <- TRUE
age_groups <- list(c(20, 29), c(30, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80,89), c(90, 150))
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

omopgenerics::logMessage(message = "Get cohort code use")

results[["code_use"]] <- CodelistGenerator::summariseCohortCodeUse(cdm, "study_cohorts_inc") 

omopgenerics::logMessage(message = "Get cohort attrition")

results[["attrition"]] <- CohortCharacteristics::summariseCohortAttrition(cdm$study_cohorts) 

source(here::here("analyses", "incidence_prevalence.R"))

source(here::here("analyses", "data_preparation.R"))

source(here::here("analyses", "multi_state_model.R"))
omopgenerics::logMessage("Analyses finished")

# Finish ----
results <- results |>
  omopgenerics::bind()
omopgenerics::exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

cli::cli_alert_success("Study finished")
