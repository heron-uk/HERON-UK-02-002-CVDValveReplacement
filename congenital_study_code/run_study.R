
# Check codeToRun inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(min_cell_count)

# Create a log file ----
createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
omopgenerics::logMessage("LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date("2000-01-01"), as.Date("2024-12-31"))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----
# CDM summary -----
results[["snapshot"]] <- OmopSketch::summariseOmopSnapshot(cdm)
results[["obs_period"]] <- OmopSketch::summariseObservationPeriod(cdm$observation_period)

# Instantiate study cohorts ----
omopgenerics::logMessage("Instantiating study cohorts")
source(here("cohorts", "instantiate_cohorts.R"))

# Apply study period restriction
cdm <- cdm$study_cohorts |>
  CohortConstructor::requireInDateRange(
    dateRange = study_period,
    name = "study_cohorts"
  )

omopgenerics::logMessage("Study cohorts instantiated")

# Cohort counts and attrition ----
results[["counts"]] <- CohortCharacteristics::summariseCohortCount(cdm$study_cohorts)
results[["attrition"]] <- CohortCharacteristics::summariseCohortAttrition(cdm$study_cohorts)

# Run analyses ----
omopgenerics::logMessage("Run study analyses")
source(here("analyses", "cohort_characteristics.R"))
source(here("analyses", "survival_analyses.R"))
omopgenerics::logMessage("Analyses finished")

# Capture log file ----
results[["log"]] <- omopgenerics::summariseLogFile(cdmName = omopgenerics::cdmName(cdm))

# Finish ----
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
omopgenerics::exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")
