
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
logMessage("LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date("2000-01-01"), as.Date("2024-12-31"))

# Initialise list to store results as we go -----
results <- list()

# CDM modifications -----
# CDM summary -----
results[["snapshot"]] <- summariseOmopSnapshot(cdm)
results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)

# Instantiate study cohorts ----
logMessage("Instantiating study cohorts")
source(here("cohorts", "instantiate_cohorts.R"))

# Apply study period restriction
cdm$study_cohorts <- cdm$study_cohorts |>
  requireInDateRange(
    dateRange = study_period,
    cohortDateRange = "cohort_start_date",
    name = "study_cohorts"
  )

logMessage("Study cohorts instantiated")


# Cohort code use ----
results[["code_use"]] <- summariseCohortCodeUse(
  list(
    congenital_aortic_stenosis = codes$aortic_stenosis,
    congenital_aortic_valve_disease = codes$aortic_valve_disease,
    avr = codes$aortic_valve_replacement
  ),
  cdm = cdm,
  cohortTable = "study_cohorts"
)

# Cohort counts and attrition ----
results[["counts"]] <- summariseCohortCount(cdm$study_cohorts, 
                                            minCellCount = min_cell_count)
results[["attrition"]] <- summariseCohortAttrition(cdm$study_cohorts, 
                                                    minCellCount = min_cell_count)

# Run analyses ----
logMessage("Run study analyses")
source(here("analyses", "cohort_characteristics.R"))
source(here("analyses", "survival_analyses.R"))
logMessage("Analyses finished")

# Capture log file ----
results[["log"]] <- summariseLogFile(cdmName = omopgenerics::cdmName(cdm))

# Finish ----
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind()
exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")
