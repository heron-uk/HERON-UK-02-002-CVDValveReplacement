# Check code_to_run inputs ----
omopgenerics::validateCdmArgument(cdm,
                                  requiredTables = c("person",
                                                     "observation_period",
                                                     "condition_occurrence",
                                                     "drug_exposure",
                                                     "concept"))
omopgenerics::assertNumeric(minCellCount)

# Create a log file ----
omopgenerics::createLogFile(logFile = tempfile(pattern = "log_{date}_{time}"))
logMessage(message = "LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date("2012-01-01"), as.Date(NA))
sex <- TRUE
age_groups <- list(c(0, 39), c(40, 64), c(65, 69), c(70, 74), c(75,79), c(80, 84), c(85, 150))
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
# results[["code_use_indications"]]   <- CodelistGenerator::summariseCohortCodeUse(cdm$indications)
# results[["code_use_procedures"]]    <- CodelistGenerator::summariseCohortCodeUse(cdm$procedures)
# results[["code_use_comorbidities"]] <- CodelistGenerator::summariseCohortCodeUse(cdm$comorbidities)
# results[["code_use_treatments"]]    <- CodelistGenerator::summariseCohortCodeUse(cdm$treatments)

omopgenerics::logMessage(message = "Get cohort attrition")
results[["attrition_proc_one"]] <- CohortCharacteristics::summariseCohortAttrition(cdm$proc_obj_one) 
results[["attrition_proc"]] <- CohortCharacteristics::summariseCohortAttrition(cdm$proc) 

source(here::here("analyses", "1-ObjectiveOne.R"))

source(here::here("analyses", "2-ObjectiveTwo.R"))

source(here::here("analyses", "3-ObjectiveThreeAndFour.R"))

omopgenerics::logMessage("Analyses finished")

# Finish ----
results <- results |>
  omopgenerics::bind()
omopgenerics::exportSummarisedResult(results,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("results"))

cli::cli_alert_success("Study finished")
