# renv::activate()
# renv::restore()

library(DBI)
library(dplyr)
library(here)
library(CDMConnector)
library(omopgenerics)
library(OmopSketch)
library(CodelistGenerator)
library(CohortConstructor)
library(PatientProfiles)
library(CohortCharacteristics)
library(IncidencePrevalence)
library(odbc)
library(RPostgres)
library(readr)
library(clock)
library(rlang)
library(stringr)
library(purrr)
library(OmopIndices) 

# database metadata and connection details
# The name/ acronym for the database
dbName <- ""

# Database connection details
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html
# for more details.
# you may need to install another package for this
# eg for postgres
# db <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = server_dbi,i
#   port = port,
#   host = host,
#   user = user,
#   password = password
# )
db <- dbConnect(RPostgres::Postgres(),
                dbname = "",
                host   = "",
                user   = "",
                password = "")

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- ""

# A prefix for all permanent tables in the database
writePrefix <- ""

# The name of the schema where results tables will be created
writeSchema <- ""

# The name of the schema where the achilles tables are
achillesSchema <- ""

# minimum counts that can be displayed according to data governance
min_cell_count <- 5

# Create cdm object ----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  writePrefix = writePrefix,
  cdmName = dbName,
  achillesSchema = achillesSchema, 
  cohortTables = c("procedures_nr",
                   "aortic_stenosis_indication",
                   "procedures",
                   "aortic_valve_disease_phenotype",
                   "cardiovascular_disease",
                   "cardiovascular_risk_factors",
                   "electronic_frailty_index",
                   "charlson_comorbidity_index"
                   )
)

omopgenerics::logMessage(message = "Comorbidities")
cdm[["comorbidities"]] <- conceptCohort(cdm,
                                        conceptSet = importCodelist(here("cohorts", "study_codelists","comorbidities"), 
                                                                    type = "csv"), 
                                        name = "comorbidities",
                                        exit = "event_start_date")


cdm[["comorbidities"]] <- cdm[["comorbidities"]] |> 
  exitAtObservationEnd(cohortId = c("chronic_liver_disease", "copd",  "dementia", "dialysis"))

charlson_comorbidity_index_codelist <- importCodelist(here("cohorts", "study_codelists", "charlson_comorbidity_index"))
electronic_frailty_index_codelist <- importCodelist(here("cohorts", "study_codelists", "electronic_frailty_index"))

# Create a log file ----
createLogFile(logFile = here("Results", "log_{date}_{time}"))
logMessage(message = "LOG CREATED")

# Define analysis settings -----
study_period <- c(as.Date("2012-01-01"), as.Date(NA))
sex <- TRUE
age_groups <- list(c(0, 64), c(65, 150))
age_groups_extended <- list(c(0, 39), c(40, 64), c(65, 69), c(70, 74), c(75,79), c(80, 84), c(85, 150))
source(here("analyses", "functions.R"))

# Run analyses ----
logMessage(message = "Run study analyses")
source(here("analyses", "2-ObjectiveTwo.R"))
source(here("analyses", "3-ObjectiveThree.R"))
source(here("analyses", "4-RiskScores.R"))
logMessage("Analyses finished")

# Finish ----
result <- bind(results)
exportSummarisedResult(result,
                       minCellCount = min_cell_count,
                       fileName = "results_{cdm_name}_{date}.csv",
                       path = here("Results"))

cli_alert_success("Study finished")
