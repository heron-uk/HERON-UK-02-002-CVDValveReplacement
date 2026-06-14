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
library(DrugUtilisation)
library(IncidencePrevalence)
library(odbc)
library(RPostgres)
library(readr)
library(clock)
library(rlang)
library(stringr)

# database metadata and connection details
# The name/ acronym for the database
dbName <- ""
db <- dbConnect(RPostgres::Postgres(),
                dbname = "",
                host   = "",
                user   = "",
                password = "")

cdmSchema <- ""
writePrefix <- ""
writeSchema <- ""
achillesSchema <- ""
min_cell_count <- 5

# Create cdm object ----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  writePrefix = writePrefix,
  cdmName = dbName,
  achillesSchema = achillesSchema
)

# Run the study
source(here("run_study.R"))
