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
minCellCount <- 5

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
