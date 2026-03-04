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
library(CohortSurvival)
library(odbc)
library(RPostgres)

# database metadata and connection details
# The name/ acronym for the database
dbName <- "CPRD GOLD" # Please choose: FinOMOP-THL, CPRD GOLD, SIDIAP, NLHR, DK-DHR, NAJS

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
                dbname = Sys.getenv("server_dbi_cprd"),
                host   = Sys.getenv("host"),
                user   = Sys.getenv("user"),
                password = Sys.getenv("password"))

# The name of the schema that contains the OMOP CDM with patient-level data
cdmSchema <- "public_100k"

# A prefix for all permanent tables in the database
writePrefix <- "avr_"

# The name of the schema where results tables will be created
writeSchema <- "results"

# minimum counts that can be displayed according to data governance
minCellCount <- 5

# Create cdm object ----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdmSchema,
  writeSchema = writeSchema,
  writePrefix = writePrefix,
  cdmName = dbName
)

# Run the study
source(here("run_study.R"))
