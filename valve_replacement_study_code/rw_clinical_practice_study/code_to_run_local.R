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
dbName <- "CPRD GOLD"

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
writePrefix <- "avr_mah_"

# The name of the schema where results tables will be created
writeSchema <- "results"

# The name of the schema where the achilles tables are
achillesSchema <- "results"

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


# db <- DBI::dbConnect(odbc::odbc(),
#                      Driver = "ODBC Driver 17 for SQL Server",
#                      Server = "163.1.64.74",
#                      Database = "vocabularies",
#                      UID = "apratsuribe",
#                      PWD = "Spring2nOxford25!",
#                      TrustServerCertificate = "yes",
#                      Port = 1433)

# cdm_vocab_2025_08 <- CDMConnector::cdmFromCon(con = db,
#                                               cdmSchema = c("vocabularies", "vocab_2025_08"),
#                                               writeSchema = c("vocabularies", "results"),
#                                               cdmName = "v_")
# 
# tavi_additional <- read_csv("~/HERON-UK-02-002-CVDValveReplacement/diagnostics_code/cohorts/study_codelists/tavi_additional.csv")
# cdm_vocab_2025_08[["concept"]] |>
#   filter(concept_id %in% tavi_additional$concept_id) |>
#   select(concept_id, concept_name) |>
#   collect() |>
#   write_csv(here("tavi_additional.csv"))
# 
# list("tavi_additional" = read_csv("~/HERON-UK-02-002-CVDValveReplacement/diagnostics_code/cohorts/study_codelists/tavi_additional_reviewed.csv") |>
#        filter(`...3` == TRUE) |>
#        pull("concept_id")) |>
#   newCodelist() |>
#   exportCodelist("~/HERON-UK-02-002-CVDValveReplacement/diagnostics_code/cohorts/study_codelists/", "csv")


