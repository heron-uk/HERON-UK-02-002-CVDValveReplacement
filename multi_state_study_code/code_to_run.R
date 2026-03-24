# renv::activate()
# renv::restore()

library(cli)
library(dplyr)
library(here)
library(readr)
library(rlang)
library(stats)
library(stringr)
library(tibble)
library(tidyr)
library(tidyselect)
library(omopgenerics)
library(PatientProfiles)
library(IncidencePrevalence)
library(bit64)
library(clock)
library(CohortCharacteristics)
library(CohortConstructor)
library(mstate)
library(snakecase)
library(bshazard)
library(forcats)
library(glue)
library(rms)
library(CDMConnector)
library(CodelistGenerator)
library(CohortSurvival)
library(DBI)
library(odbc)
library(OmopSketch)
library(RPostgres)

# database metadata and connection details
# The name/ acronym for the database
dbName <- "..."

# Database connection details
# db <- dbConnect(
#   RPostgres::Postgres(),
#   dbname = server_dbi,
#   port = port,
#   host = host,
#   user = user,
#   password = password
# )
db <- dbConnect(...)

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_schema <- "..."

# A prefix for all permanent tables in the database
write_prefix <- "..."

# The name of the schema where results tables will be created
write_schema <- "..."

# minimum counts that can be displayed according to data governance
min_cell_count <- 5

# Create cdm object ----
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_schema,
  writeSchema = write_schema,
  writePrefix = write_prefix,
  cdmName = db_name
)

# Run the study
source(here("run_study.R"))
