# create logger ----
resultsFolder <- here("results")
if(!dir.exists(resultsFolder)){
  dir.create(resultsFolder)
}

omopgenerics::createLogFile(logFile = here::here("results", "log_{date}_{time}"))
logMessage("LOG CREATED")

# run ----
source(here("cohorts","instantiate_cohorts.R"))
diagnostics <- phenotypeDiagnostics(cdm$procedures,
                                    diagnostics = "codelistDiagnostics",
                                    clinicalTableSample = 0,
                                    populationDateRange = as.Date(c("2012-01-01", NA)))
diagnostics <- phenotypeDiagnostics(cdm$indications,
                                    diagnostics = "codelistDiagnostics",
                                    clinicalTableSample = 0,
                                    populationDateRange = as.Date(c("2012-01-01", NA)))
diagnostics <- phenotypeDiagnostics(cdm$comorbidities,
                                    diagnostics = "codelistDiagnostics",
                                    clinicalTableSample = 0,
                                    populationDateRange = as.Date(c("2012-01-01", NA)))
exportSummarisedResult(diagnostics,
                       minCellCount = minCellCount,
                       fileName = "phenotyper_results_{cdm_name}_{date}.csv",
                       path = resultsFolder)
logMessage("Finished")
