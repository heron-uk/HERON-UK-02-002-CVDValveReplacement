# create logger ----
resultsFolder <- here("results")
if(!dir.exists(resultsFolder)){
  dir.create(resultsFolder)
}

omopgenerics::createLogFile(logFile = here::here("results", "log_{date}_{time}"))
logMessage("LOG CREATED")

# run ----
source(here("cohorts","instantiate_cohorts.R"))
diagnostics_comorbidities <- phenotypeDiagnostics(cdm$comorbidities,
                                                  diagnostics = "codelistDiagnostics",
                                                  clinicalTableSample = 0,
                                                  populationDateRange = as.Date(c("2012-01-01", NA)))
exportSummarisedResult(diagnostics_comorbidities,
                       minCellCount = minCellCount,
                       fileName = "phenotyper_diagnostics_comorbidities_results_{cdm_name}_{date}.csv",
                       path = resultsFolder)

diagnostics_study <- phenotypeDiagnostics(cdm$study_cohorts,
                                    clinicalTableSample = 0,
                                    populationDateRange = as.Date(c("2012-01-01", NA)))
exportSummarisedResult(diagnostics_study,
                       minCellCount = minCellCount,
                       fileName = "phenotyper_diagnostics_study_results_{cdm_name}_{date}.csv",
                       path = resultsFolder)
logMessage("Finished")
