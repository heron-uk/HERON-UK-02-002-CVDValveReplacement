createProceduresCohorts <- function(avrCohortName, taviCohortName, saviCohortName, proceduresCohortName, restrictions) {
  cdm[[paste0(taviCohortName, "_from_additional")]] <- cdm[[avrCohortName]] |>
    requireConceptIntersect(conceptSet = codelist["tavi_additional"],
                            window = c(0, 0),
                            name = paste0(taviCohortName, "_from_additional"))
  
  cdm[[paste0(taviCohortName, "_direct")]] <- conceptCohort(cdm = cdm,
                                                            name = paste0(taviCohortName, "_direct"),
                                                            conceptSet = codelist["tavi"],
                                                            exit = "event_start_date")
  
  if(isTRUE(restrictions)) {
    cdm[[paste0(taviCohortName, "_direct")]] <- cdm[[paste0(taviCohortName, "_direct")]] |>
      CohortConstructor::requireIsFirstEntry() |>
      CohortConstructor::requireInDateRange(dateRange = study_period)
  }
  
  cdm <- bind(cdm[[paste0(taviCohortName, "_from_additional")]],
              cdm[[paste0(taviCohortName, "_direct")]],
              name = taviCohortName)
  
  cdm[[taviCohortName]] <- unionCohorts(cdm[[taviCohortName]]) |>
    renameCohort("tavi")
  
  cdm[[saviCohortName]] <- cdm[[avrCohortName]] |>
    requireCohortIntersect(targetCohortTable = taviCohortName,
                           window = c(0, 0),
                           intersections = c(0, 0),
                           name = saviCohortName) |>
    renameCohort("savr")
  
  if (dbName == "CPRD GOLD") {
    cdm <- bind(cdm[[avrCohortName]], cdm[[taviCohortName]], cdm[[saviCohortName]], name = proceduresCohortName)
  } else {
    cdm <- bind(cdm[[taviCohortName]], cdm[[saviCohortName]], name = proceduresCohortName)
  }
  
  cdm <- dropSourceTable(cdm, name = c(paste0(taviCohortName, "_from_additional"),
                                       paste0(taviCohortName, "_direct"),
                                       saviCohortName,
                                       taviCohortName)
  )
                         
  return(cdm)
}

findFiles <- function(path, type, call = parent.frame()) {
  assertCharacter(path, length = 1, call = call)
  if (!file.exists(path)) {
    cli::cli_warn("directory {.path {path}} does not exist, output will be empty")
    return(list())
  }
  if (file.info(path)$isdir) {
    path <- list.files(path = path, full.names = TRUE)
  }
  path <- path[tools::file_ext(path) == type]
  names(path) <- tools::file_path_sans_ext(basename(path))
  as.list(path)
}

importCodelistWithDetails <- function(path){
  files <- findFiles(path, type)

  codelist <- purrr::map(files, \(x) read_csv(x, col_names = TRUE)) |>
    newCodelistWithDetails()
  
  return(codelist)
}
