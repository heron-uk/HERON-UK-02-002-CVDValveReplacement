addCombinations <- function(cohort, name){
  cols <- colnames(cohort)[which(grepl("indication_", colnames(cohort)))]
  
  grid <- expand.grid(replicate(length(vars), c(0, 1), simplify = FALSE))
  colnames(grid) <- vars
  combined_grid <- grid[rowSums(grid) >= 2, ]
  logic_strings <- apply(combined_grid, 1, function(row) {
    paste(names(row), "==", row, collapse = " & ")
  })
  
  new_names <- sapply(logic_strings, function(x) {
    parts <- unlist(strsplit(x, " & "))
    active_vars <- parts[grep("== 1", parts)]
    clean_names <- gsub(" == 1", "", active_vars)
    paste(clean_names, collapse = "_")
  })
  new_names <- unname(new_names)
  
  new_names <- paste0("indication_",gsub("indication_|aortic_","", new_names))
  
  for(i in seq_along(logic_strings)) {
    cohort <- cohort |>
      mutate(!!new_names[i] := if_else(!!parse_expr(logic_strings[[i]]), 1L, 0L)) |>
      compute(temporary = FALSE, name = name)
  }
  
  logic_strings <- paste(paste(cols, collapse = " == 0 & "), "== 0")
  new_names <- "indication_no_indication_identified"
  cohort <- cohort |>
    mutate(!!new_names := if_else(!!parse_expr(logic_strings), 1L, 0L)) |>
    compute(temporary = FALSE, name = name)
  
  return(cohort)
}

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
