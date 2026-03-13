createProceduresCohorts <- function(cdm, avrCohortName, taviCohortName, saviCohortName, proceduresCohortName, restrictions) {
  
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
  
  cdm <- bind(cdm[[avrCohortName]], cdm[[taviCohortName]], cdm[[saviCohortName]], name = proceduresCohortName)
  
  cdm <- dropSourceTable(cdm, name = c(paste0(taviCohortName, "_from_additional"),
                                       paste0(taviCohortName, "_direct"),
                                       saviCohortName,
                                       taviCohortName)
  )
  
  return(cdm)
}
