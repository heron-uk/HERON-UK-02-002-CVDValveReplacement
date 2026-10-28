cdm[["aortic_stenosis"]] <- cdm[["aortic_stenosis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf))

cdm[["aortic_insufficiency"]] <- cdm[["aortic_insufficiency"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf))

cdm[["aortic_stenosis_insufficiency_codes"]] <- cdm[["aortic_stenosis_insufficiency_codes"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf))

cdm[["aortic_endocarditis"]] <- cdm[["aortic_endocarditis"]] |>
  requireCohortIntersect(targetCohortTable = "aortic_valve_replacement", 
                         window = c(0, 365), 
                         intersections = c(1,Inf))