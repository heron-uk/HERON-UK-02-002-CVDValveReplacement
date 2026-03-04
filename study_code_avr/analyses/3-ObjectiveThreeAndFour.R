omopgenerics::logMessage(message = "STARTING OBJECTIVES 3 AND 4")

cdm[["proc"]] <- cdm[["proc"]] |>
  mutate("calendar_year" = get_year(cohort_start_date))

summariseCharacteristics(cdm[["proc"]], 
                         strata = c("sex", "age_group", "calendar_year", c("calendar_year", "sex"), c("calendar_year", "age_group")),
                         cohortIntersectFlag = 
                           list(
                             "Comorbidities one year prior to index date" = list(
                               targetCohortTable = "comorbidities",
                               window = c(-365, 0)
                             ),
                             "Treatments 31 days prior to index date" = list(
                               targetCohortTable = "treatments",
                               window = c(-31, 0)
                             )))

omopgenerics::logMessage(message = "OBJECTIVES 3 AND 4 FINISHED")
