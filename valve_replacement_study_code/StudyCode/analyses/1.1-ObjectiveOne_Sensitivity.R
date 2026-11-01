omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

omopgenerics::logMessage(message = "Starting sensitivity analysis")
window <- "inf"; window_value <- Inf
cdm[["aortic_stenosis_obj_one"]] <- copyCohorts(cdm[["aortic_stenosis_sa"]], 
                                                name = "aortic_stenosis_indication")

source(here("analyses", "1.0-ObjectiveOne.R"))

omopgenerics::logMessage(message = "Creating indication")
cdm <- bind(cdm[["aortic_stenosis_isolated"]], cdm[["aortic_stenosis_insufficiency_isolated"]],
            name = "aortic_stenosis_indication_sa")
cdm[["aortic_stenosis_indication_sa"]] <- unionCohorts(cohort = cdm[["aortic_stenosis_indication_sa"]],
                                                       keepOriginalCohorts = FALSE)

omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
