omopgenerics::logMessage(message = "STARTING OBJECTIVE ONE")

window <- "365"; window_value <- 365L
cdm[["aortic_stenosis_obj_one"]] <- copyCohorts(cdm[["aortic_stenosis"]], 
                                                name = "aortic_stenosis_obj_one")

source(here("analyses", "1.0-ObjectiveOne.R"))

omopgenerics::logMessage(message = "Creating indication")
cdm <- bind(cdm[["aortic_stenosis_isolated"]], cdm[["aortic_stenosis_insufficiency_isolated"]],
            name = "aortic_stenosis_indication")
cdm[["aortic_stenosis_indication"]] <- unionCohorts(cohort = cdm[["aortic_stenosis_indication"]],
                                                    keepOriginalCohorts = FALSE)
omopgenerics::logMessage(message = "FINISH OBJECTIVE ONE")
