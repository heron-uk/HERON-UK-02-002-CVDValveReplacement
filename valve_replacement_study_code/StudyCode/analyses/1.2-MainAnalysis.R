omopgenerics::logMessage(message = "Starting sensitivity analysis")
window <- "365"; window_value <- 365L
cdm[["aortic_stenosis_indication"]] <- copyCohorts(cdm[["aortic_stenosis"]], 
                                                   name = "aortic_stenosis_indication")
