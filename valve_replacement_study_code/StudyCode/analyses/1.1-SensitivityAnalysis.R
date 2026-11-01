omopgenerics::logMessage(message = "Starting sensitivity analysis")
window <- "inf"; window_value <- Inf
cdm[["aortic_stenosis_indication"]] <- copyCohorts(cdm[["aortic_stenosis_sa"]], 
                                                   name = "aortic_stenosis_indication")
