# shiny is prepared to work with this resultList:
resultList <- list(
  orphan_code_use = list(result_type = "orphan_code_use"),
  cohort_code_use = list(result_type = "cohort_code_use")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
