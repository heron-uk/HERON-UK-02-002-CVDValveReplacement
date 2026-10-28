exportSummarisedResult(results[["snapshot"]],
                       minCellCount = min_cell_count,
                       fileName = "results_snapshot_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["obs_period"]],
                       minCellCount = min_cell_count,
                       fileName = "results_obs_period_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["attrition_tavi_additional"]],
                       minCellCount = min_cell_count,
                       fileName = "results_attrition_tavi_additional_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["attrition_tavi_direct"]],
                       minCellCount = min_cell_count,
                       fileName = "results_attrition_tavi_direct_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["code_use"]],
                       minCellCount = min_cell_count,
                       fileName = "results_code_use_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["code_use_efi"]],
                       minCellCount = min_cell_count,
                       fileName = "results_code_use_efi_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["code_use_orphan"]],
                       minCellCount = min_cell_count,
                       fileName = "results_code_use_orphan_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_inf"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_inf_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_attrition_indications_inf"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_attrition_indications_inf_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_attrition_procedures"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_attrition_procedures_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_365"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_365_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_attrition_indications_365"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_attrition_indications_365_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["incidence"]],
                       minCellCount = min_cell_count,
                       fileName = "results_incidence_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["incidence_per_groups"]],
                       minCellCount = min_cell_count,
                       fileName = "results_incidence_per_groups_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["table_one"]],
                       minCellCount = min_cell_count,
                       fileName = "results_table_one_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["scores"]],
                       minCellCount = min_cell_count,
                       fileName = "results_scores_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["overlap_by_risk"]],
                       minCellCount = min_cell_count,
                       fileName = "results_overlap_by_risk_{cdm_name}_{date}.csv",
                       path = here("Results"))

