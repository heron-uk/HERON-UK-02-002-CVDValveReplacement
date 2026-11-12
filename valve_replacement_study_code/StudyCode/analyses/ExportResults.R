# Run study ----
exportSummarisedResult(results[["snapshot"]],
                       minCellCount = min_cell_count,
                       fileName = "results_snapshot_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["obs_period"]],
                       minCellCount = min_cell_count,
                       fileName = "results_obs_period_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Attrition ----
exportSummarisedResult(results[["objective_one_attrition_procedures"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_attrition_procedures_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["attrition_tavi_additional"]],
                       minCellCount = min_cell_count,
                       fileName = "results_attrition_tavi_additional_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["attrition_tavi_direct"]],
                       minCellCount = min_cell_count,
                       fileName = "results_attrition_tavi_direct_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Code use ----
exportSummarisedResult(results[["code_use"]],
                       minCellCount = min_cell_count,
                       fileName = "results_code_use_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["aortic_stenosis_code_use"]],
                       minCellCount = min_cell_count,
                       fileName = "results_aortic_stenosis_code_use_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["code_use_efi"]],
                       minCellCount = min_cell_count,
                       fileName = "results_code_use_efi_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["code_use_orphan"]],
                       minCellCount = min_cell_count,
                       fileName = "results_code_use_orphan_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Objective 1 ----
exportSummarisedResult(results[["objective_one_inf"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_inf_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_365"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_365_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_aortic_stenosis_insufficiency_codes_inf"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_aortic_stenosis_insufficiency_codes_inf_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_aortic_stenosis_insufficiency_codes_365"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_aortic_stenosis_insufficiency_codes_365_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_aortic_stenosis_insufficiency_inf"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_aortic_stenosis_insufficiency_inf_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_aortic_stenosis_insufficiency_365"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_aortic_stenosis_insufficiency_365_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_attrition_indications_inf"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_attrition_indications_inf_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["objective_one_attrition_indications_365"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_one_attrition_indications_365_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Objective 2 ----
exportSummarisedResult(results[["incidence"]],
                       minCellCount = min_cell_count,
                       fileName = "results_incidence_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["incidence_cci"]],
                       minCellCount = min_cell_count,
                       fileName = "results_incidence_cci_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["incidence_efi"]],
                       minCellCount = min_cell_count,
                       fileName = "results_incidence_efi_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["incidence_general_pop"]],
                       minCellCount = min_cell_count,
                       fileName = "results_incidence_general_pop_{cdm_name}_{date}.csv",
                       path = here("Results"))

# Characterisation ----
exportSummarisedResult(results[["objective_three_attrition_procedures"]],
                       minCellCount = min_cell_count,
                       fileName = "results_objective_three_attrition_procedures_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["table_one"]],
                       minCellCount = min_cell_count,
                       fileName = "results_table_one_{cdm_name}_{date}.csv",
                       path = here("Results"))

# scores ---
exportSummarisedResult(results[["scores"]],
                       minCellCount = min_cell_count,
                       fileName = "results_scores_{cdm_name}_{date}.csv",
                       path = here("Results"))

exportSummarisedResult(results[["overlap_by_risk"]],
                       minCellCount = min_cell_count,
                       fileName = "results_overlap_by_risk_{cdm_name}_{date}.csv",
                       path = here("Results"))

