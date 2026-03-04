x <- importCodelist(path = here("codelist", "comorbidities"), type = "csv")
i <- importCodelist(path = here("codelist", "indications"), type = "csv")

codes <- cdm_vocab_2025_08[["concept"]] |>
  filter(concept_id %in% !!as.integer(x[["valve_disorder_excl_endocarditis"]])) |>
  filter(!c(concept_id %in% !!as.integer(i[["aortic_stenosis"]]))) |>
  pull("concept_id")

concomitant_valve_disorders_excluding_endocarditis <- list(
  "concomitant_valve_disorders_excluding_endocarditis" =  codes
)

exportCodelist(concomitant_valve_disorders_excluding_endocarditis, path = here("codelist", "comorbidities"), type = "csv")
