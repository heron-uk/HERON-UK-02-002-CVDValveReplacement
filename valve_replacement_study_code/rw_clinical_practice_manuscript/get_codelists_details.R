x <- CodelistGenerator::importCodelist(path = here::here("cohorts", "study_codelists"), type = "csv")
x <- CodelistGenerator::asCodelistWithDetails(x, cdm_vocab_2025_08)

purrr::imap_dfr(x, ~ dplyr::mutate(.x, codelist_name = .y)) |>
  dplyr::filter(codelist_name %in% c("aortic_stenosis_avr", "aortic_endocarditis_avr", "aortic_insufficiency_avr")) |>
  readr::write_csv(here::here("Report", "codelists_indications.csv"))

x[c("aortic_valve_replacement", "aortic_valve_replacement_potential_tavi", "tavi", "tavi_additional")] |>
  purrr::imap_dfr(~ dplyr::mutate(.x, codelist_name = .y)) |>
  dplyr::mutate(
    "cohort_name" = dplyr::case_when(
      codelist_name == "aortic_valve_replacement_potential_tavi" ~ "aortic_valve_replacement",
      codelist_name == "tavi_additional" ~ "tavi",
      .default = codelist_name
    )
  ) |>
  readr::write_csv(here::here("Report", "codelists_avr.csv"))
