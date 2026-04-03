x <- CodelistGenerator::importCodelist(path = here::here("cohorts", "study_codelists"), type = "csv")
x <- CodelistGenerator::asCodelistWithDetails(x, cdm_vocab_2025_08)

purrr::imap_dfr(x, ~ dplyr::mutate(.x, codelist_name = .y)) |>
  dplyr::filter(codelist_name %in% c("aortic_stenosis_avr", "aortic_endocarditis_avr", "aortic_insufficiency_avr")) |>
  readr::write_csv(here::here("Report", "codelists.csv"))
