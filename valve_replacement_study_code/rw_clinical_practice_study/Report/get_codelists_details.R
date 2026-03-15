x <- CodelistGenerator::importCodelist(path = here::here("cohorts", "study_codelists"), type = "csv")
x <- CodelistGenerator::asCodelistWithDetails(x, cdm_vocab_2025_08)

purrr::imap_dfr(x, ~ dplyr::mutate(.x, codelist_name = .y)) |>
  readr::write_csv("codelists.csv")
