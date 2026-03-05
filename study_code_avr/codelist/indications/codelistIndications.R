
exportCodelist(list("aortic_endocarditis" = getCandidateCodes(cdm_vocab_2025_08,
                                                              keywords = "aortic endocarditis") |>
                      filter(vocabulary_id %in% "SNOMED") |>
                      pull("concept_id")),
               path = here("codelist", "indications"), 
               type = "csv")

x <- list("aortic_insufficiency" = getCandidateCodes(cdm_vocab_2025_08,
                                                     keywords = c("aortic insufficiency", "aortic_regurgitation")) |>
            filter(vocabulary_id %in% "SNOMED")) |>
  newCodelistWithDetails()
exc <- getDescendants(cdm = cdm_vocab_2025_08, conceptId = 320115) |> pull("concept_id")
x <- x |>
  CodelistGenerator::excludeConcepts(cdm_vocab_2025_08, concepts = exc) 
x <- x |> asCodelist()
exportCodelist(x,
               path = here("codelist", "indications"), 
               type = "csv")



