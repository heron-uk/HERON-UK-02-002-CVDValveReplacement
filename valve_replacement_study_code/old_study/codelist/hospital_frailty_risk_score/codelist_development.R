hfrs <- readr::read_csv(here::here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score_for_review.csv"))

cdm_vocab_2025_08 <- insertTable(cdm_vocab_2025_08,
                                 name = "hfrs",
                                 table = hfrs)

cdm_vocab_2025_08[["hfrs"]] |>
  left_join(
    cdm_vocab_2025_08$concept_relationship |>
      filter(relationship_id == "Maps to"),
    by = c("concept_id" = "concept_id_1")
  ) |>
  select("icd_concept_id" = "concept_id", "concept_code", "concept_id" = "concept_id_2" ,
         "icd_description", "points") |>
  left_join(
    cdm[["concept"]] |>
      select("concept_id", "concept_name", "domain_id", "vocabulary_id"),
    by = c("concept_id")
  ) |>
  collect() |>
  distinct() |>
  write_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score_for_review_1.csv"))

cl <- readr::read_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score_for_review_1.csv"))
codes <- list()
for (i in seq_along(cl[["concept_id"]]) ){
  name <- omopgenerics::toSnakeCase(cl[["icd_description"]][i])

  if( cl[["concept_code"]][i] == "G31" ){

    ci <- cl |>
      filter(concept_code == "G31") |>
      pull("concept_id")
    codes[[name]] <- CodelistGenerator::getDescendants(cdm_vocab_2025_08,
                                                       conceptId = ci)

  } else {
    codes[[name]] <- CodelistGenerator::getDescendants(cdm_vocab_2025_08,
                                                       conceptId = cl[["concept_id"]][i])
  }
}

codes <- newCodelistWithDetails(codes)

for (name in names(codes)) {
  write_csv(codes[[name]],
            file = here("cohorts", "hospital_frailty_risk_score", paste0(name, ".csv")))
}


