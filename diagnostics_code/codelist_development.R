
# procedures -----
aortic_valve_replacement_opcs4 <- c("K26.1","K26.2","K26.3","K26.4",
                                    "K26.5", # note has no mapping
                                    "K26.8","K26.9")
tavi_additional_opcs4 <- c("Y49.4", "Y79.1", "Y79.2", "Y79.3", "Y79.4", "Y79.5",
                           "Y79.8", "Y79.9")
tavi <- list(tavi = c(36676805L, 37157401L)) |>
  omopgenerics::newCodelist()

opcs4_to_standard <- function(opcs4_codes){
cdm_vocab_2025_08$concept |>
  dplyr::filter(concept_code %in% !!opcs4_codes,
                vocabulary_id == "OPCS4") |>
  dplyr::select("concept_id_1" = "concept_id") |>
  dplyr::left_join(cdm_vocab_2025_08$concept_relationship, by = "concept_id_1") |>
  dplyr::filter(relationship_id == "Maps to") |>
  dplyr::select("concept_id" = "concept_id_2") |>
  dplyr::left_join(cdm_vocab_2025_08$concept, by = "concept_id") |>
  dplyr::pull("concept_id")
  }
aortic_valve_replacement <- list(
  aortic_valve_replacement = opcs4_to_standard(aortic_valve_replacement_opcs4)) |>
  omopgenerics::newCodelist() |>
  CodelistGenerator::addConcepts(cdm_vocab_2025_08, c(4339824, 4166457, 4170281))  |>
  CodelistGenerator::excludeConcepts(cdm_vocab_2025_08, c(4141013, 44510982))

aortic_valve_replacement_potential_tavi <- aortic_valve_replacement |>
  excludeConcepts(cdm_vocab_2025_08,
                  concepts = c(4018560,
                               4018561,
                               4141013,
                               44510982))  |>
  CodelistGenerator::addConcepts(cdm_vocab_2025_08, c(4166457,4170281)) |>
  CodelistGenerator::excludeConcepts(cdm_vocab_2025_08, c(4339824))

names(aortic_valve_replacement_potential_tavi) <- "aortic_valve_replacement_potential_tavi"

tavi_additional <- list(
  tavi_additional = opcs4_to_standard(tavi_additional_opcs4)) |>
  omopgenerics::newCodelist()
tavi_additional <- tavi_additional |>
  CodelistGenerator::addConcepts(cdm_vocab_2025_08,4044372) |>
  CodelistGenerator::excludeConcepts(cdm_vocab_2025_08,4124619)


omopgenerics::exportCodelist(aortic_valve_replacement,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(aortic_valve_replacement_potential_tavi,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(tavi_additional,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(tavi,
                             here::here("cohorts", "study_codelists"), "csv")


# systematic search for aortic valve disease ----
library(writexl)
avd <- getCandidateCodes(cdm_vocab_2025_08,
                         domains = c("condition"),
                         keywords = c("aortic valve disease",
                                      "aortic stenosis",
                                      "valve regurgitation",
                                      "aortic endocarditis",
                                      "aortic insufficiency",
                                      "aortic regurgitation"),
                         searchInSynonyms = TRUE,
                         includeDescendants = TRUE)
avd <- avd |>
  filter(vocabulary_id %in% "SNOMED")

write_xlsx(list("codes_for_review" = avd |>
                  mutate(aortic_valve_disease = NA_character_,
                         aortic_stenosis = NA_character_,
                         aortic_endocarditis = NA_character_,
                         aortic_insufficiency = NA_character_)),
           "aortic_valve_disease_codes_for_review.xlsx")

# codes reviewed
library(readxl)
avd_reviewed <- read_xlsx(here::here("aortic_valve_disease_codes_reviewed.xlsx"), sheet = 1)
# colnames(avd_reviewed)

aortic_valve_disease <- list("aortic_valve_disease" = avd_reviewed |>
                               filter(aortic_valve_disease_broad == "T") |>
                               pull(concept_id)) |>
  omopgenerics::newCodelist()
aortic_stenosis <- list("aortic_stenosis" = avd_reviewed |>
                          filter(aortic_stenosis == "T") |>
                          pull(concept_id)) |>
  omopgenerics::newCodelist()
aortic_stenosis_avr <- list("aortic_stenosis_avr" = avd_reviewed |>
                          filter(aortic_stenosis == "T") |>
                          pull(concept_id)) |>
  omopgenerics::newCodelist()
# from orphan codes, add 4217691, 4195003, 4314258 to aortic stenosis avr
aortic_stenosis_avr <- aortic_stenosis_avr |>
  CodelistGenerator::addConcepts(cdm_vocab_2025_08,
                                 c(4230774, 4195003, 4314258))
aortic_stenosis_congenital <- list("aortic_stenosis_congenital" = avd_reviewed |>
                              filter(aortic_stenosis_congenital == "T") |>
                              pull(concept_id)) |>
  omopgenerics::newCodelist()


aortic_insufficiency_avr <- list("aortic_insufficiency_avr" = avd_reviewed |>
                               filter(aortic_insufficiency_avr == "T") |>
                               pull(concept_id)) |>
  omopgenerics::newCodelist()
# add 4230774 to aortic_insufficiency_avr
aortic_insufficiency_avr <- aortic_insufficiency_avr |>
  CodelistGenerator::addConcepts(cdm_vocab_2025_08,
                                 c(4230774))

aortic_endocarditis_avr <- list("aortic_endocarditis_avr" = avd_reviewed |>
                              filter(aortic_endocarditis_avr == "T") |>
                              pull(concept_id)) |>
  omopgenerics::newCodelist()
# add 4232337 to aortic endocarditis
aortic_endocarditis_avr <- aortic_endocarditis_avr |>
  CodelistGenerator::addConcepts(cdm_vocab_2025_08,
                                 c(4232337))






omopgenerics::exportCodelist(aortic_valve_disease,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(aortic_stenosis,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(aortic_stenosis_avr,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(aortic_stenosis_congenital,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(aortic_endocarditis_avr,
                             here::here("cohorts", "study_codelists"), "csv")
omopgenerics::exportCodelist(aortic_insufficiency_avr,
                             here::here("cohorts", "study_codelists"), "csv")

