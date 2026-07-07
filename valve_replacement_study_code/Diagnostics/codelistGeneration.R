library(CodelistGenerator)
library(here)

# rheumatologic_disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(257628, 134442, 80800, 80809, 256197, 255348))
exportCodelist(list("rheumatologic_disease_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

# mild_liver_disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(4064161, 4212540))
exportCodelist(list("mild_liver_disease_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

# diabetes_with_chronic_complications
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(443767, 442793))
exportCodelist(list("diabetes_with_chronic_complications_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

# hemiplegia_or_paraplegia
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(192606, 374022))
exportCodelist(list("hemiplegia_or_paraplegia_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

# renal_disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(4030518))
exportCodelist(list("renal_disease_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

# any_malignancy
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(443392))
exportCodelist(list("any_malignancy_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

# moderate_to_severe_liver_disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(4245975, 4029488, 192680, 24966))
exportCodelist(list("moderate_to_severe_liver_disease_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

# metastatic_solid_tumor
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(432851))
exportCodelist(list("metastatic_solid_tumor_not_reviewed" = x$concept_id), 
               path = here("reviewed_codelist"), 
               type = "csv")

