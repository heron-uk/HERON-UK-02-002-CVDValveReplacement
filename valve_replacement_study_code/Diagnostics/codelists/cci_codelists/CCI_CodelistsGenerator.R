# rheumatologic_disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(134442))
exportCodelist(list("systemic_sclerosis_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(80800))
exportCodelist(list("polymyositis_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(256197))
exportCodelist(list("rheumatoid_lung_disease_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(255348))
exportCodelist(list("polymyalgia_rheumatica_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

# mild liver disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(4064161))
exportCodelist(list("cirrhosis_of_liver_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(194984))
exportCodelist(list("disease_of_liver_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

# Diabetes with chronic complications
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(443767))
exportCodelist(list("eye_disorder_due_to_dm_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(442793))
exportCodelist(list("complications_due_to_dm_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")


# Hemiplegia or paraplegia
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(192606))
exportCodelist(list("hemiplegia_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(374022))
exportCodelist(list("paraplegia_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

# Malignant neoplastic disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(443392))
exportCodelist(list("malignant_neoplastic_disease_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

# moderate or severe liver disease
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(4245975))
exportCodelist(list("hepatic_failure_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(4029488))
exportCodelist(list("hepatic_encephalopathy_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(192680))
exportCodelist(list("portal_hypertension_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(24966))
exportCodelist(list("esophageal_varices_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")

# metastatic solid tumor
x <- getDescendants(cdm_vocab_2025_08,
                    conceptId = c(432851))
exportCodelist(list("metastatic_solid_tumor_not_reviewed" = x$concept_id), 
               path = here("codelists", "cci_codelists"), 
               type = "csv")


