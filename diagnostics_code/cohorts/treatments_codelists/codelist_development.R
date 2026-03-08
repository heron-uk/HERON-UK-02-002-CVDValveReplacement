x <- getDrugIngredientCodes(cdm_vocab_2025_08,
                            name = c("spironolactone", "eplerenone"),
                            nameStyle = "{concept_name}",
                            type = "codelist_with_details")
x <- unionCodelists(x)
names(x) <- "mineralocorticoid_receptor_antagonists"
write_csv(x[[1]], file = here("cohorts", "treatments_codelists", "mineralocorticoid_receptor_antagonists.csv"))

x <- getDrugIngredientCodes(cdm_vocab_2025_08,
                            name = c("furosemide"),
                            nameStyle = "{concept_name}",
                            type = "codelist_with_details")
write_csv(x[[1]], file = here("cohorts", "treatments_codelists", "furosemide.csv"))

x <- getDrugIngredientCodes(cdm_vocab_2025_08,
                            name = c("canagliflozin", "dapagliflozin", "empagliflozin", "ertugliflozin"),
                            nameStyle = "{concept_name}",
                            type = "codelist_with_details")
x <- unionCodelists(x)
names(x) <- "isglt2"
write_csv(x[[1]], file = here("cohorts", "treatments_codelists", "isglt2.csv"))

