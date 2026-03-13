# ACEi and ARBs -------
acei_arbs <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c(# acei
    "captopril", "enalapril", "fosinopril",
    "imidapril", "lisinopril", "perindopril", "quinapril",
    "ramipril", "trandolapril",
    # arbs
    "azilsartan", "candesartan", "eprosartan", "irbesartan",
    "losartan", "olmesartan", "telmisartan", "valsartan"),
  nameStyle = "{concept_name}",
  type = "codelist") |>
  unionCodelists()
names(acei_arbs) <- "acei_arbs"
exportCodelist(acei_arbs, path = here::here("cohorts", "treatments_codelists"),
               type = "csv")

# beta blockers -------
beta_blockers <- getDrugIngredientCodes(
  cdm = cdm_vocab_2025_08,
  name = c("acebutolol", "alprenolol", "atenolol",
           "bisoprolol", "carvedilol",
           "metoprolol", "nadolol",
           "oxprenolol", "pindolol",
           "propranolol", "timolol"),
  nameStyle = "{concept_name}",
  type = "codelist") |>
  unionCodelists()
names(beta_blockers) <- "beta_blockers"
exportCodelist(beta_blockers, path = here::here("cohorts", "treatments_codelists"),
               type = "csv")


x <- getDrugIngredientCodes(cdm_vocab_2025_08,
                            name = c("spironolactone", "eplerenone"),
                            nameStyle = "{concept_name}",
                            type = "codelist")
x <- unionCodelists(x)
names(x) <- "mineralocorticoid_receptor_antagonists"
omopgenerics::exportCodelist(x, path = here::here("cohorts", "treatments_codelists"),
                             type = "csv")

x <- getDrugIngredientCodes(cdm_vocab_2025_08,
                            name = c("furosemide","bumetanide"),
                            nameStyle = "{concept_name}",
                            type = "codelist")
x <- unionCodelists(x)
omopgenerics::exportCodelist(x, path = here::here("cohorts", "treatments_codelists"),
                             type = "csv")

x <- getDrugIngredientCodes(cdm_vocab_2025_08,
                            name = c("canagliflozin", "dapagliflozin", "empagliflozin", "ertugliflozin"),
                            nameStyle = "{concept_name}",
                            type = "codelist")
x <- unionCodelists(x)
names(x) <- "isglt2"
omopgenerics::exportCodelist(x, path = here::here("cohorts", "treatments_codelists"),
                             type = "csv")

