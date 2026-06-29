x <- read_csv(here("cohorts", "hospital_frailty_risk_score", "icd_mapping", "hospital_frailty_score.csv")) 

cdm_vocab_2025_08 <- insertTable(cdm_vocab_2025_08,
                                 name = "icd10_code",
                                 table = x)
