x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "aortic calcification")

write_csv(x, file = here("cohorts", "comorbidity_codelists", "aortic_calcification.csv"))

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "bicuspid aortic valve")

write_csv(x, file = here("cohorts", "comorbidity_codelists", "bicuspid_aortic_valve.csv"))

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "unicuspid aortic valve")

write_csv(x, file = here("cohorts", "comorbidity_codelists", "unicuspid_aortic_valve.csv"))

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "mitral regurgitation")

write_csv(x, file = here("cohorts", "comorbidity_codelists", "mitral_regurgitation.csv"))

x <- getCandidateCodes(cdm_vocab_2025_08,
                       keywords = "left ventricular hypertrophy")

write_csv(x, file = here("cohorts", "comorbidity_codelists", "left_ventricular_hypertrophy.csv"))




