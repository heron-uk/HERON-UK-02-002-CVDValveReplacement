# Activity limitation
newCodelist(list("activity_limitation_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                          conceptId = c(44811145, 45767124, 
                                                                                        36716238, 4110470, 
                                                                                        4109859, 4032520, 
                                                                                        4031882, 4137049, 
                                                                                        4030753, 36713755, 
                                                                                        4128088)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Arthritis
newCodelist(list("arthritis_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                conceptId = c(429102)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Cerebrovascular disease
newCodelist(list("cerebrovascular_disease_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                              conceptId = c(381591)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Dizziness
newCodelist(list("dizziness_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                conceptId = c(4223938, 4297376)), "concept_id"))) |>
  excludeConcepts(cdm = cdm_vocab_2025_08,
                  concepts = c(4011939, 4012876)) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")


# Dyspnea
newCodelist(list("dyspnea_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                              conceptId = c(4305080, 4041664)), "concept_id"))) |>
  excludeConcepts(cdm = cdm_vocab_2025_08,
                  concepts = c(45772947, 42539560, 4318857, 258866)) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Falls
newCodelist(list("falls_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                            conceptId = c(4329906, 4256754, 4087528, 4224116)), "concept_id"))) |>
  addConcepts(cdm = cdm_vocab_2025_08, concepts = c(4323345, 435991, 436583, 4184243)) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")


# Foot problem
newCodelist(list("foot_problem_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                   conceptId = c(4268887, 4053100, 4101512, 4136647, 
                                                                                 4138349, 4140790, 4140924, 42539590, 
                                                                                 4083436, 4139895, 4085778, 4139217,
                                                                                 4139218, 42539494, 4139705)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Hearing impairment
newCodelist(list("hearing_impairment_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                         conceptId = c(36715579, 439378, 42539697,
                                                                                       4246497, 378444, 377889, 379832,
                                                                                       444291, 44805060)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Heart valve disorder
newCodelist(list("heart_valve_disorder_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                           conceptId = c(4281749)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")


# Housebound
newCodelist(list("housebound_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                 conceptId = c(40299189, 4052962, 45877743)), "concept_id"))) |>
  addConcepts(cdm = cdm_vocab_2025_08, concepts = c(4022076, 4022523)) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Hypotension
newCodelist(list("hypotension_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                  conceptId = c(317002, 319041, 135360, 40316030, 
                                                                                40350983, 40498271)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")


# Memory and cognitive problems
newCodelist(list("memory_and_cognitive_problems_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                                    conceptId = c(4333671, 4009705, 3654469,
                                                                                                  42539256, 42535706, 42535682,
                                                                                                  42535681, 42539271, 42539270,
                                                                                                  42535016, 42535018, 42535017,
                                                                                                  40480615, 3654907, 761978,
                                                                                                  4023989, 4182210, 42690615,
                                                                                                  42689981, 42689830, 42689982,
                                                                                                  42537139, 4022572, 42690112, 
                                                                                                  42690742, 42690113, 42689849,
                                                                                                  4043378, 443432, 4047110, 4297400,
                                                                                                  439795, 45765899, 46271045, 4103572,
                                                                                                  4135668, 4085496, 4084412, 4131380,
                                                                                                  40482301, 45765900, 42690368, 
                                                                                                  42690369, 42690647, 42690370,
                                                                                                  4141586)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Mobility and transfer problems
newCodelist(list("mobility_and_transfer_problems_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                                     conceptId = c(4200353, 4199115,  4199721, 44790310, 45878557, 36716239,
                                                                                                   36716240, 4146424, 4136754, 46272933, 4200194,4199094, 4199114,
                                                                                                   4199116, 4199431, 4200817, 4107789, 4107851, 4199552, 4093668,
                                                                                                   4154006, 36714126, 4086550, 4112788, 4199112, 4199725, 4200815,
                                                                                                   4200193, 4200798, 4199113, 4200183, 4084746, 4106333, 4199551,
                                                                                                   4106335, 4119464, 4154005, 4086871, 4086549, 4112787, 4295037,
                                                                                                   4009877, 1621081, 4031883, 4306934, 4032531, 3198828, 4010359,
                                                                                                   4052049, 4053076, 4052047, 1314392, 1314394, 44790681, 4310235,
                                                                                                   4012646, 4012944, 4199550, 4199093, 4199111, 4200355, 4200350,
                                                                                                   4105451, 4106332,  4023187, 4060223, 4118805, 4151066, 4086548,
                                                                                                   44792042, 4086874,4116707, 44789400, 4012945, 45878235, 4266144,
                                                                                                   439405, 4023190, 4086557, 4200822)), "concept_id"))) |>
  addConcepts(cdm = cdm_vocab_2025_08, concepts = c(4240470)) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Peptic ulcer
newCodelist(list("peptic_ulcer_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                   conceptId = c(4057060, 4134146, 4027663)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Peripheral vascular disease
newCodelist(list("peripheral_vascular_disease_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                                  conceptId = c(321052)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Requirement for health
newCodelist(list("requirement_for_care_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                           conceptId = c(35609081, 4052486,  4192880, 44791364, 4074789, 4022081, 3661927, 44790305,
                                                                                         40486978, 44790706, 44791204, 44802299, 44788859, 4147552, 765265, 36713971,
                                                                                         44814152, 44814153, 4119866, 44804659, 4305680, 37310422, 4088536,37108723)), "concept_id"))) |>
  excludeConcepts(cdm = cdm_vocab_2025_08,
                  concepts = pull(getDescendants(cdm_vocab_2025_08,
                                                 c(4109825, 4107867, 4074604, 4108915, 3656375, 4075529, 4105407, 4076506, 
                                                   4053092, 4075531, 4109824, 4110771, 4109524, 4119859, 4073324, 4114884,
                                                   4075532,4075519,4076509,4075530,3656380,4075533)), "concept_id")) |> 
  addConcepts(cdm = cdm_vocab_2025_08, concepts = c(44788849, 44807727,4081589,42535090)) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Skin ulcer
newCodelist(list("skin_ulcer_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                 conceptId = c(4262920,46269752, 46269755)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Sleep disorder
newCodelist(list("sleep_disorder_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                     conceptId = c(4086851,37110488,4115402,42689991,42689992,43530738,4204989,42690122,42690123,
                                                                                   42690715,435657,434172,436522,374905,4102985,443544,3173994,4200883,4215402,
                                                                                   4305303,435524,40482260,40480927,4132137,42690379,42690380)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Social vulnerability
newCodelist(list("social_vulnerability_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                           conceptId = c(42690410,44792191, 4147192, 44807727, 44805674,44805255, 44805672,37394063,44813864,
                                                                                         44806914, 44792192,4023168, 4052158, 4053087, 45879223,44789099, 44789487,44810043,
                                                                                         44790469,44789986,37208707,4052789, 4209159, 44788883,4221049, 44791055, 44791931,
                                                                                         4151777,44803964, 4116985)), "concept_id"))) |>
  excludeConcepts(cdm = cdm_vocab_2025_08,
                  concepts = pull(getDescendants(cdm_vocab_2025_08,
                                                 c(4282391, 4020464,  4022656,  4223456,  4291024, 4199933, 4006912,  4019971,  4022658,
                                                   4023164, 4022078, 4022657, 4019970, 43020462, 4023165, 4023163, 4022077,
                                                   4114038,4019966, 4019965, 37018943, 4022652, 4211114, 42536692, 4030415,
                                                   4326235, 4167671, 4052607, 4329840, 4172693, 4172692, 4014701, 4195777,
                                                   4052171, 4146983, 4051630, 4052603, 4305714,4278980, 4049093, 4295444,
                                                   4019960, 4019959, 4215977, 4019968, 4312424, 764672, 4053103, 4052170,
                                                   4189812, 765265,4298549,4151783, 4140516, 4023162, 4308320, 4172696,
                                                   4058255, 4059169, 4309537,4171748,4175382,4022070, 37018379, 4075489,
                                                   4022655)), "concept_id")) |> 
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Thyroid disease
newCodelist(list("thyroid_disease_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                      conceptId = c(141253, 4194160, 141253)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")

# Urinary incontinence
newCodelist(list("urinary_incontinence_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                           conceptId = c(4032498,4030763, 37208161,4302457, 606405,193598,195007, 45770268, 195079, 444035,
                                                                                         4314023,40490423,4012368, 42872846, 40480232, 42538539,443524,193874, 197378,
                                                                                         42538537, 4126278,44808460,40481801, 42536555, 606955, 4032530, 4096552,197102,
                                                                                         42538538, 193326, 197672,37119132,45757352,4153667, 4172646, 4092642)), "concept_id"))) |>
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")


# Visual impairment
newCodelist(list("visual_impairment_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                        conceptId = c(374034,375545,435262,437541, 4265433,4334259, 44797518, 40305578, 4023310,
                                                                                      375545,4265433,42872584)), "concept_id"))) |>
  excludeConcepts(cdm = cdm_vocab_2025_08,
                  concepts = c(44790749,
                               pull(getDescendants(cdm_vocab_2025_08,
                                                   c(379820, 4124127, 4081307, 4079185)), "concept_id"))) |> 
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")


# Weight loss and anorexia
newCodelist(list("weight_loss_and_anorexia_not_reviewed" = pull(getDescendants(cdm_vocab_2025_08, 
                                                                               conceptId = c(436675,44784528,4269485,4300305,4091029,4333683, 134765,4109384,763515,44788734,
                                                                                             37204325, 45773690,442165, 37312021, 4156515, 4216971, 4347292, 36676905,
                                                                                             4031170, 4078430, 4031171, 4123542, 4229881)), "concept_id"))) |>
  
  exportCodelist(path = here("codelists", "efi_codelists"), type = "csv")