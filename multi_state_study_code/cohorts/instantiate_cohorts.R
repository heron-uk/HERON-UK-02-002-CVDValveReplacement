source(here::here("analyses", "functions.R"))

omopgenerics::logMessage(message = "Importing codelists")

codelist <- omopgenerics::importCodelist(here::here("codelist"), "csv")

# For testing uncomment the below to get counts
# codelist <- codelist |> CodelistGenerator::addConcepts(cdm,
#                                            concepts = c(316139L),
#                                            codelistName = "aortic_valve_replacement")

omopgenerics::logMessage(message = "Instastiating cohorts")
omopgenerics::logMessage(message = " -- AS and AVR concept cohorts")
cdm$study_cohorts_inc <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = c(
    codelist["aortic_stenosis"],
    codelist["aortic_valve_replacement"]
  ),
  name = "study_cohorts_inc",
  exit = "event_start_date"
)

omopgenerics::logMessage(message = " -- Study criteria")
cdm$study_cohorts <- cdm$study_cohorts_inc |>
  CohortConstructor::requireIsFirstEntry(name = "study_cohorts") |>
  CohortConstructor::exitAtObservationEnd(
    cohortId = c("aortic_stenosis"),
    name = "study_cohorts"
  )
cdm$study_cohorts <- cdm$study_cohorts |>
  # we allow AS and AVR to occur on the same day
  CohortConstructor::requireCohortIntersect(
    cohortId = "aortic_valve_replacement",
    targetCohortTable = "study_cohorts_inc",
    targetCohortId = "aortic_stenosis",
    intersections = c(1, Inf),
    window = c(-Inf, -1),
    name = "study_cohorts"
  ) |>
  CohortConstructor::requireCohortIntersect(
    cohortId = "aortic_stenosis",
    targetCohortTable = "study_cohorts_inc",
    targetCohortId = "aortic_valve_replacement",
    intersections = 0L,
    window = c(-Inf, -1),
    name = "study_cohorts"
  ) |>
  CohortConstructor::requireAge(
    ageRange = list(c(20, Inf)),
    name = "study_cohorts"
  ) |>
  CohortConstructor::requireInDateRange(
    dateRange = study_period,
    name = "study_cohorts"
  ) |>
  addEthnicity() |>
  addSES() |>
  PatientProfiles::addDemographics(
    sex = sex,
    age = FALSE,
    ageGroup = age_groups,
    priorObservation = FALSE,
    futureObservation = FALSE,
    name = "study_cohorts"
  )

omopgenerics::logMessage(message = "Comorbidity cohorts")
cdm$comorbidities <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = codelist[stringr::str_detect(names(codelist),
                                "aortic_stenosis|aortic_valve_replacement",
                                negate = TRUE)],
  name = "comorbidities",
  exit = "event_start_date"
) |> 
  CohortConstructor::exitAtObservationEnd()

## CKD stage from measurements
egfr_codes <- c(
  1619025,  1619026, 3029829,  3029859, 3030104,  3045262,
  3049187,  3053283, 3964988,  3965919, 4213477,  36031320,
  36031846,  36303797, 36304157,  36306178, 36307905,  36660257,
  37393690,  37399046,  40764999,  40769275,
  40771922,  42869913,  46236952,  4338520, 36303653,  37208635,
  37393011,  37393012,  40478895,  40478963, 40483219,  40485075,
  40490315,  44788275,  44790060,  44790183,  44806420,  44808279, 45766361
)
cdm$ckd_stage_meausurement <- measurementCohort(
  cdm = cdm,
  conceptSet = list("egfr" = egfr_codes),
  valueAsNumber = list("ckd_stage_1_meas" = list("8795" = c(90, 9999999),
                                                 "720870" = c(90, 9999999)),
                       "ckd_stage_2_meas" = list("8795" = c(60, 89.99999),
                                                 "720870" = c(60, 89.99999)),
                       "ckd_stage_3_meas" = list("8795" = c(30, 59.99999),
                                                 "720870" = c(30, 59.99999)),
                       "ckd_stage_4_meas" = list("8795" = c(15, 29.99999),
                                                 "720870" = c(15, 29.99999)),
                       "ckd_stage_5_meas" = list("8795" = c(0, 14.99999),
                                                 "720870" = c(0, 14.99999))
  ),
  name = "ckd_stage_meausurement"
)

## CKD stage from diagnoses -----
ckd_diag_codes <- list(ckd_stage_1_diag = c(765535, 46284566, 46284567, 46284570, 443614, 46270354,
                                            601161, 44782703, 45773576, 43531559, 44792226, 44792227,
                                            44784640, 43021853),
                       ckd_stage_2_diag = c(762000,	46284572, 46287169,46284575,
                                            443601,	46270355,	601162,	44782692,	45769901,	43531566,
                                            44792228,	44792229,	45757447,	43021836,	43021854),
                       ckd_stage_3_diag = c(37019193,	762001,	46284587,	46286992,
                                            46284588,	46284591,	46284592,	46284593,	443597,	46273636,
                                            601163,	44782691,	45771075,	43531653,
                                            44792230,	44792231,	45763854,
                                            44792232,	44792249,	45763855,	44792250,
                                            44792251,	45757446,	43021835,	43020456,	762033),
                       ckd_stage_4_diag = c(765536,	46284597,46284598,	46284599,
                                            443612,	46273514,	601164,	44782689,
                                            45769902,	43531577,	44792252,	44792253,
                                            45757445,	44784639,	43020457,	762034),
                       ckd_stage_5_diag = 	c(45768813,	760850,	46284600,	46284602,
                                             46284603,	443611, 46270356,
                                             601165,	44782690,	45769903,	43531562,
                                             37017813,	44792254,	37018761,	44792255,
                                             46273164,	37018886,	601166,	44782717,
                                             45769904,	45769906,	4030520,	4128200,
                                             4125970, 193782, 45772751,
                                             45757393,	45757392,	45757444,	762973,
                                             44784638,	43020437,	43020455,	43021864))

cdm$ckd_stage_diagnosis <- conceptCohort(cdm = cdm,
                                         ckd_diag_codes,
                                         name = "ckd_stage_diagnosis",
                                         exit = "event_start_date")
## combine
cdm <- bind(cdm$ckd_stage_meausurement,
            cdm$ckd_stage_diagnosis,
            name = "ckd_stage")

cdm$ckd_stage <- cdm$ckd_stage |>
  unionCohorts(cohortId = c("ckd_stage_1_meas", "ckd_stage_1_diag"),
               cohortName = "ckd_stage_1", name = "ckd_stage",
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_2_meas", "ckd_stage_2_diag"),
               cohortName = "ckd_stage_2", name = "ckd_stage",
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_3_meas", "ckd_stage_3_diag"),
               cohortName = "ckd_stage_3", name = "ckd_stage",
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_4_meas", "ckd_stage_4_diag"),
               cohortName = "ckd_stage_4", name = "ckd_stage",
               keepOriginalCohorts = TRUE) |>
  unionCohorts(cohortId = c("ckd_stage_5_meas", "ckd_stage_5_diag"),
               cohortName = "ckd_stage_5", name = "ckd_stage",
               keepOriginalCohorts = TRUE) |>
  subsetCohorts(cohortId = c("ckd_stage_1","ckd_stage_2","ckd_stage_3",
                             "ckd_stage_4","ckd_stage_5"),
                name = "ckd_stage")



omopgenerics::logMessage(message = "Get denominator cohort")

cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator",
  cohortDateRange = study_period,
  ageGroup = c(
    list(
      c(20, 150)
    ),
    age_groups
  ),
  sex = c("Both", "Male", "Female"),
  daysPriorObservation = 365
)

omopgenerics::logMessage(message = "Add socioeconomic status")

cdm$denominator <- cdm$denominator |>
  addSES() |>
  dplyr::compute(name = "denominator")
