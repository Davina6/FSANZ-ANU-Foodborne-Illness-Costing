

#Generic assumptions

gastroRate <- rdist(type = "pert_alt",lowq = 0.64,median = 0.74,highq = 0.84)

IBSMultiplier <- rdist("pert_alt",  lowq = 0.072, median = 0.088,  highq = 0.104) # only for Campy, Salmonella, and Shigella

GastroDRGCodes = list(`<5` = "G67B",
                      `5-64` = "G67B",
                      `65+` = "G67A")

MedicationsBacterial <- list(
  `<5`   = list(Antidiarrhoeal = rdist("pert",      min  = 0.0025, mode   = 0.003,  max   = 0.3), #Changed to match Laura's numbers -- different from the numbers Katie initially had in the table
                Painkillers    = rdist("pert_alt",  lowq = 0.12,   median = 0.38,   highq = 0.65) ,
                AntiNausea     = rdist("pert",      min  = 0.0025, mode   = 0.003,  max   = 0.305), #Changed to match Laura's numbers -- different from the numbers Katie initially had in the table
                AntiCramp      = rdist("discrete",  value = 0,     continuous = FALSE), #this is just a point mass at 0, but framing it as a distribution for consistency
                Antibiotics    = rdist("pert_alt",  lowq = 0.005,  median = 0.082,  highq = 0.305) # This was previously lowq = 0.005, but this allowed for negative values
  ),
  `5-64` = list(Antidiarrhoeal = rdist("pert_alt",  lowq = 0.166, median = 0.286,  highq = 0.439),
                Painkillers    = rdist("pert_alt",  lowq = 0.127, median = 0.242,  highq = 0.385),
                AntiNausea     = rdist("pert_alt",  lowq = 0.042, median = 0.12,   highq = 0.237),
                AntiCramp      = rdist("pert_alt",  lowq = 0.028, median = 0.077,  highq = 0.204),
                Antibiotics    = rdist("pert_alt",  lowq = 0.016, median = 0.067,  highq = 0.169)
  ),
  `65+`  = list(Antidiarrhoeal = rdist("pert_alt",  lowp = 0, lowq = 0.094, median = 0.653,  highq = 0.906, highp = 1), #Changed from original rdist("pert_alt",lowq = 0.094, median = 0.653,  highq = 0.906) as this allowed for negative values
                Painkillers    = rdist("pert_alt",  lowq = 0.051, median = 0.307,  highq = 0.708), #changed lowp from 0.008. makes surprisingly little difference to the bulk of the distribution) but is enough to prevent it from crossing 0
                AntiNausea     = rdist("pert_alt",  lowq = 0.051, median = 0.307,  highq = 0.708), #ditto
                AntiCramp      = rdist("pert_alt",  lowq = 0.051, median = 0.277,  highq = 0.708), #ditto
                Antibiotics    = rdist("pert_alt",  lowq  = 0,    median = 0.051,  highq = 0.104, lowp = 0)
  )
)

MedicationsGastro <- list(
  `<5`   = list(Antidiarrhoeal = rdist("pert_alt",  lowq  = 0.012, mode = 0.062,   highq = 0.27), #this was 2.5%, 50% and 97.5% in the report
                Painkillers    = rdist("pert_alt",  lowq = 0.29,   median = 0.38,   highq = 0.47) ,
                AntiNausea     = rdist("pert_alt",  lowq = 0.008,  mode = 0.04,   highq = 0.187) ,
                AntiCramp      = rdist("discrete",  value = 0,     continuous = FALSE),
                Antibiotics    = rdist("pert_alt",  lowq = 0.008,  mode = 0.04,   highq = 0.187)
  ),
  `5-64` = list(Antidiarrhoeal = rdist("pert_alt",  lowq = 0.107,  median = 0.145,  highq = 0.193),
                Painkillers    = rdist("pert_alt",  lowq = 0.13,   median = 0.178,  highq = 0.238),
                AntiNausea     = rdist("pert_alt",  lowq = 0.04,   median = 0.065,  highq = 0.104),
                AntiCramp      = rdist("pert_alt",  lowq = 0.012,  median = 0.028,  highq = 0.063),
                Antibiotics    = rdist("pert_alt",  lowq = 0.006,  median = 0.014,  highq = 0.035)
  ),
  `65+`  = list(Antidiarrhoeal = rdist("pert_alt",  lowq = 0.232,  median = 0.349,  highq = 0.489),
                Painkillers    = rdist("pert_alt",  lowq = 0.014,  mode = 0.053,  highq = 0.186),
                AntiNausea     = rdist("pert_alt",  lowq = 0.035,  median = 0.106,  highq = 0.279),
                AntiCramp      = rdist("pert_alt",  lowq = 0.0002, median = 0.001,  highq = 0.004),
                Antibiotics    = rdist("pert_alt",  lowq  = 0,     median = 0.051,  highq = 0.104, lowp = 0)
  )
)


GPConsultCommon <- rdist('pert_alt',  lowq = 0.241, median = 0.367, highq = 0.501)
EDCommon <- rdist('pert_alt',  lowq = 0.06, median = 0.124, highq = 0.228)


MedicationsShigella <- MedicationsBacterial
MedicationsShigella[["<5"]]$Antibiotics <- GPConsultCommon
MedicationsShigella[["5-64"]]$Antibiotics <- GPConsultCommon
MedicationsShigella[["65+"]]$Antibiotics <- GPConsultCommon

#Assumptions specific to each disease

PathogenAssumptions <- list(
  `All gastro pathogens` = disease(
    name = 'Gastroenteritis',
    pathogen = 'All gastro pathogens',
    kind = 'initial',
    caseMethod = 'GastroFraction',
    gastroFraction = rdist('discrete', value = 1, continuous = FALSE),
    foodborne = rdist("pert_alt", lowq = 0.13, median = 0.25, highq = 0.42),
    gp = rdist("pert_alt", lowq = 0.156, median = 0.196, highq = 0.234),
    gpFracLong = 0,
    ed = rdist("pert_alt", lowq = 0.025, median = 0.044, highq = 0.074),
    specialist = rdist('discrete', value = 0, continuous = FALSE), #i.e. none
    specialistToWhom = "None",
    hospMethod = "AIHW",
    hospCodes = c('A01.0',
                  paste0("A02.",0:9),
                  paste0("A03.",0:9),
                  'A04.0', 'A04.1',
                  paste0("A04.",3:6),
                  'A05.0',
                  paste0("A05.",2:4),
                  'A07.1', 'A07.2',
                  paste0("A08.",0:2),
                  'A08.5', 'A09.0', 'A09.9'),
    mortCodes = c('A01.0', 'A02', 'A03', 'A04.0', 'A04.1',
                  paste0('A04.',3:6),
                  'A04.8', 'A04.9', 'A05.0',
                  paste0('A05.',2:4),
                  'A05.8', 'A05.9', 'A07.1', 'A07.2', 'A07.8', 'A07.9',
                  paste0('A08.',0:4),
                  'A08.5', 'A09.0'),
    DRGCodes = GastroDRGCodes,
    hospPrincipalDiagnosis = rdist("discrete", value = 0.70, continuous = FALSE),
    underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
    sequelae = list(),
    medications = MedicationsGastro,
    medicationsToWhom = "Cases",
    tests = list(AllAges = list(Stool_culture = rdist('pert_alt', lowq =0.016, median=0.031, highq = 0.057, lowp = 0.05, highp = 0.95))),
    testsToWhom = 'Cases',
    duration = c(NonHosp = 3, Hosp = 5.3),
    severity = c(NonHosp = "mild", Hosp = "severe"),
    symptoms = "GI"
  ),
  Norovirus = disease(name = 'Norovirus',
                      kind = 'initial',
                      pathogen = 'Norovirus',
                      caseMethod = 'GastroFraction',
                      gastroFraction = rdist('pert_alt', lowq = 0.0772, median = 0.0982, highq = 0.1226),
                      foodborne = rdist("pert_alt", lowq = 0.0503, median = 0.18, highq = 0.35, lowp = 0.05, highp = 0.95),
                      gp = rdist("pert_alt", lowq = 0.156, median = 0.196, highq = 0.234),
                      gpFracLong = 0,
                      ed = rdist("pert_alt", lowq = 0.025, median = 0.044, highq = 0.074),
                      specialist = rdist('discrete', value = 0, continuous = FALSE), #i.e. none
                      specialistToWhom = "None",
                      hospMethod = "AIHW",
                      hospCodes = 'A08.1',
                      mortCodes = 'A08.1',
                      DRGCodes = GastroDRGCodes,
                      hospPrincipalDiagnosis = rdist("discrete", value = 0.52, continuous = FALSE),
                      underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
                      sequelae = list(),
                      medications = MedicationsGastro,
                      medicationsToWhom = "Cases",
                      tests = list(AllAges = list(Stool_culture = rdist('pert_alt', lowq = 0.016, median = 0.031, highq = 0.057, lowp = 0.05, highp = 0.95))),
                      testsToWhom = 'Cases',
                      duration = c(NonHosp = 2, Hosp = 5.7),
                      severity = c(NonHosp = "mild", Hosp = "severe"),
                      symptoms = "GI"
  ),
  `Escherichia coli (Non-STEC)` = disease(
    name = "Escherichia coli (Non-STEC)",
    pathogen = "Escherichia coli (Non-STEC)",
    kind = 'initial',
    caseMethod = 'GastroFraction',
    gastroFraction = rdist('pert_alt', lowq = 0.0525, median = 0.074, highq = 0.0914),
    foodborne = rdist("pert_alt", lowq = 0.08, median = 0.23, highq = 0.55),
    gp = GPConsultCommon,
    gpFracLong = 0,
    ed = EDCommon,
    specialist = rdist('discrete', value = 0, continuous = FALSE), #i.e. none
    specialistToWhom = "None",
    hospMethod = "AIHW",
    hospCodes = paste0('A04.',c(0:2,4)),
    mortCodes = paste0('A04.',c(0:2,4)),
    DRGCodes = GastroDRGCodes,
    hospPrincipalDiagnosis = rdist("discrete", value = 0.39, continuous = FALSE),
    underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
    sequelae = list(),
    medications = MedicationsBacterial,
    medicationsToWhom = "Cases",
    tests = list(AllAges = list(Stool_culture = rdist('pert_alt', lowq = 0.016, median = 0.031, highq = 0.057, lowp = 0.05, highp = 0.95))),
    testsToWhom = 'Cases',
    duration = c(NonHosp = 3, Hosp = 8.6),
    severity = c(NonHosp = "mild", Hosp = "severe"),
    symptoms = "GI"
  ),
  Campylobacter = disease(name = "Campylobacteriosis",
                          pathogen = 'Campylobacter',
                          kind = 'initial',
                          caseMethod = "Notifications",
                          domestic = rdist("pert", min = 0.91, mode = 0.97, max = 0.99),
                          underreporting = rdist("lnorm_alt", mean = 10.45, sd = 2.98),
                          foodborne = rdist("pert_alt", lowq = 0.62, mode = 0.77, highq = 0.89, lowp = 0.05, highp = 0.95),
                          gp = GPConsultCommon,
                          gpFracLong = 0,
                          ed = EDCommon,
                          specialist = rdist('discrete', value = 0, continuous = FALSE), #i.e. none
                          specialistToWhom = "None",
                          hospMethod = "AIHW",
                          hospCodes = c("A04.5"),
                          mortCodes = c("A04.5"),
                          DRGCodes = GastroDRGCodes,
                          hospPrincipalDiagnosis = rdist("discrete", value = 0.78, continuous = FALSE),
                          underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
                          sequelae = list(`Reactive arthritis`       = rdist("pert", min = 0.028,    mode = 0.07,     max = 0.16),
                                          `Irritable bowel syndrome` = IBSMultiplier,
                                          `Guillain-Barré syndrome`  = rdist("pert", min = 0.000192, mode = 0.000304, max = 0.000945)),
                          medications = MedicationsBacterial,
                          medicationsToWhom = "Cases",
                          tests = list(AllAges = list(Stool_culture = rdist("discrete", value = 1, continuous = FALSE))), # i.e. all notifications have tests
                          testsToWhom = "Notifications",
                          duration = c(NonHosp = 6, Hosp = 9.5),
                          severity = c(NonHosp = "severe", Hosp = "severe"),
                          symptoms = "GI"
  ),

  `Non-typhoidal Salmonella` = disease(
    name = "Salmonellosis",
    pathogen = 'Non-typhoidal Salmonella',
    kind = 'initial',
    caseMethod = "Notifications",
    domestic = rdist("pert", min = 0.7, mode = 0.85, max = 0.95),
    underreporting = rdist("lnorm_alt", mean = 7.44, sd = 2.38),
    foodborne = rdist("pert_alt", lowq = 0.53, highq = 0.86, median = 0.72, lowp = 0.05, highp = 0.95),
    gp = GPConsultCommon,
    gpFracLong = 0,
    ed = EDCommon,
    specialist = rdist('discrete', value = 0, continuous = FALSE), #i.e. none
    specialistToWhom = "None",
    sequelae = list(`Reactive arthritis` = rdist("pert", min = 0, mode = 0.085, max = 0.26),
                    `Irritable bowel syndrome` = IBSMultiplier),
    hospMethod = "AIHW",
    hospPrincipalDiagnosis = rdist("discrete", value = 0.74, continuous = FALSE),
    hospCodes = paste0("A02.",0:9),
    mortCodes = "A02",
    DRGCodes = GastroDRGCodes,
    underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
    medications = MedicationsBacterial,
    medicationsToWhom = "Cases", ##CHECK THIS
    tests = list(AllAges = list(Stool_culture = rdist("discrete", value = 1, continuous = FALSE))), # i.e. all notifications have tests
    testsToWhom = "Notifications",
    duration = c(NonHosp = 6, Hosp = 9.8),
    severity = c(NonHosp = "severe", Hosp = "severe"),
    symptoms = "GI"
  ),

  Shigella = disease(name = "Shigellosis",
                     pathogen = 'Shigella',
                     kind = 'initial',
                     caseMethod = "Notifications",
                     domestic = rdist("pert", min = 0.45, mode = 0.7, max = 0.84),
                     underreporting = rdist("lnorm_alt", mean = 7.44, sd = 2.38),
                     foodborne = rdist("pert_alt", lowq = 0.05, mode = 0.12, highq = 0.23, lowp = 0.05, highp = 0.95),
                     sequelae = list(`Reactive arthritis` = rdist("pert", min = 0.012, mode = 0.097, max = 0.098),
                                     `Irritable bowel syndrome` = IBSMultiplier),
                     hospMethod = "AIHW",
                     hospCodes = paste0("A03.",0:9),
                     mortCodes = "A03",
                     DRGCodes = GastroDRGCodes,
                     underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
                     hospPrincipalDiagnosis = rdist("discrete", value = 0.70, continuous = FALSE),
                     gp = GPConsultCommon,
                     gpFracLong = 0,
                     ed = EDCommon,
                     specialist = rdist('discrete', value = 0, continuous = FALSE), #i.e. none
                     specialistToWhom = "None",
                     medicationsToWhom = "Cases",
                     medications = MedicationsShigella,
                     testsToWhom = "Notifications",
                     tests = list(AllAges = list(Stool_culture = rdist("discrete", value = 1, continuous = FALSE))), # i.e. all notifications have tests
                     duration = c(NonHosp = 6, Hosp = 9.3),
                     severity = c(NonHosp = "severe", Hosp = "severe"),
                     symptoms = "GI"
  ),
  `Toxoplasma gondii` = disease(name = "Toxoplasmosis",
                                pathogen = 'Toxoplasma gondii',
                                kind = 'initial',
                                caseMethod = 'Seroprevalence',
                                FOI = rdist('pert', min = 0.012, mode = 0.02, max = 0.035),
                                domestic = rdist('pert', min = 0.7, mode = 0.85, max = 0.95),
                                symptomatic = rdist('pert', min = 0.11, mode = 0.15, max = 0.21),
                                foodborne = rdist('pert', min = 0.04, mode = 0.31, max = 0.74),
                                sequelae = list(),
                                hospMethod = "AIHW",
                                hospCodes = paste0("B58.",0:9),
                                mortCodes = "B58",
                                DRGCodes = list(`<5` = "T01A",
                                                `5-64` = "T01A",
                                                `65+` = "T01A"),
                                underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
                                hospPrincipalDiagnosis = rdist("discrete", value = 0.42, continuous = FALSE),
                                gp = GPConsultCommon,
                                gpFracLong = 0,
                                ed = rdist('discrete',  value = 0, continuous = FALSE), #i.e. none
                                specialist = rdist('pert', min = 1, mode = 2, max = 3),
                                specialistToWhom = "Hospitalisations",
                                medicationsToWhom = "GP",
                                medications = list(AllAges = list(TrimethoprimSulfamethoxazole = rdist('discrete', value = 1))), #This might be the wrong cost item since toxo calls for a four week course of antibiotics which is presumably more expensive?
                                testsToWhom = "GP",
                                tests = list(AllAges = list(FBC = rdist("discrete", value = 1, continuous = FALSE),
                                                            ESR = rdist("discrete", value = 1, continuous = FALSE))), # i.e. all gp visits have this tests
                                duration = c(NonHosp = 7, Hosp = 42.9),
                                severity = c(NonHosp = "mild", Hosp = "severe"),
                                symptoms = "flulike"
  ),

  `Listeria monocytogenes` = disease(name = "Listeriosis",
                                     pathogen = 'Listeria monocytogenes',
                                     kind = 'initial',
                                     sequelae = list(),
                                     caseMethod = "Notifications",
                                     domestic = rdist("discrete", value = 1, continuous = FALSE), # all cases are domestic
                                     underreporting = rdist("pert_alt", lowq = 1, mode = 2, highq = 3, lowp = 0.05, highp = 0.95),
                                     foodborne = rdist("pert", min = 0.9, mode = 0.98, max = 1),
                                     gp = rdist('pert', min = 1, mode = 2, max = 3),
                                     gpFracLong = 0.25,
                                     ed = rdist("discrete", value = 1, continuous = FALSE), # all cases go to ED and then are hospitalised
                                     specialist = rdist('pert', min = 1, mode = 2, max = 3),
                                     specialistToWhom = "Cases",
                                     underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
                                     #hospPrincipalDiagnosis = rdist("discrete", value = 0.48, continuous = FALSE),
                                     mortCodes = 'A32',
                                     hospCodes = paste0("A32.",0:9),## These are used to estimate how long people spend off work not for estimating admission
                                     hospMethod = 'AllCases',
                                     medications = list(AllAges = list(Antibiotics = rdist("discrete", value = 1, continuous = FALSE))),
                                     medicationsToWhom = "Cases",
                                     testsToWhom = 'Cases',
                                     tests = list(AllAges = list(FBC = rdist("discrete", value = 1, continuous = FALSE),
                                                                 ESR = rdist("discrete", value = 1, continuous = FALSE))),
                                     DRGCodes = list(`<5` = "T01A/T01B",
                                                     `5-64` = "T01A/T01B",
                                                     `65+` = "T01A/T01B"),
                                     symptoms = "flulike",
                                     severity = c(NonHosp = 'severe', Hosp = "severe"), #note that all cases are assumed hospitalised
                                     duration = c(NonHosp = 0, Hosp = 34.7) #note that all cases are assumed hospitalised
  ),


  `Salmonella Typhi` = disease(name = "Typhoid Fever",
                               pathogen = 'Salmonella Typhi', #Should this be Salmonella enterica ser. typhi?
                               kind = 'initial',
                               caseMethod = "Notifications",
                               sequelae = list(),
                               domestic = rdist("pert", min = 0.02, mode = 0.11, max = 0.25), # got this from old version of report but tey don't match newer versions of the report
                               underreporting = rdist("pert_alt", lowq = 1, median = 2, highq = 3), # got this from old version of report but tey don't match newer versions of the report
                               foodborne = rdist("pert", min = 0.02, mode = 0.75, max = 0.97), # got this from old version of report but tey don't match newer versions of the report
                               gp = rdist('pert', min = 1, mode = 2, max = 3),
                               gpFracLong = 0.25,
                               ed = rdist('discrete', value = 1, continuous = FALSE), #all cases are admitted to ED
                               specialist = rdist('discrete', value = 0, continuous = FALSE), #i.e. none
                               specialistToWhom = "None",
                               #hospPrincipalDiagnosis = rdist("discrete", value = 0.90, continuous = FALSE),
                               hospMethod = 'AllCases',
                               hospCodes = "A01.0",## This are used to estimate how long people spend off work not for estimating admission
                               mortCodes = 'A01.0',
                               DRGCodes  = list(`<5` = "T64B/T64C",
                                                `5-64` = "T64B/T64C",
                                                `65+` = "T64B/T64C"),
                               underdiagnosis = rdist('pert', min = 1, mode = 2, max = 3),
                               medications = list(AllAges = list()),
                               medicationsToWhom = 'None',
                               tests = list(AllAges = list()), # All tests are assumed to captured in hospital costs
                               testsToWhom = 'None',
                               symptoms = "GI",
                               severity = c(NonHosp = 'severe', Hosp = "severe"), #note that all cases are assumed hospitalised
                               duration = c(NonHosp = 0, Hosp = 26.4) #note that all cases are assumed hospitalised
  ),

  STEC = disease(name = "STEC",
                 pathogen = 'STEC',
                 kind = 'initial',
                 sequelae = list(`Haemolytic uremic syndrome` = rdist('pert_alt', lowq = 0.017, median = 0.03, highq = 0.051)),
                 caseMethod = "Notifications",
                 domestic = rdist('pert', min = 0.93, mode = 0.99, max = 1),
                 underreporting = rdist("lnorm_alt", mean = 8.83, sd = 3.7),
                 foodborne = rdist("pert_alt", lowq = 0.32, median = 0.56, highq = 0.82, lowp = 0.05, highp = 0.95),
                 gp = GPConsultCommon,
                 gpFracLong = 0,
                 ed = EDCommon,
                 specialist = rdist('discrete', value = 0, continuous = FALSE), #none
                 specialistToWhom = "None",
                 underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
                 hospPrincipalDiagnosis = rdist("discrete", value = 0.44, continuous = FALSE),
                 mortCodes = 'A04.3',
                 hospCodes = "A04.3",
                 hospMethod = 'AIHW',
                 medications = MedicationsGastro,
                 medicationsToWhom = "Cases",
                 testsToWhom = "Notifications",
                 tests = list(AllAges = list(Stool_culture = rdist("discrete", value = 1, continuous = FALSE))), # i.e. all notifications have tests
                 DRGCodes = GastroDRGCodes,
                 symptoms = "GI",
                 severity = c(NonHosp = 'severe', Hosp = "severe"),
                 duration = c(NonHosp = 6, Hosp =  6 + 3.5) #LOS figure of 3.5 taken from AIHW data cubes for 2015-2016, 2016-2017, and 2019-2020
  ),

  `Yersinia Enterocolitica` = list(name = "Yersiniosis",
                                   pathogen = "Yersinia Enterocolitica",
                                   kind = "initial",
                                   caseMethod = "Notifications",
                                   domestic = rdist("pert", min = 0.8, mode = 0.9, max = 1),
                                   underreporting = rdist("lnorm_alt", mean = 7.44, sd = 2.38),
                                   foodborne = rdist("pert", min = 0.28, mode = 0.84, max = 0.94),
                                   sequelae = list(`Reactive arthritis` = rdist("pert", min = 0, mode = 0.12, max = 0.231)),
                                   gp = GPConsultCommon,
                                   gpFracLong = 0,
                                   ed = EDCommon,
                                   specialist = rdist('discrete', value = 0, continuous = FALSE), #none
                                   specialistToWhom = "None",
                                   underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
                                   hospPrincipalDiagnosis = rdist("discrete", value = 0.35, continuous = FALSE),
                                   mortCodes = 'A04.6',
                                   hospCodes = "A04.6",
                                   hospMethod = 'AIHW',
                                   medications = MedicationsGastro,
                                   medicationsToWhom = "Cases",
                                   testsToWhom = "Notifications",
                                   tests = list(AllAges = list(Stool_culture = rdist("discrete", value = 1, continuous = FALSE))), # i.e. all notifications have tests
                                   DRGCodes = GastroDRGCodes,
                                   symptoms = "GI",
                                   severity = c(NonHosp = 'severe', Hosp = "severe"),
                                   duration = c(NonHosp = 6, Hosp = 9.4)
  )
)

SequelaeAssumptions <- list(
  `Reactive arthritis` = disease(
    name = "Reactive arthritis",
    kind = 'sequel',
    caseMethod = 'sequel',
    domestic =   rdist("pert_alt", lowq = 0.86, median = 0.91,highq = 0.95, lowp = 0.05, highp = 0.95),  #These are just for hospitalisations and deaths
    #bacterial =  rdist("pert_alt", lowp = 0,          highp = 1,          lowq = 0.5,  highq = 0.947, median = 0.66),  #ditto
    foodborne =  rdist("pert_alt", lowq = 0.36, median = 0.48,highq = 0.61, lowp = 0.05, highp = 0.95),  #ditto
    gp =         rdist("pert_alt", lowq = 0.66, median = 0.8, highq = 0.89),
    gpFracLong = 0.25,
    ed =         rdist("discrete", value = 0,   continuous = FALSE), #i.e. none
    specialist = rdist("pert_alt", lowq = 0.223,median = 0.24,highq = 0.258),
    specialistToWhom = "Cases",
    hospPrincipalDiagnosis = rdist("discrete", value = 0.45, continuous = FALSE),
    hospMethod = "AIHW",
    hospCodes = c("M02.1", "M02.3","M02.8","M03.2"),
    mortCodes = c("M02.1","M02.8"),
    DRGCodes = list(`<5` = "I66B",
                    `5-64` = "I66B",
                    `65+` = "I66B"),
    underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
    medicationsToWhom = "GP",
    medications = list(
      AllAges = list(
        Antibiotics                   = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16 ,       highq = 0.244,       median = 0.2),
        NSAID                         = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.528,       highq = 0.918,       median = 0.762),
        Eye_drops                     = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,        highq = 0.244,       median = 0.2),
        Prednisone                    = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.01,        highq = 0.099,       median = 0.039), # The multiplier in the first draft of the report was dodgy (implied min<0 and mode<5% quantile)
        Interarticular_Glucocorticoid = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,        highq = 0.244,       median = 0.2),
        DMARD_Methotrexate            = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.012 * 0.8, highq = 0.304 * 0.8, median = 0.095 * 0.8),
        DMARD_Infliximab              = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.012 * 0.2, highq = 0.304 * 0.2, median = 0.095 * 0.2),
        Joint_Aspiration              = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,        highq = 0.244,       median = 0.2)
      )
    ),
    testsToWhom = "GP",
    tests = list(
      AllAges = list(
        Stool_culture               = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.097, median=0.132, highq = 0.174),
        Serology	                  = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.097, median=0.132, highq = 0.174),
        Urine_test	                = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.097, median=0.132, highq = 0.174),
        CRP_Urate                   = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        FBC                         = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        ESR                         = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        #EUC	                        = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244), #included in two Renal function and two bloods. Check that this is correct
        #ANA	                        = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244), #included in two Renal function and two bloods. Check that this is correct
        Rheumatoid_factor	          = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        Renal_function_two_bloods	  = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        HLA_B27	                    = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.16,  median=0.2,   highq = 0.244),
        Lumbosacral_X_ray           = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.012, median=0.095, highq = 0.304),
        Lower_limb_ultrasound	      = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.017, median=0.034, highq = 0.062),
        MRI	                        = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.002, median=0.01,  highq = 0.03)
      )
    ),
    propOngoing = list(`<5` = rdist('pert_alt', mode = 0.2 * 0.41, lowq = 0.2 * 0.29, highq = 0.2 * 0.54),
                       `5-64` = rdist('pert_alt', mode = 0.2 * 0.41, lowq = 0.2 * 0.29, highq = 0.2 * 0.54),
                       `65+` = rdist('pert_alt', mode = 0.2 * 0.41, lowq = 0.2 * 0.29, highq = 0.2 * 0.54)),
    #durationOngoing = 10, #this is duration for ongoing illness in years
    durationOngoing = 5, # changing this to 5 to take a cross-sectional approach to the costing so we are costing the ongoing illness arising from the past five years of cases (with the assumption that the incidence over the past five years has been the same as in the current year)
    #severityOngoing = 'mild',
    propSevere = 0.2,
    symptoms = "ReA",
    missedWorkCarer = list(`<5` = 5.7,
                           `5-64` = 1.9,
                           `65+` = 1.9),
    missedWorkSelf = list(`<5` = 0,
                          `5-64` = 4.8,
                          `65+` = 1.1)
  ),
  `Irritable bowel syndrome` = disease(
    name = "Irritable bowel syndrome",
    kind = "sequel",
    caseMethod = 'sequel',
    domestic =   rdist("pert_alt", lowq = 0.88,  median = 0.91, highq = 0.94, lowp = 0.05, highp = 0.95),  #not sure what these multipliers are for exactly...
    foodborne =  rdist("pert_alt", lowq = 0.068, median = 0.13, highq = 0.33),
    gp      =    rdist("pert_alt", lowq = 4.27,  median = 4.5,  highq = 4.73),
    gpFracLong = 0.25,
    ed =         rdist("discrete", value = 0, continuous = FALSE), #i.e. none
    specialist = rdist("pert_alt", lowq = 0.286, median = 0.3,  highq = 0.315),
    specialistToWhom = 'Cases',
    hospPrincipalDiagnosis = rdist("discrete", value = 0.61, continuous = FALSE),
    hospMethod = "AIHW",
    hospCodes = c("K58.0", "K58.9"),
    mortCodes = c("K58"),
    DRGCodes = list(`<5` = "G67B",
                    `5-64` = "G67B",
                    `65+` = "G67B"),
    underdiagnosis = rdist("pert", min = 1, mode = 2, max = 3),
    medicationsToWhom = "Cases",
    medications = list(AllAges = list(IBSAnyMedication = rdist("pert_alt", lowp = 0.05, highp = 0.95, lowq = 0.385, highq = 0.416, median = 0.4))),
    testsToWhom = "Cases",
    tests = list(
      AllAges = list(Stool_culture        = rdist('pert',     min = 0.667, mode = 1,     max = 1),
                     FBC                  = rdist('pert',     min = 0.667, mode = 1,     max = 1),
                     ESR                  = rdist('pert',     min = 0.667, mode = 1,     max = 1),
                     Liver_function_test  = rdist('pert',     min = 0.667, mode = 1,     max = 1),
                     CRP                  = rdist('pert',     min = 0.667, mode = 1,     max = 1),
                     Coeliac_screening    = rdist('pert',     min = 0.667, mode = 1,     max = 1),
                     Abdominal_X_ray      = rdist('pert_alt', lowp = 0.05, highp = 0.95, lowq = 0.652, median = 0.667, highq = 0.681),
                     Abdominal_Ultrasound	= rdist('pert_alt', lowp = 0.05, highp = 0.95, lowq = 0.484, median = 0.5,   highq = 0.516)),
      `<5` =    list(Endoscopy_and_biopsy = rdist('discrete', value = 0,   continuous = FALSE)),
      `5-64` =  list(Endoscopy_and_biopsy = rdist('pert_alt', lowp = 0.05, highp = 0.95, lowq = 0.05,  median = 0.1,   highq = 0.15)),
      `65+` =   list(Endoscopy_and_biopsy = rdist('pert_alt', lowp = 0.05, highp = 0.95, lowq = 0.15,  median = 0.2,   highq = 0.25))
    ),
    propOngoing = list(`<5` = rdist('pert_alt', mode = 0.429, lowq = 0.218, highq = 0.66),
                       `5-64` = rdist('pert_alt', mode = 0.429, lowq = 0.218, highq = 0.66),
                       `65+` = rdist('pert_alt', mode = 0.429, lowq = 0.218, highq = 0.66)),
    #durationOngoing = 10, #this is duration for ongoing illness in years
    durationOngoing = 5, # changing this to 5 to take a cross-sectional approach to the costing so we are costing the ongoing illness arising from the past five years of cases (with the assumption that the incidence over the past five years has been the same as in the current year)
    #severityOngoing = 'mild',
    propSevere = 0.3,
    symptoms = "IBS",
    missedWorkCarer = list(`<5` = 2.9,
                           `5-64` = 1.0,
                           `65+` = 1.0),
    missedWorkSelf = list(`<5` = 0,
                          `5-64` = 2.4,
                          `65+` = 0.5)
  ),
  `Guillain-Barré syndrome` = disease(
    name = "Guillain-Barré syndrome",
    kind = "sequel",
    caseMethod = 'sequel',
    domestic  = rdist("pert",     min = 0.91,  mode = 0.97,   max = 0.99),
    foodborne = rdist("pert_alt", lowq = 0.1,  median = 0.25, highq = 0.43),
    gp        = rdist("pert_alt", lowq = 3.56, highq = 3.66,  median = 3.6),
    gpFracLong =  0.25,
    ed = rdist("discrete", value = 0, continuous = FALSE), #i.e. none. CHECK THIS!
    specialist = rdist("pert_alt", lowq = 2.5, median = 3.0, highq = 3.5),
    specialistToWhom = 'Cases',
    physio = rdist("pert_alt", lowq = 5.5, median = 6.0, highq = 6.5),
    #physioToWhom = 'Cases',
    hospMethod = "AllCases",
    mortCodes = "G61.0",
    DRGCodes =  list(`<5` = "B06A",
                     `5-64` = "B06A",
                     `65+` = "B06A"),
    underdiagnosis = rdist('pert', min = 1, mode = 2, max = 3),
    medicationsToWhom = "None", #none as they are assumed to be included in hospitalisation costs
    medications = list(AllAges = list()), #none as they are assumed to be included in hospitalisation costs
    testsToWhom = "None", #none as they are assumed to be included in hospitalisation costs
    tests = list(AllAges = list()), #none as they are assumed to be included in hospitalisation costs
    propOngoing = list(`<5`   = rdist('pert_alt', mode = 0.075, lowq = 0.065, highq = 0.085),
                       `5-64` = rdist('pert_alt', mode = 0.16,  lowq = 0.14,  highq = 0.18 ),
                       `65+`  = rdist('pert_alt', mode = 0.49,  lowq = 0.47,  highq = 0.50 )),
    #durationOngoing = 10,
    durationOngoing = 5, # changing this to 5 to take a cross-sectional approach to the costing so we are costing the ongoing illness arising from the past five years of cases (with the assumption that the incidence over the past five years has been the same as in the current year)
    #severityOngoing = 'mild',
    propSevere = 1,
    symptoms = "GBS",
    missedWorkCarer = list(`<5` = 51.4,
                           `5-64` = 17.1,
                           `65+` = 17.1),
    missedWorkSelf = list(`<5` = 0,
                          `5-64` = 43.1,
                          `65+` = 9.6)
  ),
  `Haemolytic uremic syndrome` = disease(
    name = "Haemolytic uremic syndrome",
    kind = "sequel",
    caseMethod = 'sequel',
    domestic  = rdist("pert",     min = 0.93,  mode = 0.99,   max = 1),
    foodborne = rdist("pert_alt", lowq = 0.17,  median = 0.33, highq = 0.53),
    gp        = rdist("pert_alt", lowq = 1, median = 3, highq = 5),
    gpFracLong =  0.25,
    ed = rdist("discrete", value = 0, continuous = FALSE),
    specialist = rdist("discrete", value = 0, continuous = FALSE),
    specialistToWhom = 'None',
    hospMethod = "AllCases",
    mortCodes = "D59.3",
    DRGCodes =  list(`<5` = "L02B",
                     `5-64` = "L02B",
                     `65+` = "L02B"),
    underdiagnosis = rdist('pert', min = 1, mode = 2, max = 3),
    medicationsToWhom = "None", #none as they are assumed to be included in hospitalisation costs
    medications = list(AllAges = list()), #none as they are assumed to be included in hospitalisation costs
    testsToWhom = "None", #none as they are assumed to be included in hospitalisation costs
    tests = list(AllAges = list()), #none as they are assumed to be included in hospitalisation costs
    propOngoing = list(`<5`   = rdist('pert_alt', lowq = 0.08, mode = 0.16, highq = 0.277),
                       `5-64` = rdist('pert_alt', lowq = 0.08, mode = 0.16, highq = 0.277),
                       `65+`  = rdist('pert_alt', lowq = 0.08, mode = 0.16, highq = 0.277)),
    durationOngoing = 5, # changing this to 5 to take a cross-sectional approach to the costing so we are costing the ongoing illness arising from the past five years of cases (with the assumption that the incidence over the past five years has been the same as in the current year)
    propSevere = 1,
    symptoms = "HUS",
    missedWorkCarer = list(`<5` = 13.1,
                           `5-64` = 4.4,
                           `65+` = 4.4),
    missedWorkSelf = list(`<5` = 0,
                          `5-64` = 11.0,
                          `65+` = 2.5)
  )
)
