load('AusFBDiseaseImage.RData')
library(tidyverse)
library(mc2d) #for the standard PERT distribution parameterised by min, mode, max

CostItems.Ordered <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications',
                       'HumanCapital','WTP','Deaths','WTPOngoing','TotalHumanCapital')

Diseases.Ordered <- c("Initial disease", "Irritable bowel disease", "Reactive arthritis", "Guillain-Barré syndrome", 'Haemolytic uremic syndrome')

InitialDiseaseNames <- c(unlist(map(PathogenAssumptions, ~.x$name)), `All pathogens` = 'Initial')


medianCIformat <- function(df,unit = 1000,newline = TRUE,round = FALSE,digits.round = NULL,dropnullinterval = TRUE){
  df %>%
    mutate(across(c(X5.,X95.,median),~format(ifelse(.x/unit<100 & round, round(.x/unit,digits = digits.round),signif(.x/unit,3)), #format to three significant figures (potentially rounding to closest integer)
                                             big.mark = ",",
                                             scientific = FALSE,
                                             nsmall = 0,
                                             trim = T,
                                             drop0trailing = TRUE))) %>%
    mutate(across(c(X5.,X95.),           #remove intervals when they are the same as the point estimate
                  ~if_else(.x == median & dropnullinterval,
                           '',
                           .x))) %>%
    mutate(Cost = if_else(X5. == '',  #merge cost and and interval into a single line
                          median,
                          paste0(median, ifelse(newline,'\n',' '), '(',X5.,' - ',X95.,')')))
}

PathogensWithSequelae <- map(PathogenAssumptions,~ifelse(length(.x$sequelae),
                                                         .x$pathogen,
                                                         NA)) %>%
  unlist(use.names = F) %>% `[`(.,!is.na(.))

################################################################################
################################ Epi Tables ####################################
################################################################################

EpiMeasures.Ordered <- c("Cases",'Hospitalisations', 'Deaths')

read.csv('Outputs/EpiTable.csv') %>%
  select(-X) %>%
  mutate(Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease)) %>%
  subset(Pathogen %in% PathogensWithSequelae | Disease == 'Initial disease') %>%
  #subset(Disease %in% Diseases.Ordered) %>%
  #mutate(Disease = factor(Disease, levels = Diseases.Ordered)) %>%
  group_by(Pathogen, Disease) %>%
  group_walk(~{
    .x %>%
      subset(Measure %in% EpiMeasures.Ordered) %>%
      medianCIformat(unit = 1,round = TRUE, digits.round = 0,drop = FALSE) %>%
      mutate(Measure = factor(Measure, levels = EpiMeasures.Ordered)) %>%
      select(AgeGroup, Measure, Cost) %>%
      pivot_wider(names_from = AgeGroup, values_from = Cost) %>%
      arrange(Measure) %>%
      subset(`All ages` != '0') %>%
      write_excel_csv(paste('Report/EpiTable',
                            paste(.y,collapse = '.'),
                            'csv',sep = '.'))
  })

################################################################################
####################  Detailed Cost Tables By Pathogen #########################
################################################################################

read.csv("Outputs/CostTable.csv") %>%
  select(-X) %>%
  mutate(Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease)) %>%
  subset(Pathogen %in% PathogensWithSequelae | Disease == 'Initial disease') %>%
  #subset(Disease %in% Diseases.Ordered) %>%
  #mutate(Disease = factor(Disease, levels = Diseases.Ordered)) %>%
  group_by(Pathogen, Disease) %>%
  group_walk(~{
    .x %>%
      subset(CostItem %in% CostItems.Ordered) %>%
      medianCIformat %>%
      mutate(CostItem = factor(CostItem, levels = CostItems.Ordered)) %>%
      select(AgeGroup, CostItem, Cost) %>%
      pivot_wider(names_from = AgeGroup, values_from = Cost) %>%
      arrange(CostItem) %>%
      subset(`All ages` != '0') %>%
      write_excel_csv(paste('Report/DetailedCostTable',
                            paste(.y,collapse = '.'),
                            'csv',sep = '.'))
  })

PathogenSummaryTable <- read.csv("Outputs/CostTableCategories.csv") %>%
  subset(Disease == "Initial and sequel disease" &
           AgeGroup == 'All ages' &
           CostItem %in% c("Direct", "HumanCapital","Pain and suffering","Premature mortality","TotalHumanCapital")) %>%
  medianCIformat(unit = 10^6) %>%
  select(Pathogen, CostItem, Cost) %>%
  pivot_wider(names_from = CostItem, values_from = Cost) %>%
  select(Pathogen,Direct, `Non-fatal productivity losses` = HumanCapital,`Pain and suffering`,`Premature mortality`,TotalHumanCapital) %>%
  as.data.frame
rownames(PathogenSummaryTable) <- PathogenSummaryTable$Pathogen
PathogenSummaryTable[c("All pathogens","All gastro pathogens","Campylobacter","Listeria monocytogenes",
                       "Non-typhoidal Salmonella","Norovirus","Shigella","STEC",
                       "Escherichia coli (Non-STEC)","Salmonella Typhi",
                       "Toxoplasma gondii","Yersinia Enterocolitica"),] %>%
  write_excel_csv('Report/TotalCostByPathogenTable.csv')


DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
WTPCat <- c('WTP', 'WTPOngoing')

CategorisedCosts <- CostList %>%
  map_depth(3,~{
    .x$Direct <- reduce(.x[DirectCat],`+`) #sum over direct costs
    .x$WTP <- reduce(.x[WTPCat],`+`) #sum of WTP and WTP-onging
    .x[c("Deaths","HumanCapital", "TotalHumanCapital","Direct","WTP")] #drop sub-categories
  })

# Cost of sequelae by category

CostSequelae <- CategorisedCosts$`All gastro pathogens`[2:5] %>%
  map(~do.call(add,.x)) %>% #Sum over age groups
  quantilesNestedList(2,names_to = c("Disease", "CostItem")) %>%
  rename(X5. = `5%`, X95. = `95%`) %>%
  medianCIformat(unit = 10^6) %>%
  select(Disease, CostItem, Cost) %>%
  pivot_wider(names_from = CostItem, values_from = Cost) %>%
  as.data.frame %>%
  `rownames<-`(.$Disease) %>%
  `[`(c("GBS","HUS","IBS","ReactiveArthritis"),) %>%
  select(Disease,Direct, HumanCapital,WTP,Deaths,TotalHumanCapital)

write_excel_csv(CostSequelae, './Report/TotalCostSequelae.csv')


### Lost productivity sensitivity analysis
LostProductivityCosts <- CostList %>%
  map_depth(3,~{.x[c("HumanCapital","FrictionHigh","FrictionLow")]}) %>%
  map_depth(2,~do.call(add,.x)) %>% #Sum over age groups %>%
  map_depth(1,~do.call(add,unname(.x))) %>% #Sum over diseases (initial and sequelae)
  quantilesNestedList(2,names_to = c('Pathogen','CostItem')) %>%
  rename(X5. = `5%`, X95. = `95%`) %>%
  medianCIformat %>%
  select(Pathogen,CostItem,Cost) %>%
  pivot_wider(names_from = CostItem, values_from = Cost) %>%
  as.data.frame %>%
  `rownames<-`(.$Pathogen) %>%
  `[`(c("All pathogens","All gastro pathogens","Campylobacter","Listeria monocytogenes",
        "Non-typhoidal Salmonella","Norovirus","Shigella","STEC",
        "Escherichia coli (Non-STEC)","Salmonella Typhi",
        "Toxoplasma gondii","Yersinia Enterocolitica"),) %>%
  select(Pathogen,FrictionLow,FrictionHigh,HumanCapital)

write_excel_csv(LostProductivityCosts,'./Report/SensitivityLostProductivity.csv')


############ Table 1 in the report (summary of cases, cost per case) ############
SequelaeNames <- names(SequelaeAssumptions)
CaseTable <- read.csv("Outputs/EpiTable.csv") %>%
  subset(Measure == 'Cases' &
           !(Disease %in% c(SequelaeNames, 'Initial and sequel disease')) &
           AgeGroup == 'All ages') %>%
  medianCIformat(unit = 1) %>%
  select(Pathogen, Cases = Cost)

CostPerCaseTable <- read.csv("Outputs/CostPerCase.csv") %>%
  medianCIformat(unit = 1) %>%
  select(Pathogen, `Cost per Case (AUD)` = Cost)

TotalCostWithWithoutSequelae <- read.csv("Outputs/CostTableCategories.csv") %>%
  subset(!(Disease %in% SequelaeNames) &
           AgeGroup == 'All ages' &
           CostItem  == "TotalHumanCapital") %>%
  mutate(Disease = ifelse(Disease == 'Initial and sequel disease',
                          Disease,
                          'Initial disease')) %>%
  medianCIformat(unit = 1000) %>%
  select(Pathogen, Disease, Cost) %>%
  pivot_wider(names_from = Disease, values_from = Cost) %>%
  as.data.frame %>%
  mutate(`Initial and sequel disease` = ifelse(`Initial and sequel disease` == `Initial disease`,
                                               'No sequelae',
                                               `Initial and sequel disease`))

Table1 <- CaseTable %>%
  merge(CostPerCaseTable, by = 'Pathogen') %>%
  merge(TotalCostWithWithoutSequelae, by = 'Pathogen') %>%
  `rownames<-`(.$Pathogen) %>%
  `[`(c("All pathogens","All gastro pathogens","Campylobacter","Listeria monocytogenes",
        "Non-typhoidal Salmonella","Norovirus","Shigella","STEC",
        "Escherichia coli (Non-STEC)","Salmonella Typhi",
        "Toxoplasma gondii","Yersinia Enterocolitica"),)
write_excel_csv(Table1, './Report/Table1.csv')


## Mean LOS in hospital by disease

AgeGroups <- c("<5","5-64","65+")
names(AgeGroups) <- c("<5","5-64", "65+")
year <- 2024
TimeOffWork <- map(PathogenAssumptions, function(.p){
  map(AgeGroups, function(.a){
    SepData <- subset(Hospitalisations, DC4D %in% .p$hospCode & AgeGroup == .a & FYNumeric == year)
    if(sum(SepData$Separations) == 0){
      warning('No hospital separations available for ', .p$name,
              ' in age group ', .a,
              ' in year ', year,
              '. Trying mean LOS across all age groups in this year to estimate time off work for this age group')
      SepData <- subset(Hospitalisations, DC4D %in% .p$hospCode & FYNumeric == year)
      if(sum(SepData$Separations) == 0){
        warning('No hospital separations available for ', .p$name,
                ' in any age group',
                ' in year ', year,
                '. Trying mean LOS for ', .a ,' in other years for which data is available to estimate time off work for this agegroup and year')
        SepData <- subset(Hospitalisations, DC4D %in% .p$hospCode & AgeGroup == .a)
        if(sum(SepData$Separations) == 0){
          warning('No hospital separations available for ', .p$name,
                  ' in age group ', .a,
                  ' in year ', year,
                  ' or any other year for which data is available',
                  '. Trying mean LOS in all age groups in other years for which data is available to estimate time off work for this agegroup and year')
          SepData <- subset(Hospitalisations, DC4D %in% .p$hospCode)
          if(sum(SepData$Separations) == 0){
            stop('No hospital separations available for ', .p$name,
                 ' in any age group',
                 ' in year ', year,
                 ' or any other year for which data is available.')
          }
        }
      }
    }
    meanLOS <- sum(SepData$PatientDays)/sum(SepData$Separations)

    CarerDays <- subset(MissedDaysGastro, AgeGroup == .a & Type == "Carer")$estimate.mu +
      meanLOS *
      Workforce['15-64', 'PropInWorkforce'] *
      Workforce[.a, 'PropNeedsCarer'] * 5/7
    SelfDays <- subset(MissedDaysGastro, AgeGroup == .a & Type == "Self")$estimate.mu +
      meanLOS *
      Workforce[.a, 'PropInWorkforce'] * 5/7
    list(CarerDays =CarerDays, SelfDays = SelfDays)
  })
}) %>%
  rapply(enquote,how = "unlist") %>% #make a dataframe (very wide)
  lapply(eval) %>%
  as.data.frame(check.names = F) %>%
  pivot_longer(everything(),
               names_sep = "\\.",
               names_to = c('Pathogen', "AgeGroup",'Kind')) %>%
  mutate(value = as.character(round(value,digits = 1))) %>%
  pivot_wider(Pathogen,names_from = c('Kind','AgeGroup'),names_sort = T) %>%
  as.data.frame() %>%
  `rownames<-`(.$Pathogen) %>%
  `[`(c("All pathogens","All gastro pathogens","Campylobacter","Listeria monocytogenes",
        "Non-typhoidal Salmonella","Shigella","STEC",
        "Escherichia coli (Non-STEC)","Salmonella Typhi",
        "Yersinia Enterocolitica","Toxoplasma gondii","Norovirus"),)
write_excel_csv(TimeOffWork,'./Report/LostDaysPaidWork.csv')


### Australia UK WTP comparisons
WTPValues <- data.frame(Disease = c('Gastroenteritis', 'Campylobacteriosis', 'Salmonellosis',
                                    'GBS','ReactiveArthritis','IBS'),
                        UK = c(124,159,159,15617,3263,28125),
                        Aus = c(33,138,138,1371,717,530))

WTPComparions <- read.csv('./Outputs/EpiTable.csv') %>%
  subset(Pathogen %in% c("Non-typhoidal Salmonella", 'Campylobacter', 'All gastro pathogens') &
           AgeGroup == 'All ages' &
           Measure == 'Cases' &
           Disease != 'Initial and sequel disease' &
           (Disease == 'Gastroenteritis' | Pathogen != "All gastro pathogens")
  ) %>%
  select(-c(AgeGroup, Measure)) %>%
  merge(WTPValues, all = T) %>%
  mutate(across(UK:Aus, ~.x * median)) %>%
  select(Pathogen, Disease, UK, Aus)

bind_rows(subset(WTPComparions, !(Disease %in% SequelaeNames) ),
          group_by(WTPComparions,Pathogen) %>%
            summarise(across(UK:Aus, ~sum(.x)))
) %>%
  mutate(across(UK:Aus,~format(signif(.x/10^6,3),
                               big.mark = ",",
                               scientific = FALSE,
                               nsmall = 0,
                               trim = T,
                               drop0trailing = TRUE))) %>%
  write_excel_csv("./Report/WTPUKComparison.csv")



### Increasing Hospitalisation rate for 'Other pathogenic Escherichia coli' ####

#multiply up costs for hospitalisations for STEC under the assumption that the
#fraction Ecoli cases hospitalised is equal to the fraction of all gastro hospitalised
#not used to make a table, but a text-only sensitivty analysis in the Other pathogenic Ecoli section


HospMultiplier <- map(AgeGroups, ~{
  (HospList$`All gastro pathogens`$Gastroenteritis[[.x]] /
     IncidenceList$`All gastro pathogens`$Gastroenteritis[[.x]])/
    (HospList$`Escherichia coli (Non-STEC)`$`Escherichia coli (Non-STEC)`[[.x]]/
       IncidenceList$`Escherichia coli (Non-STEC)`$`Escherichia coli (Non-STEC)`[[.x]])
})

CostList.Ecoli.Sensitivity <- map(AgeGroups,~{
  out <- CostList$`Escherichia coli (Non-STEC)`$`Escherichia coli (Non-STEC)`[[.x]]
  out$Hospitalisation <- out$Hospitalisation * HospMultiplier[[.x]]
  out
})

CostList.Ecoli.Sensitivity <- do.call(add,unname(CostList.Ecoli.Sensitivity)) %>%
  `[`(c('GPSpecialist','ED','Medications','Tests','Hospitalisation','Deaths',
        'WTP','HumanCapital'))
CostList.Ecoli.Sensitivity$TotalHumanCaptial <- reduce(CostList.Ecoli.Sensitivity, `+`)

CostSummary.Ecoli.Sensitivity <- CostList.Ecoli.Sensitivity %>%
  quantilesNestedList(1,names_to = c('CostItem'))
CostSummary.Ecoli.Sensitivity


################################################################################
############################# Outbreak Tables ##################################
################################################################################

source('./Outbreak.R')
ndraws <- 10^5
set.seed(20220222)

writeOutbreakTable <- function(Outbreak,OutbreakEst,filename){
  CostSummary <- summariseCostList(list(Outbreak = OutbreakEst$costs))$Detailed %>%
    subset(AgeGroup == 'All ages' &
             !(Disease %in% c('Initial and sequel disease', names(SequelaeAssumptions)))) %>% #only interested in initial disease costs for the report
    as.data.frame

  CostSummary %>%
    select(CostItem, median, X5. = `5%`, X95. = `95%`) %>%
    medianCIformat(unit = 1,newline = FALSE) %>%
    select(CostItem, Cost) %>%
    `rownames<-`(.$CostItem) %>%
    `[`(c('GPSpecialist','ED','Hospitalisation','Tests','Medications','HumanCapital',
          'WTP','Deaths','TotalHumanCapital'),) %>%
    write_excel_csv(paste0('./Report/',filename,'OutbreakCosts.csv'))

  # Write list of GP, Specialist, ED and Hospital Visits
  OutbreakCases <- OutbreakEst$cases[[1]] %>% reduce(`+`) #sum across agegroups
  OutbreakHosp <- OutbreakEst$separations[[1]] %>% reduce(`+`) #sum across agegroups
  GPSpecialist <- estimateGPSpecialist(Outbreak,
                                       cases = OutbreakCases,
                                       separations = OutbreakHosp,
                                       ndraws = ndraws) %>%
    `$<-`('GP',.$gpShort + .$gpLong) %>% #sum over types of gp or specialist visits
    `$<-`('Specialist',.$specialistInitial + .$specialistRepeat) %>%
    `[`(c('GP','Specialist','physio'))
  ED <- estimateGeneric(Outbreak['ed'],ndraws = ndraws,
                        OutbreakEst$cases[[1]] %>%reduce(`+`)) # sum across agegroups

  c(GPSpecialist,ED, list(Hospitalisations = OutbreakHosp)) %>%
    quantilesNestedList(1, "CostItem") %>%
    rename(X5. = '5%',X95. = '95%') %>%
    medianCIformat(newline = FALSE, unit = 1) %>%
    select(CostItem, Cost) %>%
    write_excel_csv(paste0('./Report/',filename,'OutbreakMedicalPresentations.csv'))
}

##################################################
######Listeria outbreak in rockmelons (2018)######
##################################################

ListeriaOutbreak <- PathogenAssumptions$`Listeria monocytogenes`
ListeriaOutbreak$domestic <- rdist("discrete", value = 1,  continuous = FALSE)
ListeriaOutbreak.notifications <- list(`<5` = 0, `5-64` = 22-10, `65+` = 10) #We are assuming 10 cases were 65+ and the remainder 5-64
ListeriaOutbreak.deaths <- list(`<5` = 1, `5-64` = 7, `65+` = 0) #7 deaths (assumed to be 5-64) and one miscarriage (costed as an additional death)
ListeriaOutbreak.Est <- costOutbreak(ListeriaOutbreak, ndraws = ndraws,
                                     notifications = ListeriaOutbreak.notifications,
                                     deaths = list(Listeriosis = ListeriaOutbreak.deaths))
writeOutbreakTable(ListeriaOutbreak,ListeriaOutbreak.Est,'Listeria')

##################################################
#######Salmonella enteritidis in eggs (2019)######
##################################################

EnteritidisOutbreak <- PathogenAssumptions$`Non-typhoidal Salmonella`
EnteritidisOutbreak$domestic <- rdist("discrete", value = 1,  continuous = FALSE)
EnteritidisOutbreak.notifications <- list(`<5` = 0, `5-64` = 235, `65+` = 0)
EnteritidisOutbreak.deaths <- list(`<5` = 0, `5-64` = 1, `65+` = 0)
EnteritidisOutbreak.Est <- costOutbreak(EnteritidisOutbreak,ndraws = ndraws,
                                         notifications = EnteritidisOutbreak.notifications,
                                         deaths = list(Salmonellosis = EnteritidisOutbreak.deaths))
writeOutbreakTable(EnteritidisOutbreak,EnteritidisOutbreak.Est,'Enteritidis')

##################################################
#######Salmonella Weltevreden in eggs (2019)######
##################################################
WeltevredenOutbreak <- PathogenAssumptions$`Non-typhoidal Salmonella`
WeltevredenOutbreak$domestic <- rdist("discrete", value = 1,  continuous = FALSE)
WeltevredenOutbreak.notifications <- list(`<5` = 0, `5-64` = 83, `65+` = 0)
WeltevredenOutbreak.deaths <- list(`<5` = 0, `5-64` = 0, `65+` = 0)
WeltevredenOutbreak.Est <- costOutbreak(WeltevredenOutbreak, ndraws = ndraws,
                                        notifications = WeltevredenOutbreak.notifications,
                                        deaths = list(Salmonellosis = WeltevredenOutbreak.deaths))
writeOutbreakTable(WeltevredenOutbreak,WeltevredenOutbreak.Est,'Weltevreden')


##################################################
#######Salmonella outbreak in bakery (2016)#######
##################################################

TyphimuriumOutbreak <- PathogenAssumptions$`Non-typhoidal Salmonella`
TyphimuriumOutbreak$ed <- rdist('discrete', value = 58/203, continuous = FALSE) #get exactly 58 ed visits
TyphimuriumOutbreak$gp <- rdist('discrete', value = 74/203, continuous = FALSE) #get exactly 74 gp visits
TyphimuriumOutbreak$domestic <- rdist("discrete", value = 1, continuous = FALSE)
TyphimuriumOutbreak$foodborne <- rdist("discrete", value = 1, continuous = FALSE)
TyphimuriumOutbreak$underreporting <- rdist("discrete", value = 203/91, continuous = FALSE) # get exactly 203 cases (but only 91 tests)
TyphimuriumOutbreak.notifications <- list(`<5` = 0, `5-64` = 91, `65+` = 0)
TyphimuriumOutbreak.separations <- list(`<5` = 0, `5-64` = 32, `65+` = 0)
TyphimuriumOutbreak.deaths <- list(`<5` = 0, `5-64` = 0, `65+` = 0)

TyphimuriumOutbreak.Est <- costOutbreak(TyphimuriumOutbreak, ndraws = ndraws,
                                        notifications = TyphimuriumOutbreak.notifications,
                                        separations = list(Salmonellosis = TyphimuriumOutbreak.separations),
                                        deaths = list(Salmonellosis = TyphimuriumOutbreak.deaths))
writeOutbreakTable(TyphimuriumOutbreak,TyphimuriumOutbreak.Est,'Typhimurium')



