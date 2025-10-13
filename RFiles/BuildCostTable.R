library(tidyverse)

# Set year
# As data sources release on different schedules, source year can be vary
EstQ <- "Dec-24" #quarter for overall cost estimates
YearDeaths <- 2024  #Year for determining population for population-adjusting estimates deaths (does not change data source for deaths)
YearCases <- 2024 # Year that notification data and population estimates are taken from for estimating cases
YearHosp <- 2023 # Financial year that hospitalization data is taken from --- FY2024 will probably not be available until Nov 2025 given historical reporting timelines

ndraws <- 10^6 #number of random draws for each estimate
#Set random seed for reproducibility
set.seed(20251013) #I suggest choosing the date of the last run on which inputs/code changed in ways that effected the outputs


source("./RFiles/Distributions.R")
source("./RFiles/ClassDefinitions.R")
source("./RFiles/Diseases.R")
source("./RFiles/loadData.R")
source('./RFiles/summaryFunctions.R')
source("./RFiles/estimationFunctions.R")



### Load all the data and assumptions
NNDSSIncidenceAgegroup <- getCasesNNDSSAgeGroup() %>% subset(Disease != "STEC") #STEC is in the dataset, but quality of state surveillance deemed better.
StateIncidenceAgeGroup <- getCasesStateAgeGroup()
NotificationsAgeGroup <- bind_rows(NNDSSIncidenceAgegroup,StateIncidenceAgeGroup)
AusPopAgegroup <- getAusPopAgeGroup() #Population by year for the 3 broad age-groups 
AusPopSingleYear <- getAusPopSingleYearAge() # Population by year for every age
Hospitalisations <- getHospitalisationsAgeGroup()
Costs <- getCosts()
VSL <- getValueStatisticalLife()
Deaths <- getABSDeaths()
MissedDaysGastro <- getMissedDaysGastro()
FrictionRates <- getFrictionRates()
Workforce <- getWorkforceAssumptions()

### CPI adjustment
#The only CPI adjustment is for WTP estimates for pain and suffering (values taken from CHERE report in 2017 dollars)
RefQ <- "Dec-17" #reference quarter for WTP values
CPI <- getCPI(RefQ)[EstQ,"Cumm.Inflation.Multiplier"] 

### Data completeness checks

checkMissingCodes <- function(field, datacodes, action = stop){
  UsedCodes <- map(c(PathogenAssumptions, SequelaeAssumptions), ~.x[[field]])
  UsedCodesUnlist <- UsedCodes %>% unlist %>% unique
  AllCodes <- datacodes %>% unique
  MissingCodes <- setdiff(UsedCodesUnlist, AllCodes)
  
  if(length(MissingCodes)){
    message <- paste0('Some of the ', field, ' required by the model are missing from the data :\n   ',
                      paste(MissingCodes, collapse = '\n   '))
    
    ## Flag T/F if all data for a disease is missing. True if the disease has
    ## any codes listed BUT none of these codes are in the data. I.e. does not
    ## flag a problem for GBS for which we have not listed or used the hosp
    ## codes (because we assume all cases are hospitalised)
    
    MissingDiseases <- map(UsedCodes,~{
      PresentCodes <- intersect(.x, AllCodes)
      !length(PresentCodes) & length(.x)
      
    }) %>% unlist()
    
    if(any(MissingDiseases)){
      message <- paste0('Some diseases are missing all ', field, ' :\n   ',
                        paste(names(MissingDiseases)[MissingDiseases], collapse = '\n   '),
                        '\n', message)
    }
    action(message)
  }
}

checkSurplusCodes <- function(field, datacodes, action = stop){
  UsedCodes <- map(c(PathogenAssumptions, SequelaeAssumptions), ~.x[[field]])
  AllCodes <- datacodes %>% unique
  SurplusCodes <- setdiff(AllCodes,UsedCodes)
  
  if(length(SurplusCodes)){
    action('Some of the ', field, ' provided in the data are not used in the model:\n   ',
           paste(SurplusCodes, collapse = '\n   '))
  }
}


checkMissingCodes('mortCodes', Deaths$Cause, action = stop)
checkSurplusCodes('mortCodes', Deaths$Cause, action = warning)


#This second check is perhaps not needed for the hospitalisation data, as this
#dataset only has rows if there are any hospitalisations recorded (there are no
#rows with zero separations). If the model finds no rows it already interprets
#as no separations, so perhaps we need a separate check to see if input codes
#are valid in the future
checkMissingCodes('hospCodes', Hospitalisations$DC4D, action = warning)

checkMissingCodes('hospCodes',
                  subset(Hospitalisations, Year == "2022-23")$DC4D,
                  action = warning)




# Draw from all distributions

WTPList <- getWTP(ndraws) %>%
  map_depth(2,~{.x * CPI}) #adjust costs from 2017 dollars to present

IncidenceList <- makeIncidenceList(year = YearCases, # This uses the most recent NNDSS notifications extracted above from the /Data folder
                                   pathogens = PathogenAssumptions,
                                   ndraws = ndraws,
                                   gastroRate = gastroRate)

SequelaeFractions <- calcSequelaeFractions(IncidenceList)
HospList <- makeHospList(year = YearHosp, # This uses the most recent AIHW separations extracted above from the /Data folder
                         IncidenceList,
                         pathogens = PathogenAssumptions,
                         ndraws = ndraws)
DeathList <- makeDeathList(year = YearDeaths,  # This uses ABS population data for the year of choice to population adjust ABS-death data (averaged over a decade) to the year of choice
                           pathogens = PathogenAssumptions,
                           ndraws = ndraws)
CostList <- makeCostList(yearNNDSS = YearCases, # this uses the most recent NNDSS notification data (for certain costs that are only for notifications and not for all cases) 
                         yearAIHW = YearHosp, #and the most recent AHIW hospitalisation data (to estimate the LOS to determine time off work)
                         PathogenAssumptions,
                         ndraws,
                         discount = 0) # no discounting and assuming a 5 year duration of ongoing illness is equivalent to the cross-sectional approach if we assume that case numbers were the same over the past five years.

### Include sequelae as part of 'All gastro'

appendSequelaeToAllGastro <- function(List, .f){
  List$`All gastro pathogens` <- c(List$`All gastro pathogens`,
                                   imap(SequelaeAssumptions, #re-nest list with sequelae above pathogens and dropping initial diseases
                                        function(.s,.sn){
                                          map(List,~.x[[.sn]])
                                        }) %>%
                                     map(~{.x[!unlist(map(.x,is.null))]}) %>% #drop pathogens with no sequelae
                                     map(~do.call(.f,unname(.x)))) #sum over pathogens by sequelae
  List
}
CostList <- appendSequelaeToAllGastro(CostList,add2)
HospList <- appendSequelaeToAllGastro(HospList,add)
DeathList <- appendSequelaeToAllGastro(DeathList,add)
IncidenceList <- appendSequelaeToAllGastro(IncidenceList,add)

### Create a new category called all pathogens (which includes all pathogens, not just those that cause gastro)
appendAllPathogens <- function(List, .f){
  List$`All pathogens`<- c(list(Initial = .f(List$`All gastro pathogens`$Gastroenteritis,
                                             List$`Salmonella Typhi`$`Typhoid Fever`,
                                             List$`Toxoplasma gondii`$Toxoplasmosis,
                                             List$`Listeria monocytogenes`$Listeriosis)),
                           List$`All gastro pathogens`[names(List$`All gastro pathogens`) != 'Gastroenteritis']
  )
  List
}

CostList <- appendAllPathogens(CostList,add2)
HospList <- appendAllPathogens(HospList,add)
DeathList <- appendAllPathogens(DeathList,add)
IncidenceList <- appendAllPathogens(IncidenceList,add)

warning('When summing across agegroups the draws of the multipliers used for each agegroup are considered independent. Making them dependent would require reworking the whole program, and is not necessarily a better assumption, but it is something to be aware of')

#Include `All ages` and `All diseases` sums, calculate median, and 90 CIs, then reformat as data.frame
summariseEpiList <- function(list){
  list %>%
    map_depth(2,~{.x$`All ages` <- reduce(.x,`+`);.x}) %>%
    map(~{.x$`Initial and sequel disease` <- do.call(add,unname(.x));.x}) %>%
    quantilesNestedList(3, c("Pathogen", "Disease","AgeGroup"))
}

# Include `All ages` and `All diseases` sums, calculate median, and 90 CIs, then
# reformat as data.frame. Outputs are returned in categorised format (Deaths,
# Human Captial, Direct, WTP) or detailed (Tests, Medications, Hospitalisation...)
summariseCostList <- function(list){
  #Add totals for `All ages` and `All diseases` (aka initial and sequel diseases)
  totals <- list %>%
    map_depth(2,~{.x$`All ages` <- do.call(add,unname(.x));.x}) %>%
    map(~{.x$`Initial and sequel disease` <- do.call(add2,unname(.x));.x})
  Detailed <- totals %>% quantilesNestedList(4, c("Pathogen", "Disease","AgeGroup","CostItem"))
  
  DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
  WTPCat <- c('WTP', 'WTPOngoing')
  LostProdCat <- c("HumanCapital","FrictionHigh", "FrictionLow")
  
  Categorised <- totals %>%
    map_depth(3,~{
      .x$Direct <- reduce(.x[DirectCat],`+`) #sum over direct costs
      .x$WTP <- reduce(.x[WTPCat],`+`) #sum of WTP and WTP-onging
      .x <- .x[c("Deaths",LostProdCat, paste0("Total",LostProdCat),"Direct","WTP")] #drop sub-categories
      names(.x) <- c("Premature mortality", LostProdCat, paste0("Total",LostProdCat), 'Direct','Pain and suffering')
      .x
    }) %>%
    quantilesNestedList(4, c("Pathogen", "Disease","AgeGroup","CostItem"))
  
  list(Categorised = Categorised, Detailed = Detailed)
}

##### Calculate and store selected summaries ##################################
TotalCostByPathogen <- CostList %>%
  map_depth(3,~.x$TotalHumanCapital) %>% #Extract out total cost (human capital method)
  map_depth(2,~reduce(.x,`+`)) %>% #Sum over age groups
  map_depth(1,~reduce(.x,`+`)) #Sum over diseases (initial and sequelae)

TotalIncidence <- IncidenceList %>%
  map(~.x[[1]]) %>% #only consider initial infections (drop sequelae counts)
  map_depth(1,~reduce(.x,`+`)) #sum over age groups

#Note this includes all sequelae for the purpose of counting costs, but only the initial cases for the purpose of counting cases.
CostPerCase <- map2(TotalCostByPathogen,TotalIncidence,~.x/.y) %>%
  quantilesNestedList(1,"Pathogen")
write.csv(CostPerCase, './Outputs/CostPerCase.csv')


IncidenceTable <- summariseEpiList(IncidenceList)
HospTable <- summariseEpiList(HospList)
DeathTable <- summariseEpiList(DeathList)
EpiTable <- bind_rows(Deaths = DeathTable,
                      Hospitalisations = HospTable,
                      Cases = IncidenceTable,
                      .id = 'Measure')
write.csv(EpiTable,'./Outputs/EpiTable.csv')

CostSummaries <- summariseCostList(CostList)
write.csv(CostSummaries$Detailed,'./Outputs/CostTable.csv')
write.csv(CostSummaries$Categorised,'./Outputs/CostTableCategories.csv')

gc()
# save workspace in two versions; one light version to be used by the shiny app
# and another larger version with everything (optional can be very large)

#save.image('AusFBDiseaseImage.RData') #saves full image --- can be very large


UnusedLargeObjects <- c('CostList','SequelaeFractions','TotalCostByPathogen',
                        'TotalIncidence')
#trim DeathList, HospList, and IncidenceList down to 1000 draws
trim <- 1000
DeathList <- DeathList %>% map_depth(3,~.x[1:trim])
HospList <- HospList %>% map_depth(3,~.x[1:trim])
IncidenceList <- IncidenceList %>% map_depth(3,~.x[1:trim])
WTPList <- WTPList %>% map_depth(2,~.x[1:trim])
Hospitalisations <- subset(Hospitalisations,
                           DC4D %in% (map(c(PathogenAssumptions,
                                            SequelaeAssumptions),
                                          ~.x$hospCodes) %>%
                                        unlist(use.names = F) %>% unique))

save(list = setdiff(ls(all.names = T), UnusedLargeObjects),
     file = 'Outputs/AusFBDiseaseImage-Light.RData')
