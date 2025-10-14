#Functions for extracting data

# function for reading CPI data. Reused in shiny app (while the rest of the
# functions in this script are not, hence the separate file)
source("./RFiles/loadCPIData.R")

#Population by year by the 3 age groups (<5, 5-64, 65+)
getAusPopAgeGroup <- function(){
  AusPopAgeGroup <- getAusPopSingleYearAge() %>%
    mutate(AgeGroup = ifelse(Age<5, "<5",ifelse(Age<65,"5-64", "65+"))) %>%
    group_by(Year, AgeGroup) %>%
    summarise(Persons = sum(Persons),
              .groups = "drop")
  AusPopAgeGroup
}

#Population by year and age
getAusPopSingleYearAge <- function(file = "./Data/AustralianPopulationByAge.xlsx"){
  AusPop <- getAusPopAgeSex(file) %>%
    group_by(Year,Age) %>%
    summarise(Persons = sum(Count),
              .groups = "drop")
  AusPop
}

#Population by year age and sex
getAusPopAgeSex <- function(file = "./Data/AustralianPopulationByAge.xlsx"){
  AusPopAgeSex <- readxl::read_xlsx(file,
                                    sheet = 'Data1',
                                    range = readxl::cell_cols(c("A:GU")))
  AusPopAgeSex <- AusPopAgeSex[10:nrow(AusPopAgeSex),] %>%
    rename(Year = `...1`) %>%
    pivot_longer(-Year,names_sep = ";",names_to = c(NA,'Sex','Age',NA),values_to = "Count") %>%
    mutate(Count = as.integer(Count),
           Age = trimws(Age) %>%
             recode(`100 and over` = "100") %>%
             as.integer(),
           Sex = trimws(Sex)) %>%
    mutate(Year = Year %>%
             as.integer %>%
             as.Date(origin = "1899-12-30") %>%
             lubridate::year())
  AusPopAgeSex
}



# getCasesAgeGroupOld <- function(){
#   read.csv("./Data/NationallyNotifiedDiseasesAgeGroup.csv") %>%
#     mutate(AgeGroup = recode(AgeGroup,
#                              `00-04` = '<5',
#                              `05-09` = '5-64',
#                              `10-14` = '5-64',
#                              `15-19` = '5-64',
#                              `20-24` = '5-64',
#                              `25-29` = '5-64',
#                              `30-34` = '5-64',
#                              `35-39` = '5-64',
#                              `40-44` = '5-64',
#                              `45-49` = '5-64',
#                              `50-54` = '5-64',
#                              `55-59` = '5-64',
#                              `60-64` = '5-64',
#                              `65-69` = '65+',
#                              `70-74` = '65+',
#                              `75-79` = '65+',
#                              `80-84` = '65+',
#                              `85+`   = '65+')) %>%
#     subset(AgeGroup != "Unknown") %>% ## Perhaps those of unknown age should go with '5-64' rather than being excluded?
#     group_by(Year, Disease, AgeGroup) %>%
#     summarise(Cases = sum(Cases))
# }

getCasesNNDSSAgeGroup <- function(){
  readxl::read_xlsx("./Data/NNDSS Disease by AgeGroup and Year.xlsx") %>%
    pivot_longer(`0 - 4`:`85+`, names_to = 'AgeGroup', values_to = "Cases") %>%
    mutate(AgeGroup = recode(AgeGroup,
                             `0 - 4` = '<5',
                             `5 - 9` = '5-64',
                             `10 - 14` = '5-64',
                             `15 - 19` = '5-64',
                             `20 - 24` = '5-64',
                             `25 - 29` = '5-64',
                             `30 - 34` = '5-64',
                             `35 - 39` = '5-64',
                             `40 - 44` = '5-64',
                             `45 - 49` = '5-64',
                             `50 - 54` = '5-64',
                             `55 - 59` = '5-64',
                             `60 - 64` = '5-64',
                             `65 - 69` = '65+',
                             `70 - 74` = '65+',
                             `75 - 79` = '65+',
                             `80 - 84` = '65+',
                             `85+`   = '65+')) %>%
    rename(Disease = `Disease Name`) %>%
    group_by(Year, Disease, AgeGroup) %>%
    summarise(Cases = sum(Cases),
              .groups = "drop")
}

getCasesStateAgeGroup <- function(){
  #Loads state specific data for Yersinia entercolictica and STEC and then population adjusts them to 2024 populations
  TargetYears <- 2024
  DataYears <- 2013:2015
  PopFiles <- list.files('./Data', pattern = 'PopulationAgeYear-*', full.names = T)
  names(PopFiles) <- PopFiles %>%
    sub(pattern = "./Data/PopulationAgeYear-", replacement = '') %>%
    sub(pattern = ".xlsx", replacement = '')
  StatePop <- bind_rows(map(PopFiles, getAusPopSingleYearAge),.id = "State") %>%
    subset(Year %in% c(DataYears,TargetYears)) %>%
    mutate(AgegroupMin = Age %/% 5 * 5,
           AgegroupMax = AgegroupMin + 4,
           AgeGroup = ifelse(Age>=85, "85+", paste(AgegroupMin,AgegroupMax,sep = "-"))) %>%
    select(-c(Age,AgegroupMin,AgegroupMax)) %>%
    group_by(State, Year, AgeGroup) %>%
    summarise(Persons = sum(Persons),
              .groups = "drop")
  
  AusPopTargetYears <- subset(StatePop, Year %in% TargetYears) %>%
    group_by(AgeGroup,Year) %>%
    summarise(Persons = sum(Persons),
              .groups = "drop")
  
  Yersinia <- read.csv("./Data/Yersinia-SelectStates2013-2015.csv",check.names = F) %>%
    pivot_longer(-c(Year, State), names_sep = 1, names_to = c("Sex", "AgeGroup")) %>%
    group_by(Year, State, AgeGroup) %>%
    summarise(Count = sum(value),
              .groups = "drop") %>%
    mutate(Disease = "Yersiniosis")
  
  STEC <- read.csv("./Data/STEC-SouthAustralia2013-2015.csv") %>%
    mutate(State = "SA",
           Disease = "STEC") %>%
    rename(AgeGroup = Agegroup)
  
  bind_rows(STEC, Yersinia) %>%
    merge(StatePop) %>%
    group_by(AgeGroup, Disease) %>%
    summarise(Rate = sum(Count)/sum(Persons), #Rate calculation used population in DataYears
              .groups = "drop") %>% 
    merge(AusPopTargetYears) %>%
    mutate(Cases = Rate * Persons, #Age-adjusted case numbers multiply rates by target years
           AgeGroup = recode(AgeGroup,
                             `0-4` = '<5',
                             `5-9` = '5-64',
                             `10-14` = '5-64',
                             `15-19` = '5-64',
                             `20-24` = '5-64',
                             `25-29` = '5-64',
                             `30-34` = '5-64',
                             `35-39` = '5-64',
                             `40-44` = '5-64',
                             `45-49` = '5-64',
                             `50-54` = '5-64',
                             `55-59` = '5-64',
                             `60-64` = '5-64',
                             `65-69` = '65+',
                             `70-74` = '65+',
                             `75-79` = '65+',
                             `80-84` = '65+',
                             `85+`   = '65+')) %>%
    group_by(Year,Disease,AgeGroup) %>%
    summarise(Cases = sum(Cases),
              .groups = "drop")
}


getHospitalisationsAgeGroup <- function(){
  HospFiles <- list.files("./Data", "^Principal_diagnosis_data_cube_")
  Years <- sub("\\.xlsx.*", "", sub(".*Principal_diagnosis_data_cube_", "", HospFiles))
  out <- map(HospFiles,function(x){
    readxl::read_xlsx(paste0("./Data/",x),
                      sheet = '5-character PDx Counts Data',
                      range = readxl::cell_limits(c(5, 1), c(NA, NA))) %>%
      mutate(DC3D = substr(`3 digit diagnosis`, start = 1, stop = 3),
             DC4D = substr(`4 digit diagnosis`, start = 1, stop = 5)) %>%
      mutate(DC3D = ifelse(grepl('[A-Z][0-9]{2}',DC4D),DC3D,NA),
             DC4D = ifelse(grepl('[A-Z][0-9]{2}\\.[0-9]',DC4D),DC4D,NA),
             AgeGroup = substr(`Age Group`,start = 1, stop = 2),
             AgeGroup = na_if(AgeGroup, 'n.'),  #This converts the age-groups listed as n.p. (not published?) as NAs. These rows in the data often have a lot of variables missing. May be useful to keep for calculatin LOS for all ages
             AgeGroup = as.integer(AgeGroup),
             AgeGroup = ifelse(AgeGroup<=2,'<5',
                               ifelse(AgeGroup <= 14, '5-64',
                                      ifelse(AgeGroup <= 19, '65+',NA)))) %>%
      group_by(DC3D, DC4D, AgeGroup) %>%
      summarise(Separations  = sum(Separations),
                PatientDays = sum(`Patient Days`),
                .groups = "drop")})
  names(out) <- Years
  bind_rows(out, .id = "Year") %>%
    mutate(FYNumeric = 2000 + as.integer(substr(Year, 6, 7))) %>%
    mutate(meanLOS = PatientDays/Separations)
}

getCosts <- function(){
  out <- readxl::read_xlsx("./Data/Costs.xlsx") %>%
    as.data.frame()
  rownames(out) <- out$Name
  out
}

getWTP <- function(ndraws){
  WTPinput <- readxl::read_xlsx("./Data/WTPvaluesUncertainty.xlsx") %>%
    as.data.frame()
  
  
  
  severity <- unique(WTPinput$severity)
  symptom <- unique(WTPinput$symptom)
  WTPdraws <- list()
  for(se in severity){
    tmp <- list()
    for(sy in symptom){
      se_sy <- subset(WTPinput, severity == se & symptom == sy)
      tmp[[sy]] <- rpert_alt(ndraws,
                             mode = se_sy$WTPvalue,
                             lowq = se_sy$CILower95,
                             highq = se_sy$CIUpper95)
    }
    WTPdraws[[se]]  <- tmp
  }
  WTPdraws
}

getValueStatisticalLife <- function(){
  read.csv('./Data/ValueStatisticalLife.csv')$Value
}

getABSDeaths <- function(){
  
  #Input data is in ugly format, so I need to label columns somewhat manually
  Years <- 2014:2023
  AgeGroups <- c('<5', '5-64', '65+', 'Total')
  column_names <- c('Cause',
                    paste(sort(rep(Years, length(AgeGroups))), AgeGroups))
  column_types <- c('text', 'skip', #This will skip the second column in the excel spreadsheet (which doesn't contain any information)
                    rep('numeric', length(Years) * length(AgeGroups)))
  
  out <- readxl::read_xlsx('./Data/Causes of Death data.xlsx',
                           sheet = 'Table 1', range = 'A8:AP46',
                           col_names = column_names,
                           col_types = column_types)
  
  
  out <- out %>%
    #Convert to long form data
    pivot_longer(-Cause, names_sep = ' ',
                 values_to = 'Deaths',
                 names_to = c("Year", "AgeGroup")) %>%
    #Extract out cause of death code
    mutate(CauseLong = Cause, 
           Cause = str_extract(CauseLong,'([^\\s]+)') %>% str_trim(), 
           Cause = ifelse(nchar(Cause) == 4, # include a . for codes of length 4
                          paste(substr(Cause,1,3), substr(Cause,4,4),sep = '.'),
                          Cause)
    )%>%
    #sum over years
    group_by(AgeGroup,Cause) %>% 
    summarise(Count = sum(Deaths),
              .groups = "drop")
  
  AusPop <- getAusPopAgeGroup() %>%
    subset(Year %in% Years) %>%
    group_by(AgeGroup) %>%
    summarise(PersonYears = sum(Persons),
              .groups = "drop")
  
  
  
  #Add in additional perinatal deaths for listeria --- this is a 'fudge' using data on perinatal deaths in the years 2010-2016 (9 deaths across 7 years adjusted up to account for the fact other data is across 10 years. No population adjustment)
  out[out$Cause == "A32" & out$AgeGroup == "<5",'Count'] <- out[out$Cause == "A32" & out$AgeGroup == "<5",'Count'] + 9/7 * 10
  
  # Calculate as rate per person per year
  out <- out %>% 
    merge(AusPop) %>%
    mutate(Rate = Count/PersonYears) %>% 
    subset(AgeGroup != 'Total')
  
  return(out)
}


getMissedDaysGastro <- function(){
  days <- readxl::read_xlsx("./Data/Missed work.xlsx",
                            sheet = "Missed days - machine readible") %>%
    as.data.frame()
  days[days$Type == 'Self',"<5"] <- 0
  days <-  days %>%
    pivot_longer(-c(Type, Days), names_to = 'AgeGroup', values_to = 'Count') %>%
    group_by(Days, Type, AgeGroup) %>%
    group_modify(~{data.frame(dummy= rep(1,.x$Count))}) %>%
    select(-dummy)
  
  fits <- days %>%
    subset(!(AgeGroup == "<5" & Type == "Self")) %>%
    group_by(AgeGroup, Type) %>%
    group_modify(~{
      invisible(capture.output(a <- fitdistrplus::fitdist(.x$Days,'nbinom'))) #invisible(capture.output(...)) is used to suppress the unnecessary printing to screen
      plot(a)
      as.data.frame(as.list(c(estimate = a$estimate["mu"], sd = a$sd["mu"])))})
  rbind(fits, data.frame(AgeGroup = '<5', Type = "Self", estimate.mu = 0, sd.mu = 0))
}

# getMissedDaysHospitalisation <- function(){
#   HospFiles <- list.files("./Data", "Principal_diagnosis_data_cube_")
#   Years <- sub("\\.xlsx.*", "", sub(".*Principal_diagnosis_data_cube_", "", HospFiles))
#   out <- map(HospFiles,function(x){
#     readxl::read_xlsx(paste0("./Data/",x),
#                       sheet = '5-character PDx Counts Data',
#                       range = readxl::cell_limits(c(5, 1), c(NA, NA))) %>%
#       mutate(DC3D = substr(`3 digit diagnosis`, start = 1, stop = 3),
#              DC4D = substr(`4 digit diagnosis`, start = 1, stop = 5)) %>%
#       mutate(DC3D = ifelse(grepl('[A-Z][0-9]{2}',DC4D),DC3D,NA),
#              DC4D = ifelse(grepl('[A-Z][0-9]{2}\\.[0-9]',DC4D),DC4D,NA),
#              AgeGroup = as.integer(substr(`Age Group`,start = 1, stop = 2)),
#              AgeGroup = ifelse(AgeGroup<=2,'<5',
#                                ifelse(AgeGroup <= 14, '5-64',
#                                       ifelse(AgeGroup <= 19, '65+',NA)))) %>%
#       group_by(DC3D, DC4D, AgeGroup)})
#   names(out) <- Years
#   out <- bind_rows(out, .id = "Year") %>%
#     mutate(FYNumeric = 2000 + as.integer(substr(Year, 6, 7)))
#
#   fits <- days %>%
#     subset(!(AgeGroup == "<5" & Type == "Self")) %>%
#     group_by(AgeGroup, Type) %>%
#     group_modify(~{
#       invisible(capture.output(a <- fitdistrplus::fitdist(.x$Days,'nbinom'))) #invisible(capture.output(...)) is used to suppress the unnecessary printing to screen
#       #plot(a)
#       as.data.frame(as.list(c(estimate = a$estimate["mu"], sd = a$sd["mu"])))})
#   rbind(fits, data.frame(AgeGroup = '<5', Type = "Self", estimate.mu = 0, sd.mu = 0))
# }


getWorkforceAssumptions <- function(){
  out <- read.csv("./Data/WorkforceAssumptions.csv")
  rownames(out) <- out$AgeGroup
  out
}

getFrictionRates <- function(){
  read.csv("./Data/Friction.csv")
}

