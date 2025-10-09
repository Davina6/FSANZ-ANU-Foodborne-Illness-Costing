##### Calculate attributed cases and costs for food-borne pathogens.


# This assumes you have run BuildCostTable.R previously on this machine to
# generate the objects CostList.rds, DeathList.rds, HospList.rds, and
# IncidenceList.rds

library(tidyverse)
library(pluralize)
library(stringr)
library(glue)
library(english)

source('RFiles/summaryFunctions.R')
source('RFiles/Distributions.R')
source('RFiles/ClassDefinitions.R')
source('RFiles/Diseases.R')

InflationAdjustment <- 1.151
warning('Estimates of burden and cost were done to the best of our ability, for
        2019 data. Though none of the underlying inputs have been modified, we
        have updated all the costs for inflation (+15.1%) using the all-group
        CPI published by the ABS.
        https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release
        The baseline is the last quarter 2019 and the "present" is June 2023.')

set.seed(20220628)

## Load cost and epi estimates
CostList <- readRDS('CostList.rds')
DeathList <- readRDS('DeathList.rds')
HospList <- readRDS('HospList.rds')
IncidenceList <- readRDS('IncidenceList.rds')

## Adjust all costs for inflation

#Adjust costs for inflation (see warning above)
CostList <- CostList %>% map_depth(4, ~.x * InflationAdjustment)

## Reduce size of files by dropping most draws from the distributions. Since some costs only have a single 0 element simply selecting 1:trim produces many NAs
# trim <- 10^5
# CostList <- CostList %>% map_depth(4,~.x[1:min(trim,length(.x))])
# DeathList <- DeathList %>% map_depth(3,~.x[1:min(trim,length(.x))])
# HospList <- HospList %>% map_depth(3,~.x[1:min(trim,length(.x))])
# IncidenceList <- IncidenceList %>% map_depth(3,~.x[1:min(trim,length(.x))])

## Reduce CostList further by dropping unused cost measures --- The rest of the code should work with all kinds of costs split up, but these aren't been used for now

UsedCosts <- c('TotalHumanCapital')
CostList <- CostList %>% map_depth(3, ~.x[UsedCosts])


## Load attribution proportions
ndraws <- length(CostList[[1]][[1]][[1]][[1]])

#Read in data for 2023 attribution aggregations (quite different format to pilot)
#Drawing attribution proportions will happen in the next function
readAttrProps <- function(path){
  #Inputs were supplied for each pathogen/source pair giving 5%, 50%, and 95%
  #quantiles for the aggregated attrubution percentages (divide by 100 to get
  #proportions) percentages. Sources are represented by a number so we also need
  #to read in a key

  #ASSUMES THAT THE FIRST COLUMN HAS NAME 'Id' (WITH THE
  #PATHOGEN/COMMODITY PAIR ID) AND THE THE NEXT THREE COLUMS ARE FROM THE 5, 50,
  #95 QUANTILES, ASSUMING NO NAMING CONVENTION HERE

  # Read in key for translate source numbers in WEperc.csv to source names
  SourceKey <- read.csv('GlobalWeightedAggregations/SourceKey.csv') %>%
    arrange(SourceNumber)

  #Perhaps we should also read in the key for pathogen names? Currently hard-coded

  AttrProps <- read.csv(path) %>% #read in data
    separate(Id, c('Pathogen','Source'),4) %>%          #Split ID column in to Pathogen and Source columns
    mutate(across(c(Pathogen, Source),trimws)) %>%      #drop whitespace from names
    mutate(Pathogen = case_match(Pathogen,
                                 'Cam' ~ 'Campylobacter',
                                 'NtS' ~ 'Non-typhoidal Salmonella',
                                 'Lim' ~ 'Listeria monocytogenes',
                                 'Tog' ~ 'Toxoplasma gondii',
                                 'STC' ~ 'STEC',
                                 'Yer' ~ 'Yersinia Enterocolitica',
                                 .default = Pathogen),
           Source = SourceKey$SourceName[as.integer(Source)]) %>%
    complete(Pathogen, Source)                   # Fill out all combinations of Pathogen and Source (blanks get NA for percentiles
  names(AttrProps) <- c('Pathogen', 'Source', 'X5.', 'median','X95.')
  return(AttrProps)
}

drawAttr <- function(AttrProps, ndraws){
  AttrProps %>%
    rowwise %>%             #prepare to draw random numbers for each row (Pathogen/Source Pair)
    mutate(Draws = list(rstep(ndraws,
                              c(X5., median,X95.)/100, #draw attribution proportions
                              upper = as.numeric(!is.na(median))))) %>% #ensure that if quantiles are NA attribution is 0
    group_by(Pathogen) %>%  #reformat as a list with an entry for each pathogen with a matrix of attribution draws (every column of the matrices are sources)
    group_map_named(~{
      as.data.frame(.x$Draws,col.names = .x$Source,
                    check.names = FALSE) %>%
        mutate(AllFood = 1) %>% #include 'AllFood' Source. Note that adding across the food sources will not add to 1 as they aren't constrained as a joint distribution to do so (and their means don't even add to 1)
        as.matrix
    })
}

AttrProps <- readAttrProps(path = 'GlobalWeightedAggregations/GWperc.csv')
AttrDraws <- drawAttr(AttrProps, ndraws)


warning('Code currently does not return an error if the food categories are different for each pathogen. This might be a good thing if future surveys use slightly different categories, however it could lead to fairly confusing tables and inconsistent sums over categories.')

## Check for missing-ness of cost and/or attribution data and ensure that pathogens have the same order in both
CommonPathogens <- intersect(names(AttrDraws), names(CostList))
MissingPathogens <- setdiff(names(CostList), names(AttrDraws))
if(length(MissingPathogens)){
  warning('Attribution data is not available for some pathogens for which cost data is available:\n', paste0(MissingPathogens,col = ',\n'))
}
ExtraPathogens <- setdiff(names(AttrDraws),names(CostList))
if(length(ExtraPathogens)){
  warning('Cost data is not available for some pathogens for which attribution data is available:\n', paste0(ExtraPathogens,col = ',\n'))
}

CostList <- CostList[CommonPathogens]
AttrDraws <- AttrDraws[CommonPathogens]
IncidenceList <- IncidenceList[CommonPathogens]
HospList <- HospList[CommonPathogens]
DeathList <- DeathList[CommonPathogens]
EpiList <- list(Cases = IncidenceList, Hospitalisations = HospList, Deaths = DeathList)
rm(IncidenceList,HospList,DeathList)
gc()

##Attributed costs, cases, etc.
CostList <- map2(CostList, AttrDraws,
                 function(.c, .a){
                   map_depth(.c, 3, ~{as.list(as.data.frame(.x * .a))})
                 }
)
EpiList <- map(EpiList,~{
  map2(.x,AttrDraws,
       function(.e,.a){
         map_depth(.e,2,
                   function(.n){as.list(as.data.frame(.n * .a))}
         )}
  )}
)


####Tidy up attributed costs and epi figures ready for plotting and turning into figures

#rectangularise the nested lists
CostList <- CostList %>% rectangle(names_to = c("Pathogen", "Disease",'Agegroup', "CostItem", "Source"), values_to = 'Cost')
EpiList <- EpiList %>% rectangle(names_to = c("Measure","Pathogen", "Disease",'Agegroup', "Source"), values_to = 'Count')
gc()


#collapse the detailed cost categories to the categories
DirectCat <- c('GPSpecialist','ED','Hospitalisation','Tests','Medications')
WTPCat <- c('WTP', 'WTPOngoing')
CostList <- CostList %>%
  mutate(CostCategory  = case_when(CostItem %in% DirectCat ~ 'Direct',
                                   CostItem %in% WTPCat ~ 'WTP',
                                   TRUE ~ CostItem)) %>%
  group_by(across(-c(CostItem, Cost))) %>%
  summarise(Cost = list(reduce(Cost, `+`)))
gc()


#change specific initial illness names all to 'initial'
CostList <- CostList %>% mutate(Disease = ifelse(Disease %in% names(SequelaeAssumptions), Disease, 'Initial'))
EpiList <- EpiList %>% mutate(Disease = ifelse(Disease %in% names(SequelaeAssumptions), Disease, 'Initial'))
gc()


#Make totals across agegroups, pathogens and diseases and then calculate quantiles
group_cols <- list(Pathogen = 'All pathogens', Agegroup = 'All ages', `Disease` = 'Initial and sequel disease')
CostList <- CostList %>% appendGroupTotals('Cost', group_cols) %>% quantileListColumn('Cost') %>% rename(median = `50%`)
EpiList <- EpiList %>% appendGroupTotals('Count',group_cols) %>% quantileListColumn('Count') %>% rename(median = `50%`)
gc()


## Make the tables for the report

#Epi tables
EpiList %>%
  subset(Pathogen == 'All pathogens') %>%
  ungroup %>%
  group_by(Measure, Disease) %>%
  group_walk(~{.x %>%
      rename(X5. = '5%', X95. = '95%') %>%
      medianCIformat(unit = 1) %>%
      select(Agegroup, Source, Count = Cost) %>%
      pivot_wider(names_from = Agegroup, values_from = Count) %>%
      mutate(Source = factor(Source) %>% fct_relevel('Other','AllFood', after = Inf)) %>%
      arrange(Source) %>%
      write_excel_csv(paste('AttributionReport/EpiTable',
                            paste(.y,collapse = '.'),
                            'csv',sep = '.'))
  })

#Cost tables
CostList %>%
  subset(CostCategory == "TotalHumanCapital" & Disease == "Initial and sequel disease") %>%
  group_by(Pathogen) %>%
  group_walk(~{.x %>%
      rename(X5. = '5%', X95. = '95%') %>%
      medianCIformat() %>%
      mutate(Cost = if_else(Cost == "0", "0*", Cost)) %>%
      select(Agegroup, Source, Cost) %>%
      pivot_wider(names_from = Agegroup, values_from = Cost) %>%
      mutate(Source = factor(Source) %>% fct_relevel('Other','AllFood', after = Inf)) %>%
      arrange(Source) %>%
      write_excel_csv(paste('AttributionReport/CostTable',
                            paste(.y,collapse = '.'),
                            'csv',sep = '.'))
  })

#Attribution tables

AttrProps %>%
  bind_rows(., AttrProps %>%
              group_by(Pathogen) %>%
              summarise(median = sum(median,na.rm = T)) %>%
              mutate(Source = 'Total',
                     X5. = median,
                     X95. = median)) %>%
  mutate(across(c(X5., median, X95.), ~replace_na(.x,0))) %>%
  subset(Pathogen %in% CommonPathogens) %>%
  medianCIformat(unit = 1,round = T) %>%
  mutate(Cost = if_else(Cost == "0", "0*", Cost)) %>%
  select(c(Pathogen, Source, Attribution = Cost)) %>%
  pivot_wider(names_from = Pathogen, values_from = Attribution) %>%
  mutate(Source = factor(Source) %>% fct_relevel('Other','Total', after = Inf)) %>%
  arrange(Source) %>%
  write_excel_csv('Report/AttrProps.csv')


#Order Food by cost
FoodCatOrdered <- CostList %>% subset(Pathogen == 'All pathogens' &
                                        CostCategory == 'TotalHumanCapital' &
                                        Agegroup == 'All ages' &
                                        Disease == "Initial and sequel disease") %>%
  arrange(median) %>% `[`(,'Source', drop = TRUE)

#Combine the summary data for cost and epi
CombinedSummaries <- EpiList %>% ungroup %>%
  bind_rows(CostList %>% ungroup %>%
              subset(CostCategory == 'TotalHumanCapital') %>%
              select(-CostCategory) %>%
              mutate(Measure = 'Cost')
  ) %>%
  subset(Disease == "Initial and sequel disease" &
           Agegroup == 'All ages') %>%
  mutate(SourceCat = if_else(Source == 'AllFood', 'All Food', "Individual Items"),
         #Source = factor(Source,FoodCatOrderedCases),
         Source = factor(Source,FoodCatOrdered),
         Source = recode(Source, AllFood = 'All Food'),
         Source = fct_relevel(Source, 'Other', after = 0)
  )

P.CostProp <- CombinedSummaries %>%
  subset(Measure == "Cost" &
           Pathogen != 'All pathogens') %>%
  ggplot(aes(x = Source,
             y = median * 10^-6,
             fill = Pathogen,
             label = round(median * 10^-6))) +
  geom_bar(stat = 'identity') +
  xlab('Food product') +
  ylab('Annual cost (AUD millions)') +
  coord_flip() +
  ggh4x::facet_grid2("SourceCat", scales = 'free',space = 'free_y', independent = 'x',switch = 'both') +
  theme(legend.position = c(0.70, 0.25),
        strip.text.y = element_blank(),
        text = element_text(size = 16))
P.CostProp
ggsave(filename = 'AttributionReport/CostBySourcePathogen.png',P.CostProp, width = 1941, height = 1787, units = 'px')

### Figure for proportion of cases and deaths by food and pathogen

P.EpiProp <- CombinedSummaries %>%
  subset(Pathogen != 'All pathogens') %>%
  mutate(median = if_else(Measure == 'Cases', median/1000,
                              if_else(Measure == "Cost", median/1000000, median))) %>%
  mutate(Measure = recode(Measure,
                          Cases = 'Cases (thousands)',
                          Cost = 'Cost (AUD millions)')) %>%
  mutate(Measure = factor(Measure, levels = c('Cases (thousands)', 'Hospitalisations', "Deaths",
                                              'Cost (AUD millions)'))) %>%
  ggplot(aes(x = Source,
             y = median,
             fill = Pathogen,
             label = round(median/10^6))) +
  geom_bar(stat = 'identity') +
  xlab('Food product') +
  ylab('') +
  coord_flip() +
  ggh4x::facet_grid2(SourceCat~Measure, scales = 'free',space = 'free_y', independent = 'x',switch = 'both') +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        #legend.position = c(0.9,0.125),
        strip.placement = 'outside',
        strip.text.y = element_blank(),
        strip.background.x = element_rect('white'),
        text = element_text(size = 20))
P.EpiProp
ggsave(filename = 'AttributionReport/EpiBySourcePathogen.png',P.EpiProp, width = 3600, height = 1787, units = 'px')

# write a table with the same information but including an 'all pathogens' outcome

measure.units <- c(Cases = 1e3, Cost = 1e6, Hospitalisations = 1, Deaths = 1)
measure.round <- c(Cases = FALSE, Cost = FALSE, Hospitalisations = TRUE, Deaths = TRUE)
digits.round <-  c(Cases = NA,   Cost = NA,   Hospitalisations = 0, Deaths = 0)

CombinedSummaries %>%
  rename(X5. = `5%`, X95. = `95%`) %>%
  mutate(unit = measure.units[Measure],
         digits.round = digits.round[Measure],
         measure.round = measure.round[Measure]) %>%# View
  mutate(Measure = factor(Measure, levels = c('Cases', 'Hospitalisations', "Deaths",
                                              'Cost'))) %>% #View
  group_by(Measure) %>%
  medianCIformat(unit = unit, round = measure.round,
                 digits.round = digits.round) %>%
  select(Measure, Pathogen, Source, Cost) %>%
  pivot_wider(names_from = Measure, values_from = Cost) %>%
  #filter(!(Cases == '0' & Cost == '0')) %>%
  mutate(across(Cases:Cost, \(.x){ifelse(.x == '0' & Cases == '0', '0*', .x)})) %>%
  mutate(Deaths = ifelse(Cases != '0' & Deaths == '0', '<1', Deaths) ,
         Source = Source %>% factor(unique(Source)) %>% fct_relevel(c('Other','All Food'), after = Inf)) %>%
  arrange(Pathogen, Source) %>%
  rename(`Cases (thousands)` = Cases,
         `Cost (AUD millions)` = Cost) %>%
  write.csv('AttributionReport/EpiBySourcePathogen.csv', row.names = FALSE)


P.EpiProp$data <- P.EpiProp$data %>% subset(Measure != 'Cost (AUD millions)')
ggsave(filename = 'AttributionReport/EpiBySourcePathogenNoCost.png',P.EpiProp, width = 3600, height = 1787, units = 'px')


P.CostPropAlt <- CostList %>%
  subset(Disease == "Initial and sequel disease" &
           Agegroup == 'All ages' &
           CostCategory == "TotalHumanCapital" &
           Source != 'AllFood') %>%
  mutate(PathogenCat = if_else(Pathogen == 'All pathogens', 'All Pathogens', "Individual Pathogens")) %>%
  ggplot(aes(x = Pathogen,
             y = median/10^6,
             fill = Source,
             label = round(median/10^6))) +
  geom_bar(stat = 'identity') +
  xlab('Pathogen') +
  ylab('Annual cost (AUD millions)') +
  coord_flip() +
  ggh4x::facet_grid2(PathogenCat~., scales = 'free',space = 'free_y', independent = 'x',switch = 'both') +
  theme(strip.placement = 'outside',
        strip.text.y = element_blank())

P.CostPropAlt
ggsave(filename = 'AttributionReport/CostByPathogenSource.png',P.CostPropAlt, width = 1941, height = 1787, units = 'px')


## Calculate cost per case by food product

CostPerCase <- CombinedSummaries %>% select(-c(`5%`,`95%`,Agegroup,SourceCat,Disease)) %>%
  pivot_wider(names_from = Measure, values_from = median) %>%
  mutate(`Cost per case (AUD)` = signif(Cost/Cases,3),
         `Hospitalisations per million cases` = signif(Hospitalisations/Cases * 1000000,3),
         `Deaths per million cases` = signif(Deaths/Cases * 1000000,3)) %>%
  subset(Pathogen == "All pathogens") %>%
  select(!c(Pathogen, Cost, Deaths, Hospitalisations)) %>%
  arrange(`Cost per case (AUD)`)
CostPerCase %>% write.csv('AttributionReport/CostPerCaseByFood.csv')
## Generate summary sentences for each pathogen and for the top sources

textlist <- function(cl, conj = 'and', oxford = TRUE){
  n <- length(cl)
  if(n == 0) return(character(0))
  else if(n == 1) return(as.character(cl))
  else if(n == 2) return(paste(cl[1], conj, cl[2]))
  else return(paste0(paste(cl[1:(n-1)], collapse = ', '), if(oxford){','}else{''},' ', conj, ' ', cl[n]))
}

textCountObj <- function(n, name, plural = NULL){
  plural <- pluralize(name)
  if(n == 1 || n == 'one'){return(paste(n, name))}
  else{return(paste(n, plural))}
}

matchVerbNoun <- function(verb, noun){
  if(is_plural(noun)){
    return(pluralize(verb))
  }else{
    return(singularize(verb))
  }
}

attribtionSentencesPathogen <- function(pathogen){
  cl <- CostList %>% subset(Pathogen == pathogen &
                              Agegroup == 'All ages' &
                              CostCategory == "TotalHumanCapital" &
                              median != 0)
  el <- EpiList %>% subset(Pathogen == pathogen &
                             Agegroup == 'All ages' &
                             median != 0)

  InitDisease <- PathogenAssumptions[[pathogen]]$name
  SequelDiseases <- names(PathogenAssumptions[[pathogen]]$sequelae)

  TotalCost <- subset(cl, Source == 'AllFood' &
                        Disease == 'Initial and sequel disease')$median #not tidied until later as it is needed for calculation fractions
  CasesInit <- subset(el, Source == 'AllFood' &
                        Measure == 'Cases' &
                        Disease == 'Initial')$median %>% tidyNumber(unit = 1, max.word = 21)
  CasesSequel <- subset(el, Source == 'AllFood' &
                          Measure == 'Cases' &
                          Disease %in% SequelDiseases)$median %>% sum %>% tidyNumber(unit = 1, max.word = 21)
  Hosp <- subset(el, Source == 'AllFood' &
                   Measure == 'Hospitalisations' &
                   Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1, max.word = 21)
  Deaths <- subset(el, Source == 'AllFood' &
                     Measure == 'Deaths' &
                     Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1, max.word = 21)
  SourceCosts <-  subset(cl, Disease == 'Initial and sequel disease' & Source != 'AllFood') %>% arrange(desc(median))
  SourcesOrdered <- SourceCosts$Source
  SourceCostsOrdered <- SourceCosts$median
  SourcePropOrdered <- round(SourceCostsOrdered/TotalCost* 100)
  SourceCostsOrdered <- SourceCostsOrdered %>% tidyNumber(unit = 10^6, round = FALSE)
  TotalCost <- TotalCost %>% tidyNumber(unit = 10^6, round = FALSE)


  LeadingSourceCases <- subset(el, Source == SourcesOrdered[1] &
                                 Measure == 'Cases' &
                                 Disease == 'Initial')$median %>% tidyNumber(unit = 1, max.word = 21)
  LeadingSourceCasesSequel <- subset(el, Source == SourcesOrdered[1] &
                                       Measure == 'Cases' &
                                       Disease %in% SequelDiseases)$median %>% sum %>% tidyNumber(unit = 1, max.word = 21)
  LeadingSourceHosp <- subset(el, Source == SourcesOrdered[1] &
                                Measure == 'Hospitalisations' &
                                Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1, max.word = 21)
  LeadingSourceDeaths <- subset(el, Source == SourcesOrdered[1] &
                                  Measure == 'Deaths' &
                                  Disease == 'Initial and sequel disease')$median %>% tidyNumber(unit = 1, max.word = 21)
  hasSequel <- length(SequelDiseases) > 0
  str_glue(
    '{pathogen} resulted in an annual cost of approximately AUD {TotalCost} million
    circa 2019 arising from {CasesInit} cases of',
    if(hasSequel){' initial '}else{' '},'illness, ',
    if(hasSequel){'{CasesSequel} cases of sequel illness ({textlist(tolower(SequelDiseases),conj = "or")}), '}else{''},
    '{Hosp} hospitalisations, and {textCountObj(Deaths, "death")}. {SourcesOrdered[1]}
    {matchVerbNoun("was",SourcesOrdered[1])} the leading source ({SourcePropOrdered[1]}%)
    with a total annual cost of AUD {SourceCostsOrdered[1]} million arising from
    {LeadingSourceCases} cases of', if(hasSequel){' initial '}else{' '},'illness, ',
    if(hasSequel){'{LeadingSourceCasesSequel} cases of sequel illness, '}else{''},
    '{LeadingSourceHosp} hospitalisations, and {textCountObj(LeadingSourceDeaths, "death")}. The
    next three most frequent sources were ',
    textlist(glue('{tolower(SourcesOrdered[2:4])} ({SourcePropOrdered[2:4]}%)')), '.'
  ) %>%
    gsub('\n',' ',., fixed = TRUE)
}

attributionSentencesSource <- function(){
  cl <- CostList %>% subset(Agegroup == 'All ages' &
                              CostCategory == "TotalHumanCapital" &
                              median != 0)
  el <- EpiList %>% subset(Measure == 'Cases' &
                             Agegroup == 'All ages'&
                             Disease == 'Initial' &
                             median != 0)
  PathogenNames <- setdiff(unique(CostList$Pathogen), 'All pathogens')
  TotalCost <- subset(cl, Disease == 'Initial and sequel disease' &
                        Pathogen == 'All pathogens' &
                        Source == 'AllFood')$median %>% tidyNumber(unit = 10^6, round = FALSE)
  SourceCosts <-  subset(cl, Disease == 'Initial and sequel disease' &
                           Pathogen == 'All pathogens' &
                           Source != 'AllFood') %>% arrange(desc(median))
  SourcesCostOrdered <- SourceCosts$Source
  SourcesCostOrderedCosts <- SourceCosts$median %>% tidyNumber(unit = 10^6, round = FALSE)

  LeadingSourceCosts <- subset(cl, Disease == 'Initial and sequel disease' &
                                 Pathogen != 'All pathogens' &
                                 Source == SourcesCostOrdered[1]) %>% arrange(desc(median))
  PathogensCostOrdered <- LeadingSourceCosts$Pathogen
  PathogensCostOrderedCosts <- LeadingSourceCosts$median %>% tidyNumber(unit = 10^6, round = FALSE)

  ## Sources by number of
  TotalCases <- subset(el, Pathogen == 'All pathogens' &
                         Source == 'AllFood')$median %>% tidyNumber(unit = 1, max.word = 21)
  SourceCases <-  subset(el, Pathogen == 'All pathogens' &
                           Source != 'AllFood') %>% arrange(desc(median))
  SourcesCaseOrdered <- SourceCases$Source
  SourcesCaseOrderedCases <- SourceCases$median %>% tidyNumber(unit = 1, max.word = 21)

  LeadingSourceCases <- subset(el, Pathogen != 'All pathogens' &
                                 Source == SourcesCaseOrdered[1]) %>% arrange(desc(median))
  PathogensCaseOrdered <- LeadingSourceCases$Pathogen
  PathogensCaseOrderedCases <- LeadingSourceCases$median %>% tidyNumber(unit = 1, max.word = 21)

  str_glue(
    textlist(glue('{PathogenNames}')),
    ' have an estimated combined annual burden of {TotalCases} cases and AUD {TotalCost} million.
    Among the pathogens and food categories considered, {tolower(SourcesCostOrdered[1])}
    {matchVerbNoun("was",SourcesCostOrdered[1])} associated with the greatest cost
    of foodborne illness, with a total cost of AUD {SourcesCostOrderedCosts[1]} million.
    Of this total, ',
    textlist(glue('AUD {PathogensCostOrderedCosts} million was due to {PathogensCostOrdered}')),
    '. The food category associated with the most cases of foodborne illness circa 2019
    was', if(SourcesCaseOrdered[1] == SourcesCostOrdered[1]){' also '}else{' '},
    '{tolower(SourcesCaseOrdered[1])}, with a total burden of {SourcesCaseOrderedCases[1]} cases. Of this total, ',
    textlist(glue('{PathogensCaseOrderedCases} cases were due to {PathogensCaseOrdered}')),'.'
  )%>%
    gsub('\n',' ',., fixed = TRUE)
}

attributionSentencesSource() %>% write_lines('AttributionReport/PathogenAttributionSummariesText.txt',
                                             sep = '\n\n')

CommonPathogens %>%
  map(attribtionSentencesPathogen) %>%
  write_lines('AttributionReport/PathogenAttributionSummariesText.txt',
              sep = '\n\n', append = TRUE)


#Write Cost and Epi List summaries for further analysis -- e.g. could be used to build an interactive tool
write.csv(CostList, 'AttributionReport/AttrCostTable.csv')
write.csv(EpiList, 'AttributionReport/EpiCostTable.csv')



