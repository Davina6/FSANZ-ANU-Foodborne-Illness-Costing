#Compare 2000-2010 death data to 2014-2023 death data

library(tidyverse)
library(readxl)

source("./RFiles/loadData.R")

DeathsOld <- getABSDeaths()


DeathsOld <- bind_rows(DeathsOld,DeathsOld %>%
              group_by(Cause) %>%
              summarise(Count = sum(Count)) %>%
              mutate(AgeGroup = 'Total')) %>%
  arrange(Cause,AgeGroup)
DeathsOld

#Read in 2014-2023 death data
DeathsNew <- read_xlsx('./Data/Causes of Death data.xlsx',
                      sheet = 'Table 1', range = 'A7:AP46') %>%
  select(-...2)
colnames(DeathsNew) <- c('Cause',
                         paste(sort(rep(2014:2023, 4)),
                               c('<5', '5-64', '65+', 'Total')))

#Convert to long form data
DeathsNew <- DeathsNew %>%
  pivot_longer(-Cause, names_sep = ' ',
               values_to = 'Deaths',
               names_to = c("Year", "AgeGroup")) %>%
  mutate(CauseLong = Cause,
         Cause = str_extract(CauseLong,'([^\\s]+)') %>% str_trim(), #Extract out cause of death code
         Cause = ifelse(nchar(Cause) == 4, # include a . for codes of length 4
                        paste(substr(Cause,1,3), substr(Cause,4,4),sep = '.'),
                        Cause)
         )
Years <- DeathsNew$Year %>% unique %>% as.integer
# Sum over years
DeathsNew <- DeathsNew %>%
  group_by(AgeGroup,Cause) %>% #sum over years
  summarise(Count = sum(Deaths))
colnames(DeathsNew)

AusPop <- getAusPopAgeGroup() %>%
  subset(Year %in% Years) %>%
  group_by(AgeGroup) %>%
  summarise(PersonYears = sum(Persons))

DeathsNew <- DeathsNew %>% 
  merge(AusPop) %>%
  mutate(Rate = Count/PersonYears) %>% 
  subset(AgeGroup != 'Total')

View(DeathsOld)
View(DeathsNew)

#Compare two datasets

Codes <- c("A01",DeathsNew$Cause %>% unique)

ComparisonData <-  bind_rows(New = DeathsNew, Old = DeathsOld,.id = 'Period') %>%
  filter(AgeGroup == 'Total',
         Cause %in% Codes)

ComparisonPlot <- ComparisonData %>%
  ggplot(aes(x = Cause, y= Count, fill = Period)) +
    geom_bar(stat = 'identity', position = position_dodge(0.5)) +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip()
ComparisonPlot

ggsave('DeathsComparisonABS.png',ComparisonPlot, width = 10, height = 20)


ComparisonData %>% filter(Cause %in% c('B58', 'B15', 'A09',
                                       'A08.3', 'A08.1', 'A07.8',
                                       'A05.8', 'A05.2', 'A04.6','A04.4')) %>%
  pivot_wider(names_from = Period, values_from = Count) %>%
  write.csv('./MarkedDifferenceDeathsABS.csv')

ComparisonData %>%
  pivot_wider(names_from = Period, values_from = Count) %>%
  write.csv('./AllDifferenceDeathsABS.csv')




  
  