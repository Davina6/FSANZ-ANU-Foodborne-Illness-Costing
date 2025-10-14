# Make figures for the report
library(tidyverse)
library(plotly)
library(ggtext)

source("./RFiles/Distributions.R")
source("./RFiles/ClassDefinitions.R")
source("./RFiles/Diseases.R")

SequelaeNames <- names(SequelaeAssumptions)

CostSummaries.Detailed <- read.csv('./Outputs/CostTable.csv') %>%
  #subset(CostItem %in% c('GPSpecialist','ED','Deaths','WTPOngoing','HumanCapital','Medications','Tests'))
  mutate(CostItem = recode(CostItem,
                           GPSpecialist = 'GP and specialist vists',
                           ED = 'Emergency department visits',
                           Deaths = "Premature mortality",
                           WTP = "Pain and suffering",
                           HumanCapital = 'Non-fatal productivity losses'))

CostPerCase <- read.csv('./Outputs/CostPerCase.csv')


CostSummaries.Categorised <- read.csv('./Outputs/CostTableCategories.csv') %>%
  subset(!(CostItem %in% c('FrictionLow', 'FrictionHigh','TotalFrictionHigh','TotalFrictionLow','TotalHumanCapital'))) %>%
  subset(!(Pathogen %in% c('All gastro pathogens', 'All pathogens'))) %>%
  subset(Disease != 'Initial and sequel disease') %>%
  mutate(SequelOrInitial = if_else(Disease %in% SequelaeNames, 'Sequel disease(s)', 'Initial disease')) %>%
  subset(AgeGroup == 'All ages') %>%
  mutate(CostItem = recode(CostItem,
                           Deaths = "Premature Mortality",
                           HumanCapital = 'Non-fatal productivity losses'))


PathogenByCost <- CostSummaries.Detailed %>%
  subset(!(Pathogen %in% c("All pathogens", "All gastro pathogens")) &
           Disease == 'Initial and sequel disease' &
           AgeGroup == 'All ages' &
           CostItem == "TotalHumanCapital") %>%
  with(Pathogen[order(median)])


CostTotals <- CostSummaries.Categorised %>%
  mutate(Pathogen = Pathogen %>% factor(levels = PathogenByCost),
         SequelOrInitial = forcats::fct_rev(SequelOrInitial)) %>%
  group_by(CostItem, SequelOrInitial, Pathogen) %>%
  summarise(`Annual cost (millions AUD)` = sum(median)/10^6)

CostTotalsOnly <- read.csv('./Outputs/CostTableCategories.csv') %>%
  subset(Disease == 'Initial and sequel disease' &
           CostItem == 'TotalHumanCapital' &
           AgeGroup == 'All ages'&
           !(Pathogen %in% c('All gastro pathogens', 'All pathogens'))) %>%
  mutate(Cost = median/10^6) %>%
  select(Pathogen, Cost) %>%
  mutate(Pathogen = fct_reorder(Pathogen, Cost),
         PathogenTidy = recode(Pathogen,
                               `Salmonella Typhi` = '*Salmonella*<br />Typhi',
                               Shigella = '*Shigella*',
                               `Yersinia Enterocolitica` = '*Yersinia<br />Enterocolitica*',
                               `Toxoplasma gondii`  = "*Toxoplasma<br />gondii*",
                               `Listeria monocytogenes` = "*Listeria<br />monocytogenes*",
                               `Escherichia coli (Non-STEC)` = "*Escherichia coli*<br />(non-STEC)",
                               `Non-typhoidal Salmonella` = "Non-typhoidal<br />*Salmonella*",
                               `Campylobacter` = "*Campylobacter*"
         ))

P.TotalCost <-  CostTotals %>%
  ggplot(aes(x = Pathogen, y = `Annual cost (millions AUD)`,
             fill = CostItem,
             alpha = SequelOrInitial,
             pattern = SequelOrInitial
  )) +
  geom_bar(stat = 'identity',
           position = 'stack',
           color = 'black') +
  scale_alpha_manual(values = c(`Initial disease` = 1, `Sequel disease(s)` = 0.5)) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"),
                             title = 'Cost category'),
         alpha = guide_legend(title = 'Sequel vs initial')) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.25))
P.TotalCost
ggsave(P.TotalCost,filename = 'Report/TotalCostBCatSequel.png', width = 1904, height = 1512, unit = 'px')

P.CostPerPase <- CostPerCase %>%
  select(-c(X, X5., X95.)) %>%
  merge(CostTotals %>%
          group_by(Pathogen) %>%
          group_modify(~mutate(.x, CostFraction = `Annual cost (millions AUD)`/sum(`Annual cost (millions AUD)`))),
        by ='Pathogen') %>%
  mutate(CostPerCaseByCat = CostFraction * median/1000) %>%
  ggplot(aes(x = fct_reorder(Pathogen, median), y = CostPerCaseByCat, fill = CostItem)) +
  geom_bar(stat = 'identity', position = 'stack', color = 'black') +
  ylab('Cost per case (thousands AUD)') +
  xlab('Pathogen') +
  guides(fill = guide_legend(title = 'Cost category')) +
  coord_flip() +
  theme(legend.position = c(0.75, 0.15))
P.CostPerPase
ggplotly(P.CostPerPase)
ggsave(P.CostPerPase,filename = 'Report/CostPerCase.png', width = 1904, height = 1512, units = 'px')

ggsave(P.CostPerPase + theme(text = element_text(size = 14),
                             axis.title.y=element_blank(),
                             axis.text.y=element_blank(),
                             axis.ticks.y=element_blank()),
       filename = 'Paper and Presentations/CostPerCase.png',
       height = 5, width = 5)




CostAllGastro <- CostSummaries.Detailed %>%
  subset(AgeGroup == 'All ages' &
           Pathogen == 'All gastro pathogens' &
           !(CostItem %in% c('TotalHumanCapital',
                             'TotalFrictionHigh',
                             'TotalFrictionLow',
                             'FrictionHigh',
                             'FrictionLow',
                             'WTPOngoing')))

ItemByCost.AllGastro <- CostAllGastro %>%
  subset(Disease == 'Initial and sequel disease') %>%
  with(CostItem[order(median)])

P.AllGastroByCat <- CostAllGastro %>%
  subset(Disease != 'Initial and sequel disease') %>%
  mutate(CostItem = factor(CostItem,levels =ItemByCost.AllGastro),
         SequelOrInitial = if_else(Disease %in% SequelaeNames,
                                   'Sequel diseases',
                                   'Initial disease')) %>%
  ggplot(aes(x = CostItem,
             y = median/10^6,
             label = round(median/10^6),
             alpha = fct_rev(SequelOrInitial))) +
  geom_bar(stat = 'identity') +
  #geom_text() +
  xlab('Category') +
  ylab('Annual cost (millions AUD)') +
  coord_flip() +
  scale_alpha_manual(values = c(`Initial diseases` = 1, `Sequel diseases` = 0.5)) +
  guides(alpha = guide_legend(title = 'Sequel vs Initial')) +
  theme(legend.position = c(0.75, 0.15))

P.AllGastroByCat
ggsave(P.AllGastroByCat, filename = 'Report/CostAllGastroByCat.png', width = 1904, height = 1512, units = 'px')

#######################################
# Figures for paper and presentations #
#######################################

P.TotalCost.AllPathogens <-  read.csv('./Outputs/CostTableCategories.csv') %>%
  subset(!(CostItem %in% c('FrictionLow', 'FrictionHigh','TotalFrictionHigh','TotalFrictionLow','TotalHumanCapital'))) %>%
  subset(Pathogen == 'All pathogens') %>%
  subset(Disease != 'Initial and sequel disease') %>%
  mutate(SequelOrInitial = if_else(Disease %in% SequelaeNames, 'Sequel disease(s)', 'Initial disease')) %>%
  subset(AgeGroup == 'All ages') %>%
  mutate(CostItem = recode(CostItem,
                           Deaths = "Premature Mortality",
                           HumanCapital = 'Non-fatal productivity losses')) %>%
  mutate(SequelOrInitial = forcats::fct_rev(SequelOrInitial)) %>%
  group_by(CostItem, SequelOrInitial) %>%
  summarise(`Annual cost (millions AUD)` = sum(median)/10^6) %>%
  mutate(Pathogen = 'All pathogens') %>%
  ggplot(aes(x = Pathogen, y = `Annual cost (millions AUD)`,
             fill = CostItem,
             alpha = SequelOrInitial
  )) +
  geom_bar(stat = 'identity',
           position = 'stack',
           color = 'black') +
  scale_alpha_manual(values = c(`Initial disease` = 1, `Sequel disease(s)` = 0.5)) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"),
                             title = 'Cost category'),
         alpha = guide_legend(title = 'Sequel vs initial')) +
  coord_flip() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size =  14))
P.TotalCost.AllPathogens
ggsave(P.TotalCost.AllPathogens,filename = 'Paper and Presentations/AllPathByCatSequel.png',
       height = 2.7, width = 10)

CostTotalsSequel <- CostTotals %>%
  group_by(Pathogen,SequelOrInitial) %>%
  summarise(Cost = sum(`Annual cost (millions AUD)`)) %>%
  mutate(PathogenTidy = recode(Pathogen,
                               `Salmonella Typhi` = '*Salmonella*<br />Typhi',
                               Shigella = '*Shigella*',
                               `Yersinia Enterocolitica` = '*Yersinia<br />Enterocolitica*',
                               `Toxoplasma gondii`  = "*Toxoplasma<br />gondii*",
                               `Listeria monocytogenes` = "*Listeria<br />monocytogenes*",
                               `Escherichia coli (Non-STEC)` = "*Escherichia coli*<br />(non-STEC)",
                               `Non-typhoidal Salmonella` = "Non-typhoidal<br />*Salmonella*",
                               `Campylobacter` = "*Campylobacter*"
  ))


P.CostTotalsSequel <- CostTotalsSequel  %>%
  ggplot(aes(x = PathogenTidy, y = Cost, alpha = SequelOrInitial)) +
  geom_bar(stat = 'identity',
           position = 'stack',
           color = 'black') +
  scale_alpha_manual(values = c(`Initial disease` = 1, `Sequel disease(s)` = 0.5)) +
  geom_text(data = CostTotalsOnly,
            mapping = aes(x = PathogenTidy, y = Cost,
                          label = sprintf("%0.1f", round(Cost,1))),
            hjust = -0.2, inherit.aes = FALSE) +
  coord_flip() +
  theme(legend.position = 'none',
        text = element_text(size =  12),
        axis.text.y = element_markdown(size = 12),
        axis.title.y=element_blank()) +
  ylim(0, 600) +
  ylab('Annual cost\n(millions AUD)')

P.CostTotalsSequel
ggsave(P.CostTotalsSequel,filename = 'Paper and Presentations/CostTotalsSequel.png',
       height = 12/2.54, width = 10.5/2.54)

P.CostTotalsOnly <- CostTotalsOnly %>%
  ggplot(aes(x = PathogenTidy, y = Cost,
             label = sprintf("%0.1f", round(Cost,1)))) +
  geom_bar(stat = 'identity',
           position = 'stack',
           color = 'black',
           fill = 'orange') +
  geom_text(hjust = -0.2) +
  coord_flip() +
  theme(text = element_text(size =  12),
        axis.text.y = element_markdown(size = 12),
        axis.title.y=element_blank()) +
  ylim(0, 600) +
  ylab('Annual cost\n(millions AUD)')

P.CostTotalsOnly
ggsave(P.CostTotalsOnly,filename = 'Paper and Presentations/CostTotalsOnly.png',
       height = 12/2.54, width = 10.5/2.54)
P.CostTotalsOnly$layers[[1]]$aes_params <- list(color = 'black')
ggsave(P.CostTotalsOnly + theme(axis.title.y=element_text(size =12)) + xlab('Pathogen'),filename = 'Paper and Presentations/CostTotalsOnlyWide.png',
       height = 12/2.54, width = 15.5/2.54)

P.CostProportions <-  CostTotals %>%
  group_by(Pathogen) %>%
  mutate(Proportion = `Annual cost (millions AUD)`/ sum(`Annual cost (millions AUD)`),
         CostItem = recode(CostItem,
                           Direct = 'Direct Costs',
                           `Non-fatal productivity losses` = 'Lost Productivity',
                           `Pain and suffering` = 'Pain & Suffering',
                           `Premature mortality` = 'Premature Mortality'),
         SequelOrInitial = recode(SequelOrInitial,
                                  `Initial disease` = 'Primary Illness',
                                  `Sequel disease(s)` = 'Sequel Illness(es)')) %>%
  ggplot(aes(x = Pathogen, y = Proportion,
             fill = CostItem,
             alpha = SequelOrInitial,
             pattern = SequelOrInitial
  )) +
  geom_bar(stat = 'identity',
           position = 'stack',
           color = 'black') +
  scale_alpha_manual(values = c(`Primary Illness` = 1, `Sequel Illness(es)` = 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(fill = guide_legend(override.aes = list(pattern = "none"),
                             title = 'Cost Category', order = 1),
         alpha = guide_legend(title = 'Primary vs Sequel', order = 2)) +
  coord_flip() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size =  12),
        legend.direction = 'horizontal',
        legend.box.background = element_rect(fill = 'white', color = 'white'),
        legend.spacing.y = unit(1,'mm')) +
  ylab('Proportion of cost\n')
P.CostProportions
library(ggpubr)
legend <- P.CostProportions %>% get_legend %>% as_ggplot
ggsave(legend, filename = 'Paper and Presentations/CostProportionsLegend.png',
       height = 4/2.54, width = 24/2.54)
legend <- (P.CostProportions + theme(legend.direction = 'vertical')) %>% get_legend %>% as_ggplot
ggsave(legend, filename = 'Paper and Presentations/CostProportionsLegendVertical.png',
       height = 6/2.54, width = 5.5/2.54)
ggsave(P.CostProportions + theme(legend.position = 'none'),
       filename = 'Paper and Presentations/CostProportions.png',
       height = 12/2.54, width = 7/2.54)

#plots for each pathogen
CostSummaries.Categorised %>%
  #subset(Pathogen == 'Campylobacter') %>%
  mutate(SequelOrInitial = forcats::fct_rev(SequelOrInitial)) %>%
  group_by(Pathogen, CostItem, SequelOrInitial) %>%
  summarise(`Annual cost (millions AUD)` = sum(median)/10^6) %>%
  group_by(Pathogen) %>%
  group_walk(~{
    p <- ggplot(.x, aes(x = 1, y = `Annual cost (millions AUD)`,
                        fill = CostItem,
                        alpha = SequelOrInitial
    )) +
      geom_bar(stat = 'identity',
               position = 'stack',
               color = 'black') +
      scale_alpha_manual(values = c(`Initial disease` = 1, `Sequel disease(s)` = 0.5)) +
      guides(fill = guide_legend(override.aes = list(pattern = "none"),
                                 title = 'Cost category'),
             alpha = if(length(unique(.x$SequelOrInitial)) == 1){'none'}else{guide_legend(title = 'Sequel vs initial')}
               ) +
      coord_flip() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            text = element_text(size =  14))
    ggsave(filename = paste0('Paper and Presentations/',.y,'ByCatSequel.png'),
           p,height = 2.7, width = 10)
  })


P.CostPerPaseTotals  <- CostPerCase %>%
  subset(!(Pathogen %in% c('All gastro pathogens', 'All pathogens'))) %>%
  merge(CostTotals %>%
          group_by(Pathogen, SequelOrInitial) %>%
          summarise(Cost = sum(`Annual cost (millions AUD)`)) %>%
          mutate(CostFraction = Cost/sum(Cost)) %>% select(-Cost),
        by ='Pathogen') %>%
  ggplot(aes(x = factor(Pathogen, levels = PathogenByCost),
             y = median * CostFraction/1000,
             label = sprintf("%0.1f", round(median/1000,1)))) +
  geom_bar(#aes(alpha = SequelOrInitial),
           fill = 'orange',
           stat = 'identity', position = 'stack', color = 'black') +
  scale_alpha_manual(values = c(`Initial disease` = 1, `Sequel disease(s)` = 0.5)) +
  ylab('Cost per case\n(thousands AUD)') +
  xlab('Pathogen') +
  coord_flip() +
  geom_text(data = CostPerCase %>%
              subset(!(Pathogen %in% c('All gastro pathogens', 'All pathogens'))),
            mapping = aes(y = median/1000),
            hjust = -0.2) +
  theme(legend.position = 'none',
        text = element_text(size = 12),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(0, 500)
P.CostPerPaseTotals
ggsave(P.CostPerPaseTotals,
       filename = 'Paper and Presentations/CostPerCase.png',
       height = 12/2.54, width = 7/2.54)



