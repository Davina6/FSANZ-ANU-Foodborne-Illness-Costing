
getCPI <- function(ReferenceQuarter){
  read.csv('Data/CPI-ABS.csv',skip = 1,
           col.names = c('Quarter', 'Change.Quarterly', 'Change.Annual')) %>%
    drop_na() %>%
    mutate(Date = as_date(paste0('01-',Quarter),format = '%d-%m-%y')) %>%
    subset(Date > as_date(paste0('01-',ReferenceQuarter),format = '%d-%m-%y')) %>%
    arrange(Date) %>%
    mutate(Cumm.Inflation.Multiplier = cumprod(1+Change.Quarterly/100),
           CummCPI = 100*(Cumm.Inflation.Multiplier-1)) %>%
    `row.names<-`(.$Quarter)
}
