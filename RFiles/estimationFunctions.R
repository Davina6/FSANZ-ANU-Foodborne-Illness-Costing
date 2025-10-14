estimateIncidence <- function(pathogen, ndraws = 10^6,
                              gastroRate = NULL, notifications = NULL,
                              population = NULL){
  #abbreviate
  p <- pathogen
  n <- ndraws

  ageGroups = c("<5","5-64","65+")
  names(ageGroups) <- ageGroups

  if(p$caseMethod == 'Notifications' && is.null(notifications)) stop("Notification numbers must be supplied for estimates from notifiable diseases")
  draws <- switch(p$caseMethod,
                  Notifications = draw(p$underreporting, n) * draw(p$domestic, n),
                  GastroFraction = draw(gastroRate, n) * draw(p$gastroFraction, n),
                  Seroprevalence = draw(p$FOI, n))
  foodborne <- draw(p$foodborne, n)
  if(!is.null(p$symptomatic)){
    symptomatic <- draw(p$symptomatic, n)
  }else{
    symptomatic <- 1
  }

  map(ageGroups, function(.a){
    minAge <- switch(.a, `<5` = 0, `5-64` = 5,`65+` = 65)
    maxAge <-  switch(.a, `<5` = 5, `5-64` = 65,`65+` = 101)
    if(p$caseMethod != "Notifications") population <- population[(minAge+1):maxAge]
    incidence <- switch(p$caseMethod,
                        Notifications = notifications[[.a]] * draws * foodborne * symptomatic,
                        GastroFraction = sum(population) * draws * foodborne * symptomatic,
                        Seroprevalence = {
                          as.numeric(map(draws,~{
                            S <- exp(-.x * (minAge:maxAge))
                            sum(population * (S[1:(maxAge-minAge)] - S[1+(1:(maxAge-minAge))]))
                          })) * symptomatic * foodborne
                        })
    incidence
  })
}

estimateSequelae <- function(pathogen, initialCases, ndraws){
  ageGroups = c("<5","5-64","65+")
  names(ageGroups) <- ageGroups

  map(pathogen$sequelae,function(.s){
    sMult <- draw(.s, ndraws)
    map(ageGroups,function(.a){
      sMult * initialCases[[.a]]
    })
  })
}

estimateHosp <- function(disease,
                         #population = NULL,
                         ndraws = 10^6, separations = NULL){
  #This estimates hospitalisations for a specific disease in a specific year
  #abbreviate
  d <- disease
  n <- ndraws
  if(is.null(separations)){
    seperations = 1
    stop("Seperations with primary diagnoses not provided")
  }
  out <- separations / draw(d$hospPrincipalDiagnosis, n) *
    draw(d$underdiagnosis, n) * draw(d$foodborne, n)
  if(d$caseMethod != 'GastroFraction'){
    out <- out * draw(d$domestic, n)
  }
  out
}

estimateGeneric <- function(items, ndraws = 10^6, incidence){
  if(class(items) != 'list') stop('items must be a list')
  n <- ndraws
  if(length(incidence) != 1 & length(incidence) != n){
    print(incidence)
    stop('The length of incidence should either be 1 or equal to ndraws')
  }
  map(items, ~{draw(.x, n) * incidence})
}

estimateMeds <- function(disease, ageGroup, ndraws = 10^6, incidence){
  estimateGeneric(c(disease[["medications"]][["AllAges"]], disease[["medications"]][[ageGroup]]),
                  ndraws = ndraws, incidence = incidence)
}

estimateTests <- function(disease, ageGroup, ndraws = 10^6, incidence){
  estimateGeneric(c(disease[["tests"]][["AllAges"]], disease[["tests"]][[ageGroup]]),
                  ndraws = ndraws, incidence = incidence)
}

estimateGPSpecialist <- function(disease, cases, separations, ndraws = 10^6){
  GP <- estimateGeneric(disease["gp"], ndraws = ndraws, incidence = cases)
  Specialist <- estimateGeneric(disease["specialist"], ndraws = ndraws,
                                incidence = switch(disease$specialistToWhom,
                                                   Cases = cases,
                                                   Hospitalisations = separations,
                                                   None = rep(0,ndraws)))
  if(!is.null(disease$physio)){
    Physio <- estimateGeneric(disease["physio"],ndraws = ndraws,incidence = cases)
  }else{
    Physio <- list(physio = rep(0,ndraws))}


  GPSpecialist <- list(gpShort = GP$gp * (1-disease$gpFracLong),
                       gpLong = GP$gp * disease$gpFracLong,
                       physio = Physio$physio,
                       specialistInitial = Specialist$specialist *0.5,
                       specialistRepeat = Specialist$specialist *0.5)
  GPSpecialist
}

costWTP <- function(disease, ageGroup, cases, separations = NULL, discount = NULL, ndraws = 10^6){
  if(length(cases) != 1 & length(cases) != ndraws){
    stop('The length of cases should either be 1 or equal to ndraws')
  }

  if(!is.null(separations) && length(separations) != 1 && length(cases) != ndraws){
    stop('The length of separations should either be 1 or equal to ndraws')
  }

  if(disease$kind == 'initial'){
    costNonHosp <- (cases - separations)  * disease$duration["NonHosp"] *
      WTPList[[disease$severity["NonHosp"]]][[disease$symptoms]]
    costHosp <- separations  * disease$duration["Hosp"] *
      WTPList[[disease$severity["Hosp"]]][[disease$symptoms]]
    list(FirstYear = costNonHosp + costHosp,
         Ongoing = 0)
  }else{
    WTPMild <- WTPList[['mild']][[disease$symptoms]]
    WTPSevere <- WTPList[['severe']][[disease$symptoms]]
    FirstYear <- WTPMild * (1- disease$propSevere) + WTPSevere * disease$propSevere
    DiscountedYears <- if(discount == 0){disease$durationOngoing}else{(1 - (1-discount)^disease$durationOngoing)/discount * (1 - discount)}
    Ongoing <- WTPMild * draw(disease$propOngoing[[ageGroup]],ndraws) * DiscountedYears
    list(FirstYear = FirstYear * cases,
         Ongoing = Ongoing * cases)
  }
}

costList <- function(l){
  MissingCosts <- setdiff(names(l),row.names(Costs))
  if(length(MissingCosts)){
    stop("No cost item(s) called ", paste(MissingCosts,collapse = ", "))
  }

  if(!is.numeric(Costs[names(l),"Cost"]) | any(is.na(Costs[names(l),"Cost"]))){
    stop('invalid costs:', paste(names(l)[is.na(Costs[names(l),"Cost"])]))
  }

  l %>%
    imap(~{Costs[.y,"Cost"] * .x}) %>%
    data.frame %>%
    rowSums
}

costHumanCapital <- function(year, disease,ageGroup,cases,separations,ndraws){
  if(length(cases) != 1 & length(cases) != ndraws){
    stop('The length of cases should either be 1 or equal to ndraws')
  }

  # Days off of non-hospitalised cases
  if(disease$kind == "initial"){
    parsC <- subset(MissedDaysGastro, AgeGroup == ageGroup & Type == "Carer")
    CarerDays <- (cases) * rlnorm(ndraws,
                                  meanlog = log(parsC$estimate.mu),
                                  sdlog = parsC$sd.mu/parsC$estimate.mu)
    if(ageGroup !="<5"){
      parsS <- subset(MissedDaysGastro, AgeGroup == ageGroup & Type == "Self")
      SelfDays <- (cases) * rlnorm(ndraws,
                                   meanlog = log(parsS$estimate.mu),
                                   sdlog = parsS$sd.mu/parsS$estimate.mu)
    }else SelfDays <- 0
  }else{
    CarerDays <- cases * disease$missedWorkCarer[[ageGroup]]; #warning('Carer days off work for sequelae do not have uncertainty estimates around them')
    SelfDays <- cases * disease$missedWorkSelf[[ageGroup]]; #warning('Self days off work for sequalae do not have uncertainty estimates around them')
  }

  # Additional days off for hospitalised cases based on hospital LOS
  if(disease$kind == "initial"){
    
    year <- specifyAIHWyear(year)
    SepData <- subset(Hospitalisations, DC4D %in% disease$hospCode & AgeGroup == ageGroup & FYNumeric == year)
    
    
    if(sum(SepData$Separations) == 0){
      warning('No hospital separations available for ', disease$name,
              ' in age group ', ageGroup,
              ' in year ', year,
              '. Trying mean LOS across all age groups in this year to estimate time off work for this age group')
      SepData <- subset(Hospitalisations, DC4D %in% disease$hospCode & FYNumeric == year)
      if(sum(SepData$Separations) == 0){
        warning('No hospital separations available for ', disease$name,
                ' in any age group',
                ' in year ', year,
                '. Trying mean LOS for ', ageGroup ,' in other years for which data is available to estimate time off work for this agegroup and year')
        SepData <- subset(Hospitalisations, DC4D %in% disease$hospCode & AgeGroup == ageGroup)
        if(sum(SepData$Separations) == 0){
          warning('No hospital separations available for ', disease$name,
                  ' in age group ', ageGroup,
                  ' in year ', year,
                  ' or any other year for which data is available',
                  '. Trying mean LOS in all age groups in other years for which data is available to estimate time off work for this agegroup and year')
          SepData <- subset(Hospitalisations, DC4D %in% disease$hospCode)
          if(sum(SepData$Separations) == 0){
            stop('No hospital separations available for ', disease$name,
                 ' in any age group',
                 ' in year ', year,
                 ' or any other year for which data is available.')
          }
        }
      }
    }
    meanLOS <- sum(SepData$PatientDays)/sum(SepData$Separations)

    CarerDays <- CarerDays +
      separations * meanLOS *
      Workforce['15-64', 'PropInWorkforce'] *
      Workforce[ageGroup, 'PropNeedsCarer'] * 5/7
    SelfDays <- SelfDays +
      separations * meanLOS *
      Workforce[ageGroup, 'PropInWorkforce'] * 5/7
  }

  CarerDays * Workforce["15-64",'Income'] + SelfDays * Workforce[ageGroup,'Income']
}

makeIncidenceList <- function(year, pathogens, ndraws = 10^6, gastroRate){

  ageGroups <- c("<5","5-64","65+")
  names(ageGroups) <- ageGroups
  
  year <- specifyNNDSSyear(year)

  Initial <- map(pathogens,
                 function(.p){
                   if(.p$caseMethod == "Notifications"){
                     ntfcs <- subset(NotificationsAgeGroup,Disease == .p$name & Year == year & AgeGroup %in% ageGroups)
                     if(nrow(ntfcs) == 0)stop('No notifications available for for year ', year, ', disease ', .p$name)
                     else if(nrow(ntfcs) > length(ageGroups))stop('More than one number found for notifications for year ', year, ', disease ', .p$name)
                     notifications <- as.list(ntfcs$Cases)
                     names(notifications) <- ntfcs$AgeGroup
                   } else notifications <- NULL
                   out <- estimateIncidence(.p,ndraws = ndraws, notifications = notifications,
                                            population = subset(AusPopSingleYear, Year == year)$Persons,
                                            gastroRate = gastroRate)

                   out <- list(out)
                   names(out) <- .p$name
                   out
                 })
  Sequel <- map(pathogens,
                function(.p){estimateSequelae(.p,Initial[[.p$pathogen]][[.p$name]],ndraws = ndraws)})

  #combine
  imap(pathogens, ~c(Initial[[.y]], Sequel[[.y]]))
}

calcSequelaeFractions <- function(Incidence){
  # lapply(rapply(Incidence, enquote, how="unlist"), eval) %>%
  #   as.data.frame(check.names = FALSE) %>%
  #   mutate(Draw = 1:ndraws) %>%
  #   pivot_longer(-Draw,names_sep = "\\.", names_to = c("Pathogen","Disease", "AgeGroup")) %>%
  #   subset(Disease != "Initial") %>%
  #   pivot_wider(values_from = value, names_from = Pathogen) %>%
  #   mutate(Total = rowSums(across(-c(Draw,Disease,AgeGroup)), na.rm = TRUE)) %>%
  #   mutate(across(-c(Draw,Disease,AgeGroup),~.x/Total)) %>%
  #   select(-Total)

  SequelaeIncidenceList <- imap(SequelaeAssumptions,
                                function(.s,.sn){
                                  map(Incidence,~.x[[.sn]])
                                }) %>% #keep only sequel diseases
    map(function(.s){.s[!unlist(map(.s,is.null))]}) #drop pathogens with no sequelae

  TotalSequelaeIncidence <- SequelaeIncidenceList %>%
    map(~do.call(add,.x))

  SequelaeFractions <- SequelaeIncidenceList %>%
    imap(function(.s,.sn){
      map(.s,function(.p){
        map2(.p,TotalSequelaeIncidence[[.sn]],~.x/.y)
      })
    })

  #str(map(SequelaeFractions,~do.call(add,.x))) #check that they add to 1
  SequelaeFractions
}

makeDeathList <- function(year, pathogens, ndraws = 10^6){

  ageGroups <- c("<5","5-64","65+")
  names(ageGroups) <- ageGroups
  
  PopDataYears <- unique(AusPopAgegroup$Year)
  if(!(year %in% PopDataYears)){
    stop('No population data available for year ', year)
  }

  # Adjust to target year
  PopInTargetYear <- subset(AusPopAgegroup, Year == year)

  Deaths <- map(pathogens, function(.p){
    dlist <- c(list(.p), SequelaeAssumptions[names(.p$sequelae)])
    names(dlist)[1] <- .p$name
    map(dlist,function(.d){
      map(ageGroups,function(.a){
        dths <- subset(Deaths,Cause %in% .d$mortCodes & AgeGroup == .a)
        #rgamma(ndraws, sum(dths$Count) + 0.5, dths$PersonYears[1]) * subset(PopInTargetYear, AgeGroup == .a)$Population *
        dths <- rbeta(ndraws, sum(dths$Count) + 0.5, dths$PersonYears[1] - sum(dths$Count) + 0.5) *
          subset(PopInTargetYear, AgeGroup == .a)$Persons *
          draw(.d$underdiagnosis,ndraws) *
          draw(.d$foodborne,ndraws)
        if(.d$caseMethod != "GastroFraction"){
          dths <- dths * draw(.d$domestic,ndraws)
        }
        if(.d$kind == 'sequel'){
          dths <- dths * SequelaeFractions[[.d$name]][[.p$pathogen]][[.a]]
        }
        dths
      })
    })
  })
  Deaths
}

makeHospList <- function(year = 'most_recent', pathogens, incidenceList, ndraws = 10^6){
  ageGroups <- c("<5","5-64","65+")
  names(ageGroups) <- ageGroups
  # InitialCases <- incidenceList$Initial
  # SequelCases <- incidenceList$Sequel
  
  year <- specifyAIHWyear(year)
  HospDataYears <- unique(Hospitalisations$FYNumeric)
  

  Hosp <- map(pathogens, function(.p){
    dlist <- c(list(.p), SequelaeAssumptions[names(.p$sequelae)])
    names(dlist)[1] <- .p$name
    map(dlist, function(.d){
      map(ageGroups,function(.a){
        if(.d$hospMethod == 'AllCases'){return(incidenceList[[.p$pathogen]][[.d$name]][[.a]])}
        else if(.d$hospMethod == 'AIHW'){
          sep <- sum(subset(Hospitalisations, DC4D %in% .d$hospCodes & AgeGroup == .a & FYNumeric == year)$Separations)
          if(sep == 0){
            sep <- subset(Hospitalisations, DC4D %in% .d$hospCodes & AgeGroup == .a)
            if(nrow(sep) == 0){
              error('No separations recorded for ', .d$name ,' in agegroup ', .a ,
                    ' for any of the years for which we have separations data: ', paste(HospDataYears,collapse = ", "))
            }
            sep <- sum(sep$Separations)/length(HospDataYears)
            warning('No separations recorded for ', .d$name ,' in agegroup ', .a , ' for ', year,
                    '. Using mean number of separations from years: ',
                    paste(HospDataYears,collapse = ", "))

          }
          hosp <- estimateHosp(.d,ndraws = ndraws,separations = sep)
          if(.d$kind == 'sequel'){
            hosp <- hosp * SequelaeFractions[[.d$name]][[.p$pathogen]][[.a]]
          }
          return(hosp)
        }else  stop('I have not implemented this method of estiamting intitial disease hospitalisations')
      })
    })
  })
  Hosp
}

warning('Friction costs assume time off is below the 3 month cap!')
estimateCosts <- function(pathogen, year, ndraws = 10^6,
                          notifications, cases, separations, deaths,
                          discount){
  AgeGroups <- c("<5","5-64","65+")
  names(AgeGroups) <- c("<5","5-64", "65+")

  p <- pathogen
  dList <- c(list(p),
             SequelaeAssumptions[names(p$sequelae)])
  names(dList)[1] <- p$name

  pmap(list(dList,cases,separations,deaths),~{
    map(AgeGroups, function(.a){
      #Maybe make this it own function
      GPSpecialist <- estimateGPSpecialist(disease = ..1, cases = ..2[[.a]],
                                           separations = ..3[[.a]], ndraws = ndraws)
      ED <- estimateGeneric(..1["ed"],ndraws = ndraws, incidence = ..2[[.a]])
      Meds <- estimateMeds(..1, ageGroup = .a,
                           incidence = switch(..1$medicationsToWhom,
                                              GP = GPSpecialist$gpShort + GPSpecialist$gpLong,
                                              Cases = ..2[[.a]],
                                              Notifications = notifications[[.a]],
                                              None = rep(0,ndraws)),
                           ndraws = ndraws)
      Tests <- estimateTests(..1, ageGroup = .a,
                             incidence = switch(..1$testsToWhom,
                                                GP = GPSpecialist$gpShort + GPSpecialist$gpLong,
                                                Cases = ..2[[.a]],
                                                Notifications = notifications[[.a]],
                                                None = rep(0,ndraws)),
                             ndraws=ndraws)

      HumanCapital <- costHumanCapital(year, ..1, .a, ..2[[.a]], ..3[[.a]], ndraws = ndraws)
      FrictionHigh <- HumanCapital * FrictionRates$High
      FrictionLow <- HumanCapital * FrictionRates$Low

      WTP = costWTP(..1,.a,cases = ..2[[.a]], separations = ..3[[.a]],discount = discount, ndraws = ndraws)
      out <- list(GPSpecialist = costList(GPSpecialist),
                  ED = costList(ED),
                  Medications = costList(Meds),
                  Tests = costList(Tests),
                  Hospitalisation = ..3[[.a]] * Costs[..1$DRGCodes[[.a]],"Cost"],
                  Deaths = VSL * ..4[[.a]],
                  WTP = WTP$FirstYear,
                  WTPOngoing = WTP$Ongoing,
                  HumanCapital = HumanCapital,
                  FrictionHigh = FrictionHigh,
                  FrictionLow = FrictionLow)
      out <- map(out, ~{if(length(.x) == 0){rep(0, ndraws)}else{.x}})
      out$TotalHumanCapital <- with(out, HumanCapital + GPSpecialist + ED + Medications + Tests + Hospitalisation + Deaths + WTP + WTPOngoing)
      out$TotalFrictionHigh <- with(out, FrictionHigh + GPSpecialist + ED + Medications + Tests + Hospitalisation + Deaths + WTP + WTPOngoing)
      out$TotalFrictionLow <-  with(out, FrictionLow + GPSpecialist + ED + Medications + Tests + Hospitalisation + Deaths + WTP + WTPOngoing)
      out
    })
  })
}


makeCostList <- function(yearNNDSS,
                         yearAIHW,
                         pathogens,
                         ndraws = 10^6,
                         discount){
  
  yearNNDSS <- specifyNNDSSyear(yearNNDSS)
  

  map(pathogens, function(.p){
    cases <- IncidenceList[[.p$pathogen]]
    dList <- c(.p, SequelaeAssumptions[names(.p$sequelae)])
    deaths <- DeathList[[.p$pathogen]]
    separations <- HospList[[.p$pathogen]]
    ntfctns <- subset(NotificationsAgeGroup, Disease == .p$name & Year == yearNNDSS)
    notifications <- as.list(ntfctns$Cases)
    names(notifications) <- ntfctns$AgeGroup
    estimateCosts(.p,yearAIHW,ndraws = ndraws, discount = discount,
                  cases = cases, notifications = notifications,
                  separations = separations, deaths = deaths)
  })
}

specifyNNDSSyear <- function(year){
  NNDSSDataYears <- unique(NotificationsAgeGroup$Year)
  if(year == 'most_recent'){
    year <- max(NNDSSDataYears)
  }else if(!(year %in% NNDSSDataYears)){
    stop('No notification data available for year ', year)
  }
  year
}

specifyAIHWyear <-function(year){
  HospDataYears <- unique(Hospitalisations$FYNumeric)
  if(year == 'most_recent'){
    year <- max(HospDataYears)
  }else if(!(year %in% HospDataYears)){
    stop('No hospitalisation data available for year ', year)
  }
  year
}
  
