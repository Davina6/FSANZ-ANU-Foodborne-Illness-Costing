library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(tidyverse)
library(mc2d) #for the standard PERT distribution parameterised by min, mode, max
load('Outputs/AusFBDiseaseImage-Light.RData', globalenv())
source("Outbreak.R")
source("RFiles/loadCPIData.R")

# Set year for reference point for inflation adjustments and banner text 
ReferenceYear <- 2024
CPIReferenceQuarter <- "Dec-24" #Follows ABS naming convention for quarters. Quarter's name is the three letter abbr of last month followed by last two digits of year, e.g. 'Dec-24'
CPIReferenceQuarterText <- paste(CPIReferenceQuarter, '(Baseline - No adjustment)')


CPIData <- getCPI(CPIReferenceQuarter)
InitialDiseaseNames <- c(unlist(map(PathogenAssumptions, ~.x$name)), `All pathogens` = 'Initial')

CostTable <- read.csv("Outputs/CostTable.csv") %>%
  select(-X) %>%
  select(Pathogen, Disease, AgeGroup, CostItem, median ,X5., X95.) %>%
  mutate(CostItem = recode(CostItem,
                           GPSpecialist = 'GP and specialist vists',
                           ED = 'Emergency department visits',
                           Deaths = "Premature mortality",
                           WTP = "Pain and suffering",
                           WTPOngoing = "Pain and suffering (ongoing illness)",
                           FrictionLow = 'Friction (low)',
                           FrictionHigh = 'Friction (high)',
                           HumanCapital = 'Human capital',
                           TotalFrictionHigh = 'Total.Friction (high)',
                           TotalFrictionLow = 'Total.Friction (low)',
                           TotalHumanCapital = 'Total.Human capital'),
         Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease))

CostTableSummaries <- read.csv("Outputs/CostTableCategories.csv") %>%
  select(-X) %>%
  select(Pathogen, Disease, AgeGroup, CostItem, median ,X5., X95.) %>%
  mutate(CostItem = recode(CostItem,
                           Deaths = "Premature Mortality",
                           FrictionLow = 'Friction (low)',
                           FrictionHigh = 'Friction (high)',
                           HumanCapital = 'Human capital',
                           TotalFrictionHigh = 'Total.Friction (high)',
                           TotalFrictionLow = 'Total.Friction (low)',
                           TotalHumanCapital = 'Total.Human capital'),
         Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease))

EpiTable <- read.csv('Outputs/EpiTable.csv') %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~format(ifelse(.x<1000, round(.x,digits = 0),signif(.x,3)),
                                           big.mark = ",",
                                           scientific = FALSE,
                                           trim = T,
                                           drop0trailing = TRUE))) %>%
  mutate(`90% UI` = paste(X5.,X95.,sep = '-'),
         Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease)) %>%
  rename("Count" = "median") %>%
  select(Pathogen, Disease, AgeGroup, Measure, Count, `90% UI`)

ProductivityOptions <- c("Human capital", "Friction (high)", "Friction (low)")
TotalOptions <- paste0("Total.",ProductivityOptions)

EpiMeasures <- unique(EpiTable$Measure)
CostItems <- c(setdiff(unique(CostTable$CostItem),c(ProductivityOptions,TotalOptions)), "Non-fatal productivity losses", "Total")
PathogenNames <- unique(EpiTable$Pathogen) %>% setdiff(c('All pathogens', 'All gastro pathogens')) %>% sort %>% c('All pathogens', 'All gastro pathogens',.)
DiseaseNames <- unique(EpiTable$Disease) %>% setdiff(c('Initial disease', 'Initial and sequel disease')) %>% sort %>% c('Initial disease', 'Initial and sequel disease',.)
CostCategories <- c(setdiff(unique(CostTableSummaries$CostItem), c(ProductivityOptions, paste0('Total.',ProductivityOptions))), "Non-fatal productivity losses", "Total")
AgeGroups <- unique(EpiTable$AgeGroup)

PathogenSelect <- c('All pathogens','Norovirus','Campylobacter')
DiseaseSelect <- c('Initial disease','Initial and sequel disease')
MeasureSelect <- c('Hospitalisations','Cases')
CostItemSelect <- c('Total','Hospitalisation')
AgeGroupSelect <- 'All ages'

explainer_text <-
  paste0("The value of this multiplier is pathogen-specific, inlcudes uncertainty, ",
         "and is the same as in the FSANZ report \\'The annual cost of foodborne ",
         "illness in Australia\\' (2022). This multiplier will not be applied to ",
         "any death or hospitalisation figures supplied by the user.")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("FSANZ Foodborne Disease Costing Model"),
  textOutput('Banner'),
  tabsetPanel(type = "pills",
              tabPanel('Info',
                       #fluidRow(includeHTML('./InfoText.html')),
                       column(width = 1),
                       column(width = 10,
                              fluidRow(includeMarkdown('./InfoText.md')),
                              fluidRow(column(width = 6,
                                              selectInput('Quarter.Inflation',
                                                   'Quarter for Inflation adjustment',
                                                   c(CPIReferenceQuarterText,CPIData$Quarter),
                                                   selected = CPIReferenceQuarterText,
                                                   multiple = FALSE)),
                                       column(6,
                                              HTML('<br />'), #add some vertical whitespace
                                              textOutput('Inflation'))),
                              fluidRow(HTML(paste0(rep('<br />',10),collapse = ' '))) ## adds whitespace after selection so that it doesn't run off screen. There is probably a better wat to do this but I couldn't find one easily!
                              )
                       ),
              tabPanel('Epi Summaries',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen.Epi",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Epi",
                                         "Disease",
                                         DiseaseNames,
                                         selected = DiseaseSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Epi",
                                         "Age group",
                                         AgeGroups,
                                         selected = AgeGroupSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Measures.Epi",
                                         "Measure",
                                         EpiMeasures,
                                         selected = MeasureSelect,
                                         multiple = TRUE
                                       ))
                       ),
                       DT::dataTableOutput("EpiDT")),
              tabPanel('Cost Summaries',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen.Summary",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenNames[1],
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Summary",
                                         "Disease",
                                         DiseaseNames,
                                         selected = 'All Diseases',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Summary",
                                         "Age group",
                                         AgeGroups,
                                         selected = 'All ages',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Productivity.Summary",
                                         "Non-fatal productivity losses method",
                                         ProductivityOptions,
                                         selected = "HumanCapital",
                                         multiple = FALSE
                                       ))
                       ),
                       DT::dataTableOutput("SummaryDT")),
              tabPanel("Cost Comparisons",
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen.Detailed",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Detailed",
                                         "Disease",
                                         DiseaseNames,
                                         selected = DiseaseSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Detailed",
                                         "Age group",
                                         AgeGroups,
                                         selected = AgeGroupSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "CostItem.Detailed",
                                         "Cost items",
                                         CostItems,
                                         selected = CostItemSelect,
                                         multiple = TRUE
                                       ))
                       ),
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Productivity.Detailed",
                                         "Non-fatal productivity losses method",
                                         ProductivityOptions,
                                         selected = "HumanCapital",
                                         multiple = FALSE
                                       ))),
                       DT::dataTableOutput("ComparisonDT")),
              tabPanel('Outbreak',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "PathogenOutbreak",
                                         "Pathogen",
                                         setdiff(PathogenNames,c("All gastro pathogens",'All pathogens')),
                                         selected = "Non-typhoidal salmonella",
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       br(),
                                       checkboxInput(
                                         "UnderMult",
                                         "Use multiplier for under-reporting of cases?",
                                         value = FALSE
                                       )),
                                column(width = 3,
                                       br(),
                                       checkboxInput(
                                         "FoodborneMult",
                                         "Use multiplier for food-borne acquisition?",
                                         value = FALSE
                                       )),
                                column(width = 3,
                                       br(),
                                       checkboxInput(
                                         "DomesticMult",
                                         "Use multiplier for domestic acquisition?",
                                         value = FALSE
                                       ))
                       ),
                       bsPopover("DomesticMult",
                                 'Domestically Acquired Multiplier',
                                 paste("Select this option if you think only a fraction of outbreak cases were acquired domestically and you want to exclude those acquired elsewhere.",
                                       explainer_text,
                                       "This option is only available for nationally notifiable diseases.")),
                       bsPopover("UnderMult",
                                 'Under-reporting multiplier',
                                 paste("Select this option if you think only a fraction of outbreak cases were identified and you want to adjust estimates (up) to account for unidentified cases.",
                                       explainer_text,
                                       "This option is only available for nationally notifiable diseases.")),
                       bsPopover("FoodborneMult",'Foodborne multiplier',
                                 paste("Select this option if you think only a fraction of outbreak cases were acquired from food and you want to exclude those acquired via other routes.",
                                       explainer_text)),
                       fluidRow(column(width = 3,
                                       textInput(
                                         "NotificationsOutbreak",
                                         "Confirmed cases for ages <5, 5-64, 65+",
                                         '',
                                         placeholder = "e.g. 1,2,3 (musn't be left blank)"
                                       )),
                                column(width = 3,
                                       textInput(
                                         "SeparationsOutbreak",
                                         "Hospitalisations for ages <5, 5-64, 65+" ,
                                         placeholder = 'leave blank to estimate from cases'
                                       )),
                                column(width = 3,
                                       textInput(
                                         "DeathsOutbreak",
                                         "Deaths for ages <5, 5-64, 65+",
                                         placeholder = 'leave blank to estimate from cases'
                                       )),
                                column(width = 3,
                                       br(),
                                       actionButton("Trigger.Outbreak", "Estimate")
                                )
                       ),
                       bsPopover("NotificationsOutbreak",
                                 'Known number of infections associated with the outbreak',
                                 "The total number of cases will be adjusted up or down if the above multipliers are selected. The number of cases are used to estimate number of sequelae (if any) and number of hospitalisations and deaths due to the initial infection (if not provided)" ),
                       bsPopover("SeparationsOutbreak",
                                 "Known number of hospitalisations associated with outbreak",
                                 "Estimated from confirmed cases if left blank. Should only inlcude hospitalisations due to initial infections; hospitalisations due to sequelae are estimated from number of cases" ),
                       bsPopover("DeathsOutbreak",
                                 "Known number of deaths associated with outbreak",
                                 "Estimated from confirmed cases if left blank. Should only inlcude deaths due to initial infections; deaths due to sequelae are estimated from number of cases" ),
                       hr(),
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Disease.Outbreak",
                                         "Disease",
                                         DiseaseNames,
                                         selected = 'All Diseases',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Outbreak",
                                         "Age group",
                                         AgeGroups,
                                         selected = 'All ages',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Productivity.Outbreak",
                                         "Non-fatal productivity losses method",
                                         ProductivityOptions,
                                         selected = "HumanCapital",
                                         multiple = FALSE
                                       ))
                       ),
                       DT::dataTableOutput("OutbreakDT")
              )
  )
)

server <- function(input, output) {

  observe({
    OutbreakNotifiableSelected <- PathogenAssumptions[[input$PathogenOutbreak]]$caseMethod == "Notifications"
    toggleState(id = "UnderMult", condition =  OutbreakNotifiableSelected)
    toggleState(id = "DomesticMult", condition =  OutbreakNotifiableSelected)
    if(!OutbreakNotifiableSelected){
      updateCheckboxInput(inputId = "UnderMult", value = F)
      updateCheckboxInput(inputId = 'DomesticMult', value = F)
    }
  })

  #Multiplier for inflation calculations
  InflationMult <- reactiveVal(1)
  BasicBanner <- paste0('Burden and cost estimates circa ',ReferenceYear,
                        ' with data from ', ReferenceYear,' or older,')
  observeEvent(input$Quarter.Inflation,{
    if(input$Quarter.Inflation == CPIReferenceQuarterText){
      InflationMult(1)
      output$Banner <- renderText(c(BasicBanner,' with no inflation adjustment.',sep = ''))
    }else{
      InflationMult(CPIData[input$Quarter.Inflation,'Cumm.Inflation.Multiplier'])
      output$Banner <- renderText(c(BasicBanner,' with cost estimates inflation adjusted to ', input$Quarter.Inflation, ' (',
                                    if(InflationMult()>1){'+'}else{''},
                                    round(100*(InflationMult()-1),1),
                                    '%).'), sep = '')
    }
  })

  output$Inflation <- renderText(c('Total inflation adjustment from baseline is ',
                                   if(InflationMult()>1){'+'}else{''},
                                   round(100*(InflationMult()-1),1),
                                   '%.'), sep = '')


  output$EpiDT = DT::renderDataTable(EpiTable %>%
                                       subset(Measure %in% input$Measures.Epi &
                                                AgeGroup %in% input$AgeGroup.Epi &
                                                Pathogen %in% input$Pathogen.Epi &
                                                Disease %in% input$Disease.Epi),
                                     rownames = FALSE,
                                     server = FALSE,
                                     extensions = c("Buttons"),
                                     options = list(dom = 'Bfrtip',
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                     )
  )

  output$ComparisonDT <- DT::renderDataTable(
    {renderCostTableDetailed(input,CostTable,InflationMult())},
    server = FALSE)

  output$SummaryDT = DT::renderDataTable(
    {renderCostTableSummaries(input,CostTableSummaries,"Summary",InflationMult())},
  server = FALSE)

  OutbreakSummary <- reactive({
    Outbreak <- PathogenAssumptions[[input$PathogenOutbreak]]
    #All pathogens need to use the notification method of estimating case numbers for outbreak costing purposes
    #this requires domestic and undereporting multipliers which we set to 1 by default
    if(Outbreak$caseMethod != 'Notifications'){
      Outbreak$domestic <- rdist('discrete', value = 1, continuous = FALSE)
      Outbreak$underreporting <- rdist('discrete', value = 1, continuous = FALSE)
      Outbreak$caseMethod <- 'Notifications'
    }
    if(!input$DomesticMult) Outbreak$domestic <- rdist('discrete', value = 1, continuous = FALSE)
    if(!input$UnderMult) Outbreak$underreporting <- rdist('discrete', value = 1, continuous = FALSE)
    if(!input$FoodborneMult) Outbreak$foodborne <- rdist('discrete', value = 1, continuous = FALSE)
    separations <- list(read.input(input$SeparationsOutbreak))
    names(separations) <- Outbreak$name
    deaths <- list(read.input(input$DeathsOutbreak))
    names(deaths) <- Outbreak$name

    OutbreakCost <- costOutbreak(pathogen = Outbreak,
                                 notifications = read.input(input$NotificationsOutbreak),
                                 separations = separations,
                                 deaths = deaths,
                                 ndraws = 10^3)$costs

    summariseCostList(list(Outbreak = OutbreakCost))[['Categorised']] %>%
      rename('X5.' = `5%`,
             'X95.' = `95%`) %>%
      mutate(CostItem = recode(CostItem,
                               Deaths = "Premature Mortality",
                               FrictionLow = 'Friction (low)',
                               FrictionHigh = 'Friction (high)',
                               HumanCapital = 'Human capital',
                               TotalFrictionHigh = 'Total.Friction (high)',
                               TotalFrictionLow = 'Total.Friction (low)',
                               TotalHumanCapital = 'Total.Human capital'),
             Disease = ifelse(Disease == Outbreak$name,
                              'Initial disease',
                              Disease))
  })


  # Only display once users click the Estimate button AND clear display any time
  # the user changes the inputs defining the outbreak
  display <- reactiveValues(display = FALSE)

  observe({display$display <- FALSE}) %>%
    bindEvent(input$PathogenOutbreak,
              input$FoodborneMult,
              input$UnderMult,
              input$DomesticMult,
              input$DeathsOutbreak,
              input$SeparationsOutbreak,
              input$NotificationsOutbreak)
  observe({display$display <- TRUE}) %>%
    bindEvent(input$Trigger.Outbreak)


  output$OutbreakDT = DT::renderDataTable({
    if(!display$display){
      data.frame()
    }else{
      if(input$NotificationsOutbreak == ''){
        stop('Confirmed number of cases in each age group must be provided')
      }
      renderCostTableSummaries(input,
                               OutbreakSummary(),
                               'Outbreak',InflationMult(),'Outbreak')
    }
  },
  server = FALSE)
}



read.input <- function(x){
  if(x == ''){out <- NULL}
  else{
    out <- as.list(as.numeric(strsplit(x, ',')[[1]]))
    if(length(out) != 3) stop('Number of cases, hospitalisations, and deaths must either be left blank or must exactly three numbers (one for each agegroup <5, 5-64 , 65+) seperated by commas')
    if(any(is.na(out))) stop('Numbers of cases, hospitalisation, and deaths must be numbers seperated by commas (or blank)')
    names(out) <- c("<5","5-64","65+")
  }
  out
}

renderCostTableSummaries <- function(input,.CostTableSummaries,tabname,.InflationMult,.Pathogen = NULL){
  .Productivity <- input[[paste0("Productivity.", tabname)]]
  .AgeGroup <- input[[paste0("AgeGroup.", tabname)]]
  if(is.null(.Pathogen)){
    .Pathogen <- input[[paste0("Pathogen.", tabname)]]
  }
  .Disease  <- input[[paste0("Disease.", tabname)]]
  cn <- colnames(.CostTableSummaries)
  CostCols <- cn[grep("Cost \\(",cn)]


  .CostTableSummaries %>%
    mutate(across(c(median,X5.,X95.),
                  ~.x * .InflationMult * ifelse(tabname == 'Summary', 0.001, 1)), #adjust for inflation (and divide by 1000 for annual summaries)
           across(c(median,X5.,X95.), #format numbers
                  ~format(ifelse(.x<0.1,
                                 round(.x,digits = 3),
                                 signif(.x,3)),
                          big.mark = ",",
                          scientific = FALSE,
                          trim = T,
                          drop0trailing = TRUE)),
           `90% UI` = paste(X5.,X95.,sep = ' - '), #paste UI into single variables
           `Cost Category` = if_else(CostItem == .Productivity, #select the costs associated with the productivity loss method (inlcuding total cost)
                                     'Non-fatal productivity losses',
                                     if_else(CostItem == paste0('Total.',.Productivity),
                                             'Total', CostItem))) %>%
    subset(`Cost Category` %in% CostCategories &
             AgeGroup == .AgeGroup &
             Pathogen == .Pathogen &
             Disease == .Disease) %>%
    rename_with(~{if(tabname == 'Summary'){'Cost (thousands AUD)'}else{'Cost (AUD)'}}, median) %>% #rename the cost estimate column from median to Cost (AUD) or Cost (thousands AUD)
    select(-c(AgeGroup, Pathogen, Disease, CostItem, X5., X95.)) %>%
    relocate(`Cost Category`) %>%

    DT::datatable(rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(searching = FALSE,
                                 order = list(1, 'asc'),
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) #%>%
}


renderCostTableDetailed <- function(input,.CostTableDetailed,.InflationMult){
  .Productivity <- input$Productivity.Detailed
  .AgeGroup  <- input$AgeGroup.Detailed
  .Pathogen  <- input$Pathogen.Detailed
  .Disease   <- input$Disease.Detailed
  .CostItem  <- input$CostItem.Detailed

  CostTable %>%
    mutate(CostItem = if_else(CostItem == .Productivity,
                              'Non-fatal productivity losses',
                              if_else(CostItem == paste0('Total.',.Productivity),
                                      'Total', CostItem)),
           across(c(median,X5.,X95.),
                  ~.x * .InflationMult * 0.001), #adjust for inflation and divide by 1000
           across(c(median,X5.,X95.), #format numbers
                  ~format(ifelse(.x<0.1,
                                 round(.x,digits = 3),
                                 signif(.x,3)),
                          big.mark = ",",
                          scientific = FALSE,
                          trim = T,
                          drop0trailing = TRUE)),
           `90% UI` = paste(X5.,X95.,sep = ' - '))  %>% #paste UI into single variables)
    subset(CostItem %in% .CostItem &
             AgeGroup %in% .AgeGroup &
             Pathogen %in% .Pathogen &
             Disease %in% .Disease) %>%
    rename(`Age group` = AgeGroup,
           `Cost Item` = CostItem,
           `Cost (thousands AUD)` = median) %>%
    select(-c(X5., X95.)) %>%
    DT::datatable(rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) # %>%
    # DT::formatCurrency(columns = "Cost (thousands AUD)", currency = "", interval = 3, mark = ",", digits = 0)

}

shinyApp(ui, server)
