# FSANZ Foodborne Disease Costing Model
This repository contains the R code and data for estimating the burden and cost of food-borne illness, ten priority pathogens and four sequel illnesses in Australia circa 2024.

The code estimates the burden (cases, hospitalisations, and deaths) and estimates the associated costs (direct medical costs, lost productivity, pain and suffering, and fatality). Estimates can be run by executing `Rfiles/BuildCostTable.R`. Cost tables are outputted as .csv files, but can also be explored in an interactive shiny app (`app.R`). The app also allows users to estimate costs for foodborne disease outbreaks based on estimated number of cases, hospitalisations, and deaths using the same costing framework for the main estimates.

Web-hosted version of interactive table can be found at https://angusmclure.shinyapps.io/AusFBDCosting/

The initial development of this code was commissioned by Australian Department of Health, and managed by Food Standards Australia New Zealand (FSANZ). Updates have been commissioned by FSANZ. Code development is by Angus McLure at the Australian National University.

This code was used to calculate the estimates presented in the [FSANZ report]( https://www.foodstandards.gov.au/publications/Documents/ANU%20Foodborne%20Disease%20Final%20Report.pdf) *The annual cost of foodborne disease in Australia* (published 2022). The model and results are available as a [peer-reviewed journal article](https://doi.org/10.1089/fpd.2023.0015) in *Foodborne Pathogens and Disease*.

# Updating Data

This app uses the best data available to the ANU team in early 2025 to estimate burden and cost of disease around the year 2024. There were many data inputs, some up-to-date as of the time of analysis, but others dating back to the early 2000s. Going forward it may be beneficial to update some or all of these data inputs to make better estimates of burden and cost.

The estimation process is complex but involves three broad steps: estimation of burden, estimation of number of cost items, and multiplication of cost items by unit costs. Each of these steps require data, many of them specific to each pathogen being considered. The lists below describe the data, the source and list the names of the files where these data are stored in the model, with notes on how to update them.

## What do I do if I only want to update for inflation?

You can update estimate for inflation without updating or re-running the underlying model. Try launching the app locally  (instructions below) or visit [FSANZ Foodborne Disease Costing Model](https://angusmclure.shinyapps.io/AusFBDCosting/). The app has an options for adjusting for inflation on the first page. If the FY quarter you want to update to is not listed as an option, you will need to update `Data/CPI-ABS.csv` with the latest ABS data, then relaunch your app locally. See end of this document for details about updating `Data/CPI-ABS.csv`.

## How is the input data stored?

**A full list of sources for these inputs and the storage type are listed at the end of this document.**

All data inputs are stored in one of three ways:

1. Data stored and read in the raw, unaltered format in which it is *published or provided* to authors (e.g. population estimates, CPI).
2. Data that is stored in spreadsheets in a custom format (created and maintained by authors)
3. Data that is written into model code

> **Note:** The storage type, source, and instructions to update each data type are listed at the end of this document.

Data storage 1 makes up the largest portion of inputs in terms of sheer number of datapoints. Most data of this type are very simple to update.

Data storage 2 was used for data for which we did not have access to a nicely formatted machine-readable datasets, or when we were plucking a one or two numbers from a very large dataset. This category includes all of the unit costs and a few other inputs

Data storage 3 was used for data which would require (often major) additional research studies to update. They are stored by pathogen in a format which is designed to be human and machine readable. These data types include the number of cost items (medications, tests, GP/ED/specialist visits) per case, underreporting and domestically acquired multipliers.

## Where is the data stored?

All of the data of storage types 1 and 2 are in the `./Data` directory. All data of storage type 3 are in `RFiles/Diseases.R`. The same file also indicates which method (and therefore data sources) should be used to estimate the burden of the disease for each pathogen or disease. Details for each data type are in the final section of this document.

## How do I update the data?

### Data storage type 1 

* Download/access the latest version of the data and save in folder “./Data/”
  * For most files you *replace* old file with new file reusing the old name.
  * For AIHW hospitalisation data we need a file for each FY, so we *add* the latest file and *keep* the older ones. Make sure to use the consistent naming convention. E.g. the FY2018-19 data is called `Principal_diagnosis_data_cube_2018-19.xlsx`
* Sometimes the data custodian will change their data format:
  * If the data format hasn’t changed: we’re done!
  * If data format has changed: either
    * update matching function in “./Rfiles/loadData.R” to read in new format (recommended), OR
    * manually reformat the new dataset into the old format (not recommended)

### Data storage type 2

* Source latest data
* Manually enter/copy values into appropriate spreadsheet in `./Data/`:
  * For most files: replace old values with new values
  * For NNDSS notifications: paste in new rows *after* (in addition to) old rows

### Data storage type 3 

* Source new data (typically requires a new study!)
* Update matching data in `.Rfiles/Diseases.R`

## What if I want to add an additional pathogen?

To include an additional pathogen in the costing requires extensive additional information.

First it would require it's own entry in `RFiles/Diseases.R`. This would tell the model which frameworks are being used to estimate burden (cases and hospitalisations). It would also need to include a wide range of assumptions about symptom type and severity, treatments and healthcare seeking outcomes, underreporting and domestically acquired multipliers and probability of resulting in sequel or ongoing illnesses, and ARDG codes. Many of the inputs would require literature searches, original studies, or expert elicitation.

Second, you would also need to ensure that the matching input data (i.e. deaths and notifications if applicable) are available in the appropriate files in `./Data/`. While AIHW data on hospitalisation already includes all ARDG codes, the deaths dataset only includes selected causes of death and may not cover your disease of interest.

## How I run the model with updated data?

The main script in the programme is `Rfiles/BuildCostTable.R`. Once data and data reading code in `./Rfiles/loadData` are up to date you are ready to run the model to get new estimates of burden and cost.

> **Note:** If you are just updating an existing estimate for inflation, you can skip this step and go straight to running the shiny app. 

1. Open the project file `FSANZ-ANU-Foodborne-Illness-Costing.Rproj`. This should automatically also activate `renv` which will ensure that you have the correct versions of every package you need.

2. Open `Rfiles/BuildCostTable.R`. You will be making a few small edits to opening ~15 lines of this script before running the whole thing.

3. Set the reference quarter for cost estimates.

   ```R
   EstQ <- "Dec-24" #quarter for overall cost estimates
   ```

4. Set the reference year for deaths, cases, and hospitalisations:

   ```R
   YearDeaths <- 2024 # Year for determing population for population-adjusting estimates of deaths (does not change data source for deaths)
   YearCases <- 2024 # Year that notification data and population estimates are taken from for estimating cases
   YearHosp <- 2023 # Financial year that hospitalization data is taken from --- FY2024 will probably not be available until Nov 2025 given historical reporting timelines
   ```

5. Set the random seed for random number generation. If you don't make changes to code or inputs, running the code twice with the same random seed shouldn't change the results. This is helpful for reproducibility and to identify if changes to the code are making any difference to the outputs.

   ```R
   set.seed(20251013) #I suggest choosing the date of the last run on which inputs/code changed in ways that effected the outputs
   ```

6. Run the entire script `Rfiles/BuildCostTable.R`. This may take ~10 minutes. 

   * If there are any errors: Fix errors then clear your workspace before trying to re-run script
     ```R
     rm(list = ls(all.names = TRUE))
     ```
   * If there are no errors: All model outputs have been updated!
   
> **Note:** If you running into errors and find you have to run the model multiple times to fix them, you could speed up run-time be reducing the number of random draws taken. This is set on line ~10 and is currently set to `10^6` (i.e. one million). When you have identified any bugs you can set to the defaul values again.


## Where are model outputs stored?

All model outputs are stored in the folder `./Outputs/`.  All outputs include estimates and uncertainty intervals. There are four human readable outputs:

* `CostPerCase.csv`: The estimated cost per case
* `CostTable.csv`: Detailed breakdown of costs by pathogen, disease, age group, and detailed cost item
* `CostTableCategories.csv`: Costs by pathogen, disease, age group, and four broad cost categories
* `EpiTable.csv`: Summaries of estimated burden (Cases, Hospitalisations and Deaths) by pathogen, disease, and age group

There is also an additional output `AusFBDiseaseImage-Light.RData`, which is not human readable, but can be loaded into R to access a (lightweight) version of the full model outputs. This will not be useful most of the time, but is required for the shiny app to run the Outbreak tab.

## How do I launch the shiny app with updated results?

After you have run the model with updated data successfully (no error messages) you are ready to launch the shiny app.

The simplest option is to launch a local version of the app that is only running on your computer using Rstudio. Simply:

* Open `app.R`
* Click 'Run app' in the top bar of the script editor pane.
* That's it!

If you want to make a version of this app that can be accessed by anyone using a URL you'll need to host the app on a server. See [shinyapps.io](https://www.shinyapps.io/) for free and paid hosting options and guides for how to set this up. A copy of the app is maintained at [angusmclure.shinyapps.io/AusFBDCosting/](https://angusmclure.shinyapps.io/AusFBDCosting/).

## How do I make custom figures and summary tables from results?

The outputs in `./Outputs` are all designed to be easily machine readable so can be used to create Figures and tables programmatically. For example code and outputs for figures See `./Report/Figures.R` and `./Paper and Presentations`. For example code and outputs for summary tables see `./Report/BuildReportTables.R` and the many csv files in the same folder (e.g. `./Report/DetailedCostTable.All pathogens.Initial Disease.csv`).

# Details of each data type 

## Data for estimating burden

These data inform the underlying estimate of burden (cases, hospitalisation, and deaths) that form the core of the costing model.

- **Population estimates (2024, ABS)** Storage format 1

  These are used to calculate the total number of gastro cases and the burden of the non-notifiable pathogens.

  National estimates are stored in `AustralianPopulationByAge.xls` The data are read in by `getAusPopSingleYearAge()` and further processed by `getAusPopAgeGroup` in `Rfiles/loadData.R`.

  State and territory population estimates are stored in files with names following this format `PopulationAgeYear-XXX.xlsx` where XXX is the 2 or 3 letter abbreviation for the jurisdiction. The data are read in by `getCasesStateAgeGroup()` in `Rfiles/loadData.R`.

  All data are in the format published by the ABS: 3101.0 Australian Demographic Statistics; Tables 51-59).

  Assuming that the ABS continues to use the same format going forward, one should be able to update the files by downloading the latest version of this and replacing the file. Modifications to `Rfiles/loadData.R` may be required here if the ABS format changes.

- **Number of notifications for nationally notifiable pathogens (2024*, NNDSS)** Storage format 2

  Each disease has a `caseMethod` attribute in `.Rfiles/Diseases.R` that determines how cases numbers are estimated. If `caseMethod = 'Notifications'`, then notifications for the year must be provided in the `Data` directory.

  For nationally notifiable diseases, these data are stored in `Data/NNDSS Disease by AgeGroup and Year.xslx` and read into the model by `getCasesNNDSSAgeGroup` in `Rfiles/loadData.R`. There are rows for the name of the disease and reporting year, followed by columns for each 5 year age-group (and 85+). Updating the data requires adding rows for each notifiable pathogen for each year for which estimates are required.

  For some data where only (high quality) data were available from certain states, we used older notifications data for those states and performed population adjustment to get national estimates circa 2024. The data are read into the model and population adjusted by `getCasesStateAgeGroup` in `Rfiles/loadData.R`. It reads population data for each jurisdiction (from separate files for each jurisdiction in `Data`) both for the reporting years (2013-2015) and the estimation year (2024) by state, sex, and agegroup and performs adjustments accordingly. The format of the data are the same once read into the model and are combined into a single object `NotificationsAgeGroup`. Currently *Yersinia entercolictica* and STEC use this method. Updating this data will likely be on a case-by-case basis. It might be appropriate in the future for estimates to use national notifications data which will simplify the process.


- **Number of foodborne gastroenteritis events per person per year from the second national gastroenteritis survey (2008, NGSII)** Storage format 3

  This is hard-coded into `Rfiles/Diseases.R` as the `gastroRate` as an `rdist` object specifying the uncertainty distribution for this number.

- **The proportion of gastroenteritis cases due to specific pathogens from the Victorian water quality study (2001, Hellard) — used only for non-notifiable diseases.** Storage format 3

  This is hard-coded into `Rfiles/Diseases.R` for each disease. Each disease has a `caseMethod` attribute that determines how cases numbers are estimated. If `caseMethod = 'GastroFraction'`, then the disease also requires an attribute `gastroFraction` which is another `rdist` object specifying the fraction of gastro that is due to this pathogen.  

- **Seroprevalence studies for *Toxoplasma gondii* infection collected 2005-2007 from Busselton, Western Australia (2020, Molan)** Storage format 3

  This data is used to estimate the force of infection of the disease (units of infections per person per year) by fitting a very simple SIR model to the disease assuming the population is at endemic equilibrium and the force of infection is constant over time. The fitting is performed separately from this code. Each disease has a `caseMethod` attribute that determines how cases numbers are estimated. If `caseMethod = 'Seroprevalence'`, then the disease also requires an attribute `FOI` which is a `rdist` object specifying the force of infection estimate. Currently this is only used for Toxoplasmosis.

- **Number of hospitalisations (FY 2023 and earlier, AIHW)** Storage format 1

  These data are used to estimate the number of hospitalisation for each disease. Each disease in `Rfiles/Diseases.R` has an attribute called `hospMethod`. If `hospMethod = 'AllCases'` then we assume that every case is hospitalised and therefore do not require additional data. If `hospMethod = 'AIHW'` then the reported number of hospitalisations with the principle diagnosis matching the codes listed in attribute `hospCodes` is extracted from AIHW datasets, then multiplied by `underdiagnosis` and divided by`hospPrincipalDiagnosis` attributes for the disease. If there are no hospitalisations reported for the estimation year, the program will use the average number of reported hospitalisations listed across all datasets available to the programme. AIHW reports by financial year and the current version of the software has data for financial years 2015-16 through 2022-23 (latest data as of writing). For circa 2024 estimates we used FY 2022-2023 as the starting point, but expanded to all datasets if there were no hospitalisations reported for FY 2022-23. The data are stored in `./Data` with names of the format `Principle_diagnosis_data_cube_20XX-YY.xlsx`. The data are read in by `getHospitalisationsAgeGroup` in `Rfiles/loadData.R` which assumes that all relevant data will be named with exact matching format. File format is  exactly as downloaded from AIHW website (e.g. [Admitted patient care NMDS 2019-20 (aihw.gov.au)](https://meteor.aihw.gov.au/content/699728)). Updating the model will require downloading the latest data and changing the file name to the matching format. If the format that AIHW uses changes in future releases there may be a requirement to re-write `getHospitalisationsAgeGroup`.

- **Deaths (2014-2023, ABS)** Storage format 1

  Each diseases listed in `Rfiles/Diseases.R` has a an attribute `mortCodes` listing the codes associated with this disease. Given the small number of deaths, there are sensitivities around these data and they are only available on request. Furthermore, as cause of death coding is not always complete, the number of deaths are underreported.  We proceed by applying under-diagnosis multipliers but if the reported number of deaths were 0 for a given year, even multiplying by a large multiplier would have no effect. To deal with these complexities we use data from across 10 years (2014-2023) to estimate mortality incidence rates. Data is read in by `getABSDeaths` in `Rfiles/loadData.R` which reads from the file `Data/Causes of Death data.xlsx` and returns death rates by age-group for different causes of death that can be multiplied by population to get estimates of deaths per year. To update for future years this can either be left as is (in which case population adjustment will be performed for the target year), or reworked with newer data.

- **Multipliers (Various sources and assumptions)** Storage format 3

  There are a number of multipliers used to estimate burden. These are hard-coded for each disease in `Rfiles/Diseases.R` as attributes of each disease. Though different diseases use different multipliers depending on the method used to estimate the burden, these multipliers are `domestic` (fraction of cases that are domestically acquired) `underreporting` (factor to account for underreporting of cases) `foodborne` (fraction of cases that are foodborne), `hospPrincipalDiagnosis` (fraction of hospitalisations where the disease or related code is listed as the principle diagnosis) `underdiagnosis` (factor to account for under-diagnosis of the disease as the cause of death or hospitalisation). There are many different sources for each of these multipliers, and they can be updated by editing the matching attributes in `Rfiles/Diseases.R`.

## Data for estimating number of cost items

This data is used to estimate the number of cost items. There are many cost items. Hospitalisation and deaths are two key cost items that are outlined in the previous section, however other cost items are calculated from the estimates of burden (e.g. GP visits, doses of medication, time off work, days of illness).

* **Direct costs** Storage method 3

  There are many kinds of direct costs considered (GP/ED/Specialist visits, medications and testing) and these are sufficiently different for each disease that they are hard-coded directly into `Rfiles/Diseases.R`. 

* **Duration of illness** Storage method 3

  These are also inputted directly for each disease into the `Rfiles/Diseases.R` under the `duration` attribute, with entries for hospitalised and non-hospitalised cases

- **Days of lost work** Storage method 2

  For non-hospitalised cases the time off work was assumed to be the same for all initial infections and estimated from the Second National Gastroenteritis Survey (NGSII). The data is stored in and read in by `getMissedDaysGastro` from `Rfiles/loadData.R`. If this data were updated this would probably have to be reworked through the whole model as the data outputs is quite specific to NGSII. The number of days taken off work for hospitalised cases is based on the mean length of stay, manually extracted from the same AIHW files reporting the number of hospitalisation discussed in the previous section. This is combined with workforce participation rates (and average daily earnings) by age-group reported adapted from standard reports from the ABS. The data is stored in a custom format in `Data/WorkforceAssumptions.csv` and read into the model by `getWorkforceAssumptions()` from `Rfiles/loadData.R`.  The data can be updated for the latest values by editing `Data/WorkforceAssumptions.csv`. If future updates wish to perform calculations for multiple timepoints, `Data/WorkforceAssumptions.csv` should be be modified to include the published workforce data value for a range of years and `getWorkforceAssumptions()` should be modified to read in data for all years in a format where the program can select from a list of years.

## Data for unit costs

In the final step we multiply cost items by unit prices.

- **Cost of premature mortality** Storage method 2

  Premature mortality is costed using the Value of Statistical Life published by the Department of Prime Minister and Cabinet, in 2024. This is stored in `Data/ValueStatisticalLife.csv` and read in by `getValueStatisticalLife()` from `Rfiles/loadData.R`. If future updates wish to perform calculations for multiple timepoints, `Data/ValueStatisticalLife.csv` should be be modified to include the published workforce data value for a range of years and `getValueStatisticalLife()` should be modified to read in data for all years in a format where the program can select from a list of years.

- **Direct costs** Storage method 2

  As described above, the cost items for each diseases are listed in `Rfiles/Diseases.R` in a custom format. This format includes codes that can be used to lookup costs. The costs themselves are stored in `Data/Costs.xlsx` and read in by `getCosts()` from `Rfiles/loadData.R`. The source for each data point is noted in the spreadsheet.

  In summary, MBS and PBS fees are taken from data published by Medicare (2024), and hospitalisation costs are linked to a AR-DRG (or an average of two such costs) most appropriate for the condition. IHCPA publishes costs for each AR-DRG for public hospitals that can be used to update this data. Calculating costs requires extracting the National Efficient Price (NEP) and the Price weight for each AR-DRG considered. Both NEP and weight tables are updated annually and are available at the IHCPA website. See `./Update/CompareDRG/` for historical data tables and a comparison of AR-DRG costs over time. 

  If future updates wish to perform calculations for multiple timepoints, `Data/Costs.xlsx` should be updated to include the published costs for a range of years and `getCosts()` should be modified to read data for all years in a format where the model can select from a list of years. Another option may be to transition to storing this data in format 1, i.e. read in directly from datasets provided by Medicare and IHCPA directly. However, there are some costs (e.g. for generic classes of drugs or non-PBS medicines) that are not published in public datasets and would need to be estimated an imported into the model separately complicating this approach.

- **Average weekly earnings (2024, ABS)**  Storage format 2

  This is a key cost that is multiplied by number of days of lost work. Data is stored in `Data/WorkforceAssumptions.csv` and read in by `getWorkforceAssumptions()` from `Rfiles/loadData.R`.  If future updates wish to perform calculations for multiple timepoints, the `Data/WorkforceAssumptions.csv` should be modified to include the published workforce data value for a range of years and `getWorkforceAssumptions()` should be modified to read in all of these data in a format such that the program can select from a list of years.

- **Willingness to pay to avoid pain grief and suffering associated with disease estimated from a discrete choice experiment in Australia (2023, CHERE)** Storage format 2

  The willingness to be pay values (elicited in units of per day or per year) are multiplied by total estimated duration of illness (estimated as above) to get total cost of pain and suffering. As these values come from a research study, any update will likely update the format of the data and thus the data importation and the estimation process. However, currently the WTP values and descriptions of uncertainty are stored in `Data/WTPvaluesUncertainty.xlsx` and read into the model by `getWTP()` in `Rfiles/loadData.R`. As WTP values are reported in 2017 dollars, these costs need to be adjusted for inflation.

- **Inflation (2024 and earlier, ABS)** Storage format 1
  The base costs estimates are for December 2024 and expressed in 2024 dollars. Most unit costs were sourced late 2024 or early 2025 so do not require inflation adjustments. However WTP values for grief, pain, and suffering are estimated in 2017 dollars and needed to be adjusted up to Dec 2024. Additionally the shiny app has the option to adjust all costs to the latest quarter without updating all the underlying data. CPI data is stored in `Data/CPI-ABS.csv` in the format published by the [ABS](https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/consumer-price-index-australia/latest-release) (Accessed June 2025). 

  Assuming that the ABS continues to use the same format for these downloads and continues to include dates back to Dec 2017, the `Data/CPI-ABS.csv` can be replaced by the latest release to include the latest CPI rates. However, if the ABS changes the format, the input file can be updated by appending the CPI values for the latest financial quarters in a consistent format. Note that the program uses the 'change from previous quarter' column of the file, so any appended CPI rates should *not* be annualised.

> **Note:** Inflation adjustment in the app only effects the *cost* estimates presented in the shiny app. It does not change any of the estimates in the core model or saved outputs of the model in `Outputs/` or `Report/`.  Even in the shiny app it does *not* update any of the underlying data, estimates of burden, or perform population adjustment to the latest population data.

 

