# Hospitalisation ratios (Primary to Additional) for key conditions

library(tidyverse)

# Step 0:Done outside R

# Data is stored in password protected format and should not be shared publicly
# on GitHub. Also, data was provided in two parts: Part A which had the bulk of
# the data, and part B which included a few rows that were suppressed at first
# pending jurisdictional clearance. Therefore before running the below script
# you need to combine the two documents into a machine readable (non-password
# protected) spreadsheet. The way to do this is to copy Part A into a new
# spreadsheet and then complete the missing rows by copying from part B. The
# combined spreadsheet should then be stored at the location given below and
# then deleted once this script has been run

ReadibleDataLocation <- "path/to/file.xslx"

# Read in hospitalisation numbers  ------------------------------------

# Data is formatted poorly. In blocks by FY. Need to read in block for each FY
# first, then combine, then pivot to long form

#metadata to help read in data
meta <- data.frame(FY = 2019:2023, ## FY is the year of the end e.g. FY2019 is AKA 2018-19
                   StartRow = 4 + 28*(0:4), # First row for each FY block
                   EndRow = 28 + 28*(0:4)) %>% # Last  row for each FY block
  cross_join(data.frame(Kind = c('Principal', 'Additional')))

# read in data
d <- meta %>% 
  group_by(FY, Kind) %>%
  group_modify(~{
    readxl::read_excel(path = ReadibleDataLocation,
                       sheet = paste(.y$Kind, 'diagnosis'),
                       range = readxl::cell_limits(ul = c(.x$StartRow,1), 
                                                   lr = c(.x$EndRow, 16)))
    }) %>%
# convert to long form for age groups
  ungroup() %>%
  pivot_longer(`Under 5`:`65 and older`,
               names_to = 'Age', values_to = 'Count') %>%
# convert to numeric for counts, handling blanks
  mutate(Count = as.numeric(na_if(Count, 'n.p.'))) %>%
# collapse down to preferred age-groups
  mutate(Agegroup = case_match(Age,
                              "Under 5" ~ "<5",
                              "65 and older" ~ "65+",
                              .default = "5-64"
                              )) %>%
  group_by(`ICD-10 code`, Descriptor, FY, Kind, Agegroup) %>%
  summarise(Count = sum(Count)) %>%
  ungroup()

# Add row for 'All gastro'
GastroDescriptors <- c("Salmonellosis", "Shigellosis", "Campylobacteriosis",
                      "Escherichia coli infection",
                      #"Gastroenteritis of unknown origin",
                       "STEC", "Yersiniosis", "Norovirus infection")

d <- d %>% subset(Descriptor %in% GastroDescriptors) %>%
  mutate(Descriptor = 'Gastro (except unknown)',
         `ICD-10 code` = 'Many') %>%
  group_by(`ICD-10 code`,Descriptor,FY, Kind, Agegroup) %>%
  summarise(Count = sum(Count,na.rm = TRUE)) %>%
  bind_rows(d)
d <- d %>% subset(Descriptor %in% c(GastroDescriptors,"Gastroenteritis of unknown origin")) %>%
  mutate(Descriptor = 'Gastro (including unknown)',
         `ICD-10 code` = 'Many') %>%
  group_by(`ICD-10 code`,Descriptor,FY, Kind, Agegroup) %>%
  summarise(Count = sum(Count,na.rm = TRUE)) %>%
  bind_rows(d)


## Summaries

KeyDiseases <- c(GastroDescriptors, "Gastroenteritis of unknown origin",
                 "Irritable Bowel Syndrome", "Toxoplasmosis", "Listeriosis",
                 "Reactive Arthritis",'Gastro (including unknown)',
                 'Gastro (except unknown)', "Typhoid fever")

# Calculate ratios by FY and agegroup
p <- d %>%
  pivot_wider(names_from = Kind, values_from = Count) %>%
  mutate(Prop = Principal/(Additional + Principal)) %>%
  subset(Descriptor %in% KeyDiseases) %>%
  ggplot(aes(x= FY, y=Prop, color = Descriptor)) +
  geom_line() +
  facet_wrap(~Agegroup)
plotly::ggplotly(p)


# Calculate ratios across all FY by agegroup

d %>%
  group_by(`ICD-10 code`, Descriptor, Kind, Agegroup) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  pivot_wider(names_from = Kind, values_from = Count) %>%
  mutate(Prop = Principal/(Additional + Principal))

# Calculate ratios by FY and across agegroup
p <- d %>%
  group_by(`ICD-10 code`, Descriptor, Kind, FY) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  subset(Descriptor %in% KeyDiseases) %>%
  pivot_wider(names_from = Kind, values_from = Count) %>%
  mutate(Prop = Principal/(Additional + Principal)) %>%
  ggplot(aes(x= FY, y=Prop, color = Descriptor)) +
  geom_line()
plotly::ggplotly(p)

# Calculate ratios across all FY and agegroups

d.summary <- d %>%
  group_by(`ICD-10 code`, Descriptor, Kind) %>%
  summarise(Count = sum(Count, na.rm = TRUE)) %>%
  pivot_wider(names_from = Kind, values_from = Count) %>%
  mutate(Prop = Principal/(Additional + Principal)) %>%
  subset(Descriptor %in% KeyDiseases)

View(d.summary)



## Read in hospitalisation ratios from previous modelling work (assumes the
## files haven't been updated yet --- once update this will just be reading in
## the updated data)

source("./RFiles/Distributions.R")
source("./RFiles/ClassDefinitions.R")
source("./RFiles/Diseases.R")

DiseaseAssumtions <- c(PathogenAssumptions, SequelaeAssumptions)
OldProps <- DiseaseAssumtions %>%
  map(~.x$hospPrincipalDiagnosis$params$value)
names(OldProps) <- DiseaseAssumtions %>%
  map(~.x$name)

OldProps %>%
  unlist() %>% 
  data.frame(Prop = .) %>%
  rownames_to_column(var = 'Descriptor') %>%
  full_join(d.summary, by = 'Descriptor', suffix = c(".Old", ".New")) %>%
  select(Descriptor, Prop.Old, Prop.New) %>%
  View
  


  
