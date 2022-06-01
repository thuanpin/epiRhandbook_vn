pacman::p_load(
  rio,        # importing data  
  here,       # relative file pathways  
  janitor,    # data cleaning and tables
  lubridate,  # working with dates
  epikit,     # age_categories() function
  tidyverse,  # data management and visualization
  skimr,
  dplyr
)
# 8.4
# clean names of vars
linelist_raw <- import(here("R4Epis", "all data", "linelist_raw.xlsx"))

skimr::skim(linelist_raw)

names(linelist_raw)

linelist <- linelist_raw %>% 
  janitor::clean_names()

names(linelist)

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome)

#change names using select()
linelist_raw %>% 
  select(# NEW name             # OLD name
    date_infection       = `infection date`,    # rename and KEEP ONLY these columns
    date_hospitalisation = `hosp date`)

#8.5
# change position of column
rename(newNameForFirstColumn  = 1,
       newNameForSecondColumn = 2)

# move date_onset and date_hospitalisation to beginning
linelist %>% 
  select(case_id, date_onset, date_hospitalisation, everything()) %>% 
  names()

# select columns that are class Numeric: logic of column -> return TRUE
linelist %>% 
  select(where(is.numeric)) %>% 
  names()

# select columns containing certain characters
linelist %>% 
  select(contains("date")) %>% 
  names()

# searched for multiple character matches; notify error if not found
linelist %>% 
  select(matches("onset|hosp|fev")) %>%   # note the OR symbol "|"
  names()

# function any not notify error if not found
linelist %>% 
  select(any_of(c("date_onset", "village_origin", "village_detection", "village_residence", "village_travel"))) %>% 
  names()

# use select() with '-' to delete unwanted column
linelist %>% 
  select(-c(date_onset, fever:vomit)) %>% # remove date_onset and all columns from fever to vomit
  names()

# use NULL in baseR
linelist$date_onset <- NULL   # deletes column with base R syntax 

## Create a new linelist with id and age-related columns
linelist_age <- select(linelist, case_id, contains("age"))

# display the column names
names(linelist_age)

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  #####################################################

# remove column
select(-c(row_num, merged_header, x28))


#8.6 distinct duplicates
nrow(linelist)
linelist <- linelist %>% 
  distinct()

# CLEANING 'PIPE' CHAIN (starts with raw data and pipes it through cleaning steps)
##################################################################################

# begin cleaning pipe chain
###########################
linelist <- linelist_raw %>%
  
  # standardize column name syntax
  janitor::clean_names() %>% 
  
  # manually re-name columns
  # NEW name             # OLD name
  rename(date_infection       = infection_date,
         date_hospitalisation = hosp_date,
         date_outcome         = date_of_outcome) %>% 
  
  # remove column
  select(-c(row_num, merged_header, x28)) %>% 
  
  # ABOVE ARE UPSTREAM CLEANING STEPS ALREADY DISCUSSED
  #####################################################

# de-duplicate
distinct()

#8.7 create new volumn using mutate() in dplyr
# create BMI variable
linelist <- linelist %>% 
  mutate(bmi = wt_kg / (ht_cm/100)^2)

new_col_demo <- linelist %>%                       
  mutate(
    new_var_dup    = case_id,             # new column = duplicate/copy another existing column
    new_var_static = 7,                   # new column = all values the same
    new_var_static = new_var_static + 5,  # you can overwrite a column, and it can be a calculation using other variables
    new_var_paste  = stringr::str_glue("{hospital} on ({date_hospitalisation})") # new column = pasting together values from other columns
  ) %>% 
  select(case_id, hospital, date_hospitalisation, contains("new"))        # show only new columns, for demonstration purposes

# dealt with class
class(linelist$age)
class(linelist$date_onset)

linelist <- linelist %>% 
  mutate(age = as.numeric(age))

# group data
#age normalized to mean of ALL rows
linelist %>% 
  mutate(age_norm = age / mean(age, na.rm=T))

# age normalized to mean of hospital group
linelist %>% 
  group_by(hospital) %>% 
  mutate(age_norm = age / mean(age, na.rm=T))
