

pacman::p_load(
  rio,                 # import/export data
  here,                # locate files
  tidyverse,           # data management and visualization
  stringr,             # cleaning characters and strings
  frailtypack,         # needed for dsr, for frailty models
  dsr,                 # standardise rates
  PHEindicatormethods) # alternative for rate standardisation

#not use Mac
packageurl <- "https://cran.r-project.org/src/contrib/Archive/dsr/dsr_0.2.2.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

# Other solution that may work
require(devtools)
devtools::install_version("dsr", version="0.2.2", repos="http:/cran.us.r.project.org")

# import demographics for country A directly from Github
A_demo <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/country_demographics.csv")

# import deaths for country A directly from Github
A_deaths <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/deaths_countryA.csv")

# import demographics for country B directly from Github
B_demo <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/country_demographics_2.csv")

# import deaths for country B directly from Github
B_deaths <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/deaths_countryB.csv")

# import demographics for country B directly from Github
standard_pop_data <- import("https://github.com/appliedepi/epirhandbook_eng/raw/master/data/standardization/world_standard_population_by_sex.csv")


#Import data from my folder
setwd("C:/Users/vohuu/Dropbox/1 R4Epis/all data")
A_demo <- import("country_demographics.csv")
#can not calculate rates because age_cat5 level 85+
A_demo <- A_demo %>%
  mutate(
        age_cat5 = str_replace_all(age_cat5, "\\+", ""))    # remove "+"


B_demo <- import("country_demographics_2.csv")
standard_pop_data<- import("world_standard_population_by_sex.csv")

#clean data
A_demo$Country = ifelse(is.na(A_demo$m),0,"A")
B_demo$Country = ifelse(is.na(B_demo$m),0,"B")

pop_countries <- A_demo %>%  # begin with country A dataset
  bind_rows(B_demo) %>%        # bind rows, because cols are identically named
  pivot_longer(                       # pivot longer
    cols = c(m, f),                   # columns to combine into one
    names_to = "Sex",                 # name for new column containing the category ("m" or "f") 
    values_to = "Population") %>%     # name for new column containing the numeric values pivoted
  mutate(Sex = recode(Sex,            # re-code values for clarity
                      "m" = "Male",
                      "f" = "Female"))

deaths_countries <- A_deaths %>%    # begin with country A deaths dataset
  bind_rows(B_deaths) %>%        # bind rows with B dataset, because cols are identically named
  pivot_longer(                  # pivot longer
    cols = c(Male, Female),        # column to transform into one
    names_to = "Sex",              # name for new column containing the category ("m" or "f") 
    values_to = "Deaths") %>%      # name for new column containing the numeric values pivoted
  rename(age_cat5 = AgeCat)      # rename for clarity


#missing death A 85+
country_data <- pop_countries %>% 
  left_join(deaths_countries, by = c("Country", "age_cat5", "Sex"))
#missing death, pop A
#country_data=merge(pop_countries, deaths_countries, by=c("Country", "age_cat5", "Sex"), all=T)

#create a key column
#pop_countries$dup_check <- as.factor(paste(pop_countries$Country, pop_countries$age_cat5, pop_countries$Sex, sep = "_"))
#deaths_countries$dup_check <- as.factor(paste(deaths_countries$Country, deaths_countries$age_cat5, deaths_countries$Sex, sep = "_"))
#country_data=merge(pop_countries, deaths_countries, by=dup_check, all=T)
#not work well???

#write_xlsx(country_data,"country_data.xlsx")
#input and reload

#country_data <- import("country_data.xlsx")
###ERROR AT A DEMO DATA, + SYMBOL REMOVED


country_data <- country_data %>% 
  mutate(
    Country = fct_relevel(Country, "A", "B"),
    
    Sex = fct_relevel(Sex, "Male", "Female"),
    
    age_cat5 = fct_relevel(
      age_cat5,
      "0-4", "5-9", "10-14", "15-19",
      "20-24", "25-29",  "30-34", "35-39",
      "40-44", "45-49", "50-54", "55-59",
      "60-64", "65-69", "70-74",
      "75-79", "80-84", "85")) %>% 
  
  arrange(Country, age_cat5, Sex)


# Reference population
standard_pop_data <- import("world_standard_population_by_sex.csv")


# Remove specific string from column values
standard_pop_clean <- standard_pop_data %>%
  mutate(
    age_cat5 = str_replace_all(AgeGroup, "years", ""),   # remove "year"
    age_cat5 = str_replace_all(age_cat5, "plus", ""),    # remove "plus"
    age_cat5 = str_replace_all(age_cat5, " ", "")) %>%   # remove " " space
  
  rename(pop = WorldStandardPopulation)   # change col name to "pop", as this is expected by dsr package

#not work well
#age_cat5 line 114 change to AgeGroup


all_data <- left_join(country_data, standard_pop_clean, by=c("age_cat5", "Sex"))


# Calculate rates per country directly standardized for age and sex
mortality_rate <- dsr::dsr(
  data = country_data,  # specify object containing number of deaths per stratum
  event = Deaths,       # column containing number of deaths per stratum 
  fu = Population,      # column containing number of population per stratum
  subgroup = Country,   # units we would like to compare
  age_cat5,             # other columns - rates will be standardized by these
  Sex,
  refdata = standard_pop_clean, # reference population data frame, with column called pop
  method = "gamma",      # method to calculate 95% CI
  sig = 0.95,            # significance level
  mp = 100000,           # we want rates per 100.000 population
  decimals = 2)          # number of decimals)


# Print output as nice-looking HTML table
knitr::kable(mortality_rate) # show mortality rate before and after direct standardization



# Calculate RR
mortality_rr <- dsr::dsrr(
  data = country_data, # specify object containing number of deaths per stratum
  event = Deaths,      # column containing number of deaths per stratum 
  fu = Population,     # column containing number of population per stratum
  subgroup = Country,  # units we would like to compare
  age_cat5,
  Sex,                 # characteristics to which we would like to standardize 
  refdata = standard_pop_clean, # reference population, with numbers in column called pop
  refgroup = "B",      # reference for comparison
  estimate = "ratio",  # type of estimate
  sig = 0.95,          # significance level
  mp = 100000,         # we want rates per 100.000 population
  decimals = 2)        # number of decimals

# Print table
knitr::kable(mortality_rr) 



# Calculate RD
mortality_rd <- dsr::dsrr(
  data = country_data,       # specify object containing number of deaths per stratum
  event = Deaths,            # column containing number of deaths per stratum 
  fu = Population,           # column containing number of population per stratum
  subgroup = Country,        # units we would like to compare
  age_cat5,                  # characteristics to which we would like to standardize
  Sex,                        
  refdata = standard_pop_clean, # reference population, with numbers in column called pop
  refgroup = "B",            # reference for comparison
  estimate = "difference",   # type of estimate
  sig = 0.95,                # significance level
  mp = 100000,               # we want rates per 100.000 population
  decimals = 2)              # number of decimals

# Print table
knitr::kable(mortality_rd) 



# Calculate rates per country directly standardized for age and sex
mortality_ds_rate_phe <- all_data %>%
  group_by(Country) %>%
  PHEindicatormethods::phe_dsr(
    x = Deaths,                 # column with observed number of events
    n = Population,             # column with non-standard pops for each stratum
    stdpop = pop,               # standard populations for each stratum
    stdpoptype = "field")       # either "vector" for a standalone vector or "field" meaning std populations are in the data  

# Print table
knitr::kable(mortality_ds_rate_phe)



# Create reference population
refpopCountryB <- country_data %>% 
  filter(Country == "B") 

# Calculate rates for country A indirectly standardized by age and sex
mortality_is_rate_phe_A <- country_data %>%
  filter(Country == "A") %>%
  PHEindicatormethods::phe_isr(
    x = Deaths,                 # column with observed number of events
    n = Population,             # column with non-standard pops for each stratum
    x_ref = refpopCountryB$Deaths,  # reference number of deaths for each stratum
    n_ref = refpopCountryB$Population)  # reference population for each stratum

# Print table
knitr::kable(mortality_is_rate_phe_A)
