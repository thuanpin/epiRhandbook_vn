counts <- c(1220, 26882, 1359, 22777) # ~ a, b, c, d 
tab <- matrix( counts, nrow=2, byrow=T)
twoby2(tab)

pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)

linelist <- import("linelist_cleaned.rds")

linelist <- linelist %>% 
  
  # Create new "age_years" column from "age" column
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,       # if age is given in years, assign original value
    age_unit == "months" ~ age/12,    # if age is given in months, divide by 12
    is.na(age_unit)      ~ age,       # if age UNIT is missing, assume years
    TRUE                 ~ NA_real_)) # any other circumstance, assign missing

z <- c(1, 22, NA, Inf, NaN, 5)
max(z)                           # returns NA
max(z, na.rm=T)                  # returns Inf
max(z[is.finite(z)])             # returns 22

as.numeric(c("10", "20", "thirty", "40"))


my_vector <- c(25, NA, 10, NULL)  # define
my_vector                         # print


my_vector <- c(1, 4, 56, NA, 5, NA, 22)
is.na(my_vector)

!is.na(my_vector)

sum(is.na(my_vector))

na.omit(my_vector)

linelist %>% 
  drop_na(case_id, date_onset, age) # drops rows missing values for any of these columns


my_vector <- c(1, 4, 56, NA, 5, NA, 22)

mean(my_vector)     

mean(my_vector, na.rm = TRUE)



# install and/or load package
pacman::p_load(naniar)


# percent of ALL data frame values that are missing
pct_miss(linelist)

# number of ALL data frame values that are missing
n_miss(linelist)

# Percent of rows with any value missing
pct_miss_case(linelist)   # use n_complete() for counts

# Percent of rows that are complete (no values missing)  
pct_complete_case(linelist) # use n_complete() for counts

#Visualizing missing values in each column
gg_miss_var(linelist, show_pct = TRUE)

linelist %>% 
  gg_miss_var(show_pct = TRUE, facet = outcome)


# Heatplot of missingness across the entire data frame  
vis_miss(linelist)

#Explore and visualize missingness relationships
ggplot(
  data = linelist,
  mapping = aes(x = age_years, y = temp)) +     
  geom_miss_point()


gg_miss_fct(linelist, age_cat5)


gg_miss_fct(linelist, date_onset)

#shadow column
shadowed_linelist <- linelist %>% 
  bind_shadow()

names(shadowed_linelist)


ggplot(data = shadowed_linelist,          # data frame with shadow columns
       mapping = aes(x = date_hospitalisation, # numeric or date column
                     colour = age_years_NA)) + # shadow column of interest
  geom_density()                          # plots the density curves


linelist %>%
  bind_shadow() %>%                # create the shows cols
  group_by(date_outcome_NA) %>%    # shadow col for stratifying
  summarise(across(
    .cols = age_years,             # variable of interest for calculations
    .fns = list("mean" = mean,     # stats to calculate
                "sd" = sd,
                "var" = var,
                "min" = min,
                "max" = max),  
    na.rm = TRUE))                 # other arguments for the stat calculations


outcome_missing <- linelist %>%
  mutate(week = lubridate::floor_date(date_onset, "week")) %>%   # create new week column
  group_by(week) %>%                                             # group the rows by week
  summarise(                                                     # summarize each week
    n_obs = n(),                                                  # number of records
    
    outcome_missing = sum(is.na(outcome) | outcome == ""),        # number of records missing the value
    outcome_p_miss  = outcome_missing / n_obs,                    # proportion of records missing the value
    
    outcome_dead    = sum(outcome == "Death", na.rm=T),           # number of records as dead
    outcome_p_dead  = outcome_dead / n_obs) %>%                   # proportion of records as dead
  
  tidyr::pivot_longer(-week, names_to = "statistic") %>%         # pivot all columns except week, to long format for ggplot
  filter(stringr::str_detect(statistic, "_p_"))                  # keep only the proportion values


ggplot(data = outcome_missing)+
  geom_line(
    mapping = aes(x = week, y = value, group = statistic, color = statistic),
    size = 2,
    stat = "identity")+
  labs(title = "Weekly outcomes",
       x = "Week",
       y = "Proportion of weekly records") + 
  scale_color_discrete(
    name = "",
    labels = c("Died", "Missing outcome"))+
  scale_y_continuous(breaks = c(seq(0,1,0.1)))+
  theme_minimal()+
  theme(legend.position = "bottom")


linelist %>% 
  drop_na() %>%     # remove rows with ANY missing values
  nrow()


linelist %>% 
  drop_na(date_onset) %>% # remove rows missing date_onset 
  nrow()


linelist %>% 
  drop_na(contains("date")) %>% # remove rows missing values in any "date" column 
  nrow()


central_data=Central_Hospital
labs(
  title = "",
  y = "",
  x = "",
  caption  = stringr::str_glue(
    "n = {nrow(central_data)} from Central Hospital;
  {nrow(central_data %>% filter(is.na(date_onset)))} cases missing date of onset and not shown."))  


pacman::p_load(forcats)   # load package

linelist <- linelist %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "Missing"))

levels(linelist$gender)


#Imputation
## Mean imputation
pacman::p_load(mice)

linelist <- linelist %>%
  mutate(temp_replace_na_with_mean = replace_na(temp, mean(temp, na.rm = T)))

linelist <- linelist %>%
  mutate(outcome_replace_na_with_death = replace_na(outcome, "Death"))

## reg imputation

simple_temperature_model_fit <- lm(temp ~ fever + age_years, data = linelist)

#using our simple temperature model to predict values just for the observations where temp is missing
predictions_for_missing_temps <- predict(simple_temperature_model_fit,
                                         newdata = linelist %>% filter(is.na(temp))) 

# using mice package for imputation
model_dataset <- linelist %>%
  select(temp, fever, age_years)  

temp_imputed <- mice(model_dataset,
                     method = "norm.predict",
                     seed = 1,
                     m = 1,
                     print = F)

temp_imputed_values <- temp_imputed$imp$temp

#creating our simple dataset
disease <- tibble::tribble(
  ~quarter, ~year, ~cases,
  "Q1",    2000,    66013,
  "Q2",      NA,    69182,
  "Q3",      NA,    53175,
  "Q4",      NA,    21001,
  "Q1",    2001,    46036,
  "Q2",      NA,    58842,
  "Q3",      NA,    44568,
  "Q4",      NA,    50197)

#imputing the missing year values:
disease %>% fill(year)


#creating our slightly different dataset
disease <- tibble::tribble(
  ~quarter, ~year, ~cases,
  "Q1",      NA,    66013,
  "Q2",      NA,    69182,
  "Q3",      NA,    53175,
  "Q4",    2000,    21001,
  "Q1",      NA,    46036,
  "Q2",      NA,    58842,
  "Q3",      NA,    44568,
  "Q4",    2001,    50197)

#imputing the missing year values in the "up" direction:
disease %>% fill(year, .direction = "up")


# imputing missing values for all variables in our model_dataset, and creating 10 new imputed datasets
multiple_imputation = mice(
  model_dataset,
  seed = 1,
  m = 10,
  print = FALSE) 
