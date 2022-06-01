

library(epirhandbook); library(pacman); library(ggplot2); library(tidyverse)

# install the latest version of the Epi R Handbook package
#pacman::p_install_gh("appliedepi/epirhandbook")

# download a specific file into a folder on your computer
#get_data("linelist_cleaned.rds")
#get_data(file = "linelist_cleaned.rds")

# import the linelist
#linelist <- import("linelist_cleaned.rds")
linelist=linelist_cleaned

#44 writing functions ----------------------------------------------------------------------

function_name <- function(argument_1, argument_2, argument_3){
  
  function_task
  
  return(output)
}

contain_covid19 <- function(barrier_gest, wear_mask, get_vaccine){
  
  if(barrier_gest == "yes" & wear_mask == "yes" & get_vaccine == "yes" ) 
    
    return("success")
  
  else("please make sure all are yes, this pandemic has to end!")
}

formals(contain_covid19)
body(contain_covid19)
environment(contain_covid19)


contain_covid19(barrier_gest = "yes", wear_mask = "yes", get_vaccine = "yes")

contain_covid19("yes", "yes", "yes")

contain_covid19(barrier_gest = "yes", wear_mask = "yes", get_vaccine = "no")

contain_covid19(barrier_gest = "yes", wear_mask = "sometimes", get_vaccine = "sometimes")


linelist %>% 
  dplyr::slice_head(n=10) %>%  #equivalent to R base "head" function and that return first n observation of the  dataset
  select(function(x) is.character(x)) 

linelist %>%   
  slice_head(n=20) %>% 
  tibble::rownames_to_column() %>% # add indices of each obs as rownames to clearly see the final selection
  filter(row_number() %%2 == 0)

linelist_firstobs <- head(linelist, 20)

linelist_firstobs[base::Filter(function(x) (x%%2 == 0), seq(nrow(linelist_firstobs))),]


proptab_multiple <- function(my_data, var_to_tab){
  
  #print the name of each variable of interest before doing the tabulation
  print(var_to_tab)
  
  with(my_data,
       rbind( #bind the results of the two following function by row
         #tabulate the variable of interest: gives only numbers
         table(my_data[[var_to_tab]], useNA = "no"),
         #calculate the proportions for each variable of interest and round the value to 2 decimals
         round(prop.table(table(my_data[[var_to_tab]]))*100,2)
       )
  )
}


proptab_multiple(linelist, "gender")

proptab_multiple(linelist, "age_cat")


for(var_to_tab in c("gender","age_cat",  "outcome")){
  
  print(proptab_multiple(linelist, var_to_tab))
  
}


#vong lap
linelist_factor2 <- linelist %>%
  purrr::map_if(is.character, as.factor)


linelist_factor2 %>%
  tibble::glimpse()

# plotting
View(fluH7N9_China_2013)
flu_china=fluH7N9_China_2013

#precising options for the use of highchart
options(highcharter.theme =   highcharter::hc_theme_smpl(tooltip = list(valueDecimals = 2)))


#create a function called "chart_outcome_province" that takes as argument the dataset and the name of the province for which to plot the distribution of the outcome.

chart_outcome_province <- function(data_used, prov){
  
  tab_prov <- data_used %>% 
    filter(province == prov,
           !is.na(outcome))%>% 
    group_by(outcome) %>% 
    count() %>%
    adorn_totals(where = "row") %>% 
    adorn_percentages(denominator = "col", )%>%
    mutate(
      perc_outcome= round(n*100,2))
  
  
  tab_prov %>%
    filter(outcome != "Total") %>% 
    highcharter::hchart(
      "pie", hcaes(x = outcome, y = perc_outcome),
      name = paste0("Distibution of the outcome in:", prov)
    )
  
}

chart_outcome_province(flu_china, "Shanghai")

# tables

indic_1 <- flu_china %>% 
  group_by(province) %>% 
  mutate(
    date_hosp= strptime(date_of_hospitalisation, format = "%m/%d/%Y"),
    date_ons= strptime(date_of_onset, format = "%m/%d/%Y"), 
    delay_onset_hosp= as.numeric(date_hosp - date_ons)/86400,
    mean_delay_onset_hosp = round(mean(delay_onset_hosp, na.rm=TRUE ), 0)) %>%
  select(province, mean_delay_onset_hosp)  %>% 
  distinct()


indic_2 <-  flu_china %>% 
  filter(!is.na(outcome)) %>% 
  group_by(province, outcome) %>% 
  count() %>%
  pivot_wider(names_from = outcome, values_from = n) %>% 
  adorn_totals(where = "col") %>% 
  mutate(
    perc_recovery= round((Recover/Total)*100,2))%>% 
  select(province, perc_recovery)



indic_3 <-  flu_china %>% 
  group_by(province) %>% 
  mutate(
    median_age_cases = median(as.numeric(age), na.rm = TRUE)
  ) %>% 
  select(province, median_age_cases)  %>% 
  distinct()
## Warning in median(as.numeric(age), na.rm = TRUE): NAs introduced by coercion
#join the three indicator datasets

table_indic_all <- indic_1 %>% 
  dplyr::left_join(indic_2, by = "province") %>% 
  left_join(indic_3, by = "province")


#print the indicators in a flextable


print_indic_prov <-  function(table_used, prov){
  
  #first transform a bit the dataframe for printing ease
  indic_prov <- table_used %>%
    filter(province==prov) %>%
    pivot_longer(names_to = "Indicateurs", cols = 2:4) %>% 
    mutate( indic_label = factor(Indicateurs,
                                 levels= c("mean_delay_onset_hosp","perc_recovery","median_age_cases"),
                                 labels=c("Mean delay onset-hosp","Percentage of recovery", "Median age of the cases"))
    ) %>% 
    ungroup(province) %>% 
    select(indic_label, value)
  
  
  tab_print <- flextable(indic_prov)  %>%
    theme_vanilla() %>% 
    flextable::fontsize(part = "body", size = 10) 
  
  
  tab_print <- tab_print %>% 
    autofit()   %>%
    set_header_labels( 
      indic_label= "Indicateurs", value= "Estimation") %>%
    flextable::bg( bg = "darkblue", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::color(color = "white", part = "header") %>% 
    add_header_lines(values = paste0("Indicateurs pour la province de: ", prov)) %>% 
    bold(part = "header")
  
  tab_print <- set_formatter_type(tab_print,
                                  fmt_double = "%.2f",
                                  na_str = "-")
  
  tab_print 
  
}




print_indic_prov(table_indic_all, "Shanghai")
print_indic_prov(table_indic_all, "Jiangsu")


#meo
var_summary <- function(data, var) {
  data %>%
    summarise(n = n(), min = min({{ var }}), max = max({{ var }}))
}
mtcars %>% 
  group_by(cyl) %>% 
  var_summary(mpg)


contain_covid19_missing <- function(barrier_gest, wear_mask, get_vaccine){
  
  if (missing(barrier_gest)) (print("please provide arg1"))
  if (missing(wear_mask)) print("please provide arg2")
  if (missing(get_vaccine)) print("please provide arg3")
  
  
  if (!barrier_gest == "yes" | wear_mask =="yes" | get_vaccine == "yes" ) 
    
    return ("you can do better")
  
  else("please make sure all are yes, this pandemic has to end!")
}


contain_covid19_missing(get_vaccine = "yes")


contain_covid19_stop <- function(barrier_gest, wear_mask, get_vaccine){
  
  if(!is.character(barrier_gest)) (stop("arg1 should be a character, please enter the value with `yes`, `no` or `sometimes"))
  
  if (barrier_gest == "yes" & wear_mask =="yes" & get_vaccine == "yes" ) 
    
    return ("success")
  
  else("please make sure all are yes, this pandemic has to end!")
}


contain_covid19_stop(barrier_gest=1, wear_mask="yes", get_vaccine = "no")


map(linelist, mean)
safe_mean <- safely(mean)
linelist %>% 
  map(safe_mean)


# missing value
pacman::p_load(
  rio,           # import/export
  tidyverse,     # data mgmt and viz
  naniar,        # assess and visualize missingness
  mice           # missing data imputation
)

# import the linelist
linelist <- import("linelist_cleaned.rds")

linelist <- linelist %>% 
  
  # Create new "age_years" column from "age" column
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,       # if age is given in years, assign original value
    age_unit == "months" ~ age/12,    # if age is given in months, divide by 12
    is.na(age_unit)      ~ age,       # if age UNIT is missing, assume years
    TRUE                 ~ NA_real_)) # any other circumstance, assign missing

as.numeric(c("10", "20", "thirty", "40"))


