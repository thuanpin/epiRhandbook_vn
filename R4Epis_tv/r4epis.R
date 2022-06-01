

library(epirhandbook); library(pacman); library(ggplot2); library(tidyverse)

# install the latest version of the Epi R Handbook package
#pacman::p_install_gh("appliedepi/epirhandbook")

# download a specific file into a folder on your computer
#get_data("linelist_cleaned.rds")
#get_data(file = "linelist_cleaned.rds")

# import the linelist
#linelist <- import("linelist_cleaned.rds")
linelist=linelist_cleaned

#18 basic analysis------------------------------------------------------------------------
library(rstatix)
linelist %>%
  rstatix::get_summary_stats(age, temp)

linelist %>%
  group_by(hospital) %>%
  rstatix::get_summary_stats(age, temp, type = "common")

linelist %>%
  group_by(hospital) %>%
  dplyr::summarize(age, temp, type = "common")

linelist %>% 
  t_test(age_years ~ gender)

linelist %>% 
  head(500) %>%            # first 500 rows of case linelist, for example only
  shapiro_test(age_years)

library(gtsummary)
linelist %>% 
  select(gender, outcome) %>%    # keep variables of interest
  tbl_summary(by = outcome) %>%  # produce summary table and specify grouping variable
  add_p()                        # specify what test to perform


#19 regression---------------------------------------------------------------------------

pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # data management + ggplot2 graphics, 
  stringr,      # manipulate text strings 
  purrr,        # loop over objects in a tidy way
  gtsummary,    # summary statistics and tests 
  broom,        # tidy up results from regressions
  lmtest,       # likelihood-ratio tests
  parameters,   # alternative to tidy up results from regressions
  see          # alternative to visualise forest plots
)

# import the linelist
linelist <- rio::import("linelist_cleaned.rds")
## define variables of interest 
explanatory_vars <- c("gender", "fever", "chills", "cough", "aches", "vomit")

## convert dichotomous variables to 0/1 
library(dplyr)
linelist <- linelist %>%  
  mutate(across(                                      
    .cols = all_of(c(explanatory_vars, "outcome")),  ## for each column listed and "outcome"
    .fns = ~case_when(                              
      . %in% c("m", "yes", "Death")   ~ 1,           ## recode male, yes and death to 1
      . %in% c("f", "no",  "Recover") ~ 0,           ## female, no and recover to 0
      TRUE                            ~ NA_real_)    ## otherwise set to missing
  )
  )

## add in age_category to the explanatory vars 
explanatory_vars <- c(explanatory_vars, "age_cat")

## drop rows with missing information for variables of interest 
linelist <- linelist %>% 
  tidyr::drop_na(any_of(c("outcome", explanatory_vars)))

#univariate

lm_results <- lm(ht_cm ~ age, data = linelist)
summary(lm_results)

broom::tidy(lm_results)

library(ggplot)
## pull the regression points and observed data in to one dataset
points <- augment(lm_results)

## plot the data using age as the x-axis 
ggplot(points, aes(x = age)) + 
  ## add points for height 
  geom_point(aes(y = ht_cm)) + 
  ## add your regression line 
  geom_line(aes(y = .fitted), colour = "red")

#logistic 

model <- glm(outcome ~ age_cat, family = "binomial", data = linelist)
summary(model)

model <- glm(outcome ~ age_cat, family = "binomial", data = linelist) %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>%        # exponentiate and produce CIs
  mutate(across(where(is.numeric), round, digits = 2))  # round all numeric columns


counts_table <- linelist %>% 
  janitor::tabyl(age_cat, outcome)

counts_table <- linelist %>% 
  janitor::tabyl(age_cat, hospital ,outcome)

#combine results
combined <- counts_table %>%           # begin with table of counts
  bind_cols(., model) %>%              # combine with the outputs of the regression 
  select(term, 2:3, estimate,          # select and re-order cols
         conf.low, conf.high, p.value) %>% 
  mutate(across(where(is.numeric), round, digits = 2)) ## round to 2 decimal places

combined <- combined %>% 
  flextable::qflextable()                   


explanatory_vars %>% str_c("outcome ~ ", .)
models <- explanatory_vars %>%       # begin with variables of interest
  str_c("outcome ~ ", .) %>%         # combine each variable into formula ("outcome ~ variable of interest")
  
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       # pass the formulas one-by-one to glm()
      formula = as.formula(.x),      # within glm(), the string formula is .x
      family = "binomial",           # specify type of glm (logistic)
      data = linelist)) %>%          # dataset
  
  # tidy up each of the glm regression outputs from above
  map(
    .f = ~tidy(
      .x, 
      exponentiate = TRUE,           # exponentiate 
      conf.int = TRUE)) %>%          # return confidence intervals
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))


## for each explanatory variable
univ_tab_base <- explanatory_vars %>% 
  map(.f = 
        ~{linelist %>%                ## begin with linelist
            group_by(outcome) %>%     ## group data set by outcome
            count(.data[[.x]]) %>%    ## produce counts for variable of interest
            pivot_wider(              ## spread to wide format (as in cross-tabulation)
              names_from = outcome,
              values_from = n) %>% 
            drop_na(.data[[.x]]) %>%         ## drop rows with missings
            rename("variable" = .x) %>%      ## change variable of interest column to "variable"
            mutate(variable = as.character(variable))} ## convert to character, else non-dichotomous (categorical) variables come out as factor and cant be merged
  ) %>% 
  
  ## collapse the list of count outputs in to one data frame
  bind_rows() %>% 
  
  ## merge with the outputs of the regression 
  bind_cols(., models) %>% 
  
  ## only keep columns interested in 
  select(term, 2:3, estimate, conf.low, conf.high, p.value) %>% 
  
  ## round decimal places
  mutate(across(where(is.numeric), round, digits = 2))

#gtsummary
univ_tab <- linelist %>% 
  dplyr::select(explanatory_vars, outcome) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = outcome,                            ## define outcome variable
    method.args = list(family = binomial),  ## define what type of glm want to run (logistic)
    exponentiate = TRUE                     ## exponentiate to produce odds ratios (rather than log odds)
  )

## view univariate results table 
univ_tab

##Conduct multivariable

mv_reg <- glm(outcome ~ gender + fever + chills + cough + aches + vomit + age_cat, family = "binomial", data = linelist)

summary(mv_reg)
glm(outcome ~ gender + age_cat * fever, family = "binomial", data = linelist)
glm(outcome ~ gender + age_cat : fever, family = "binomial", data = linelist)

## run a regression with all variables of interest 
mv_reg <- explanatory_vars %>%  ## begin with vector of explanatory column names
  str_c(collapse = "+") %>%     ## combine all names of the variables of interest separated by a plus
  str_c("outcome ~ ", .) %>%    ## combine the names of variables of interest with outcome in formula style
  glm(family = "binomial",      ## define type of glm as logistic,
      data = linelist)          ## define your dataset

model1 <- glm(outcome ~ age_cat, family = "binomial", data = linelist)
model2 <- glm(outcome ~ age_cat + gender, family = "binomial", data = linelist)

lmtest::lrtest(model1, model2)
anova(model1, model2, test = "Chisq")

## choose a model using forward selection based on AIC
## you can also do "backward" or "both" by adjusting the direction
final_mv_reg <- mv_reg %>%
  step(direction = "forward", trace = FALSE)
#turn on or off scientific notation 
options(scipen=999)
options(scipen = 0)

mv_tab_base <- final_mv_reg %>% 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%  ## get a tidy dataframe of estimates 
  mutate(across(where(is.numeric), round, digits = 2))          ## round 

## show results table of final regression 
mv_tab <- tbl_regression(final_mv_reg, exponentiate = TRUE)


## combine with univariate results 
tbl_merge(
  tbls = list(univ_tab, mv_tab),                          # combine
  tab_spanner = c("**Univariate**", "**Multivariable**")) # set header names


#dplyr
## combine univariate and multivariable tables 
left_join(univ_tab_base, mv_tab_base, by = "term") %>% 
  ## choose columns and rename them
  select( # new name =  old name
    "characteristic" = term, 
    "recovered"      = "0", 
    "dead"           = "1", 
    "univ_or"        = estimate.x, 
    "univ_ci_low"    = conf.low.x, 
    "univ_ci_high"   = conf.high.x,
    "univ_pval"      = p.value.x, 
    "mv_or"          = estimate.y, 
    "mvv_ci_low"     = conf.low.y, 
    "mv_ci_high"     = conf.high.y,
    "mv_pval"        = p.value.y 
  ) %>% 
  mutate(across(where(is.double), round, 2))   


# forest plot 

## remove the intercept term from your multivariable results
mv_tab_base %>% 
  
  #set order of levels to appear along y-axis
  mutate(term = fct_relevel(
    term,
    "vomit", "gender", "fever", "cough", "chills", "aches",
    "age_cat5-9", "age_cat10-14", "age_cat15-19", "age_cat20-29",
    "age_cat30-49", "age_cat50-69", "age_cat70+")) %>%
  
  # remove "intercept" row from plot
  filter(term != "(Intercept)") %>% 
  
  ## plot with variable on the y axis and estimate (OR) on the x axis
  ggplot(aes(x = estimate, y = term)) +
  
  ## show the estimate as a point
  geom_point() + 
  
  ## add in an error bar for the confidence intervals
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) + 
  
  ## show where OR = 1 is for reference as a dashed line
  geom_vline(xintercept = 1, linetype = "dashed")


pacman::p_load(easystats)
## Installing package into 'C:/Users/Neale/OneDrive - Neale Batra/Documents/R/win-library/4.1'
## (as 'lib' is unspecified)
## Warning: package 'easystats' is not available for this version of R
## 
## A version of this package for your version of R might be available elsewhere,
## see the ideas at
## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages


final_mv_reg %>% 
  model_parameters(exponentiate = TRUE) %>% 
  plot()

#27survival analysis----------------------------------------------------------------------
library(survival)
#create a new data called linelist_surv from the linelist_case_data

linelist_case_data <- rio::import("linelist_cleaned.rds")

linelist_surv <-  linelist_case_data %>% 
  
  dplyr::filter(
    # remove observations with wrong or missing dates of onset or date of outcome
    date_outcome > date_onset) %>% 
  
  dplyr::mutate(
    # create the event var which is 1 if the patient died and 0 if he was right censored
    event = ifelse(is.na(outcome) | outcome == "Recover", 0, 1), 
    
    # create the var on the follow-up time in days
    futime = as.double(date_outcome - date_onset), 
    
    # create a new age category variable with only 3 strata levels
    age_cat_small = dplyr::case_when( 
      age_years < 5  ~ "0-4",
      age_years >= 5 & age_years < 20 ~ "5-19",
      age_years >= 20   ~ "20+"),
    
    # previous step created age_cat_small var as character.
    # now convert it to factor and specify the levels.
    # Note that the NA values remain NA's and are not put in a level "unknown" for example,
    # since in the next analyses they have to be removed.
    age_cat_small = fct_relevel(age_cat_small, "0-4", "5-19", "20+")
  )

summary(linelist_surv$futime)

# cross tabulate the new event var and the outcome var from which it was created
# to make sure the code did what it was intended to
linelist_surv %>% 
  janitor::tabyl(outcome, event)

linelist_surv %>% 
  janitor::tabyl(age_cat_small, age_cat)

linelist_surv %>% 
  select(case_id, age_cat_small, date_onset, date_outcome, outcome, event, futime) %>% 
  head(10)

linelist_surv %>% 
  tabyl(gender, age_cat_small, show_na = F) %>% 
  adorn_totals(where = "both") %>% 
  adorn_percentages() %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front")


# Use Suv() syntax for right-censored data
survobj <- Surv(time = linelist_surv$futime,
                event = linelist_surv$event)

linelist_surv %>% 
  select(case_id, date_onset, date_outcome, futime, outcome, event) %>% 
  head(10)

#print the 50 first elements of the vector to see how it presents
head(survobj, 10)

# fit the KM estimates using a formula where the Surv object "survobj" is the response variable.
# "~ 1" signifies that we run the model for the overall survival  
linelistsurv_fit <-  survival::survfit(survobj ~ 1)

#print its summary for more details
summary(linelistsurv_fit)

#print its summary at specific times
summary(linelistsurv_fit, times = c(5,10,20,30,60))

# print linelistsurv_fit object with mean survival time and its se. 
print(linelistsurv_fit, print.rmean = TRUE)

str(linelistsurv_fit)

plot(linelistsurv_fit, 
     xlab = "Days of follow-up",    # x-axis label
     ylab="Survival Probability",   # y-axis label
     main= "Overall survival curve" # figure title
)

lines(
  linelistsurv_fit,
  lty = 3,             # use different line type for clarity
  fun = "event",       # draw the cumulative events instead of the survival 
  mark.time = FALSE,
  conf.int = FALSE
)

legend(
  "topright",                               # position of legend
  legend = c("Survival", "Cum. Mortality"), # legend text 
  lty = c(1, 3),                            # line types to use in the legend
  cex = .85,                                # parametes that defines size of legend text
  bty = "n"                                 # no box type to be drawn for the legend
)

# create the new survfit object based on gender
linelistsurv_fit_sex <-  survfit(Surv(futime, event) ~ gender, data = linelist_surv)

# set colors
col_sex <- c("lightgreen", "darkgreen")

# create plot
plot(
  linelistsurv_fit_sex,
  col = col_sex,
  xlab = "Days of follow-up",
  ylab = "Survival Probability")

# add legend
legend(
  "topright",
  legend = c("Female","Male"),
  col = col_sex,
  lty = 1,
  cex = .9,
  bty = "n")

survminer::ggsurvplot(
  linelistsurv_fit_sex, 
  data = linelist_surv,          # again specify the data used to fit linelistsurv_fit_sex 
  conf.int = FALSE,              # do not show confidence interval of KM estimates
  surv.scale = "percent",        # present probabilities in the y axis in %
  break.time.by = 10,            # present the time axis with an increment of 10 days
  xlab = "Follow-up days",
  ylab = "Survival Probability",
  pval = T,                      # print p-value of Log-rank test 
  pval.coord = c(40,.91),        # print p-value at these plot coordinates
  risk.table = T,                # print the risk table at bottom 
  legend.title = "Gender",       # legend characteristics
  legend.labs = c("Female","Male"),
  font.legend = 10, 
  palette = "Dark2",             # specify color palette 
  surv.median.line = "hv",       # draw horizontal and vertical lines to the median survivals
  ggtheme = theme_light()        # simplify plot background
)

linelistsurv_fit_source <-  survfit(
  Surv(futime, event) ~ source,
  data = linelist_surv
)

# plot
survminer::ggsurvplot( 
  linelistsurv_fit_source,
  data = linelist_surv,
  size = 1, linetype = "strata",   # line types
  conf.int = T,
  surv.scale = "percent",  
  break.time.by = 10, 
  xlab = "Follow-up days",
  ylab= "Survival Probability",
  pval = T,
  pval.coord = c(40,.91),
  risk.table = T,
  legend.title = "Source of \ninfection",
  legend.labs = c("Funeral", "Other"),
  font.legend = 10,
  palette = c("#E7B800","#3E606F"),
  surv.median.line = "hv", 
  ggtheme = theme_light()
)

#fitting the cox model
linelistsurv_cox_sexage <-  survival::coxph(
  Surv(futime, event) ~ gender + age_cat_small, 
  data = linelist_surv
)


#printing the model fitted
linelistsurv_cox_sexage

#summary of the model
summary(linelistsurv_cox_sexage)


test_ph_sexage <- survival::cox.zph(linelistsurv_cox_sexage)
test_ph_sexage

#fit the model
linelistsurv_cox <-  coxph(
  Surv(futime, event) ~ gender + age_years+ source + days_onset_hosp,
  data = linelist_surv
)


#test the proportional hazard model
linelistsurv_ph_test <- cox.zph(linelistsurv_cox)
linelistsurv_ph_test

#test proportional hazard assumption by graph 
survminer::ggcoxzph(linelistsurv_ph_test)

#print the summary of the model
summary(linelistsurv_cox)

linelist_case_data %>% 
  tabyl(days_onset_hosp, outcome) %>% 
  adorn_percentages() %>%  
  adorn_pct_formatting()

#forest plot
survminer::ggforest(linelistsurv_cox, data = linelist_surv)

#time dependent covariates

data(BMT, package = "SemiCompRisks")

bmt <- tibble::rowid_to_column(BMT, "my_id")

td_dat <- 
  tmerge(
    data1 = bmt %>% dplyr::select(my_id, T1, delta1), 
    data2 = bmt %>% dplyr::select(my_id, T1, delta1, TA, deltaA), 
    id = my_id, 
    death = event(T1, delta1),
    agvhd = tdc(TA)
  )

bmt %>% 
  dplyr::select(my_id, T1, delta1, TA, deltaA) %>% 
  filter(my_id %in% seq(1, 5))

td_dat %>% 
  filter(my_id %in% seq(1, 5))

bmt_td_model = coxph(
  Surv(time = tstart, time2 = tstop, event = death) ~ agvhd, 
  data = td_dat
)

summary(bmt_td_model)

survminer::ggforest(bmt_td_model, data = td_dat)

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


#24 modeling ------------------------------------------------------------------------------------------------
pacman::p_load(
  rio,          # File import
  here,         # File locator
  tidyverse,    # Data management + ggplot2 graphics
  epicontacts,  # Analysing transmission networks
  EpiNow2,      # Rt estimation
  EpiEstim,     # Rt estimation
  projections,  # Incidence projections
  incidence2,   # Handling incidence data
  epitrix,      # Useful epi functions
  distcrete     # Discrete delay distributions
)

# import the cleaned linelist
linelist <- import("linelist_cleaned.rds")