

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


