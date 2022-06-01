library(pacman)
p_load(here)

linelist<- rio::import("linelist_cleaned.rds")

#shortcuts
#Ctrl + Shift + m    Chèn %>%
#Tab/enter           T??? d???ng di???n
#Ctrl + L	          D???n d???p R console

pacman::p_load_gh("reconhub/epicontacts@timeline") # install from Github

# Create an age pyramid
age_pyramid(data = linelist, age_group = "age_cat5", split_by = "gender")

age_pyramid(
  linelist,                    # use case linelist
  "age_cat5",                  # age group column
  "gender",                    # split by gender
  proportional = TRUE,         # percents instead of counts
  pal = c("orange", "purple")  # colors
)

#directly imput missing data using indexing 
my_vector <- c("a", "b", "c", "d", "e", "f")  # define the vector
my_vector[5]                                  # print the 5th element
my_vector[5]<-9
my_vector
#[1] "a" "b" "c" "d" "9" "f"


# View a specific row (2) from dataset, with all columns (don't forget the comma!)
linelist[2,]

# View all rows, but just one column
linelist[, "date_onset"]

# View values from row 2 and columns 5 through 10 and 18
linelist[2, c(5:10, 18)] 

# View rows 2 through 20, and specific columns
linelist[2:20, c("date_onset", "outcome", "age")]

# View rows and columns based on criteria
# *** Note the dataframe must still be named in the criteria!
linelist[linelist$age > 25 , c("date_onset", "outcome", "age")]

# Use View() to see the outputs in the RStudio Viewer pane (easier to read) 
# *** Note the capital "V" in View() function
View(linelist[2:20, "date_onset"])

# Save as a new object
new_table <- linelist[2:20, c("date_onset")] 

# Using dplyr to view, filter, count, ...
#View rows 2 through 20, and three specific columns (note no quotes necessary on column names)
linelist %>% filter(row_number() %in% 2:20) %>% select(date_onset, outcome, age)

age_cat_50 <- linelist %>%
  filter(age >= 50) %>% 
  count(age_cat=="50-69")

linelist_summary <- linelist %>% 
  count(age_cat)

# assign using pipe
linelist <- linelist %>% 
  mutate(age_months = age_years * 12)

linelist %<>% mutate(age_months_1 = age_years * 12)


# remove objects
rm(object_name)
# remove all objects
rm(list = ls(all = TRUE))

# creat new var using dplyr case_when
linelist_cleaned <- linelist %>%
  mutate(case_def = case_when(
    is.na(rdt_result) & is.na(other_case_in_home)            ~ NA_character_,
    rdt_result == "Positive"                                 ~ "Confirmed",
    rdt_result != "Positive" & other_cases_in_home == "Yes"  ~ "Probable",
    TRUE  # not meet the above criteria                                                   ~ "Suspected"
  ))

# Use janitor::round_half_up(c(2.5, 3.5)) instead of round()


