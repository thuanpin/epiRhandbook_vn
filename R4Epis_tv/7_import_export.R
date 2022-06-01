library(pacman)
p_load(here)
here()
library(rio)

linelist <- import(here("R4Epis" , "all data", "linelist_cleaned.xlsx"))

linelist <- import("linelist_cleaned.csv") # need to provide working directory

my_data <- import("my_excel_file.xlsx", which = "Sheetname")

#import and convert types of imputed missing data7
linelist <- import(here("data", "my_linelist.xlsx"), na = c("Missing", "", " ", "99"))

#remove some rows when importing
linelist_raw <- import("linelist_raw.xlsx", skip = 1)  # does not import header


export(linelist, "my_linelist.xlsx") # will save to working directory
export(linelist, here("data", "clean", "my_linelist.csv"))
export(linelist, here("data", "clean", "my_linelist.rds"))
