#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(readr)

mycsv <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/csv/LIS_DEEP_full.csv"
#png_dir <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/analysis/plots"

df <- read_csv(mycsv, show_col_types=FALSE)

df <- df %>% mutate(ActivityStartDate = as.POSIXct(ActivityStartDate))

which.row.first <- function(varname, col, d){
# Sort first
sorted <- d %>% arrange(col)

# Find the indices of Alkalinity in the NEW sorted order
which_indices <- which(sorted$CharacteristicName == varname)

print(which_indices[1])

# Look at the first one (using the first number in the vector)
sorted[which_indices[1], which(names(d)==col)] %>% print()
}
