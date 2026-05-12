#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(readr)

mycsv <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/csv/LIS_DEEP_full.csv"
png_directory <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/analysis/plots/surface-waters/trendline"

df <- read_csv(mycsv, show_col_types=FALSE)

df <- df %>% mutate(ActivityStartDate = as.POSIXct(ActivityStartDate))
df <- df %>% subset(`ActivityDepthHeightMeasure/MeasureValue` <= 2.10)

plot_one_var <- function(var_name) {
  subset <- df %>% filter(CharacteristicName == var_name)

  p <- ggplot(subset, aes(x = ActivityStartDate, y = ResultMeasureValue)) +
    geom_point(size = 0.8, alpha = 0.7) +
    geom_smooth(method = "lm",        # Linear model
              se = FALSE,             # Rm ribbon std error
              color = "red")+
    labs(
      title = paste(var_name, "from 2008-2024"),
      x = "Date",
      y = paste(var_name,"(", subset[["ResultMeasure/MeasureUnitCode"]], ")")
    ) +
    theme_minimal()

  outfile <- file.path(png_directory, paste0(gsub("[^A-Za-z0-9]+", "_", var_name), ".pdf"))
  ggsave(outfile, p, width = 8, height = 4, dpi = 200)
}

# ---- APPLY TO ALL CHARACTERISTICS ----
chars <- unique(df$CharacteristicName)

for (char in chars) {
  message("Plotting: ", char)
  plot_one_var(char)
}
