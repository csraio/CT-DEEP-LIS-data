#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)

mycsv <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/csv/LIS_DEEP_full.csv"
png_dir <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/analysis/plots/Eh-time-series"

df <- read_csv(mycsv, show_col_types=FALSE)

df <- df %>% mutate(ActivityStartDate = as.POSIXct(ActivityStartDate))

## Surface water (s.w) and bottom water (b.w) subsets
sw <- df %>% subset(`ActivityDepthHeightMeasure/MeasureValue` <= 2.05)
bw <- df %>% subset(`ActivityDepthHeightMeasure/MeasureValue` >= 39 & `ActivityDepthHeightMeasure/MeasureValue` <= 45)

#Calculate one Eh for one [O2] and ph
Eh <- function(DO, pH) {
    H <- 10^(-pH)
    E <- 1.23 - (0.0592/4)*log10(1/((DO/32000)*H^4))
    return(E)
}

#Construct a pivot table where DO and pH are column headers with values from ResultMeasureValue
# with respect to time.
pivot_subset <- function(df) {
  ret <- df %>%
    select(ActivityStartDate, CharacteristicName, ResultMeasureValue) %>%
    filter(CharacteristicName %in% c("Dissolved oxygen (DO)", "pH")) %>%

    pivot_wider(
      names_from = CharacteristicName,
      values_from = ResultMeasureValue,
      values_fn = list(ResultMeasureValue = mean) # Averages list cols wherein multiple measurements
                                                  # were taken on the same day
    ) %>%
    select(ActivityStartDate, `Dissolved oxygen (DO)`, pH) %>%
    filter(!is.na(`Dissolved oxygen (DO)`) & !is.na(pH)) %>%
    #Creates a new column called RedoxPotential
    mutate(RedoxPotential = Eh(`Dissolved oxygen (DO)`, pH))
    return(ret)
}
subset_sw <- pivot_subset(sw)
subset_bw <- pivot_subset(bw)

# Add a label column to distinguish the two subsets
subset_sw <- subset_sw %>% mutate(sw.bw = "Surface")
subset_bw <- subset_bw %>% mutate(sw.bw = "Bottom")

# Stack the subsets on top of each other
combined_df <- bind_rows(subset_sw, subset_bw) #%>%
               #filter(RedoxPotential > quantile(RedoxPotential, .25) - 1.5*IQR(RedoxPotential),
               #    RedoxPotential < quantile(RedoxPotential, .75) + 1.5*IQR(RedoxPotential))
# 1. Fit the model to get the expected values
fit <- lm(RedoxPotential ~ sin(2 * pi * as.numeric(ActivityStartDate) / 365.25) +
            cos(2 * pi * as.numeric(ActivityStartDate) / 365.25), data = combined_df)

# 2. Add residuals to the dataframe
#fcombined_df <- fcombined_df %>%
  #mutate(res = resid(fit)) %>%

# 3. Filter based on the residuals (distance from the curve)
  #filter(
  #  res > (quantile(res, .25, na.rm = TRUE) - 1.5 * IQR(res, na.rm = TRUE)),
  #  res < (quantile(res, .75, na.rm = TRUE) + 1.5 * IQR(res, na.rm = TRUE))
  #)

plot_Eh <- function(df) {
  p <- ggplot(df, aes(x = ActivityStartDate, y = RedoxPotential, color = sw.bw)) +
    geom_point(size = 0.8, alpha = 0.5) +
    scale_color_manual(
      name = "Water Column Zone",
      values = c("Surface" = "#A66B50", "Bottom" = "#4682B4") # Using names from sw.bw
    ) +
    theme_classic()+

    labs(
      title = "Redox potential from 2010-2024",
      x = "Date",
      y = "Eh (V)",
      color = "Water Column Zone"
    ) +
    scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  # ADD GRIDLINES HERE
    theme(
      #panel.grid.major = element_line(color = "grey90", linewidth = 0.5), # Standard grid
      #panel.grid.minor = element_line(color = "grey95", linewidth = 0.25), # Finer marks
      panel.background = element_rect(fill = "white", color = NA), # Solid white background
      panel.grid.major = element_line(color = "black", linewidth = 0.2), # Sharp black major grid
      panel.grid.minor = element_line(color = "grey80", linewidth = 0.1) # Fainter grey for sub-marks
    )
    print(p)
  # Output the plot to PDF image
  outfile_pdf <- file.path(png_dir, "RedoxPotential.pdf")
  ggsave(outfile_pdf, p, width = 8, height = 4)}
