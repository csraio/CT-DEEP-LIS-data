#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(readr)

mycsv <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/csv/LIS_DEEP_full.csv"
png_dir <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/analysis/plots/time-avg-depth-prof-no-outliers"

df <- read_csv(mycsv, show_col_types=FALSE)

df <- df %>% mutate(ActivityStartDate = as.POSIXct(ActivityStartDate))

avg_ovr_time <- df %>%
    group_by(CharacteristicName, `ActivityDepthHeightMeasure/MeasureValue`, `ResultMeasure/MeasureUnitCode`) %>%
    summarize(mean_value = mean(ResultMeasureValue, na.rm=TRUE, .groups="drop")) %>% ungroup()

plot_depth_profile <- function(var_name) {
    sub <- avg_ovr_time %>% dplyr::filter(CharacteristicName == var_name)
    #Getting rid of outliers before plotting
    Q1 <- quantile(sub$mean_value, 0.25, na.rm=TRUE)
    Q3 <- quantile(sub$mean_value, 0.75, na.rm=TRUE)
    IQR_val <- IQR(sub$mean_value, na.rm=TRUE)

    # Filter out points beyond 1.5 * IQR
    no_outliers <- sub %>%
        dplyr::filter(
        !is.na(mean_value),
        mean_value >= (Q1 - 1.5 * IQR_val),
        mean_value <= (Q3 + 1.5 * IQR_val)
  )

    p <- ggplot (no_outliers,
                 aes(
                     x = mean_value,
                     y = `ActivityDepthHeightMeasure/MeasureValue`
                     )
                 ) +
        geom_point() +
        #geom_smooth(method = "loess", se=FALSE, color="blue") +
        theme_bw(base_size=14) +
        theme(
              plot.title = element_text(face = "bold"),
              panel.grid.minor = element_blank()
              ) +
        scale_y_reverse() +
        labs(
             title = paste("Time-Averaged Depth Profile 2008-2024:", var_name),
             x = paste("Mean", var_name,"(", no_outliers$`ResultMeasure/MeasureUnitCode`, ")"),
             y = "Depth (m)"
             )
    outfile <- file.path(png_dir, paste0(gsub("[^A-Za-z0-9]+", "_", var_name), ".pdf"))
    ggsave(outfile, p, width = 8, height = 4, dpi = 200)

    return(p)
}
chars <- unique(df$CharacteristicName)
for (i in chars) {
  message("Plotting: ", i)
  plot_depth_profile(i)
}

