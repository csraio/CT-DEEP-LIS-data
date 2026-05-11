#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library(gsw)

mycsv <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/csv/LIS_DEEP_full.csv"
png_dir <- "/Users/calderraio/Documents/Lab-documents/4-senior/3-spring-2026/env-chem/final-project/analysis/plots/DIC-time-series"

df <- read_csv(mycsv, show_col_types=FALSE)

df <- df %>% mutate(ActivityStartDate = as.POSIXct(ActivityStartDate))

# Formula to calculate the DIC aka C_total based on pH and [Alk]
calc_dic <- function(pH, alk) {
    H <- 10^(-pH)
    OH <- 1/(H*(10^14))
    Alk <- alk*(10^(-6)) # convert umol to mol
    Ka1 <- 10^(-6.3)
    Ka2 <- 10^(-10.3)
    E <- (H^2)+ Ka1*(H)+ Ka1*Ka2
    a1 <- (Ka1*H)/E
    a2 <- (Ka1*Ka2)/E
    dic <- ((Alk - OH + H)/(a1 + 2*a2))*(10^6) # convert back to umol/kg
    return(dic)
}

calc_mu <- function(salinity) {
    mu <- salinity*(2.5e-5)
}


# 1. Calculate Density (rho) using the GSW Pakcage (Gibbs SeaWater Oceanographic Toolbox)
rho_kg_m3 <- gsw_rho(SA = pivot_table$Salinity, CT = pivot_table$Temperature, p = pivot_table$Pressure*0.689476) # convert psi to decibar
rho_g_cm3 <- rho_kg_m3/1000

# 2. Calculate Ionic Strength (mu) using the Millero Equation
# The linear TDS approximation (2e-5 * TDS) is not good enough for seawater
mu <- (19.92*pivot_table$Salinity)/(1000 - 1.005*pivot_table$Salinity)*rho_g_cm3

# 3. Calculate the 'A' parameter for the Davies Equation
# Using the Malmberg & Maryott (1956) dielectric constant fit https://nvlpubs.nist.gov/nistpubs/jres/56/jresv56n1p1_a1b.pdf
temp_c <- pivot_table$Temperature
epsilon <- 87.740 - 0.40008*temp_c + 9.398e-4*temp_c^2 - 1.410e-6*temp_c^3
A <- (1.8248e6*sqrt(rho_g_cm3))/(epsilon*(temp_c + 273.15))^1.5

# 4. Calculate the Activity Coefficient (gamma) using THE DAVIES EQUATION
gamma<- function(z){
log10_gamma <- -A* (z^2) * (sqrt(mu)/(1 + sqrt(mu)) - 0.3*mu)
gamma <- 10^log10_gamma
return(gamma)}

# Because ionic strenght is ~0.7095777M on average (2.5e-5 * TDS), this is too high to use the debeye-huckel, so we must use the Davies equation to calculate activity.
calc_dic_nonideal <- function(pH, alk) {
    H <- gamma(1)*10^(-pH)
    OH <- 0.98/(H*(10^14)) # Activity of water is 0.98 in seawater
    Alk <- alk*(10^(-6)) # convert umol to mol
    Ka1.star <- (10^(-6.3))/(gamma(1)*gamma(1)) # Ka1.star = Ka1 divided by γ_H+ * γ_HCO3-
    Ka2.star <- (10^(-10.3))/(gamma(1)*gamma(2)) # Ka1.star = Ka2 divided by γ_H+ * γ_CO3-2
    E <- (H^2)+ Ka1.star*(H)+ Ka1.star*Ka2.star
    a1 <- (Ka1.star*H)/E
    a2 <- (Ka1.star*Ka2.star)/E
    dic <- ((Alk - OH + H)/(a1 + 2*a2))*(10^6) # convert back to umol/kg
    return(dic)
}

pivot_table <- df %>%
    # Only pH has non-NA depth values. We only want to look at surface water, so exclude all pH readings deeper than 2.05m:
    filter(!(CharacteristicName == "pH" & `ActivityDepthHeightMeasure/MeasureValue` > 2.05))%>%
    select(ActivityStartDate, CharacteristicName, ResultMeasureValue)%>%
    filter(CharacteristicName %in% c("pH", "Alkalinity", "Inorganic carbon", "Salinity", "Temperature", "Pressure"))%>%
    pivot_wider(
        names_from = CharacteristicName,
        values_from = ResultMeasureValue,
        values_fn = list(ResultMeasureValue = mean) #Averages list cols wherein multiple measurements were taken on the same day
    )%>%
    #Create a new column called "DIC" (dissolved inorganic carbon)
    mutate(DIC = calc_dic(pH, Alkalinity))%>%
    mutate(DIC_nonideal = calc_dic_nonideal(pH, Alkalinity))

plot_DIC <- function(d) {
# 1. Reshape the data so DIC and Inorganic carbon are both in the same column
d_long <- d %>%
  pivot_longer(cols = c(DIC, DIC_nonideal, `Inorganic carbon`),
               names_to = "Series",
               values_to = "Values")%>%
  filter(!is.na(Values))
# 2. Plot with Date-specific formatting
p <- ggplot(d_long, aes(x = ActivityStartDate, y = Values, color = Series)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 0.8, alpha = 0.5) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal() +
  scale_color_manual(
    name = "Data Origin", # Changes the title of the legend
    values = c("red", "blue", "dark green"), # You have to specify colors here
    labels = c("DIC" = "Calculated DIC Ideal System", "Inorganic carbon" = "Measured DIC", "DIC_nonideal" ="Calculated DIC Non-ideal")
  ) +
  labs(title = "Calculated DIC and Measured DIC versus time",
       x = "Date (2022-2023)",
       y = "Dissolved Inorganic Carbon (umol/kg)")

  outfile_pdf <- file.path(png_dir, "DIC-calc-vs-measured-over-time-nonideal.pdf")
  ggsave(outfile_pdf, p, width = 8, height = 4)}
