#Show distribution of Climate Stress Indicators by Households
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","exactextractr")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read Data ---------------------------------------------------------------
hh_temp_variables <- readRDS(
        file = paste(here(),"Data","Temperature","ERA5",
                     "Panel","hh_temp_variables.Rds",sep = "/"))

####Shockdays Distribution####
#Showing changes in Temp/Shocks at Household level
hh_shockdays_plot <- hh_temp_variables |>
  mutate(
    summer_change = temp_shockdays_summer_20202024 - temp_shockdays_summer_20152019,
    winter_change = temp_shockdays_winter_20202024 - temp_shockdays_winter_20152019
  ) |>
  tidyr::pivot_longer(
    cols = c(summer_change, winter_change),
    names_to = "season",
    values_to = "change"
  ) |>
  mutate(season = if_else(season == "summer_change", "Summer", "Winter"))

#box + violin
ggplot(hh_shockdays_plot, aes(x = season, y = change)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Change in Shock Days (2020–24 vs. 2015–19)",
    x = "",
    y = "Change in shock days"
  ) +
  theme_minimal(base_size = 14)

#density curves
ggplot(hh_shockdays_plot, aes(x = change, fill = season)) +
  geom_density(alpha = 0.4) +
  geom_vline(aes(xintercept = median(change), color = season), linewidth = 1) +
  labs(
    title = "Distribution of Change in Shock Days",
    x = "Change in shock days",
    y = "Density",
    fill = "Season",
    color = "Season"
  ) +
  theme_minimal(base_size = 14)
