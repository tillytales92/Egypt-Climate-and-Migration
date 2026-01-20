#Mapping Time series: Temperature data
#Measures inspired by: https://data360.worldbank.org/en/dataset/WB_CCKP
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "lubridate","patchwork","zoo")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read Data ---------------------------------------------------------------
#Load temp. data for Alexandria, Cairo, Assiut and Sohag
era5_temp_df <- readRDS(file = paste(here(),"Data",
                                     "intermediate","Governorate Data",
                                     "era5_temp_19602024",sep = "/"))

####Create annual data####
era5_temp_annual <-  era5_temp_df |>
  group_by(name, year) |>
  summarise(
    annual_mean_temp       = mean(mean_temp, na.rm = TRUE),
    annual_mean_temp_pop   = mean(mean_temp_pop_weighted, na.rm = TRUE),
    annual_mean_maxtemp = mean(mean_maxtemp,na.rm = TRUE),
    annual_mean_maxtemp_pop = mean(mean_maxtemp_pop_weighted,na.rm = TRUE),
    annual_mean_mintemp = mean(mean_mintemp,na.rm = TRUE),
    annual_mean_mintemp_pop = mean(mean_mintemp_pop_weighted,na.rm = TRUE),
    .groups = "drop")

#create deviations from mean (1960-2010) measure
#plot annual mean unweighted and annual mean pop. weighted
mean_19602010 <- era5_temp_annual |>
  filter(year <= 2010) |>
  group_by(name) |>
  summarise(mean_temp19602010_pop = mean(annual_mean_temp_pop , na.rm = TRUE),
            sd_temp19602010_pop   = sd(annual_mean_temp_pop, na.rm = TRUE),
            mean_maxtemp19602010_pop = mean(annual_mean_maxtemp_pop , na.rm = TRUE),
            sd_maxtemp19602010_pop   = sd(annual_mean_maxtemp_pop, na.rm = TRUE),
            mean_mintemp19602010_pop = mean(annual_mean_mintemp_pop , na.rm = TRUE),
            sd_mintemp19602010_pop   = sd(annual_mean_mintemp_pop, na.rm = TRUE),
            .groups = "drop")

#join and take deciles
era5_temp_annual <- era5_temp_annual |>
  left_join(mean_19602010, by = "name") |>
  group_by(name) |>  # group by governorate
  mutate(
    # mean temperature
    temp_diff_mean       = annual_mean_temp_pop - mean_temp19602010_pop,
    temp_decile_mean     = ntile(annual_mean_temp_pop, 10),
    z_score_meantemp     = (annual_mean_temp_pop - mean_temp19602010_pop) / sd_temp19602010_pop,
    shock_meantemp       = case_when(
      z_score_meantemp >  2  ~  1,
      z_score_meantemp < -2  ~ -1,
      TRUE                    ~ 0
    ),
    # max temperature
    temp_diff_maxtemp    = annual_mean_maxtemp_pop - mean_maxtemp19602010_pop,
    z_score_maxtemp      = (annual_mean_maxtemp_pop - mean_maxtemp19602010_pop) / sd_maxtemp19602010_pop,
    shock_maxtemp        = case_when(
      z_score_maxtemp >  2  ~  1,
      z_score_maxtemp < -2  ~ -1,
      TRUE                    ~ 0
    ),
    # min temperature
    temp_diff_mintemp    = annual_mean_mintemp_pop - mean_mintemp19602010_pop,
    z_score_mintemp      = (annual_mean_mintemp_pop - mean_mintemp19602010_pop) / sd_mintemp19602010_pop,
    shock_mintemp        = case_when(
      z_score_mintemp >  2  ~  1,
      z_score_mintemp < -2  ~ -1,
      TRUE                    ~ 0
    )
  ) |>
  ungroup()

####Create seasonal data####
# 1. Add a "season" variable to the monthly ERA5 dataset
era5_temp_seasonal <- era5_temp_df |>
  mutate(
    # define seasons
    season = case_when(
      month %in% c(11, 12, 1, 2, 3, 4,5) ~ "Winter",# Nov–May (can adjust to Nov–May if needed)
      month %in% c(5, 6, 7, 8, 9, 10)  ~ "Summer"),    # May–Oct
    # handle November–December: assign them to *next year's* winter season
    season_year = if_else(month %in% c(11, 12), year + 1, year)
  )

# 2. Aggregate by governorate, season, and season_year
era5_temp_seasonal <- era5_temp_seasonal |>
  group_by(name, season_year, season) |>
  summarise(
    seasonal_mean_temp       = mean(mean_temp, na.rm = TRUE),
    seasonal_mean_temp_pop   = mean(mean_temp_pop_weighted, na.rm = TRUE),
    seasonal_mean_temp_crop  = mean(mean_temp_cropland_weighted, na.rm = TRUE),
    seasonal_mean_maxtemp    = mean(mean_maxtemp, na.rm = TRUE),
    seasonal_mean_maxtemp_pop= mean(mean_maxtemp_pop_weighted, na.rm = TRUE),
    seasonal_mean_maxtemp_crop  = mean(mean_maxtemp_cropland_weighted, na.rm = TRUE),
    seasonal_mean_mintemp    = mean(mean_mintemp, na.rm = TRUE),
    seasonal_mean_mintemp_pop= mean(mean_mintemp_pop_weighted, na.rm = TRUE),
    seasonal_mean_mintemp_crop  = mean(mean_mintemp_cropland_weighted, na.rm = TRUE),
    .groups = "drop"
  )

# 1. Compute 1970–2010 seasonal baseline (per governorate & season)
mean_19702010_seasonal <- era5_temp_seasonal |>
  filter(season_year >= 1960, season_year <= 2010) |>
  group_by(name, season) |>
  summarise(
    mean_temp19702010_crop     = mean(seasonal_mean_temp_crop , na.rm = TRUE),
    sd_temp19702010_crop       = sd(seasonal_mean_temp_crop, na.rm = TRUE),
    mean_maxtemp19702010_crop  = mean(seasonal_mean_maxtemp_crop , na.rm = TRUE),
    sd_maxtemp19702010_crop    = sd(seasonal_mean_maxtemp_crop, na.rm = TRUE),
    mean_mintemp19702010_crop  = mean(seasonal_mean_mintemp_crop , na.rm = TRUE),
    sd_mintemp19702010_crop    = sd(seasonal_mean_mintemp_crop, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Join baselines and compute deviations/z-scores/shocks
era5_temp_seasonal <- era5_temp_seasonal |>
  left_join(mean_19702010_seasonal, by = c("name", "season")) |>
  group_by(name, season) |>
  mutate(
    # mean temperature deviation
    temp_diff_mean   = seasonal_mean_temp_crop - mean_temp19702010_crop,
    z_score_meantemp = (seasonal_mean_temp_crop - mean_temp19702010_crop) / sd_temp19702010_crop,
    shock_meantemp   = case_when(
      z_score_meantemp >  2  ~  1,
      z_score_meantemp < -2  ~ -1,
      TRUE                   ~  0
    ),

    # max temperature deviation
    temp_diff_maxtemp   = seasonal_mean_maxtemp_crop - mean_maxtemp19702010_crop,
    z_score_maxtemp     = (seasonal_mean_maxtemp_crop - mean_maxtemp19702010_crop) / sd_maxtemp19702010_crop,
    shock_maxtemp       = case_when(
      z_score_maxtemp >  2  ~  1,
      z_score_maxtemp < -2  ~ -1,
      TRUE                   ~  0
    ),

    # min temperature deviation
    temp_diff_mintemp   = seasonal_mean_mintemp_crop - mean_mintemp19702010_crop,
    z_score_mintemp     = (seasonal_mean_mintemp_crop - mean_mintemp19702010_crop) / sd_mintemp19702010_crop,
    shock_mintemp       = case_when(
      z_score_mintemp >  2  ~  1,
      z_score_mintemp < -2  ~ -1,
      TRUE                   ~  0
    )
  ) |>
  ungroup()



####Create monthly data####
era5_temp_monthly <-  era5_temp_df |>
  group_by(name, year,month) |>
  summarise(
    monthly_mean_temp       = mean(mean_temp, na.rm = TRUE),
    monthly_mean_temp_pop   = mean(mean_temp_pop_weighted, na.rm = TRUE),
    monthly_mean_maxtemp = mean(mean_maxtemp,na.rm = TRUE),
    monthly_mean_maxtemp_pop = mean(mean_maxtemp_pop_weighted,na.rm = TRUE),
    monthly_mean_mintemp = mean(mean_mintemp,na.rm = TRUE),
    monthly_mean_mintemp_pop = mean(mean_mintemp_pop_weighted,na.rm = TRUE),
    .groups = "drop"
  )

# Plotting - Time Series ----------------------------------------------------------------
####Annual Measures####
#####Average Mean temperature - Time Series#####
#plot annual mean unweighted and annual mean pop. weighted
era5_temp_annual |>
  ggplot(aes(x = year, y = annual_mean_temp_pop)) +
  # lines + points
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.6) +
  # smoother for trend
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  # facets by governorate
  facet_wrap(~name) +
  # axis: full range 1960–2024 with neat 5-year breaks
  scale_x_continuous(
    limits = c(1960, 2024),
    breaks = c(
      1960,
      seq(from = ceiling(1960/5)*5, to = floor(2024/5)*5, by = 5),
      2024
    )
  ) +
  # style
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual Mean Temperatures by Governorate",
    subtitle = "ERA5 data (1960–2024)",
    x = "Year",
    y = "Temperature (°C)",
    color = "Measure"
  ) +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(fill = "lightgray", color = NA),
    strip.text        = element_text(face = "bold"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

#####Average Mean temperature - Diff. from 1960 - 2010 Mean#####
#plot to show deviations from mean temp.
era5_temp_annual |>
  filter(name %in% c("Assiut","Suhag")) |>
  ggplot(aes(x = year, y = temp_diff_mean, fill = temp_diff_mean)) +
  geom_col(color = "black",width = 0.9) +  # bars almost touching
  facet_wrap(~name,nrow = 2) +
  scale_x_continuous(
    limits = c(1960, 2025),  # give extra space for last bar
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  )+
  scale_y_continuous(
    limits = c(-1.5,2.5),
    breaks = seq(-1.5, 2.5, by = 0.5),
    expand = c(0, 0))+
  # continuous flashy diverging color scale
  scale_fill_gradient2(
    low = "#0571b0",
    mid = "white",
    high = "#ca0020",
    midpoint = 0,
    name = "Deviation (°C)",
    breaks = seq(-1.5, 2.5, by = 1),  # or adjust to your data range
    limits = c(-1.5, 2.5))+               # fixes the color scale symmetrically
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual Mean Temperature (Pop. weighted) Changes (1960–2024)",
    subtitle = "Relative to 1960-2010 average (°C) - ERA5 data ",
    x = "Year",
    y = "Temperature Deviation (°C)"
  ) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "lightgray", color = NA),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#####Average Minimum temperature - Time Series#####
#plot annual mean unweighted and annual mean pop. weighted
era5_temp_annual |>
  ggplot(aes(x = year, y = annual_mean_mintemp_pop)) +
  # lines + points
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.6) +
  # smoother for trend
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  # facets by governorate
  facet_wrap(~name) +
  # axis: full range 1960–2024 with neat 5-year breaks
  scale_x_continuous(
    limits = c(1961, 2024),
    breaks = c(
      1961,
      seq(from = ceiling(1961/5)*5, to = floor(2024/5)*5, by = 5),
      2024
    )
  ) +
  # style
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual Mean Daily Minimum Temperatures by Governorate",
    subtitle = "ERA5 data (1961–2024)",
    x = "Year",
    y = "Temperature (°C)",
    color = "Measure"
  ) +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(fill = "lightgray", color = NA),
    strip.text        = element_text(face = "bold"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#####Average Minimum temperature - Diff. from 1970 - 2000 Mean#####
#plot to show deviations from mean temp.
era5_temp_annual |>
  ggplot(aes(x = year, y = temp_diff_mintemp, fill = temp_diff_mintemp)) +
  geom_col(color = "black", width = 0.9) +  # bars almost touching
  facet_wrap(~name) +
  scale_x_continuous(
    limits = c(1960, 2025),  # give extra space for last bar
    breaks = seq(1960, 2024, by = 2),
    expand = c(0, 0)
  )+
  scale_y_continuous(
    limits = c(-1,2.5),
    breaks = seq(-1, 2.5, by = 0.5),
    expand = c(0, 0)
  )+
  # continuous flashy diverging color scale
  scale_fill_gradient2(
    low = "#0571b0",
    mid = "white",
    high = "#ca0020",
    midpoint = 0,
    name = "Deviation (°C)",
    breaks = seq(-1.5, 2.5, by = 1),  # or adjust to your data range
    limits = c(-1.5, 2.5))+               # fixes the color scale symmetrically
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual Minimum Temperature (Pop. weighted) Changes (1960–2024)",
    subtitle = "Relative to 1960-2010 average (°C) - ERA5 data ",
    x = "Year",
    y = "Temperature Deviation (°C)"
  ) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#####Average Maximum temperature - Time Series#####
#plot annual mean unweighted and annual mean pop. weighted
era5_temp_annual |>
  ggplot(aes(x = year, y = annual_mean_maxtemp_pop)) +
  # lines + points
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.6) +
  # smoother for trend
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  # facets by governorate
  facet_wrap(~name) +
  # axis: full range 1960–2024 with neat 5-year breaks
  scale_x_continuous(
    limits = c(1961, 2024),
    breaks = c(
      1961,
      seq(from = ceiling(1961/5)*5, to = floor(2024/5)*5, by = 5),
      2024
    )
  ) +
  # style
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual Mean Daily Maximum Temperatures by Governorate",
    subtitle = "ERA5 data (1961–2024)",
    x = "Year",
    y = "Temperature (°C)",
    color = "Measure"
  ) +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(fill = "lightgray", color = NA),
    strip.text        = element_text(face = "bold"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#####Average Maximum temperature - Diff. from 1970 - 2000 Mean#####
#plot to show deviations from mean temp.
era5_temp_annual |>
  ggplot(aes(x = year, y = temp_diff_maxtemp, fill = temp_diff_maxtemp)) +
  geom_col(color = "black", width = 0.9) +  # bars almost touching
  facet_wrap(~name) +
  scale_x_continuous(
    limits = c(1960, 2025),  # give extra space for last bar
    breaks = seq(1960, 2024, by = 2),
    expand = c(0, 0)
  )+
  scale_y_continuous(
    limits = c(-1,2.5),
    breaks = seq(-1, 2.5, by = 0.5),
    expand = c(0, 0)
  )+
  # continuous flashy diverging color scale
  scale_fill_gradient2(
    low = "#0571b0",
    mid = "white",
    high = "#ca0020",
    midpoint = 0,
    name = "Deviation (°C)",
    breaks = seq(-1.5, 2.5, by = 1),#or adjust to your data range
    limits = c(-1.5, 2.5))+#fixes the color scale symmetrically
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual Maximum Temperature (Pop. weighted) Changes (1960–2024)",
    subtitle = "Relative to 1960-2010 average (°C) - ERA5 data ",
    x = "Year",
    y = "Temperature Deviation (°C)"
  ) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

####Seasonal Measures####
#####Time Series#####
#plot annual mean unweighted and annual mean pop. weighted
era5_temp_seasonal |>
  ggplot(aes(x = season_year, y = seasonal_mean_temp_pop,colour = season)) +
  # lines + points
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.6) +
  # smoother for trend
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  # facets by governorate
  facet_wrap(~name) +
  # axis: full range 1960–2024 with neat 5-year breaks
  scale_x_continuous(
    limits = c(1960, 2024),
    breaks = c(
      1960,
      seq(from = ceiling(1960/5)*5, to = floor(2024/5)*5, by = 5),
      2024
    )
  ) +
  # style
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual Mean Temperatures by Governorate",
    subtitle = "ERA5 data (1960–2024)",
    x = "Year",
    y = "Temperature (°C)",
    color = "Measure"
  ) +
  theme(
    legend.position   = "bottom",
    strip.background  = element_rect(fill = "lightgray", color = NA),
    strip.text        = element_text(face = "bold"),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#####Average Mean Temp.#####
era5_temp_seasonal |>
  filter(name %in% c("Assiut","Suhag")) |>
  ggplot(aes(x = season_year, y = temp_diff_mean, fill = factor(shock_meantemp))) +
  geom_col(width = 0.9) +  # bars almost touching
  facet_wrap(name ~ season,nrow = 2) +
  scale_x_continuous(
    limits = c(1960, 2025),  # give extra space for last bar
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-2, 2.5),
    breaks = seq(-2, 2.5, by = 0.5),
    expand = c(0, 0)
  ) +
  # categorical color scale for shocks
  scale_fill_manual(
    name   = "Shock",
    values = c(
      "-1" = "#0571b0",  # cold shock (blue)
      "0"  = "grey80",   # no shock (neutral)
      "1"  = "#ca0020"   # heat shock (red)
    ),
    labels = c("-1" = "Cold shock", "0" = "No shock", "1" = "Heat shock")
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title    = "Annual Mean Temperature Changes (1960–2024) by Season",
    subtitle = "Relative to 1960-2010 average (°C) - ERA5 data.\nShock years +- 2 SD from 1960-2010 seasonal mean",
    x        = "Year",
    y        = "Temperature Deviation (°C)"
  ) +
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#####Average Max. Temp.#####




#####Average Min. Temp.#####



####Daily Measures####
####Avg. temp above 30 degrees ####
#days where weighted mean above 30 degrees
days_above30 <- era5_temp_df |>
  filter(name %in% c("Assiut","Suhag")) |>
  group_by(name, year) |>
  summarise(
    n_days_above30_unweighted = sum(mean_temp > 30, na.rm = TRUE),
    n_days_above30_cropland   = sum(mean_temp_cropland_weighted > 30, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("n_days"),
    names_to = "type",
    values_to = "n_days_above30"
  ) |>
  mutate(
    type = recode(type,
                  "n_days_above30_unweighted" = "Unweighted",
                  "n_days_above30_cropland"   = "Cropland-weighted"
    )
  )

#plot
days_above30 |>
  ggplot(aes(x = year, y = n_days_above30, color = type)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  facet_wrap(~name) +
  scale_x_continuous(
    limits = c(1960, 2025),  # consistent with other plots
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 150),  # consistent with other plots
    breaks = seq(0, 150, by = 50),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Number of Days with Average Temp. > 30°C by Governorate",
    subtitle = "Unweighted vs Cropland-weighted, 1960–2024",
    x = "Year",
    y = "Number of Days",
    color = "Measure"
  ) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

####Avg. temp above 32 degrees ####
#days where weighted mean above 32 degrees
days_above32 <- era5_temp_df |>
  filter(name %in% c("Assiut","Suhag")) |>
  group_by(name, year) |>
  summarise(
    n_days_above32_unweighted = sum(mean_temp > 32, na.rm = TRUE),
    n_days_above32_cropland   = sum(mean_temp_cropland_weighted > 32, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("n_days"),
    names_to = "type",
    values_to = "n_days_above32"
  ) |>
  mutate(
    type = recode(type,
                  "n_days_above32_unweighted" = "Unweighted",
                  "n_days_above32_cropland"   = "Cropland-weighted"
    )
  )

#plot
days_above32 |>
  filter(type == "Cropland-weighted") |>
  ggplot(aes(x = year, y = n_days_above32)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  facet_wrap(~name) +
  scale_x_continuous(
    limits = c(1960, 2025),
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 120),  # adjust max according to your data
    breaks = seq(0, 120, by = 20),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Number of Days with Average Temp. > 32°C by Governorate",
    subtitle = "Cropland-weighted average, 1960–2024 ERA5 Data",
    x = "Year",
    y = "Number of Days",
    color = "Measure"
  ) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

####Winter season max. temp above 30 degrees ####
#Winter Season (Nov. - May)
#days where weighted mean above 30 degrees --> damaging to wheat (main winter crop)
days_above30_maxtemp_winterseason <- era5_temp_df |>
  filter(name %in% c("Assiut","Suhag"),
         month %in% c(11,12,1,2,3,4)) |>
  group_by(name, year) |>
  summarise(
    n_days_above30max_unweighted = sum(mean_maxtemp > 30, na.rm = TRUE),
    n_days_above30max_cropland   = sum(mean_maxtemp_cropland_weighted > 30, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("n_days"),
    names_to = "type",
    values_to = "n_days_above30"
  ) |>
  mutate(
    type = recode(type,
                  "n_days_above30max_unweighted" = "Unweighted",
                  "n_days_above30max_cropland"   = "Cropland-weighted"
    )
  )

#plot 1960-2025
days_above30_maxtemp_winterseason |>
  filter(type == "Cropland-weighted") |>
  ggplot(aes(x = year, y = n_days_above30)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  facet_wrap(~name) +
  scale_x_continuous(
    limits = c(1960, 2025),  # consistent with other plots
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 70),  # consistent with other plots
    breaks = seq(0,70, by = 10),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Number of Winter Season Days with Max. Temp. > 30°C",
    subtitle = "Restricted to Cropland Cells - 1960–2024 (Nov. - May.) ERA5 Data\n Temp. above 30°C damaging to wheat and clover.",
    x = "Year",
    y = "Number of Days",
    color = "Measure"
  ) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


####Number of Tropical Nights (Tmin > 20° Celsius)####
tropicalnights_count <- era5_temp_df |>
  filter(name %in% c("Assiut","Suhag")) |>
  filter(year > 1960) |> #no data for 1960
  group_by(name, year) |>
  summarise(
    n_days_above20 = sum(mean_mintemp > 20, na.rm = TRUE),
    n_days_above20_popcells   = sum(mean_mintemp_pop_weighted > 20, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("n_days"),
    names_to = "type",
    values_to = "n_days_above20"
  ) |>
  mutate(
    type = recode(type,
                  "n_days_above20" = "Any cells",
                  "n_days_above20_popcells"   = "Populated cells"
    )
  )

#plot
tropicalnights_count |>
  ggplot(aes(x = year, y = n_days_above20, color = type)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_smooth(se = FALSE, linetype = "dashed", linewidth = 1) +
  facet_wrap(~name) +
  scale_x_continuous(
    limits = c(1960, 2025),
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 200),  # adjust based on your data
    breaks = seq(0, 200, by = 50),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Number of Tropical Nights (Tmin > 20°C) by Governorate",
    subtitle = "Unweighted vs Population-weighted, 1960–2024 ERA5 Data.\n
    The most serious health impacts of a heat wave are often associated with high
    temperatures at night.",
    x = "Year",
    y = "Number of Days",
    color = "Measure"
  ) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

####Heat Waves####
#Follows the definition of a heat wave explained in the following:
#https://www.epa.gov/climate-indicators/climate-change-indicators-heat-waves
#defined as 2 or more consec. days of min.temp above 85th perc. of historical July and August temp. (can be adjusted!)
#"The 85th percentile of July and August temperatures equates to the nine hottest days during the hottest two months of the year. A temperature that is typically only recorded nine times during the hottest part of the year is rare enough that most people would consider it to be unusually hot."
#Four key characteristics of heat waves:
#Frequency: the number of heat waves that occur every year.
#Duration: the length of each individual heat wave, in days.
#Season length: the number of days between the first heat wave of the year and the last.
#Intensity: how hot it is during the heat wave.

#1.Step: Compute 1970–2010 July & August baseline (per governorate)
historical_temps <- era5_temp_df |>
  filter(year >= 1970, year <= 2010,month %in% c(7,8)) |>
  group_by(name) |>
  summarise(
    mintemp_historical_85pct = quantile(mean_mintemp_pop_weighted,.85),
    maxtemp_historical_85pct = quantile(mean_maxtemp_pop_weighted,.85),
    .groups = "drop")

#2.Step: Join with Daily temp. data
heatwaves_df <- era5_temp_df |>
  left_join(historical_temps,by = c("name")) |>
  mutate(heatday_mintemp = if_else(mean_mintemp_pop_weighted > mintemp_historical_85pct,1,0),
         heatday_maxtemp = if_else(mean_maxtemp_pop_weighted > maxtemp_historical_85pct,1,0))
#this counts any day where temp. above 85th percentile;
#2 or more consecutive days for a heat wave

# Step 3. Identify consecutive runs of heat days
heatdays_df <- heatwaves_df |>
  arrange(name, date) |>
  group_by(name, year) |>
  mutate(
    heatday_mintemp_run = rleid(heatday_mintemp)  # run-length encoding ID
  ) |>
  group_by(name, year, heatday_mintemp_run) |>
  mutate(
    run_length = n(),
    is_heatwave = heatday_mintemp & run_length >= 2   # mark runs >=2 days
  ) |>
  ungroup()

# Step 4. Summarize heat wave characteristics (adjust intensity)
heatwave_events <- heatdays_df |>
  filter(is_heatwave) |>
  group_by(name, year, heatday_mintemp_run) |>
  summarise(
    start_date = min(date),
    end_date   = max(date),
    duration   = n(),   # duration of this heatwave
    # average *excess* temp above threshold
    intensity  = mean(mean_mintemp_pop_weighted - mintemp_historical_85pct, na.rm = TRUE),
    .groups = "drop_last"
  )

# Step 5. Yearly summary metrics (now intensity = °C above threshold)
heatwave_stats <- heatwave_events |>
  group_by(name, year) |>
  summarise(
    frequency     = n(),
    mean_duration = mean(duration),
    season_length = as.numeric(max(end_date) - min(start_date)),
    mean_intensity = mean(intensity),  # avg °C above threshold
    .groups = "drop"
  )

# Step 6. Decadal summary (same logic)
heatwave_stats_decade <- heatwave_stats |>
  mutate(decade = floor(year / 10) * 10) |>
  group_by(name, decade) |>
  summarise(
    frequency      = mean(frequency, na.rm = TRUE),
    mean_duration  = mean(mean_duration, na.rm = TRUE),
    season_length  = mean(season_length, na.rm = TRUE),
    mean_intensity = mean(mean_intensity, na.rm = TRUE), # °C above threshold
    .groups = "drop"
  )

####Plotting####
# Reshape to long format
heatwave_stats_long <- heatwave_stats_decade |>
  pivot_longer(
    cols = c(frequency, mean_duration, season_length, mean_intensity),
    names_to = "metric",
    values_to = "value")

#####Line plot#####
ggplot(heatwave_stats_long,
       aes(x = decade, y = value, color = name, group = name)) +
  geom_line() +
  geom_point() +
  facet_wrap(
    ~metric,
    scales = "free_y",
    ncol = 2,
    switch = "y",   # move facet labels to y-axis side
    labeller = as_labeller(c(
      frequency      = "Frequency (# heatwaves)",
      mean_duration  = "Mean Duration (days)",
      season_length  = "Season Length (days)",
      mean_intensity = "Mean Intensity (°C)"
    ))
  ) +
  labs(
    title = "Heatwave Characteristics by Decade",
    subtitle = "Computed from daily temperature data",
    x = "Decade",
    color = "Governorate"
  ) +
  theme_minimal() +
  theme(
    strip.placement = "outside",       # place facet labels outside axes
    strip.background = element_blank() # cleaner look
  )

#####Decadal Bar plot####
#patchwork
#subset for clarity
df_sub <- heatwave_stats_long |>
  filter(name %in% c("Assiut", "Suhag"))

# Common theme modification
axis_theme <- theme(
  axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(size = 9)
)

# (recreate p1..p4 but include axis_theme in each)
p1 <- df_sub |>
  filter(metric == "frequency") |>
  ggplot(aes(x = factor(decade), y = value, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Heat Wave Frequency",
       x = "Decade", y = "Average number of\n heat waves per year",
       fill = "Governorate") +
  theme_minimal() + axis_theme

p2 <- df_sub |>
  filter(metric == "mean_duration") |>
  ggplot(aes(x = factor(decade), y = value, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Heat Wave Duration",
       x = "Decade", y = "Average length of\n individual heat waves (days)",
       fill = "Governorate") +
  theme_minimal() + axis_theme

p3 <- df_sub |>
  filter(metric == "season_length") |>
  ggplot(aes(x = factor(decade), y = value, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Heat Wave Season",
       x = "Decade", y = "Average length of\n annual heat wave season (days)",
       fill = "Governorate") +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  theme_minimal() + axis_theme

p4 <- df_sub |>
  filter(metric == "mean_intensity") |>
  ggplot(aes(x = factor(decade), y = value, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Heat Wave Intensity",
       x = "Decade",
       y = "Average temperature above local\n threshold during heat waves (°C)",
       fill = "Governorate") +
  theme_minimal() + axis_theme

# --- Combine safely and collect single legend ---
combined <- (p1 | p2) / (p3 | p4) + plot_layout(guides = "collect")

# Apply one shared theme to the whole patchwork using &
combined <- combined & theme(
  # legend
  legend.position   = "bottom",
  legend.justification = "center",
  legend.title      = element_text(size = 8),
  legend.text       = element_text(size = 7),
  legend.key.size   = unit(0.35, "cm"),
  legend.box.margin = margin(t = 0, b = 0),

  # plot annotation text sizes (use plot.title, NOT title)
  plot.title        = element_text(size = 14, face = "bold", hjust = 0.5),
  plot.subtitle     = element_text(size = 10, hjust = 0.5),
  plot.caption      = element_text(size = 8)
)

# Force a single-row legend (nice and compact)
combined <- combined & guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# Add overall title / subtitle / caption
combined + plot_annotation(
  title = "Heatwave Characteristics in Assiut and Suhag by Decade (1960 - 2024)",
  subtitle = "Heatwaves: At least 2 consecutive days above the local 85th percentile (1970–2010 baseline, Jul–Aug).\nIntensity = °C above that threshold during heat waves.",
  caption = "Source: Author's calculations based on ERA5 data"
)



# Unpredictability --------------------------------------------------------
####1)Seasonal Variance####
# define meteorological seasons
era5_seasonal <- era5_temp_df |>
  mutate(
    season = case_when(
      month %in% c(12,1,2) ~ "Winter",
      month %in% c(3,4,5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9,10,11) ~ "Fall"
    ),
    # ensure December is assigned to the *next year's* winter season
    season_year = if_else(month == 12, year + 1, year)
  )

# compute within-season temp variance
seasonal_var <- era5_seasonal |>
  filter(name %in% c("Assiut","Suhag")) |>
  group_by(season_year, season) |>
  summarise(
    temp_sd_mean = sd(mean_temp, na.rm = TRUE),
    temp_sd_max = sd(mean_maxtemp, na.rm = TRUE),
    temp_sd_min = sd(mean_mintemp,na.rm = TRUE),
    temp_var_mean = var(mean_temp, na.rm = TRUE),
    temp_var_max = var(mean_maxtemp, na.rm = TRUE),
    temp_var_min = var(mean_mintemp,na.rm = TRUE),
    .groups = "drop"
  )

# plot variance over time
#####SD Max Temp.#####
seasonal_var |>
  ggplot(aes(x = season_year, y = temp_sd_max, color = season)) +
  geom_line(alpha = 0.25, linewidth = 0.7, aes(group = season)) +
  geom_smooth(se = FALSE, linewidth = 1.1) +
  labs(
    title = "Seasonal Temperature Variability in Upper Egypt (1960–2024)",
    subtitle = "Within-season standard deviation of daily maximum temperature (ERA5)",
    x = "Year",
    y = "SD of Daily Max. Temperature (°C)",
    color = "Season"
  ) +
  theme_minimal(base_size = 10) +
  scale_x_continuous(
    limits = c(1960, 2025),
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  scale_color_brewer(palette = "Set2")

#####SD Mean Temp.#####
seasonal_var |>
  ggplot(aes(x = season_year, y = temp_sd_mean, color = season)) +
  geom_line(alpha = 0.25, linewidth = 0.7, aes(group = season)) +
  geom_smooth(se = FALSE, linewidth = 1.1) +
  labs(
    title = "Seasonal Temperature Variability in Upper Egypt (1960–2024)",
    subtitle = "Within-season standard deviation of Daily Mean Temperature (ERA5)",
    x = "Year",
    y = "SD of Daily Mean Temperature (°C)",
    color = "Season"
  ) +
  theme_minimal(base_size = 10) +
  scale_x_continuous(
    limits = c(1960, 2025),
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_color_brewer(palette = "Set2")

#does not provide evidence for increasing seasonal variability,
#if anything, then slightly decreased in Summer & Winter

####2) IQR within Seasons####
#IQR more robust than SD
seasonal_iqr <- era5_seasonal |>
  group_by(name, season_year, season) |>
  summarise(
    iqr_temp = IQR(mean_temp, na.rm = TRUE),
    .groups = "drop"
  )

seasonal_iqr |>
  filter(name %in% c("Suhag","Assiut")) |>
  ggplot(aes(x = season_year,y = iqr_temp,colour = season))+
  geom_line()+
  geom_smooth(se = FALSE)+
  facet_wrap(~name)+
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

####3) Year to year anomalies; relative to previous year####
#1. year to year changes, anomalies relative to previous year
yearly_mean <- era5_temp_df |>
  group_by(name, year) |>
  summarise(
    mean_temp_year = mean(mean_temp, na.rm = TRUE),
    max_temp_year = mean(mean_maxtemp,na. = TRUE),
    .groups = "drop"
  )

yearly_anomaly <- yearly_mean |>
  arrange(name, year) |>
  group_by(name) |>
  mutate(
    anomaly_y2y_meantemp = mean_temp_year - lag(mean_temp_year),
    anomaly_y2y_maxtemp = max_temp_year - lag(max_temp_year)
  )

#####Mean temp anomalies####
yearly_anomaly |>
  filter(name %in% c("Suhag","Assiut")) |>
  ggplot(aes(x = year,y = abs(anomaly_y2y_meantemp)))+
  geom_line()+
  geom_smooth(se = FALSE)+
  facet_wrap(~name)

#####Max temp. anomalies####
yearly_anomaly |>
  filter(name %in% c("Suhag","Assiut")) |>
  ggplot(aes(x = year,y = abs(anomaly_y2y_maxtemp)))+
  geom_line()+
  geom_smooth(se = FALSE)+
  facet_wrap(~name)

#### 4)Seasonal Range####
#max-min range per season
seasonal_range <- era5_seasonal |>
  group_by(name, season_year, season) |>
  summarise(
    range_temp = max(mean_temp, na.rm = TRUE) - min(mean_temp, na.rm = TRUE),
    .groups = "drop"
  )

seasonal_range |>
  filter(name %in% c("Suhag","Assiut")) |>
  ggplot(aes(x = season_year,y = range_temp,colour = season))+
  geom_line()+
  geom_smooth(se = FALSE)+
  facet_wrap(~name)


seasonal_var |>
  filter(name %in% c("Assiut","Suhag")) |>
  ggplot(aes(x = season_year, y = temp_sd_max, color = season)) +
  geom_line(alpha = 0.3, aes(group = interaction(name, season))) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  labs(
    title = "Seasonal Temperature Variability Over Time",
    subtitle = "Within-season SD of Daily Max. Temperature",
    x = "Year",
    y = "SD of Daily Max. Temperature (°C)",
    color = "Season"
  ) +
  theme_minimal()+
  facet_wrap(~name,nrow = 2)+
  scale_x_continuous(
    limits = c(1960, 2025),
    breaks = seq(1960, 2024, by = 4),
    expand = c(0, 0)
  ) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Comparing Temp., Heat Index & UTCI --------------------------------------
#Load Heat Index Data
heatindex_govs <- readRDS(file = paste(here(),
                                       "Data", "UTCI","UTCI_daily",
                                       "heatindex_govpanel_20002024",sep = "/"))

#Load UTCI Data
utci_govs <- readRDS(file = paste(here(),
                                       "Data", "UTCI","UTCI_daily",
                                       "utci_govpanel_20002024",sep = "/"))

#Join all together
temp_joined <- era5_temp_df |>
  left_join(heatindex_govs, by = c("name" = "ADM1_EN","date")) |>
  left_join(utci_govs,by = c("name" = "ADM1_EN","date"))

#plot daily max temp. by gov.
temp_joined |>
  dplyr::select(name,date,mean_maxtemp,hi_dailymax,utci_dailymax) |>
  filter(date >= "2024-01-01") |>
  pivot_longer(cols = c(mean_maxtemp,hi_dailymax,utci_dailymax),
                           names_to = "temp_measure",
                           values_to = "value") |>
  ggplot(aes(x = date,y = value,group = temp_measure, colour = temp_measure))+
  geom_line()+
  facet_wrap(~name)

#####7-day rolling mean#####
temp_joined |>
  dplyr::select(name, date, mean_maxtemp, hi_dailymax, utci_dailymax) |>
  filter(date >= "2024-01-01") |>
  pivot_longer(
    cols = c(mean_maxtemp, hi_dailymax, utci_dailymax),
    names_to = "temp_measure",
    values_to = "value"
  ) |>
  mutate(
    temp_measure = factor(
      temp_measure,
      levels = c("mean_maxtemp", "hi_dailymax", "utci_dailymax"),
      labels = c(
        "Air Temperature (max)",
        "Heat Index (max)",
        "UTCI (max)"))) |>
  arrange(name, temp_measure, date) |>
  group_by(name, temp_measure) |>
  mutate(value = zoo::rollmean(value, k = 7, fill = NA, align = "right")) |>
  ggplot(aes(x = date, y = value, colour = temp_measure)) +
  geom_line(
    aes(linewidth = temp_measure == "UTCI (max)"),
    alpha = 0.6) +
  scale_linewidth_manual(values = c(`TRUE` = 1, `FALSE` = 0.4),
                         guide = "none")+
  scale_colour_manual(
    values = c(
      "Air Temperature (max)" = "#1b9e77",
      "Heat Index (max)"     = "#d95f02",
      "UTCI (max)" = "#7570b3"))+
  scale_y_continuous(
    limits = c(10, 50),  # consistent with other plots
    breaks = seq(10, 50, by = 10),
    expand = c(0, 0)) +
  labs(
    title = "Daily Maximum Temperatures across Governorates in 2024",
    subtitle = "Comparing Air Temperature with Heat Index and Universal Thermal Climate Index (UTCI)",
    x = NULL,
    y = "Temperature (°C)",
    colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "lightgray", color = NA),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  facet_wrap(~ name)

#####Monthly means#####
temp_joined |>
  dplyr::select(name, date, mean_maxtemp, hi_dailymax, utci_dailymax) |>
  filter(date >= "2020-01-01") |>
  mutate(month = as.Date(format(date, "%Y-%m-01"))) |>
  pivot_longer(
    cols = c(mean_maxtemp, hi_dailymax, utci_dailymax),
    names_to = "temp_measure",
    values_to = "value"
  ) |>
  group_by(name, month, temp_measure) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = month, y = value, colour = temp_measure)) +
  geom_line(linewidth = 0.9) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"))+
  facet_wrap(~ name)

# UTCI: Extreme Heat Stress Days ----------------------------------------------
#Extreme Heat Stress ( >46° C) and strong heat stress (38°-46° C)
utci_govs |>
  mutate(year = year(date)) |>
  filter(utci_dailymax > 46) |>
  group_by(ADM1_EN) |>
  count(year) |>
  ggplot(aes(x = year,y = n,group = ADM1_EN,colour = ADM1_EN))+
  geom_line()



