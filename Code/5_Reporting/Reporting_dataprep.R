#Preparing data for reporting documents
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "lubridate","patchwork","zoo","data.table")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read Data ---------------------------------------------------------------
#Temperature Data
#Load ERA5 temp. data for Alexandria, Cairo, Assiut and Sohag
era5_temp_df <- readRDS(file = paste(here(),"Data",
                                     "intermediate","Governorate Data",
                                     "era5_temp_19602024.Rds",sep = "/"))

#Precipitation Data






# Creating reporting datasets ---------------------------------------------
####Temperature data####
#####Create annual data#####
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

# Calculate baseline statistics (1960-2010)
baseline_stats <- era5_temp_annual |>
  filter(year <= 2010) |>
  group_by(name) |>
  summarise(
    # Unweighted baselines
    mean_temp_baseline = mean(annual_mean_temp, na.rm = TRUE),
    sd_temp_baseline = sd(annual_mean_temp, na.rm = TRUE),
    mean_maxtemp_baseline = mean(annual_mean_maxtemp, na.rm = TRUE),
    sd_maxtemp_baseline = sd(annual_mean_maxtemp, na.rm = TRUE),
    mean_mintemp_baseline = mean(annual_mean_mintemp, na.rm = TRUE),
    sd_mintemp_baseline = sd(annual_mean_mintemp, na.rm = TRUE),
    # Population-weighted baselines
    mean_temp_pop_baseline = mean(annual_mean_temp_pop, na.rm = TRUE),
    sd_temp_pop_baseline = sd(annual_mean_temp_pop, na.rm = TRUE),
    mean_maxtemp_pop_baseline = mean(annual_mean_maxtemp_pop, na.rm = TRUE),
    sd_maxtemp_pop_baseline = sd(annual_mean_maxtemp_pop, na.rm = TRUE),
    mean_mintemp_pop_baseline = mean(annual_mean_mintemp_pop, na.rm = TRUE),
    sd_mintemp_pop_baseline = sd(annual_mean_mintemp_pop, na.rm = TRUE),
    .groups = "drop")

# Join and calculate deviations
era5_temp_annual <- era5_temp_annual |>
  left_join(baseline_stats, by = "name") |>
  mutate(
    # Unweighted deviations
    temp_diff_mean = annual_mean_temp - mean_temp_baseline,
    temp_diff_max = annual_mean_maxtemp - mean_maxtemp_baseline,
    temp_diff_min = annual_mean_mintemp - mean_mintemp_baseline,
    z_score_mean = (annual_mean_temp - mean_temp_baseline) / sd_temp_baseline,
    z_score_max = (annual_mean_maxtemp - mean_maxtemp_baseline) / sd_maxtemp_baseline,
    z_score_min = (annual_mean_mintemp - mean_mintemp_baseline) / sd_mintemp_baseline,
    # Population-weighted deviations
    temp_diff_mean_pop = annual_mean_temp_pop - mean_temp_pop_baseline,
    temp_diff_max_pop = annual_mean_maxtemp_pop - mean_maxtemp_pop_baseline,
    temp_diff_min_pop = annual_mean_mintemp_pop - mean_mintemp_pop_baseline,
    z_score_mean_pop = (annual_mean_temp_pop - mean_temp_pop_baseline) / sd_temp_pop_baseline,
    z_score_max_pop = (annual_mean_maxtemp_pop - mean_maxtemp_pop_baseline) / sd_maxtemp_pop_baseline,
    z_score_min_pop = (annual_mean_mintemp_pop - mean_mintemp_pop_baseline) / sd_mintemp_pop_baseline)

#####Heatwave data#####
#1.Step: Compute 1970–2010 July & August baseline (per governorate)
historical_temps <- era5_temp_df |>
  filter(year >= 1961, year <= 2010,month %in% c(7,8)) |>
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

####Precipitation data####
# Load precipitation data
chirps_yearly <- readRDS(file = paste(here(), "Data", "intermediate",
                                      "Governorate Data",
                                      "chirps_prec_yearly_19812024", sep = "/"))

# Calculate precipitation statistics
stats_yearly <- chirps_yearly |>
  group_by(name) |>
  summarise(
    mean_popland_prec = mean(mean_prec_popweighted_chirps, na.rm = TRUE),
    sd_popland_prec = sd(mean_prec_popweighted_chirps, na.rm = TRUE),
    .groups = "drop"
  )

# Merge back and calculate z-scores and extremes
chirps_yearly <- chirps_yearly |>
  left_join(stats_yearly, by = "name") |>
  mutate(
    z_score = (mean_prec_popweighted_chirps - mean_popland_prec) / sd_popland_prec,
    shock = case_when(
      z_score > 2 ~ 1,   # extreme wet year
      z_score < -2 ~ 1,  # extreme dry year
      TRUE ~ 0
    )
  )

# Add deciles of annual precipitation
chirps_yearly <- chirps_yearly |>
  group_by(name) |>
  mutate(prec_decile = ntile(mean_prec_popweighted_chirps, 10)) |>
  ungroup()

# Create shading ranges for extreme years
shading_yearly <- chirps_yearly |>
  mutate(
    xmin = as.Date(paste0(year, "-01-01")),
    xmax = as.Date(paste0(year, "-12-31")),
    shade_type = case_when(
      prec_decile == 1 ~ "Driest 10%",
      prec_decile == 10 ~ "Wettest 10%",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(shade_type))





# Save Data ---------------------------------------------------------------
#ERA5 annual
saveRDS(era5_temp_annual,
  file = paste(here(),"Data","final","era5_temp_annual.Rds",sep = "/"))

#Heatwave stats
saveRDS(heatwave_stats_long,
        file = paste(here(),"Data","final","heatwave_stats_long.Rds",sep ="/"))


