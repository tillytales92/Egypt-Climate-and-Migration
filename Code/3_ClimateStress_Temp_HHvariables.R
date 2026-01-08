#Temperature extraction using exactextractr
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

# Load Data ---------------------------------------------------------------
hh_temp_long <- readRDS(file = paste(here(),"Data", "Temperature","ERA5",
                     "Panel","hh_era5_20002024.Rds",sep = "/"))

#####1) Daily measures: How many days temp. shocks?#####
#mean in base period (Here: 2000 - 2015)
base_meantemp_monthly <- hh_temp_long |>
  mutate(
    season = case_when(
      month %in% c(11,12,1,2,3,4) ~ "winter",
      month %in% c(5,6,7,8,9,10) ~ "summer")) |>
  filter(year >= 2000,year <= 2015)|>
  group_by(hhid,month,season) |>
  summarise(meantemp_base = mean(mean_temp, na.rm = TRUE),
            sdtemp_base = sd(mean_temp,na.rm = TRUE),
            meanutcimax_base = mean(utci_max,na.rm = TRUE),
            sdutcimax_base = sd(utci_max,na.rm = TRUE),
            .groups = "drop")

#count days where temp. +/- 2SD diff. from base
meantemp_shockdays <- hh_temp_long |>
  left_join(base_meantemp_monthly, by = c("hhid","month")) |>
  group_by(hhid,season) |>#group by Household
  mutate(
    #diff. from mean temp.
    temp_diff_mean       = mean_temp - meantemp_base,
    z_score_meantemp     = (mean_temp - meantemp_base) / sdtemp_base,
    shock_meantemp_any       = if_else(
      z_score_meantemp >  2|z_score_meantemp < -2,1,0),
    shock_meantemp_cold = if_else(z_score_meantemp < -2,1,0),
    shock_meantemp_hot = if_else(z_score_meantemp > 2,1,0),
    #diff from UTCI max
    temp_diff_utcimax       = utci_max - meanutcimax_base,
    z_score_utcimax      = (utci_max - meanutcimax_base) / sdutcimax_base,
    shock_utcimax_any       = if_else(
      z_score_utcimax >  2|z_score_utcimax < -2,1,0),
    shock_utcimax_cold = if_else(z_score_utcimax < -2,1,0),
    shock_utcimax_hot = if_else(z_score_utcimax > 2,1,0),
    ) |>
  ungroup()

######No. of Temp. Shock days#####
#TEST to see if measures are plausible
#dist. of shock days in 2024
meantemp_shockdays |>
  group_by(hhid,year) |>
  count(shock_meantemp_any) |>
  ungroup() |>
  filter(shock_meantemp_any == 1,year == 2024) |>
  ggplot()+
  geom_density(aes(x = n))
#some variation, not much

#shockdays over time for ex. HH (ID == 111)
meantemp_shockdays |>
  group_by(hhid,year) |>
  count(shock_meantemp_any) |>
  ungroup() |>
  filter(hhid == 111,
         shock_meantemp_any == 1) |>
  ggplot(aes(x = year,y = n))+
  geom_line()+
  geom_smooth(se = FALSE)
#increase in shock days; 2010 stands out as big outlier

#Test for utci max
#dist. of shock days in 2024
meantemp_shockdays |>
  group_by(hhid,year) |>
  count(shock_utcimax_any) |>
  ungroup() |>
  filter(shock_utcimax_any == 1,year == 2024) |>
  ggplot()+
  geom_density(aes(x = n))
#some variation, not much

#shockdays over time for ex. HH (ID == 111)
meantemp_shockdays |>
  filter(hhid == 111) |>
  group_by(year) |>
  count(shock_utcimax_any) |>
  ungroup() |>
  filter(shock_utcimax_any == 1) |>
  ggplot(aes(x = year,y = n))+
  geom_line()+
  geom_smooth(se = FALSE)

######Create Shock day vars.######
######Mean temp shockdays######
#Shock days between 2015-2019 & 2020-2024
hh_shockdays_meantemp <- meantemp_shockdays |>
    mutate(
      period = case_when(
        year >= 2015 & year < 2020 ~ "20152019",
        year >= 2020 & year <= 2024  ~ "20202024",
        TRUE ~ NA_character_)) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id,season,period) |>
  count(shock_meantemp_any) |>
  #add pre-treatment period
  bind_rows(
    meantemp_shockdays |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id,season) |>
      count(shock_meantemp_any) |>
      mutate(period = "pretreatment")) |>
  #add 2015-2024
  bind_rows(
    meantemp_shockdays |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id,season) |>
      count(shock_meantemp_any) |>
      mutate(period = "20152024")) |>
  filter(shock_meantemp_any == 1) |>
  pivot_wider(
    names_from = c(season, period),
    values_from = n,
    names_glue = "temp_shockdays_{season}_{period}") |>
  ungroup() |>
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

#Plot summer shockdays:2020-2024
hh_shockdays_meantemp |>
  ggplot(aes(x = temp_shockdays_summer_20202024 ))+
  geom_density()

######UTCI max. temp shockdays######
hh_shockdays_utcimax <- meantemp_shockdays |>
  mutate(
    period = case_when(
      year >= 2015 & year < 2020 ~ "20152019",
      year >= 2020 & year <= 2024  ~ "20202024",
      TRUE ~ NA_character_)) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id,season,period) |>
  count(shock_utcimax_any) |>
  #add pre-treatment period
  bind_rows(
    meantemp_shockdays |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id,season) |>
      count(shock_utcimax_any) |>
      mutate(period = "pretreatment")) |>
  #add 2015-2024
  bind_rows(
    meantemp_shockdays |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id,season) |>
      count(shock_utcimax_any) |>
      mutate(period = "20152024")) |>
  filter(shock_utcimax_any == 1) |>
  pivot_wider(
    names_from = c(season, period),
    values_from = n,
    names_glue = "utcimax_shockdays_{season}_{period}") |>
  ungroup() |>
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

#Plot summer shockdays:2020-2024
hh_shockdays_utcimax |>
  ggplot(aes(x = utcimax_shockdays_summer_20202024 ))+
  geom_density()

####2) Average mean. temp and growth rate for 2015-2020,2020-2025 and 2015-2025####
hh_avgmeantemp <- hh_temp_long |>
  mutate(
  season = case_when(
    month %in% c(11,12,1,2,3,4) ~ "winter",
    month %in% c(5,6,7,8,9,10) ~ "summer"),
  period = case_when(
    year >= 2015 & year < 2020 ~ "20152019",
    year >= 2020 & year <= 2024  ~ "20202024",
    TRUE ~ NA_character_)) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id,season,period) |>
  summarise(avg_meantemp = mean(mean_temp)) |>
  #add pre-treatment period
  bind_rows(
    hh_temp_long |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer")) |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id,season) |>
      summarise(avg_meantemp = mean(mean_temp)) |>
      mutate(period = "pretreatment")) |>
  #add 2015-2024
  bind_rows(
    hh_temp_long |>
    mutate(
      season = case_when(
        month %in% c(11,12,1,2,3,4) ~ "winter",
        month %in% c(5,6,7,8,9,10) ~ "summer")) |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id,season) |>
      summarise(avg_meantemp = mean(mean_temp)) |>
      mutate(period = "20152024")) |>
    pivot_wider(
      names_from = c(season, period),
      values_from = avg_meantemp,
      names_glue = "temp_avgmean_{season}_{period}") |>
  mutate(
    temp_mean_growth_summer = (temp_avgmean_summer_20202024  - temp_avgmean_summer_20152019)
    / temp_avgmean_summer_20152019,
    temp_mean_growth_winter = (temp_avgmean_winter_20202024  - temp_avgmean_winter_20152019)
    / temp_avgmean_winter_20152019) |>
  ungroup() |>
#relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
              contains("_20152019"),contains("_20152024"),
              contains("_20202024"),temp_mean_growth_summer,temp_mean_growth_winter)

####3) Number of Heat Wave Days####
#Follows the definition of a heat wave explained in the following:
#https://www.epa.gov/climate-indicators/climate-change-indicators-heat-waves
#defined as 2 or more consec. days of min.temp above 85th perc. of historical July and August temp. (can be adjusted!)
#"The 85th percentile of July and August temperatures equates to the nine hottest days during the hottest two months of the year. A temperature that is typically only recorded nine times during the hottest part of the year is rare enough that most people would consider it to be unusually hot."

#1.Step: Compute 2000–2015 July & August baseline (by Household)
historical_temps <- hh_temp_long  |>
  filter(year >= 2000, year <= 2015,month %in% c(7,8)) |>
  group_by(hhid,agglom_id) |>
  summarise(
    meantemp_historical_85pct = quantile(mean_temp,.85),
    .groups = "drop")

#2.Step: Join with Daily temp. data
heatwaves_df <- hh_temp_long |>
  left_join(historical_temps,by = c("hhid","agglom_id")) |>
  mutate(heatday_meantemp = if_else(mean_temp > meantemp_historical_85pct,1,0))

#3.Count Heatwave Days yearly
heatwaves_yearly <- heatwaves_df |>
  group_by(hhid, agglom_id, year) |>
  summarise(
    heatwave_days = sum(heatday_meantemp, na.rm = TRUE),
    .groups = "drop")

#Aggregate over periods (2015-2020,2020-2025,2015-2025)
hh_heatwavedays <- heatwaves_yearly |>
  mutate(period = case_when(
    year >= 2015 & year < 2020 ~ "20152019",
    year >= 2020 & year <= 2024 ~ "20202024",
    TRUE ~ NA_character_)) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id, period) |>
  summarise(
    temp_heatwave_days_avg = mean(heatwave_days, na.rm = TRUE),
    temp_heatwave_days_n = sum(heatwave_days, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # add the combined pretreatment period
  bind_rows(
    heatwaves_yearly |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id) |>
      summarise(
        temp_heatwave_days_avg = mean(heatwave_days, na.rm = TRUE),
        temp_heatwave_days_n = sum(heatwave_days, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "pretreatment")
  ) |>
  # add the combined 2015–2024 period
  bind_rows(
    heatwaves_yearly |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id) |>
      summarise(
        temp_heatwave_days_avg = mean(heatwave_days, na.rm = TRUE),
        temp_heatwave_days_n = sum(heatwave_days, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "20152024")
  ) |>
  # reshape to wide format
  pivot_wider(
    names_from = period,
    values_from = c(temp_heatwave_days_avg, temp_heatwave_days_n),
    names_sep = "_") |>
  ungroup() |>
  #relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

#plot
hh_heatwavedays |>
  ggplot(aes(x = temp_heatwave_days_n_20152024))+
  geom_density()

####4) Length of heatwave season####
heatwaves_yearly <- heatwaves_df |>
  filter(heatday_meantemp == 1) |>
  group_by(hhid,agglom_id,year) |>
  summarise(
    start_date = min(date),
    end_date   = max(date),
    season_length = as.numeric(max(end_date) - min(start_date)),
    .groups = "drop_last") |>

#by period
hh_heatwaves_length <- heatwaves_yearly |>
  mutate(period = case_when(
    year >= 2015 & year < 2020 ~ "20152019",
    year >= 2020 & year <= 2024 ~ "20202024",
    TRUE ~ NA_character_)) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id, period) |>
  summarise(
    temp_heatwave_length = mean(heatwave_days, na.rm = TRUE),
    .groups = "drop") |>
  # add the pretreatment period
  bind_rows(
    heatwaves_yearly |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id) |>
      summarise(
        temp_heatwave_length = mean(heatwave_days, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "pretreatment")) |>
  # add the combined 2015–2024 period
  bind_rows(
    heatwaves_yearly |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id) |>
      summarise(
        temp_heatwave_length = mean(heatwave_days, na.rm = TRUE),
        .groups = "drop") |>
      mutate(period = "20152024")) |>
  # reshape to wide format
  pivot_wider(
    names_from = period,
    values_from = temp_heatwave_length,
    names_glue = "temp_heatwave_length_{period}") |>
  ungroup() |>
  #relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

#####5) Winter days with max temp above 30 degrees######
hh_winterdays_above30_yearly <- hh_temp_long |>
  filter(month %in% c(11,12,1,2,3,4)) |>
  group_by(hhid,agglom_id,year) |>
  summarise(
    n_winterdaysabove30 = sum(max_temp > 30, na.rm = TRUE),
    .groups = "drop")

hh_winterdays_above30 <- hh_winterdays_above30_yearly |>
  mutate(period = case_when(
    year >= 2015 & year < 2020 ~ "20152019",
    year >= 2020 & year <= 2024 ~ "20202024",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id, period) |>
  summarise(
    temp_winterdaysabove30_avg = mean(n_winterdaysabove30, na.rm = TRUE),
    temp_winterdaysabove30_n = sum(n_winterdaysabove30, na.rm = TRUE),
    .groups = "drop"
  ) |>
  #add the pretreatment period
  bind_rows(
    hh_winterdays_above30_yearly |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id) |>
      summarise(
        temp_winterdaysabove30_avg = mean(n_winterdaysabove30, na.rm = TRUE),
        temp_winterdaysabove30_n = sum(n_winterdaysabove30, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "pretreatment")) |>
  #add the combined 2015–2024 period
  bind_rows(
    hh_winterdays_above30_yearly |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id) |>
      summarise(
        temp_winterdaysabove30_avg = mean(n_winterdaysabove30, na.rm = TRUE),
        temp_winterdaysabove30_n = sum(n_winterdaysabove30, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "20152024")
  ) |>
  # reshape to wide format
  pivot_wider(
    names_from = period,
    values_from = c(temp_winterdaysabove30_avg,temp_winterdaysabove30_n),
    names_sep = "_") |>
  ungroup() |>
  #relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

#### 6) Temperature Variations ####
hh_temp_variations <- hh_temp_long |>
  # Define season and period
  mutate(
    season = case_when(
      month %in% c(11, 12, 1, 2, 3, 4) ~ "winter",
      month %in% c(5, 6, 7, 8, 9, 10) ~ "summer",
      TRUE ~ NA_character_),
    period = case_when(
      year >= 2015 & year < 2020 ~ "20152019",
      year >= 2020 & year <= 2024 ~ "20202024",
      TRUE ~ NA_character_)) |>
  filter(!is.na(period)) |>
  # Compute period-specific SDs
  group_by(hhid, agglom_id, season, period) |>
  summarise(
    temp_sd_max = sd(max_temp, na.rm = TRUE),
    temp_sd_min = sd(min_temp, na.rm = TRUE),
    temp_sd_mean = sd(mean_temp, na.rm = TRUE),
    .groups = "drop") |>
  # Add pretreatment period
  bind_rows(
    hh_temp_long |>
      mutate(
        season = case_when(
          month %in% c(11, 12, 1, 2, 3, 4) ~ "winter",
          month %in% c(5, 6, 7, 8, 9, 10) ~ "summer",
          TRUE ~ NA_character_)) |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id, season) |>
      summarise(
        temp_sd_max = sd(max_temp, na.rm = TRUE),
        temp_sd_min = sd(min_temp, na.rm = TRUE),
        temp_sd_mean = sd(mean_temp, na.rm = TRUE),
        .groups = "drop") |>
      mutate(period = "pretreatment")) |>
  # Add combined 2015–2024 period
  bind_rows(
    hh_temp_long |>
      mutate(
        season = case_when(
          month %in% c(11, 12, 1, 2, 3, 4) ~ "winter",
          month %in% c(5, 6, 7, 8, 9, 10) ~ "summer",
          TRUE ~ NA_character_
        )
      ) |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id, season) |>
      summarise(
        temp_sd_max = sd(max_temp, na.rm = TRUE),
        temp_sd_min = sd(min_temp, na.rm = TRUE),
        temp_sd_mean = sd(mean_temp, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "20152024")) |>
  # Reshape to wide format (combine season and period in column names)
  pivot_wider(
    names_from = c(season, period),
    values_from = c(temp_sd_max, temp_sd_min, temp_sd_mean),
    names_glue = "{.value}_{season}_{period}") |>
  ungroup() |>
  #relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

####7) Extreme UTCI days ####
#see: https://climate-adapt.eea.europa.eu/en/metadata/indicators/high-utci-days
hh_extremeutcidays_yearly <- hh_temp_long |>
  group_by(hhid,agglom_id,year) |>
  summarise(
    #extreme heat stress when UTCI > 46° Celsius
    n_extremeutcidays = sum(utci_max > 46, na.rm = TRUE),
    .groups = "drop")

hh_extremeutcidays <- hh_extremeutcidays_yearly |>
  mutate(period = case_when(
    year >= 2015 & year < 2020 ~ "20152019",
    year >= 2020 & year <= 2024 ~ "20202024",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id, period) |>
  summarise(
    utci_extremeheatstress_avg = mean(n_extremeutcidays, na.rm = TRUE),
    utci_extremeheatstress_n = sum(n_extremeutcidays, na.rm = TRUE),
    .groups = "drop"
  ) |>
  #add the pretreatment period
  bind_rows(
    hh_extremeutcidays_yearly |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id) |>
      summarise(
        utci_extremeheatstress_avg = mean(n_extremeutcidays, na.rm = TRUE),
        utci_extremeheatstress_n = sum(n_extremeutcidays, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "pretreatment")) |>
  #add the combined 2015–2024 period
  bind_rows(
    hh_extremeutcidays_yearly |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id) |>
      summarise(
        utci_extremeheatstress_avg = mean(n_extremeutcidays, na.rm = TRUE),
        utci_extremeheatstress_n = sum(n_extremeutcidays, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "20152024")
  ) |>
  # reshape to wide format
  pivot_wider(
    names_from = period,
    values_from = c(utci_extremeheatstress_avg,utci_extremeheatstress_n),
    names_sep = "_") |>
  ungroup() |>
  #relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

# Join Measures -----------------------------------------------------------
hh_temp_variables <- hh_shockdays_meantemp |>
  left_join(hh_shockdays_utcimax,by = c("hhid","agglom_id")) |>
  left_join(hh_avgmeantemp,by = c("hhid","agglom_id")) |>
  left_join(hh_heatwavedays,by = c("hhid","agglom_id")) |>
  left_join(hh_heatwaves_length,by = c("hhid","agglom_id")) |>
  left_join(hh_winterdays_above30,by = c("hhid","agglom_id")) |>
  left_join(hh_temp_variations,by = c("hhid","agglom_id")) |>
  left_join(hh_extremeutcidays,by = c("hhid","agglom_id"))

# Save Data ---------------------------------------------------------------
saveRDS(hh_temp_variables,
        file = paste(here(),"Data","Temperature","ERA5",
                     "Panel","hh_temp_variables.Rds",sep = "/"))



