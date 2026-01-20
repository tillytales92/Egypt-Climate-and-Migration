#Creating Evaporative Stress Index (ESI) Variables at Household Level
#NOTE: there are 271 missing obs. since ESI is only defined for cells with cropland
#Missing values change over the years --> now, excluding HH that do not have
#values for all years
#Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read Data ---------------------------------------------------------------
#Load ESI data
#use 12 week data for now; could also use 4 week ESI as robustness (see below)
#ESI 12 week timeseries (2001 - 2024)
esi_12week_hh <- read_csv(file = paste(here(),
                    "Data","raw","ESI",
                    "ESI_timeseries_households_12week_1kmbuffer_fullsample.csv",sep = "/")) |>
  dplyr::select(-c(`system:index`,.geo)) |>
  #date columns
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         m_name = month(date, label = TRUE, abbr = TRUE)) |>
  rename("esi" = mean)

#do households stay NA throughout all time periods
gg_miss_var(esi_12week_hh, facet = year)
#NO, missingness varies by year -> lower NA share from 2021 onwards

#hence: only keep HHID where values for all dates are present
esi_12week_hh_complete <- esi_12week_hh |>
  group_by(hhid) |>
  summarise(no_missings = all(!is.na(esi)), .groups = "drop") |>
  group_by(hhid) |>
  filter(no_missings == TRUE) |>
  ungroup() |>
  inner_join(esi_12week_hh, by = c("hhid"))

#Compare how many households dropped: should be 271
#Number of unique HHIDs before filtering
n_before <- esi_12week_hh |>
  distinct(hhid) |>
  nrow()

#Number of unique HHIDs after filtering
n_after <- esi_12week_hh_complete |>
  distinct(hhid) |>
  nrow()

#Print summary
cat("Households before filtering:", n_before, "\n")
cat("Households after filtering:", n_after, "\n")
cat("Dropped households:", n_before - n_after, "\n")
#dropped 177 households since they do not have ESI values for all years

# #ESI 4 week timeseries (2001 - 2024)
# esi_4week_hh <- read_csv(file = paste(here(),"Data","Drought Indices","ESI",
#                   "ESI_timeseries_households_4week_1kmbuffer_fullsample.csv",sep = "/")) |>
#   dplyr::select(-c(`system:index`,.geo)) |>
#   #date columns
#   mutate(year = year(date),
#          month = month(date),
#          day = day(date),
#          m_name = month(date, label = TRUE, abbr = TRUE)) |>
#   rename("esi" = mean)

####ESI 12 week####
#####Mean Levels & Growth rate by period#####
#Mean Levels and Growth rate by period
hh_esimean_12week <- esi_12week_hh_complete |>
  # Define seasons and periods
  mutate(
    season = case_when(
      month %in% c(11, 12, 1, 2, 3, 4) ~ "winter",
      month %in% c(5, 6, 7, 8, 9, 10) ~ "summer",
      TRUE ~ NA_character_
    ),
    period = case_when(
      year >= 2015 & year < 2020 ~ "20152019",
      year >= 2020 & year <= 2024 ~ "20202024",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period)) |>
  group_by(hhid, agglom_id, season, period) |>
  summarise(
    esi_mean = mean(esi, na.rm = TRUE),
    esi_sd   = sd(esi, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Add pretreatment period
  bind_rows(
    esi_12week_hh_complete |>
      mutate(
        season = case_when(
          month %in% c(11, 12, 1, 2, 3, 4) ~ "winter",
          month %in% c(5, 6, 7, 8, 9, 10) ~ "summer",
          TRUE ~ NA_character_)
      ) |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id, season) |>
      summarise(
        esi_mean = mean(esi, na.rm = TRUE),
        esi_sd   = sd(esi, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "pretreatment")
  ) |>
  # Add combined 2015–2024 period
  bind_rows(
    esi_12week_hh_complete |>
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
        esi_mean = mean(esi, na.rm = TRUE),
        esi_sd   = sd(esi, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(period = "20152024")
  ) |>
  ungroup() |>
  # Reshape to wide format
  pivot_wider(
    names_from = c(season, period),
    values_from = c(esi_mean, esi_sd),
    names_sep = "_"
  ) |>
  # Compute growth between 2015–2019 and 2020–2024
  mutate(
    esi_mean_growth_summer = (esi_mean_summer_20202024 - esi_mean_summer_20152019) /
      esi_mean_summer_20152019,
    esi_mean_growth_winter = (esi_mean_winter_20202024 - esi_mean_winter_20152019) /
      esi_mean_winter_20152019) |>
  #relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"),esi_mean_growth_summer,
                esi_mean_growth_winter)

#####Shock days#####
#How often does ESI cross a critical threshold per year (e.g. -2,-1)
hh_esi_droughtdays_12week <-  esi_12week_hh_complete |>
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
  summarise(
    esi_mdrought = sum(esi < -1, na.rm = TRUE),
    esi_sdrought = sum(esi < -2, na.rm = TRUE),
    .groups = "drop") |>
  #Add pretreatment period
  bind_rows(
    esi_12week_hh_complete |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer")) |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id,season) |>
      summarise(
        esi_mdrought = sum(esi < -1, na.rm = TRUE),
        esi_sdrought = sum(esi < -2, na.rm = TRUE),
        .groups = "drop") |>
      mutate(period = "pretreatment")) |>
  #Add combined 2015–2024 period
  bind_rows(
    esi_12week_hh_complete |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer")) |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id,season) |>
      summarise(
        esi_mdrought = sum(esi < -1, na.rm = TRUE),
        esi_sdrought = sum(esi < -2, na.rm = TRUE),
        .groups = "drop") |>
  mutate(period = "20152024")) |>
  pivot_wider(
    names_from = c(season,period),
    values_from = c(esi_mdrought,esi_sdrought)) |>
  #relocate columns
  dplyr::select(hhid,agglom_id,contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

#Join Data ---------------------------------------------------------------
hh_esi <- hh_esimean_12week |>
  left_join(hh_esi_droughtdays_12week,by = c("hhid","agglom_id"))

#Save Data ---------------------------------------------------------------
saveRDS(hh_esi,
        file = paste(here(),"Data","intermediate","Household Data",
                     "hh_esi_20002024.Rds",sep = "/"))

