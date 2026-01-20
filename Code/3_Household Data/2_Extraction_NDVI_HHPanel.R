#Creating NDVI/EVI Variables at Household Level
#NDVI data at 250 metres resolution - data was extracted through GEE
#NDVI on cropland cells only in 1km buffer around household locations
#For now: use Copernicus 2019 Land Cover data
#there also is a file using ESRI 2024 Cropland at 10 m resolution that can
#be used for robustness
#Data was extracted through GEE
#Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","here")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read Data ------------------------------------------------------------
#Cropland NDVI
#Load NDVI & EVI Data (Vegetation Indices)
ndvi_cop19 <- read_csv(file = paste(here(),"Data","raw","NDVI",
        "NDVI_EVI_timeseries_Copernicus2019_cropland_2001_2024_fullsample.csv",sep = "/")) |>
  dplyr::select(-c(`system:index`,.geo,source)) |>
  #date columns
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         m_name = month(date, label = TRUE, abbr = TRUE))

#Distinct HHIDs to merge later
hh_id <- ndvi_cop19 |>
  distinct(hhid,agglom_id)

#col names
colnames(ndvi_cop19) <- tolower(colnames(ndvi_cop19))

#order variables
ndvi_cop19 <- ndvi_cop19 |>
  relocate(hhid,.before = evi) |>
  relocate(agglom_id,.after = hhid) |>
  relocate(date, .after = agglom_id)

#rename var names
ndvi_cop19 <- ndvi_cop19 |>
  rename("evi_cropland" = "evi",
         "ndvi_cropland" = "ndvi")

#calculate monthly ndvi
ndvi_cop19_monthly <- ndvi_cop19 |>
  group_by(hhid,agglom_id,month,m_name,year) |>
  summarise(mean_evi_cropland = mean(evi_cropland,na.rm = TRUE),
            mean_ndvi_cropland = mean(ndvi_cropland,na.rm = TRUE)) |>
  ungroup()

####Base period####
#How many months are shock months (-2SD from hist. mean)?
#mean in base period (Here: 2000 - 2015)
base_monthly <- ndvi_cop19 |>
  filter(year >= 2000,year <= 2015)|>
  group_by(hhid,month) |>
  summarise(
            meanndvi_cropland_base = mean(ndvi_cropland,na.rm = TRUE),
            sdndvi_cropland_base = sd(ndvi_cropland,na.rm = TRUE),
            meanevi_cropland_base = mean(evi_cropland,na.rm = TRUE),
            sdevi_cropland_base = sd(evi_cropland,na.rm = TRUE),
            .groups = "drop")

#Create Shock variables: capture deviations from monthly means
ndvicop19_shocks <- ndvi_cop19_monthly |>
  left_join(base_monthly, by = c("hhid","month")) |>
  group_by(hhid) |>#group by governorate
  mutate(
    #NDVI
    ndvi_diff_cropland = mean_ndvi_cropland - meanndvi_cropland_base ,
    zscore_ndvi_cropland = (mean_ndvi_cropland - meanndvi_cropland_base) / sdndvi_cropland_base,
    shock_ndvi_any_cropland = if_else(
      zscore_ndvi_cropland >  2|zscore_ndvi_cropland < -2,1,0),
    shock_ndvi_cropland_drought = if_else(zscore_ndvi_cropland < -2,1,0),
    #EVI
    evi_diff_cropland = mean_evi_cropland - meanevi_cropland_base ,
    zscore_evi_cropland = (mean_evi_cropland - meanevi_cropland_base) / sdevi_cropland_base,
    shock_evi_any_cropland = if_else(
      zscore_evi_cropland >  2|zscore_evi_cropland < -2,1,0),
    shock_evi_cropland_drought = if_else(zscore_evi_cropland < -2,1,0)) |>
  ungroup()

#TEST to see if measures are plausible
#dist. of shock days in 2024
#NDVI
ndvicop19_shocks |>
  group_by(hhid,year) |>
  count(shock_ndvi_cropland_drought) |>
  ungroup() |>
  filter(shock_ndvi_cropland_drought == 1,year == 2024) |>
  ggplot()+
  geom_density(aes(x = n))

#EVI
ndvicop19_shocks |>
  group_by(hhid,year) |>
  count(shock_evi_cropland_drought) |>
  ungroup() |>
  filter(shock_evi_cropland_drought == 1,year == 2024) |>
  ggplot()+
  geom_density(aes(x = n))

#####Mean Levels & Growth rate by period#####
#Mean Levels and Growth rate by period
hh_ndvi_mean <- ndvi_cop19 |>
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
  summarise(ndvi_avg = mean(ndvi_cropland),
            evi_avg = mean(evi_cropland)) |>
  #Add pretreatment period
  bind_rows(
    ndvi_cop19 |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer")) |>
      filter(year >= 2000, year < 2020) |>
      group_by(hhid, agglom_id,season) |>
      summarise(ndvi_avg = mean(ndvi_cropland),
                evi_avg = mean(evi_cropland)) |>
      mutate(period = "pretreatment")) |>
  #Add combined 2015–2024 period
  bind_rows(
    ndvi_cop19 |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer")) |>
      filter(year >= 2015, year <= 2024) |>
      group_by(hhid, agglom_id,season) |>
      summarise(ndvi_avg = mean(ndvi_cropland),
                evi_avg = mean(evi_cropland)) |>
      mutate(period = "20152024")) |>
  pivot_wider(
    names_from = c(season, period),
    values_from = c(ndvi_avg, evi_avg),
    names_glue = "{.value}_{season}_{period}") |>
  mutate(
    ndvi_avg_growth_summer = (ndvi_avg_summer_20202024  - ndvi_avg_summer_20152019 )/
      ndvi_avg_summer_20152019,
    ndvi_avg_growth_winter = (ndvi_avg_winter_20202024  - ndvi_avg_winter_20152019 )/
      ndvi_avg_winter_20152019,
    evi_avg_growth_summer = (evi_avg_summer_20202024  - evi_avg_summer_20152019 )/
      evi_avg_summer_20152019,
    evi_avg_growth_winter = (evi_avg_winter_20202024  - evi_avg_winter_20152019 )/
      evi_avg_winter_20152019) |>
  ungroup() |>
  #relocate columns
  dplyr::select(hhid,agglom_id,
                contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"),ndvi_avg_growth_summer,
                ndvi_avg_growth_winter,evi_avg_growth_summer,
                evi_avg_growth_winter)

# ---- NDVI shock months (by season and period) ----
hh_shockmonths_ndvi <- ndvicop19_shocks |>
  mutate(
    season = case_when(
      month %in% c(11,12,1,2,3,4) ~ "winter",
      month %in% c(5,6,7,8,9,10) ~ "summer",
      TRUE ~ NA_character_
    ),
    period = case_when(
      year >= 2015 & year < 2020 ~ "2015-2019",
      year >= 2020 & year <= 2024 ~ "2020-2024",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period), !is.na(season)) |>
  group_by(hhid, agglom_id, period, season) |>
  count(shock_ndvi_cropland_drought) |>
  #Add pretreatment period
  bind_rows(
    ndvicop19_shocks |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer",
          TRUE ~ NA_character_
        )
      ) |>
      filter(year >= 2000, year < 2020, !is.na(season)) |>
      group_by(hhid, agglom_id, season) |>
      count(shock_ndvi_cropland_drought) |>
      mutate(period = "pretreatment")) |>
  #Add combined 2015–2024 period
  bind_rows(
    ndvicop19_shocks |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer",
          TRUE ~ NA_character_
        )
      ) |>
      filter(year >= 2015, year <= 2024, !is.na(season)) |>
      group_by(hhid, agglom_id, season) |>
      count(shock_ndvi_cropland_drought) |>
      mutate(period = "2015-2024")
  ) |>
  filter(shock_ndvi_cropland_drought == 1) |>
  pivot_wider(
    names_from = c(season,period),
    values_from = n,
    names_prefix = "ndvi_shockmonths_"
  ) |>
  ungroup() |>
  rename_with(~ gsub("-", "", .x), starts_with("ndvi_shockmonths_")) |>
  #relocate columns
  dplyr::select(hhid,agglom_id,
                contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

# ---- EVI shock months (by season and period) ----
hh_shockmonths_evi <- ndvicop19_shocks |>
  mutate(
    season = case_when(
      month %in% c(11,12,1,2,3,4) ~ "winter",
      month %in% c(5,6,7,8,9,10) ~ "summer",
      TRUE ~ NA_character_
    ),
    period = case_when(
      year >= 2015 & year < 2020 ~ "2015-2019",
      year >= 2020 & year <= 2024 ~ "2020-2024",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(period), !is.na(season)) |>
  group_by(hhid, agglom_id, period, season) |>
  count(shock_evi_cropland_drought) |>
  #pretreatment period
  bind_rows(
    ndvicop19_shocks |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer",
          TRUE ~ NA_character_
        )
      ) |>
      filter(year >= 2000, year < 2020, !is.na(season)) |>
      group_by(hhid, agglom_id, season) |>
      count(shock_evi_cropland_drought) |>
      mutate(period = "pretreatment")
  ) |>
  #2020-2024 period
  bind_rows(
    ndvicop19_shocks |>
      mutate(
        season = case_when(
          month %in% c(11,12,1,2,3,4) ~ "winter",
          month %in% c(5,6,7,8,9,10) ~ "summer",
          TRUE ~ NA_character_
        )
      ) |>
      filter(year >= 2015, year <= 2024, !is.na(season)) |>
      group_by(hhid, agglom_id, season) |>
      count(shock_evi_cropland_drought) |>
      mutate(period = "2015-2024")
  ) |>
  filter(shock_evi_cropland_drought == 1) |>
  pivot_wider(
    names_from = c(season,period),
    values_from = n,
    names_prefix = "evi_shockmonths_"
  ) |>
  ungroup() |>
  rename_with(~ gsub("-", "", .x), starts_with("evi_shockmonths_")) |>
  #relocate columns
  dplyr::select(hhid,agglom_id,
                contains("_pretreatment"),
                contains("_20152019"),contains("_20152024"),
                contains("_20202024"))

# ---- Combine NDVI + EVI ----
hh_shockmonths <- hh_shockmonths_ndvi |>
  full_join(hh_shockmonths_evi, by = c("hhid", "agglom_id")) |>
  mutate(across(
    starts_with("ndvi_shockmonths_") | starts_with("evi_shockmonths_"),
    ~replace_na(.x, 0)))

# ---- Join with HH IDs ----
hh_shocks_vegetationindices <- hh_id |>
  left_join(hh_shockmonths, by = c("hhid", "agglom_id")) |>
  mutate(across(
    starts_with("ndvi_shockmonths_") | starts_with("evi_shockmonths_"),
    ~replace_na(.x, 0)))

#join mean with shock days
hh_shocks_vegetationindices <- hh_ndvi_mean |>
  left_join(hh_shocks_vegetationindices,by = c("hhid","agglom_id"))

#reorder variables
hh_shocks_vegetationindices <- hh_shocks_vegetationindices |>
  dplyr::select(hhid,agglom_id,
                starts_with("ndvi_"),starts_with("evi_"))

# Save Data ---------------------------------------------------------------
saveRDS(hh_shocks_vegetationindices,
        file = paste(here(),"Data","intermediate","Household Data",
        "hh_shocks_vegindices.Rds",sep = "/"))

