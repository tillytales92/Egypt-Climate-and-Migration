#Here:Combine data for HH dataset
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","exactextractr",
          "labelled")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load Data ---------------------------------------------------------------
#Temperature Variables --> Updated (26.11.2025)
#Done in R - see scripts "2_Extraction_Temp_HHPanel.R" & "3_ClimateStress_Temp_HHvariables"
hh_temp_variables <- readRDS(file = paste(here(),
                     "Data","intermediate","Household Data",
                     "hh_temp_variables.Rds",sep = "/"))

#Done in R - see Script "2_Elevation_Extraction.R" -> Updated (11.11.2025)
#Elevation/Slope data
hhdata_dem <- readRDS(file = paste(here(),
                      "Data","intermediate","Household Data",
                      "hhdata_dem.rds",sep = "/"))

#Done in R
##HH dist. to waterways -> Updated (11.11.2025)
hh_distwaterways <- readRDS(file = paste(here(),
                            "Data","intermediate","Household Data",
                            "hh_distwaterways.rds",sep = "/"))

#Done in "2_Extraction_ESI_HHPanel" --> Updated (26.11.2025)
#HH ESI
hh_esi <- readRDS(file = paste(here(),
                  "Data","intermediate","Household Data",
                  "hh_esi_20002024.Rds",sep = "/"))

#Done in "2_Extraction_NDVI_HHPanel" --> Updated (26.11.2025)
#HH Vegetation Indices
hh_shocks_vegetationindices <- readRDS(file = paste(here(),
                                      "Data","intermediate","Household Data",
                                      "hh_shocks_vegindices.Rds",sep = "/"))

# Join Data ---------------------------------------------------------------
hh_data <- hhdata_dem |>
  full_join(hh_distwaterways,by = c("hhid")) |>
  full_join(hh_temp_variables,by = c("hhid")) |>
  full_join(hh_esi,by = c("hhid")) |>
  full_join(hh_shocks_vegetationindices,by = c("hhid")) |>
  dplyr::select(-c(agglom_id.y,agglom_id.x.x,agglom_id.y.y)) |>
  rename("agglom_id" = "agglom_id.x") |>
  relocate(agglom_id,.after = hhid)

#replace "" in agglom_id with NA
hh_data <- hh_data |>
  mutate(agglom_id = if_else(agglom_id == "",NA_character_,agglom_id))

#for ESI,NDVI,EVI var.: turn "avg" into "mean"
hh_data <- hh_data |>
  rename_with(
    ~ str_replace(., "avg", "mean"),
    .cols = c(
      temp_winterdaysabove30_avg_pretreatment:temp_winterdaysabove30_n_20202024,
      utci_extremeheatstress_avg_pretreatment:utci_extremeheatstress_n_20202024,
      esi_mean_summer_pretreatment:last_col())
  )

#shorten "winterdays"
hh_data <- hh_data |>
  rename_with(
    ~ str_replace(., "winterdaysabove30", "wdays30"),
    .cols = c(
      temp_shockdays_summer_pretreatment:last_col())
  )

#rename col. names from "pretreatment" to "pre" (for STATA)
hh_data <- hh_data |>
  rename_with(
    ~ str_replace(., "pretreatment", "pre"),
    .cols = temp_shockdays_summer_pretreatment:last_col()
  )

#Shorten variable names for UTCI Heatstress variable
hh_data <- hh_data |>
  rename_with(
    ~ str_replace(., "utci_extremeheatstress", "utci_heatstress"),
    .cols = utci_extremeheatstress_mean_pre:utci_extremeheatstress_n_20202024
  )

#Shorten UTCI Max
hh_data <- hh_data |>
  rename_with(
    ~ str_replace(., "utcimax", "utci"),
    .cols = utcimax_shockdays_summer_pre:utcimax_shockdays_winter_20202024
  )

#Add variable labels
hh_data_dta <- hh_data |>
  set_variable_labels(
    slope = "Slope in degrees",
    elevation_dem = "Elevation in metres above sea level",
    distance_nile_metres = "Distance to Nile in metres (as the crow flies)",
    distance_waterway_metres = "Distance to nearest waterway in metres (as the crow flies)",
    temp_shockdays_summer_pre = "No. of summer days where temperature 2SD from hist. (2000-2015) monthly mean",
    temp_shockdays_summer_20152019 = "No. of summer days where temperature 2SD from hist. (2000-2015) monthly mean",
    temp_shockdays_summer_20202024 = "No. of summer days where temperature 2SD from hist. (2000-2015) monthly mean",
    temp_shockdays_summer_20152024 = "No. of summer days where temperature 2SD from hist. (2000-2015) monthly mean",
    temp_shockdays_winter_pre = "No. of winter days where temperature 2SD from hist. (2000-2015) monthly mean",
    temp_shockdays_winter_20152019 = "No. of winter days where temperature 2SD from hist. (2000-2015) monthly mean",
    temp_shockdays_winter_20202024 = "No. of winter days where temperature 2SD from hist. (2000-2015) monthly mean",
    temp_shockdays_winter_20152024 = "No. of winter days where temperature 2SD from hist. (2000-2015) monthly mean",
    utci_shockdays_summer_pre = "No. of summer days where UTCI 2SD from hist. (2000-2015) monthly mean",
    utci_shockdays_winter_pre = "No. of winter days where UTCI 2SD from hist. (2000-2015) monthly mean",
    utci_shockdays_summer_20152019 = "No. of summer days where UTCI 2SD from hist. (2015-2019) monthly mean",
    utci_shockdays_winter_20152019 = "No. of winter days where UTCI 2SD from hist. (2015-2019) monthly mean",
    utci_shockdays_summer_20152024 = "No. of summer days where UTCI 2SD from hist. (2015-2024) monthly mean",
    utci_shockdays_winter_20152024 = "No. of winter days where UTCI 2SD from hist. (2015-2024) monthly mean",
    utci_shockdays_summer_20202024 = "No. of summer days where UTCI 2SD from hist. (2020-2024) monthly mean",
    utci_shockdays_winter_20202024 = "No. of winter days where UTCI 2SD from hist. (2020-2024) monthly mean",
    temp_avgmean_summer_pre = "Avg. daily mean temperature (2000-2019) in summer",
    temp_avgmean_summer_20152019 = "Avg. daily mean temperature (2015-2019) in summer",
    temp_avgmean_summer_20202024 = "Avg. daily mean temperature (2020-2024) in summer",
    temp_avgmean_summer_20152024 = "Avg. daily mean temperature (2015-2024) in summer",
    temp_avgmean_winter_pre = "Avg. daily mean temperature (2000-2019) in winter",
    temp_avgmean_winter_20152019 = "Avg. daily mean temperature (2015-2019) in winter",
    temp_avgmean_winter_20202024 = "Avg. daily mean temperature (2020-2024) in winter",
    temp_avgmean_winter_20152024 = "Avg. daily mean temperature (2015-2024) in winter",
    temp_mean_growth_summer  = "Growth in Avg. daily mean temperature (2015-2019 to
    2020-2024) in summer",
    temp_mean_growth_winter  = "Growth in Avg. daily mean temperature (2015-2019 to
    2020-2024) in winter",
    temp_heatwave_days_avg_pre = "Avg. no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_days_avg_20152019 = "Avg. no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_days_avg_20202024 = "Avg. no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_days_avg_20152024 = "Avg. no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_days_n_pre  = "Total no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_days_n_20152019  = "Total no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_days_n_20202024  = "Total no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_days_n_20152024  = "Total no. of yearly heatwave days (+85th perc. of hist. summer temp.)",
    temp_heatwave_length_pre = "Avg. length of heatwave season (days from first to last heatwave day)",
    temp_heatwave_length_20152019 = "Avg. length of heatwave season (days from first to last heatwave day)",
    temp_heatwave_length_20202024 = "Avg. length of heatwave season (days from first to last heatwave day)",
    temp_heatwave_length_20152024 = "Avg. length of heatwave season (days from first to last heatwave day)",
    temp_wdays30_mean_pre = "Avg. no. of winter days with max.temp above 30 (2000-2019)",
    temp_wdays30_mean_20152019 = "Avg. no. of winter days with max.temp above 30",
    temp_wdays30_mean_20202024 = "Avg. no. of winter days with max.temp above 30",
    temp_wdays30_mean_20152024 = "Avg. no. of winter days with max.temp above 30",
    temp_wdays30_n_pre = "Total no. of winter days with max. temp above 30 (2000-2019)",
    temp_wdays30_n_20152019 = "Total no. of winter days with max. temp above 30",
    temp_wdays30_n_20202024 = "Total no. of winter days with max. temp above 30",
    temp_wdays30_n_20152024 = "Total no. of winter days with max. temp above 30",
    temp_sd_max_summer_pre = "SD of max. daily temp (Summers 2000-2019)",
    temp_sd_max_summer_20152019 = "SD of max. daily temp (Summers 2015-2019)",
    temp_sd_max_summer_20202024 = "SD of max. daily temp (Summers 2020-2024)",
    temp_sd_max_summer_20152024 = "SD of max. daily temp (Summers 2015-2024)",
    temp_sd_max_winter_pre = "SD of max. daily temp (Winters 2000-2019)",
    temp_sd_max_winter_20152019 = "SD of max. daily temp (Winters 2015-2019)",
    temp_sd_max_winter_20202024 = "SD of max. daily temp (Winters 2020-2024)",
    temp_sd_max_winter_20152024 = "SD of max. daily temp (Winters 2015-2024)",
    temp_sd_min_summer_pre= "SD of min. daily temp (Summers 2000-2019)",
    temp_sd_min_summer_20152019 = "SD of min. daily temp (Summers 2015-2019)",
    temp_sd_min_summer_20202024 = "SD of min. daily temp (Summers 2020-2024)",
    temp_sd_min_summer_20152024 = "SD of min. daily temp (Summers 2015-2024)",
    temp_sd_min_winter_pre = "SD of min. daily temp (Winters 2000-2019)",
    temp_sd_min_winter_20152019 = "SD of min. daily temp (winters 2015-2019)",
    temp_sd_min_winter_20202024 = "SD of min. daily temp (winters 2020-2024)",
    temp_sd_min_winter_20152024 = "SD of min. daily temp (winters 2015-2024)",
    temp_sd_mean_summer_pre = "SD of mean. daily temp (Summers 2000-2019)",
    temp_sd_mean_summer_20152019 = "SD of mean. daily temp (Summers 2015-2019)",
    temp_sd_mean_summer_20202024 = "SD of mean. daily temp (Summers 2020-2024)",
    temp_sd_mean_summer_20152024 = "SD of mean. daily temp (Summers 2015-2024)",
    temp_sd_mean_winter_pre = "SD of mean. daily temp (Winters 2000-2019)",
    temp_sd_mean_winter_20152019 = "SD of mean. daily temp (winters 2015-2019)",
    temp_sd_mean_winter_20202024 = "SD of mean. daily temp (winters 2020-2024)",
    temp_sd_mean_winter_20152024 = "SD of mean. daily temp (winters 2015-2024)",
    utci_heatstress_mean_pre = "Avg. no. of days with UTCI above 46 (2000-2019)",
    utci_heatstress_n_pre = "Total no. of days with UTCI above 46 (2000-2019)",
    utci_heatstress_mean_20152019 = "Avg. no. of days with UTCI above 46 (2015-2019)",
    utci_heatstress_n_20152019 = "Total no. of days with UTCI above 46 (2015-2019)",
    utci_heatstress_mean_20152024 = "Avg. no. of days with UTCI above 46 (2015-2024)",
    utci_heatstress_n_20152024 = "Total no. of days with UTCI above 46 (2015-2024)",
    utci_heatstress_mean_20202024 = "Avg. no. of days with UTCI above 46 (2020-2024)",
    utci_heatstress_n_20202024 = "Total no. of days with UTCI above 46 (2020-2024)",
    esi_mean_summer_pre = "Mean. ESI value summer (2000-2019)",
    esi_mean_summer_20152019 = "mean. ESI value summer (2015-2019)",
    esi_mean_summer_20202024 = "mean. ESI value summer (2020-2024)",
    esi_mean_summer_20152024 = "mean. ESI value summer (2015-2024)",
    esi_mean_winter_pre = "Mean. ESI value winter (2000-2019)",
    esi_mean_winter_20152019 = "mean. ESI value winter (2015-2024)",
    esi_mean_winter_20202024 = "mean. ESI value winter (2015-2024)",
    esi_mean_winter_20152024 = "mean. ESI value winter (2015-2024)",
    esi_sd_summer_pre = "sd. ESI value summer (2000-2019)",
    esi_sd_summer_20152019 = "sd. ESI value summer (2015-2019)",
    esi_sd_summer_20202024 = "sd. ESI value summer (2020-2024)",
    esi_sd_summer_20152024 = "sd. ESI value summer (2015-2024)",
    esi_sd_winter_pre = "sd. ESI value winter (2000-2019)",
    esi_sd_winter_20152019 = "sd. ESI value winter (2015-2019)",
    esi_sd_winter_20202024 = "sd. ESI value winter (2015-2024)",
    esi_sd_winter_20152024 = "sd. ESI value winter (2015-2024)",
    esi_mean_growth_summer = "Growth in mean. ESI Summer (2015-2019 to 2020-2024)",
    esi_mean_growth_winter = "Growth in mean. ESI Winter (2015-2019 to 2020-2024)",
    esi_mdrought_summer_pre = "Total no. of moderate drought (ESI < -1) days (Summers 2000-2019)",
    esi_mdrought_summer_20152019 = "Total no. of moderate drought (ESI < -1) days (Summers 2015-2019)",
    esi_mdrought_summer_20202024 = "Total no. of moderate drought (ESI < -1) days (Summers 2020-2024)",
    esi_mdrought_summer_20152024 = "Total no. of moderate drought (ESI < -1) days (Summers 2015-2024)",
    esi_mdrought_winter_pre = "Total no. of moderate drought (ESI < -1) days (Winters 2000-2019)",
    esi_mdrought_winter_20152019 = "Total no. of moderate drought (ESI < -1) days (Winters 2015-2019)",
    esi_mdrought_winter_20202024 = "Total no. of moderate drought (ESI < -1) days (Winters 2020-2024)",
    esi_mdrought_winter_20152024 = "Total no. of moderate drought (ESI < -1) days (Winters 2015-2024)",
    esi_sdrought_summer_pre = "Total no. of severe drought (ESI < -2) days (Summers 2000-2019)",
    esi_sdrought_summer_20152019 = "Total no. of severe drought (ESI < -2) days (Summers 2015-2019)",
    esi_sdrought_summer_20202024 = "Total no. of severe drought (ESI < -2) days (Summers 2020-2024)",
    esi_sdrought_summer_20152024 = "Total no. of severe drought (ESI < -2) days (Summers 2015-2024)",
    esi_sdrought_winter_pre = "Total no. of severe drought (ESI < -2) days (Winters 2000-2019)",
    esi_sdrought_winter_20152019 = "Total no. of severe drought (ESI < -2) days (Winters 2015-2019)",
    esi_sdrought_winter_20202024 = "Total no. of severe drought (ESI < -2) days (Winters 2020-2024)",
    esi_sdrought_winter_20152024 = "Total no. of severe drought (ESI < -2) days (Winters 2015-2024)",
    ndvi_mean_summer_pre = "mean. NDVI value summer (2000-2019)",
    ndvi_mean_summer_20152019 = "mean. NDVI value summer (2015-2019)",
    ndvi_mean_summer_20202024 = "mean. NDVI value summer (2020-2024)",
    ndvi_mean_summer_20152024 = "mean. NDVI value summer (2015-2024)",
    ndvi_mean_winter_pre = "mean. NDVI value winter (2000-2019)",
    ndvi_mean_winter_20152019 = "mean. NDVI value winter (2015-2019)",
    ndvi_mean_winter_20202024 = "mean. NDVI value winter (2015-2024)",
    ndvi_mean_winter_20152024 = "mean. NDVI value winter (2015-2024)",
    ndvi_mean_growth_summer = "Growth in mean. NDVI Summer (2015-2019 to 2020-2024)",
    ndvi_mean_growth_winter = "Growth in mean. NDVI Winter (2015-2019 to 2020-2024)",
    ndvi_shockmonths_summer_pre= "No. of NDVI shock months (NDVI -2SD from hist. mean) (Summers 2000-2019)",
    ndvi_shockmonths_summer_20152019 = "No. of NDVI shock months (NDVI -2SD from hist. mean) (Summers 2015-2019)",
    ndvi_shockmonths_summer_20202024 = "No. of NDVI shock months (NDVI -2SD from hist. mean) (Summers 2020-2024)",
    ndvi_shockmonths_summer_20152024 = "No. of NDVI shock months (NDVI -2SD from hist. mean) (Summers 2015-2024)",
    ndvi_shockmonths_winter_pre = "No. of NDVI shock months (NDVI -2SD from hist. mean) (Winters 2000-2019)",
    ndvi_shockmonths_winter_20152019 = "No. of NDVI shock months (NDVI -2SD from hist. mean) (Winters 2015-2019)",
    ndvi_shockmonths_winter_20202024 = "No. of NDVI shock months (NDVI -2SD from hist. mean) (Winters 2020-2024)",
    ndvi_shockmonths_winter_20152024 = "No. of NDVI shock months (NDVI -2SD from hist. mean) (Winters 2015-2024)",
    evi_mean_summer_pre = "mean. EVI value summer (2000-2019)",
    evi_mean_summer_20152019 = "mean. EVI value summer (2015-2019)",
    evi_mean_summer_20202024 = "mean. EVI value summer (2020-2024)",
    evi_mean_summer_20152024 = "mean. EVI value summer (2015-2024)",
    evi_mean_winter_pre = "mean. EVI value winter (2000-2019)",
    evi_mean_winter_20152019 = "mean. EVI value winter (2015-2019)",
    evi_mean_winter_20202024 = "mean. EVI value winter (2015-2024)",
    evi_mean_winter_20152024 = "mean. EVI value winter (2015-2024)",
    evi_mean_growth_summer = "Growth in mean. EVI Summer (2015-2019 to 2020-2024)",
    evi_mean_growth_winter = "Growth in mean. EVI Winter (2015-2019 to 2020-2024)",
    evi_shockmonths_summer_pre = "No. of EVI shock months (EVI -2SD from hist. mean)
    (Summers 2000-2019)",
    evi_shockmonths_summer_20152019 = "No. of EVI shock months (EVI -2SD from hist. mean) (Summers 2015-2019)",
    evi_shockmonths_summer_20202024 = "No. of EVI shock months (EVI -2SD from hist. mean) (Summers 2020-2024)",
    evi_shockmonths_summer_20152024 = "No. of EVI shock months (EVI -2SD from hist. mean) (Summers 2015-2024)",
    evi_shockmonths_winter_pre = "No. of EVI shock months (EVI -2SD from hist. mean) (Winters 2000-2019)",
    evi_shockmonths_winter_20152019 = "No. of EVI shock months (EVI -2SD from hist. mean) (Winters 2015-2019)",
    evi_shockmonths_winter_20202024 = "No. of EVI shock months (EVI -2SD from hist. mean) (Winters 2020-2024)",
    evi_shockmonths_winter_20152024 = "No. of EVI shock months (EVI -2SD from hist. mean) (Winters 2015-2024)")

# Save Data ---------------------------------------------------------------
saveRDS(hh_data, file = paste(here(),"Data","final",
                 "hh_data_v09012025.Rds",sep = "/"))
haven::write_dta(hh_data_dta,path = paste(here(),"Data","final",
                 "hh_data_v09012025.dta",sep = "/"))



