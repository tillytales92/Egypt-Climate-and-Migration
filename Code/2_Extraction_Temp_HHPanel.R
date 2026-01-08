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

# Load Climate Data and HH Geodata-----------------------------------------
####Cropped raster stacks####
#Load Daily mean raster (1960 - 2024)
temp_era5_dailymean_19602024 <- terra::rast(paste(here(),"Data",
                                "Temperature","ERA5",
                                "temp_era5_dailymean_19602024.tif",sep = "/"))

#Load Daily max raster (1960 - 2024)
temp_era5_dailymax_19602024 <- terra::rast(paste(here(),"Data",
                                "Temperature","ERA5",
                                "temp_era5_dailymax_19602024.tif",sep = "/"))

#Load Daily min raster (1961 - 2024)
temp_era5_dailymin_19612024 <- terra::rast(paste(here(),"Data",
                                "Temperature","ERA5",
                                "temp_era5_dailymin_19612024.tif",sep = "/"))

#Load Mean Heat Index (2000 - 2024)
heatindex_dailymean <- terra::rast(paste(here(),"Data",
                                "Temperature","ERA5","Heatindex",
                                "heatindex_dailymean_2000_2024.tif",sep = "/"))

#Load Max. Heat Index (2000 - 2024)
heatindex_dailymax <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                "Heatindex",
                                "heatindex_dailymax_2000_2024.tif",sep = "/"))

#Load Max. UTCI (2000 - 2024)
utci_daily_max_rast <- terra::rast(paste(here(),"Data", "UTCI","UTCI_daily",
        "utci_dailymax_rast_20002024.tif",sep = "/"))

####Households####
hh_data_imputedgps <- readRDS(file = paste(here(),"Data",
                              "hhdata_imputedgps_fullsample.rds",sep = "/"))

#Convert to sf object
hh_sf <- hh_data_imputedgps |>
  #filter out the 208 households without GPS location/could also not be imputed
  filter(!is.na(hh_gpslongitude_imputed),
         !is.na(hh_gpslatitude_imputed)) |>
  #turn into SF object
  st_as_sf(coords = c("hh_gpslongitude_imputed", "hh_gpslatitude_imputed"),
           crs = 4326, remove = FALSE)

#####Mean Daily Temperature#####
#check time dimension
time_info <- time(temp_era5_dailymean_19602024)

# subset only the 1980s
temp_era5_dailymean_2000s <- temp_era5_dailymean_19602024[[time_info >=
                                                  as.Date("2000-01-01")]]

#adjust layer names
names(temp_era5_dailymean_2000s) <- format(time(temp_era5_dailymean_2000s), "%Y-%m-%d")

temp_era5_dailymean_2000s

# Extract temperature values at each point (in °C)
temp_values <- terra::extract(
  temp_era5_dailymean_2000s - 273.15,  # convert K to °C
  hh_sf,
  method = "simple"#nearest cell value
)

# Combine extracted values with the points’ data
egypt_temp_mean <- cbind(hh_sf, temp_values) |>
  st_drop_geometry() |>
  dplyr::select(hhid,agglom_id,X2000.01.01:last_col())

#pivot long
egypt_temp_mean_long <- egypt_temp_mean |>
  pivot_longer(
    cols = X2000.01.01:last_col(),
    names_to = "date",
    values_to = "mean_temp"
  )

#fix the date column
egypt_temp_mean_long <- egypt_temp_mean_long |>
  mutate(date = ymd(sub("^X", "", date)),
         year = year(date),
         month = month(date),
         day = day(date))

#####Max Daily Temperature#####
#check time dimension
time_info <- time(temp_era5_dailymax_19602024)

# subset only the 2000s
temp_era5_dailymax_2000s <- temp_era5_dailymax_19602024[[time_info >=
                                                      as.Date("2000-01-01")]]

#adjust layer names
names(temp_era5_dailymax_2000s) <- format(time(temp_era5_dailymax_2000s), "%Y-%m-%d")

temp_era5_dailymax_2000s

# Extract temperature values at each point (in °C)
temp_values_max <- terra::extract(
  temp_era5_dailymax_2000s - 273.15,  # convert K to °C
  hh_sf,
  method = "simple"#nearest cell value
)

# Combine extracted values with the points’ data
egypt_temp_max <- cbind(hh_sf, temp_values_max) |>
  st_drop_geometry() |>
  dplyr::select(hhid,agglom_id,X2000.01.01:last_col())

#pivot long
egypt_temp_max_long <- egypt_temp_max |>
  pivot_longer(
    cols = X2000.01.01:last_col(),
    names_to = "date",
    values_to = "max_temp")

#fix the date column
egypt_temp_max_long <- egypt_temp_max_long |>
  mutate(date = ymd(sub("^X", "", date)),
         year = year(date),
         month = month(date),
         day = day(date))

#####Min Daily Temperature#####
# create time vector from layer names
time_vector <- as.Date(names(temp_era5_dailymin_19612024))

# assign time correctly
time(temp_era5_dailymin_19612024) <- time_vector

# check
time_info <- time(temp_era5_dailymin_19612024)

# subset only the 2000s
temp_era5_dailymin_2000s <- temp_era5_dailymin_19612024[[time_info >=
                                                       as.Date("2000-01-01")]]

#adjust layer names
names(temp_era5_dailymin_2000s) <- format(time(temp_era5_dailymin_2000s),
                                          "%Y-%m-%d")

temp_era5_dailymin_2000s

# Extract temperature values at each point (in °C)
temp_values_min <- terra::extract(
  temp_era5_dailymin_2000s - 273.15,  # convert K to °C
  hh_sf,
  method = "simple"#nearest cell value
)

# Combine extracted values with the points’ data
egypt_temp_min <- cbind(hh_sf, temp_values_min) |>
  st_drop_geometry() |>
  dplyr::select(hhid,agglom_id,X2000.01.01:last_col())

#pivot long
egypt_temp_min_long <- egypt_temp_min |>
  pivot_longer(
    cols = X2000.01.01:last_col(),
    names_to = "date",
    values_to = "min_temp"
  )

#fix the date column
egypt_temp_min_long <- egypt_temp_min_long |>
  mutate(date = ymd(sub("^X", "", date)),
         year = year(date),
         month = month(date),
         day = day(date))

#####Heat Index: Daily mean#####
# create a time vector from names
time_vector <- as.Date(names(heatindex_dailymean))

# assign it as time
time(heatindex_dailymean) <- time_vector

# Extract temperature values at each point (in °C)
heatindex_mean <- terra::extract(
  heatindex_dailymean,
  hh_sf,
  method = "simple")#nearest cell value

# Combine extracted values with the points’ data
egypt_heatindex_mean <- cbind(hh_sf, heatindex_mean) |>
  st_drop_geometry() |>
  dplyr::select(hhid, agglom_id, X2000.01.01:last_col())

#pivot long
egypt_heatindex_mean_long <- egypt_heatindex_mean |>
  pivot_longer(
    cols = X2000.01.01:last_col(),
    names_to = "date",
    values_to = "hi_mean")

#Fix the date column
egypt_heatindex_mean_long <- egypt_heatindex_mean_long |>
  mutate(date = ymd(sub("^X", "", date)),
         year = year(date),
         month = month(date),
         day = day(date))

#####Heat Index: Daily max#####
# create a time vector from names
time_vector <- as.Date(names(heatindex_dailymax))

# assign it as time
time(heatindex_dailymax) <- time_vector

# Extract temperature values at each point (in °C)
heatindex_max <- terra::extract(
  heatindex_dailymax,
  hh_sf,
  method = "simple")#nearest cell value

# Combine extracted values with the points’ data
egypt_heatindex_max <- cbind(hh_sf, heatindex_max) |>
  st_drop_geometry() |>
  dplyr::select(hhid, agglom_id, X2000.01.01:last_col())

#pivot long
egypt_heatindex_max_long <- egypt_heatindex_max |>
  pivot_longer(
    cols = X2000.01.01:last_col(),
    names_to = "date",
    values_to = "hi_max")

#Fix the date column
egypt_heatindex_max_long <- egypt_heatindex_max_long |>
  mutate(date = ymd(sub("^X", "", date)),
         year = year(date),
         month = month(date),
         day = day(date))

#####UTCI: Daily max#####
# Extract temperature values at each point (in °C)
utci_max <- terra::extract(
  utci_daily_max_rast,
  hh_sf,
  method = "simple")#nearest cell value

# Combine extracted values with the points’ data
egypt_utci_max <- cbind(hh_sf, utci_max) |>
  st_drop_geometry() |>
  dplyr::select(hhid, agglom_id,`utci_2000.01.01`:last_col())

#pivot long
egypt_utci_max_long <- egypt_utci_max |>
  pivot_longer(
    cols = `utci_2000.01.01`:last_col(),
    names_to = "date",
    values_to = "utci_max")

#Fix the date column
egypt_utci_max_long <- egypt_utci_max_long |>
  mutate(
    date = ymd(gsub("^utci_|\\.", "-", date)),
    year = year(date),
    month = month(date),
    day = day(date))

#Join data ---------------------------------------------------------------
hh_temp_long <- egypt_temp_mean_long |>
  left_join(egypt_temp_max_long,by = c("hhid","agglom_id","date","year","month","day")) |>
  left_join(egypt_temp_min_long,by = c("hhid","agglom_id","date","year","month","day")) |>
  left_join(egypt_heatindex_mean_long,by = c("hhid","agglom_id","date","year","month","day")) |>
  left_join(egypt_heatindex_max_long,by = c("hhid","agglom_id","date","year","month","day")) |>
  left_join(egypt_utci_max_long,by = c("hhid","agglom_id","date","year","month","day"))

#reorder variables
hh_temp_long <- hh_temp_long |>
  relocate(c(year:day),.after = date)

#Save Data ---------------------------------------------------------------
saveRDS(hh_temp_long,
        file = paste(here(),"Data", "Temperature","ERA5",
                     "Panel","hh_era5_20002024.Rds",sep = "/"))

####Time Series for ex. HHID####
#show time series for examplary hhid
#mean temp vs. mean heat index (incl. dew temp.)
hh_temp_long |>
  dplyr::select(hhid,date,max_temp,hi_max,utci_max) |>
  filter(hhid == "11101",date >= "2023-01-01") |>
  pivot_longer(
    cols = c("max_temp","hi_max","utci_max"),
    names_to = "temp_measure",
    values_to = "value") |>
  ggplot(aes(x = date,y = value,group = temp_measure,
             colour = temp_measure))+
  geom_line(linewidth = 0.5) +
  labs(
    x = NULL,
    y = "Temperature (°C)",
    colour = NULL)

#7-day rolling average
hh_temp_long |>
  dplyr::select(hhid, date, max_temp, hi_max, utci_max) |>
  filter(hhid == "11101", date >= as.Date("2023-01-01")) |>
  pivot_longer(
    cols = c(max_temp, hi_max, utci_max),
    names_to = "temp_measure",
    values_to = "value"
  ) |>
  arrange(temp_measure, date) |>
  group_by(temp_measure) |>
  mutate(
    value = zoo::rollmean(
      value,
      k = 7,
      align = "right",
      fill = NA
    )
  ) |>
  ungroup() |>
  ggplot(aes(
    x = date,
    y = value,
    colour = temp_measure
  )) +
  geom_line(linewidth = 0.7) +
  labs(
    x = NULL,
    y = "7-day rolling mean temperature (°C)",
    colour = NULL
  )
