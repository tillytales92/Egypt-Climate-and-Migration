#Combine Temperature and Dew point temperature rasters
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools","lubridate",
          "terra","raster","geodata","sf","exactextractr",
          "patchwork","weathermetrics")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load Data ---------------------------------------------------------------
####Temperature####
#Load 2000-2010 temp
era5_temp_daily_2000s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_temp_2000_2010.tif", sep = "/"))

#Load 2010-2020 temp
era5_temp_daily_2010s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_temp_2011_2020.tif", sep = "/"))

#Load 2021-2024 temp
era5_temp_daily_2020s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_temp_2021_2024.tif", sep = "/"))

#combine all raster stacks (2000-2024)
temp_era5_dailytemp_mean <- c(era5_temp_daily_2000s,
                              era5_temp_daily_2010s,
                              era5_temp_daily_2020s)

#turn into celsius
temp_era5_dailytemp_mean <- temp_era5_dailytemp_mean - 273.15

####Daily Dew Temp.####
#Load 2000-2010 Dew temp
era5_dewtemp_daily_2000s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_dew_2000_2010.tif", sep = "/"))

#Load 2010-2020 Dew temp
era5_dewtemp_daily_2010s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_dew_2011_2020.tif", sep = "/"))

#Load 2021-2024 Dew temp
era5_dewtemp_daily_2020s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_dew_2021_2024.tif", sep = "/"))

#combine all raster stacks (2000-2024)
temp_era5_dailydewtemp_mean <- c(era5_dewtemp_daily_2000s,
                                 era5_dewtemp_daily_2010s,
                                 era5_dewtemp_daily_2020s)

#turn into celsius
temp_era5_dailydewtemp_mean <- temp_era5_dailydewtemp_mean - 273.15

# Check if dew < temp -----------------------------------------------------
#create raster to check if dewtemp always below measured temp.
dew_less_temp <- temp_era5_dailydewtemp_mean < temp_era5_dailytemp_mean

#.Dew point lower for all pixels on all days
global_all <- global(dew_less_temp, "min", na.rm = TRUE)
skimr::skim(global_all)#all are 1 --> means: true for all pixels for all days that
#dew point is lower than temp.--> GOOD!

# Convert to DF -----------------------------------------------------------
# Convert to data frame with coordinates
df_temp <- as.data.frame(temp_era5_dailytemp_mean, xy = TRUE)
df_dew  <- as.data.frame(temp_era5_dailydewtemp_mean, xy = TRUE)

df_temp_long <- df_temp |>
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "temp")

df_dew_long <- df_dew |>
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "dew")

#adjust date columns
df_temp_long <- df_temp_long |>
  mutate(
    date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}"), # extract YYYY-MM-DD
    date = ymd(date)                                  # convert to Date
  )

df_dew_long <- df_dew_long |>
  mutate(
    date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}"), # extract YYYY-MM-DD
    date = ymd(date)                                  # convert to Date
  )

#join both
df_all <- df_temp_long |>
  inner_join(df_dew_long, by = c("x", "y", "date"))

#Calculate Heat Index & Humidity ----------------------------------------------
#Use weathermetrics pck. to calculate Heat index
df_all_hi <- df_all |>
  mutate(
    HI_C = heat.index(
      t  = temp,
      dp = dew,
      temperature.metric = "celsius",
      output.metric = "celsius",
      round = 1
    ),
    rh = dewpoint.to.humidity(
      dp = dew,
      t = temp,
      temperature.metric = "celsius"
    ))

# Turn into Raster --------------------------------------------------------
#Create Daily Heat Index raster
#Pivot: one column per date
r_wide <- df_all_hi |>
  dplyr::select(x, y, date, HI_C) |>
  mutate(date = as.character(date)) |>
  pivot_wider(names_from = date, values_from = HI_C)

#Convert to SpatRaster
r_rast <- rast(r_wide, type = "xyz", crs = "EPSG:4326")

#Check spat raster
heatindex_dailymean <- r_rast

# Max. Temperature --------------------------------------------------------
####Temperature####
#Load 2000-2010 maxtemp
era5_maxtemp_daily_2000s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_maxtemp_2000_2010.tif", sep = "/"))

#Load 2010-2020 maxtemp
era5_maxtemp_daily_2010s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_maxtemp_2011_2020.tif", sep = "/"))

#Load 2021-2024 maxtemp
era5_maxtemp_daily_2020s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_maxtemp_2021_2024.tif", sep = "/"))

#combine all raster stacks (2000-2024)
temp_era5_dailymaxtemp <- c(era5_maxtemp_daily_2000s,
                            era5_maxtemp_daily_2010s,
                            era5_maxtemp_daily_2020s)

#turn into celsius
temp_era5_dailymaxtemp <- temp_era5_dailymaxtemp - 273.15

####Daily Dew Temp.####
#Load 2000-2010 Dew temp
era5_maxdewtemp_daily_2000s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_maxdew_2000_2010.tif", sep = "/"))

#Load 2010-2020 Dew temp
era5_maxdewtemp_daily_2010s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_maxdew_2011_2020.tif", sep = "/"))

#Load 2021-2024 Dew temp
era5_maxdewtemp_daily_2020s <- terra::rast(
  paste(here(), "Data", "Temperature", "ERA5","Heatindex",
        "era5_daily_maxdew_2021_2024.tif", sep = "/"))

#combine all raster stacks (2000-2024)
temp_era5_dailymaxdewtemp <- c(era5_maxdewtemp_daily_2000s,
                                 era5_maxdewtemp_daily_2010s,
                                 era5_maxdewtemp_daily_2020s)

#turn into celsius
temp_era5_dailymaxdewtemp <- temp_era5_dailymaxdewtemp - 273.15

# Check if dew < temp -----------------------------------------------------
#create raster to check if dewtemp always below measured temp.
maxdew_less_maxtemp <- temp_era5_dailymaxdewtemp < temp_era5_dailymaxtemp

#.Dew point lower for all pixels on all days
global_all_maxtemp <- global(maxdew_less_maxtemp, "min", na.rm = TRUE)
skimr::skim(global_all_maxtemp)#all are 1 --> means: true for all pixels for all days that
#dew point is lower than temp.--> GOOD!

# Convert to DF -----------------------------------------------------------
# Convert to data frame with coordinates
df_maxtemp <- as.data.frame(temp_era5_dailymaxtemp,xy = TRUE)
df_maxdew  <- as.data.frame(temp_era5_dailymaxdewtemp,xy = TRUE)

#pivot long
df_maxtemp_long <- df_maxtemp |>
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "temp")

df_maxdew_long <- df_maxdew |>
  pivot_longer(
    cols = -c(x, y),
    names_to = "date",
    values_to = "dew")

#adjust date columns
df_maxtemp_long <- df_maxtemp_long |>
  mutate(
    date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}"), # extract YYYY-MM-DD
    date = ymd(date)                                  # convert to Date
  )

df_maxdew_long <- df_maxdew_long |>
  mutate(
    date = str_extract(date, "\\d{4}-\\d{2}-\\d{2}"), # extract YYYY-MM-DD
    date = ymd(date)                                  # convert to Date
  )

#join both
df_all_maxtemp <- df_maxtemp_long |>
  inner_join(df_maxdew_long, by = c("x", "y", "date"))

#Calculate Heat Index & Humidity ----------------------------------------------
#Use weathermetrics pck. to calculate Heat index
df_all_maxtemp_hi <- df_all_maxtemp |>
  mutate(
    HI_C = heat.index(
      t  = temp,
      dp = dew,
      temperature.metric = "celsius",
      output.metric = "celsius",
      round = 1
    ),
    rh = dewpoint.to.humidity(
      dp = dew,
      t = temp,
      temperature.metric = "celsius"
    ))

# Turn into Raster --------------------------------------------------------
#Create Daily Heat Index raster
#Pivot: one column per date
r_wide_maxtemp <- df_all_maxtemp_hi |>
  dplyr::select(x, y, date, HI_C) |>
  mutate(date = as.character(date)) |>
  pivot_wider(names_from = date, values_from = HI_C)

#Convert to SpatRaster
heatindex_dailymax <- rast(r_wide_maxtemp, type = "xyz", crs = "EPSG:4326")

# Save Raster Stacks ------------------------------------------------------
terra::writeRaster(heatindex_dailymean,
                   filename = paste(here(),"Data", "Temperature","ERA5","Heatindex",
                   "heatindex_dailymean_2000_2024.tif",sep = "/"))

terra::writeRaster(heatindex_dailymax,
                    filename = paste(here(),"Data", "Temperature","ERA5","Heatindex",
                    "heatindex_dailymax_2000_2024.tif",sep = "/"))


