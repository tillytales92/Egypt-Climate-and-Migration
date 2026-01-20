#Combining ERA 5 Temperature Rasters for different decades
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("here","terra")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load ERA5 Climate Data Rasters------------------------------------------------
#Loading the Raster Stack: Data was downloaded in 1_ERA5download & through GEE (daily min)
####Air Temperature####
#####Load Mean Daily Rasters, combine and save#####
#1960 raster
egypt_temp_1960 <- terra::rast(paste(here(),"Data", "raw","ERA5",
                                     "egypt_era_temp_1960_decade.tif",sep = "/"))

#1970 raster
egypt_temp_1970 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                     "egypt_era_temp_1970_decade.tif",sep = "/"))

#1980 raster
egypt_temp_1980 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                     "egypt_era_temp_1980_decade.tif",sep = "/"))

#1990 raster
egypt_temp_1990 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                     "egypt_era_temp_1990decade.tif",sep = "/"))

#2000 raster
egypt_temp_2000 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                     "egypt_era_temp_2000decade.tif",sep = "/"))

#2010 raster
egypt_temp_2010 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                     "egypt_era_temp_2010decade.tif",sep = "/"))

#2020 raster
egypt_temp_2020 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                     "egypt_era_temp_2020decade.tif",sep = "/"))

#combine all raster stacks (1960 - 2024)
temp_era5_dailymean_19602024 <- c(egypt_temp_1960,egypt_temp_1970,egypt_temp_1980,
                     egypt_temp_1990,egypt_temp_2000,egypt_temp_2010,
                     egypt_temp_2020)

#Turn from Fahrenheit to Celsius
temp_era5_dailymean_19602024 <- temp_era5_dailymean_19602024-273.15

#use dates as names for raster layers
names(temp_era5_dailymean_19602024) <- time(temp_era5_dailymean_19602024)

#Write combined raster file
terra::writeRaster(temp_era5_dailymean_19602024, filename =
                     paste(here(),"Data","intermediate","ERA5",
                           "temp_era5_dailymean_19602024.tif",sep = "/"))

#####Load Max Daily Rasters,combine and save########
temp_era5_dailymax_19601979 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                        "era5_MaxTemp_1960_1979.tif",sep = "/"))

temp_era5_dailymax_19802024 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                    "era5_MaxTemp_1980_2024.tif",sep = "/"))

#combine raster stacks (1960 - 2024)
temp_era5_dailymax_19602024 <- c(temp_era5_dailymax_19601979,
                                 temp_era5_dailymax_19802024)

#Turn from Fahrenheit to Celsius
temp_era5_dailymax_19602024 <- temp_era5_dailymax_19602024-273.15

#use dates as names for raster layers
names(temp_era5_dailymax_19602024) <- time(temp_era5_dailymax_19602024)

#Write combined raster
terra::writeRaster(temp_era5_dailymax_19602024,
                   filename =  paste(here(),"Data","intermediate","ERA5",
                   "temp_era5_dailymax_19602024.tif",sep = "/"))

#####Load Min Daily Rasters , combine and save########
#1960 raster
temp_era5_dailymin_19611970 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                     "era5_MinTemp_1961_1970.tif",sep = "/"))

#1970 raster
temp_era5_dailymin_19711980 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                                 "era5_MinTemp_1971_1980.tif",sep = "/"))

#1980 raster
temp_era5_dailymin_19811990 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                                 "era5_MinTemp_1981_1990.tif",sep = "/"))

#1990 raster
temp_era5_dailymin_19912000 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                                 "era5_MinTemp_1991_2000.tif",sep = "/"))

#2000 raster
temp_era5_dailymin_20012010 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                                 "era5_MinTemp_2001_2010.tif",sep = "/"))

#2010 raster
temp_era5_dailymin_20112020 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                                 "era5_MinTemp_2011_2020.tif",sep = "/"))

#2020 raster
temp_era5_dailymin_20212024 <- terra::rast(paste(here(),"Data","raw","ERA5",
                                                 "era5_MinTemp_2021_2024.tif",sep = "/"))

#combine all raster stacks (1961 - 2024)
temp_era5_dailymin_19612024 <- c(temp_era5_dailymin_19611970,temp_era5_dailymin_19711980,
                                 temp_era5_dailymin_19811990,temp_era5_dailymin_19912000,
                                 temp_era5_dailymin_20012010,temp_era5_dailymin_20112020,
                                 temp_era5_dailymin_20212024)

#Turn from Fahrenheit to Celsius
temp_era5_dailymin_19612024 <- temp_era5_dailymin_19612024-273.15

#use names as time dimension
time(temp_era5_dailymin_19612024) <- as.Date(names(temp_era5_dailymin_19612024))

#Write combined raster
terra::writeRaster(temp_era5_dailymin_19612024, filename =
                     paste(here(),"Data","intermediate","ERA5",
                           "temp_era5_dailymin_19612024.tif",sep = "/"))

#### Dew temperature ####
#####Mean Dew Temp.#####
#Load 2000-2010 Dew temp
era5_dewtemp_daily_2000s <- terra::rast(
  paste(here(), "Data","raw","ERA5",
        "era5_daily_dew_2000_2010.tif", sep = "/"))

#Load 2010-2020 Dew temp
era5_dewtemp_daily_2010s <- terra::rast(
  paste(here(), "Data","raw","ERA5",
        "era5_daily_dew_2011_2020.tif", sep = "/"))

#Load 2021-2024 Dew temp
era5_dewtemp_daily_2020s <- terra::rast(
  paste(here(), "Data","raw","ERA5",
        "era5_daily_dew_2021_2024.tif", sep = "/"))

#combine all raster stacks (2000-2024)
temp_era5_dailydewtemp_mean <- c(era5_dewtemp_daily_2000s,
                                 era5_dewtemp_daily_2010s,
                                 era5_dewtemp_daily_2020s)

#use names as time dimension
time(temp_era5_dailydewtemp_mean) <- as.Date(names(temp_era5_dailydewtemp_mean))

#Write combined raster
terra::writeRaster(temp_era5_dailydewtemp_mean, filename =
                     paste(here(),"Data","intermediate","ERA5",
                           "temp_era5_meandewtemp_20002024.tif",sep = "/"))

#####Max. Dew Temp.#####
#Load 2000-2010 Dew temp
era5_maxdewtemp_daily_2000s <- terra::rast(
  paste(here(),"Data","raw","ERA5",
        "era5_daily_maxdew_2000_2010.tif", sep = "/"))

#Load 2010-2020 Dew temp
era5_maxdewtemp_daily_2010s <- terra::rast(
  paste(here(),"Data","raw","ERA5",
        "era5_daily_maxdew_2011_2020.tif", sep = "/"))

#Load 2021-2024 Dew temp
era5_maxdewtemp_daily_2020s <- terra::rast(
  paste(here(),"Data","raw","ERA5",
        "era5_daily_maxdew_2021_2024.tif", sep = "/"))

#combine all raster stacks (2000-2024)
temp_era5_dailymaxdewtemp <- c(era5_maxdewtemp_daily_2000s,
                               era5_maxdewtemp_daily_2010s,
                               era5_maxdewtemp_daily_2020s)

#use names as time dimension
time(temp_era5_dailymaxdewtemp) <- as.Date(names(temp_era5_dailymaxdewtemp))

#Write combined raster
terra::writeRaster(temp_era5_dailymaxdewtemp, filename =
                     paste(here(),"Data","intermediate","ERA5",
                           "temp_era5_maxdewtemp_20002024.tif",sep = "/"))



