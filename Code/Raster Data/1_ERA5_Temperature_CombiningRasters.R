#Combining ERA 5 Temperature Rasters for different decades
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

# Load ERA5 Climate Data Rasters------------------------------------------------
#Loading the Raster Stack: Data was downloaded in 1_ERA5download & through GEE (daily min)
####Temperatures####
#####Load Mean Daily Rasters, combine and save#####
#1960 raster
egypt_temp_1960 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "egypt_era_temp_1960_decade.tif",sep = "/"))

#1970 raster
egypt_temp_1970 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "egypt_era_temp_1970_decade.tif",sep = "/"))

#1980 raster
egypt_temp_1980 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "egypt_era_temp_1980_decade.tif",sep = "/"))

#1990 raster
egypt_temp_1990 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "egypt_era_temp_1990decade.tif",sep = "/"))

#2000 raster
egypt_temp_2000 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "egypt_era_temp_2000decade.tif",sep = "/"))

#2010 raster
egypt_temp_2010 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "egypt_era_temp_2010decade.tif",sep = "/"))

#2020 raster
egypt_temp_2020 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "egypt_era_temp_2020decade.tif",sep = "/"))

#combine all raster stacks (1960 - 2024)
temp_era5_dailymean_19602024 <- c(egypt_temp_1960,egypt_temp_1970,egypt_temp_1980,
                     egypt_temp_1990,egypt_temp_2000,egypt_temp_2010,
                     egypt_temp_2020)

#Write combined raster
terra::writeRaster(temp_era5_dailymean_19602024, filename =
                     paste(here(),"Data", "Temperature","ERA5",
                           "temp_era5_dailymean_19602024.tif",sep = "/"))


#####Load Max Daily Rasters , combine and save########
temp_era5_dailymax_19601979 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                        "era5_MaxTemp_1960_1979.tif",sep = "/"))

temp_era5_dailymax_19802024 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                    "era5_MaxTemp_1981_2024.tif",sep = "/"))

#combine all raster stacks (1960 - 2024)
temp_era5_dailymax_19602024 <- c(temp_era5_dailymax_19601979,
                                 temp_era5_dailymax_19802024)

#Write combined raster
terra::writeRaster(temp_era5_dailymax_19602024, filename =
                     paste(here(),"Data", "Temperature","ERA5",
                           "temp_era5_dailymax_19602024.tif",sep = "/"))

#####Load Min Daily Rasters , combine and save########
#1960 raster
temp_era5_dailymin_19611970 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                     "era5_MinTemp_1961_1970.tif",sep = "/"))

#1970 raster
temp_era5_dailymin_19711980 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                                 "era5_MinTemp_1971_1980.tif",sep = "/"))

#1980 raster
temp_era5_dailymin_19811990 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                                 "era5_MinTemp_1981_1990.tif",sep = "/"))

#1990 raster
temp_era5_dailymin_19912000 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                                 "era5_MinTemp_1991_2000.tif",sep = "/"))

#2000 raster
temp_era5_dailymin_20012010 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                                 "era5_MinTemp_2001_2010.tif",sep = "/"))

#2010 raster
temp_era5_dailymin_20112020 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                                 "era5_MinTemp_2011_2020.tif",sep = "/"))

#2020 raster
temp_era5_dailymin_20212024 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                                 "era5_MinTemp_2021_2024.tif",sep = "/"))

#combine all raster stacks (1961 - 2024)
temp_era5_dailymin_19612024 <- c(temp_era5_dailymin_19611970,temp_era5_dailymin_19711980,
                                 temp_era5_dailymin_19811990,temp_era5_dailymin_19912000,
                                 temp_era5_dailymin_20012010,temp_era5_dailymin_20112020,
                                 temp_era5_dailymin_20212024)

#Write combined raster
terra::writeRaster(temp_era5_dailymin_19612024, filename =
                     paste(here(),"Data", "Temperature","ERA5",
                           "temp_era5_dailymin_19612024.tif",sep = "/"))


