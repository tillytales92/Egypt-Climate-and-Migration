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
####Precipitation Data####
#####Load Mean Daily Rasters, combine and save#####
# #1960 raster
# egypt_temp_1960 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
#                                      "egypt_era_temp_1960_decade.tif",sep = "/"))
#
# #1970 raster
# egypt_temp_1970 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
#                                      "egypt_era_temp_1970_decade.tif",sep = "/"))

#1980 raster
egypt_prec_1980 <- terra::rast(paste(here(),"Data", "Precipitation","ERA5",
                                     "era5_Prec_1981_1990.tif",sep = "/"))

#1990 raster
egypt_prec_1990 <- terra::rast(paste(here(),"Data", "Precipitation","ERA5",
                                     "era5_Prec_1991_2000.tif",sep = "/"))

#2000 raster
egypt_prec_2000 <- terra::rast(paste(here(),"Data", "Precipitation","ERA5",
                                     "era5_Prec_2001_2010.tif",sep = "/"))

#2010 raster
egypt_prec_2010 <- terra::rast(paste(here(),"Data", "Precipitation","ERA5",
                                     "era5_Prec_2011_2020.tif",sep = "/"))

#2020 raster
egypt_prec_2020 <- terra::rast(paste(here(),"Data", "Precipitation","ERA5",
                                     "era5_Prec_2021_2024.tif",sep = "/"))

#combine all raster stacks (1960 - 2024)
prec_era5_dailysum_19812024 <- c(egypt_prec_1980,
                                 egypt_prec_1990,egypt_prec_2000,egypt_prec_2010,
                                 egypt_prec_2020)

#Write combined raster
terra::writeRaster(prec_era5_dailysum_19812024, filename =
                     paste(here(),"Data", "Precipitation","ERA5",
                           "prec_era5_dailysum_19812024.tif",sep = "/"))

