libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","exactextractr")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load ERA5 Climate Data Rasters------------------------------------------------
#Loading the Raster Stack: Data was downloaded in 1_ERA5download & through GEE (daily min)
####Precipitation Data####
#####Load Mean Daily Rasters, combine and save#####
#1980 raster
chirps_prec_1980 <- terra::rast(paste(here(),"Data", "raw","CHIRPS",
                                     "chirps_1981_1990_daily.tif",sep = "/"))

#1990 raster
chirps_prec_1990 <- terra::rast(paste(here(),"Data", "raw","CHIRPS",
                                      "chirps_1991_2000_daily.tif",sep = "/"))

#2000 raster
chirps_prec_2000 <- terra::rast(paste(here(),"Data", "raw","CHIRPS",
                                      "chirps_2001_2010_daily.tif",sep = "/"))

#2010 raster
chirps_prec_2010 <- terra::rast(paste(here(),"Data", "raw","CHIRPS",
                                      "chirps_2011_2020_daily.tif",sep = "/"))

#2020 raster
chirps_prec_2020 <- terra::rast(paste(here(),"Data", "raw","CHIRPS",
                                      "chirps_2021_2024_daily.tif",sep = "/"))

#combine all raster stacks (1960 - 2024)
prec_chirps_dailysum_19812024 <- c(chirps_prec_1980,
                                 chirps_prec_1990,chirps_prec_2000,chirps_prec_2010,
                                 chirps_prec_2020)

#Write combined raster
terra::writeRaster(prec_chirps_dailysum_19812024, filename =
                     paste(here(),"Data", "intermediate","CHIRPS",
                           "prec_chirps_dailysum_19812024.tif",sep = "/"))

