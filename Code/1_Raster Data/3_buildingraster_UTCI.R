#UTCI: Univ. Thermal Climate Index
#Thermal comfort indices derived from ERA5 reanalysis (download through CDS)
#Build raster stacks
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

#Load Climate Data Rasters------------------------------------------------
#Using daily rasters
# 1. List all files in folder
utci_files <- list.files(
  path = paste(here(), "Data", "raw", "UTCI",sep = "/"),
  full.names = TRUE)

# 2. Keep only files containing "daily_stats"
utci_daily_files <- utci_files[grepl("daily_stats", utci_files)]

# 3. Read into a single SpatRaster
utci_daily_rast <- terra::rast(utci_daily_files)

# 4. Keep only layers containing "utci_daily_max"
utci_daily_max_rast <- utci_daily_rast[[grep("utci_daily_max", names(utci_daily_rast))]]

# 5. Subset to layers before 2025
bef2025 <- time(utci_daily_max_rast) <= as.Date("2025-01-01")
#use 2025-01-01 since dates have time = UTC 11:30

utci_daily_max_rast <- utci_daily_max_rast[[bef2025]]

# 6. Split into two parts before celsius conversion
utci_daily_max_rast_p1 <- utci_daily_max_rast[[1:5000]]
utci_daily_max_rast_p2 <- utci_daily_max_rast[[5001:9132]]

# 6.1 Convert from Fahrenheit to Celsius
utci_daily_max_rast_p1 <- utci_daily_max_rast_p1-273.15
utci_daily_max_rast_p2 <- utci_daily_max_rast_p2-273.15

#merge together
utci_daily_max_rast <- c(utci_daily_max_rast_p1,utci_daily_max_rast_p2)

#rename the layers according to dates
dates <- time(utci_daily_max_rast)

names(utci_daily_max_rast) <- paste0("utci_", format(dates, "%Y-%m-%d"))

#test plot
plot(utci_daily_max_rast[[9132]])#December 31st, 2024
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

#Saving Raster Stack ----------------------------------------------------
terra::writeRaster(utci_daily_max_rast,
                   filename = paste(here(),"Data", "intermediate","UTCI",
                  "utci_dailymax_rast_20002024.tif",sep = "/"))




