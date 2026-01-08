#UTCI: Univ. Thermal Climate Index
#Thermal comfort indices derived from ERA5 reanalysis (download through CDS)
#Build raster stacks & panel data (at Governorate level)
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

# Load Shapefiles ---------------------------------------------------------
#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#Select relevant governorates
egypt_gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag" ,"Alexandria","Cairo"))

#Load Climate Data Rasters------------------------------------------------
#Using hourly rasters
# #Loading the Raster Stack: Data was downloaded through Copernicus CDS
# #This is for 1st of Jan 2022, nlyr = 24 -> one for each hour of the day
# egypt_utci <- terra::rast(paste(here(),"Data","UTCI","UTCI_Egypt",
#         "ECMWF_utci_20220101_v1.1_con.area-subset.32.125.37.125.20.875.23.875.nc",sep = "/"))
#
# #plot Africa SPI
# plot(egypt_utci[[24]]-273.15)#24: UTCI at 23:00
# plot(st_geometry(egypt_hdx), add = TRUE, border = "red")
#
# #transform to celsius
# egypt_utci <- egypt_utci-273.15
#
# #calc. daily mean, max and min
# # Create grouping: all 24 hours belong to group 1 -> Warning message
# g <- rep(1, nlyr(egypt_utci))
#
# # Daily mean
# utci_daily_mean <- tapp(egypt_utci, g, fun = mean, na.rm = TRUE)
#
# # Daily max
# utci_daily_max  <- tapp(egypt_utci, g, fun = max, na.rm = TRUE)
#
# # Daily min
# utci_daily_min  <- tapp(egypt_utci, g, fun = min, na.rm = TRUE)
#
# #keep names
# names(utci_daily_mean) <- "UTCI_mean_2022-01-01"
# names(utci_daily_max)  <- "UTCI_max_2022-01-01"
# names(utci_daily_min)  <- "UTCI_min_2022-01-01"
#
# #plot mean UTCI for 1st of January 2022
# plot(utci_daily_mean)#24: UTCI at 23:00
# plot(st_geometry(egypt_hdx), add = TRUE, border = "red")
#
# #compare values for 1st of Jan with values from temp. raster
# #load ERA5 temp data:note, resolution finer (0.1,0.1)
# temp_era5_dailymean_19602024 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
#                                         "temp_era5_dailymean_19602024.tif",sep = "/"))
#
# tvec <- time(temp_era5_dailymean_19602024)
# idx <- which(as.Date(tvec) == "2022-01-01")
# temp_2022_01_01 <- temp_era5_dailymean_19602024[[idx]]
#
# plot(temp_2022_01_01-273.15)
# plot(st_geometry(egypt_hdx), add = TRUE, border = "red")
#
# #resample temp. to UTCI
# temp_resampled <- resample(
#   temp_2022_01_01,
#   utci_daily_mean,
#   method = "bilinear"
# )
#
# #turn into Celsius
# temp_resampled <- temp_resampled-273.15
#
# #plot
# plot(temp_resampled)
# plot(st_geometry(egypt_hdx), add = TRUE, border = "red")
#
# #correlation between both rasters (Temp. & UTCI)
# # mask NA in the same places
# r1 <- values(temp_resampled)
# r2 <- values(utci_daily_mean)
#
# # remove NA pairs
# ok <- complete.cases(r1, r2)
#
# correlation <- cor(r1[ok], r2[ok], method = "pearson")
# correlation
# #correlation of 0.51 between temp & UTCI

####Using daily rasters####
# 1. List all files in folder
utci_files <- list.files(
  path = paste(here(), "Data", "UTCI", "UTCI_daily","daily_temp",sep = "/"),
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

# Extracting Time Series --------------------------------------------------
#extract time series for Governorates
egypt_gov$gov_id <- seq_len(nrow(egypt_gov))

utci_govs <- terra::extract(
  utci_daily_max_rast,
  vect(egypt_gov),
  fun = "mean",
  ID = FALSE)

#add gov. names
utci_govs <- bind_cols(
  tibble(gov_id = egypt_gov$gov_id,
         ADM1_EN = egypt_gov$ADM1_EN),
  as_tibble(utci_govs)
)

#turn into long format
utci_govs_long <- utci_govs |>
  dplyr::select(ADM1_EN, starts_with("utci_")) |>
  pivot_longer(
    cols = starts_with("utci_"),
    names_to = "date",
    values_to = "utci_dailymax"
  ) |>
  mutate(
    date = as.Date(sub("^utci_", "", date))
  ) |>
  arrange(ADM1_EN, date)

#Saving Raster Stacks ----------------------------------------------------
terra::writeRaster(utci_daily_max_rast,
                   filename = paste(here(),"Data", "UTCI","UTCI_daily",
                  "utci_dailymax_rast_20002024.tif",sep = "/"))

#Saving Gov. panel data --------------------------------------------------
####Panel Data####
#####R####
saveRDS(utci_govs_long,file = paste(here(),
                                  "Data", "UTCI","UTCI_daily",
                                  "utci_govpanel_20002024",sep = "/"))

#####CSV####
write_csv(utci_govs_long,file = paste(here(),
                                    "Data", "UTCI","UTCI_daily",
                                    "utci_govpanel_20002024.csv",sep = "/"))


