#Comparions of Heat Indices with Temp. values
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

####Households####
hh_data_imputedgps <- readRDS(file = paste(here(),
                                           "Data","intermediate","Household Data",
                                           "hhdata_imputedgps.rds",sep = "/"))

# Convert to sf object
hh_sf <- hh_data_imputedgps |>
  #filter out the 208 households without GPS location/could also not be imputed
  filter(!is.na(hh_gpslongitude_imputed),
         !is.na(hh_gpslatitude_imputed)) |>
  #turn into SF object
  st_as_sf(coords = c("hh_gpslongitude_imputed", "hh_gpslatitude_imputed"),
           crs = 4326, remove = FALSE)

#Comparison: Heat index vs. measured temp. --------------------------------
#Max temp.
#Load Daily max raster (1960 - 2024)
temp_era5_dailymax_19602024 <- terra::rast(paste(here(),
                                                 "Data","intermediate","ERA5",
                                                 "temp_era5_dailymax_19602024.tif",sep = "/"))

#check time dimension
time_info <- time(temp_era5_dailymax_19602024)

# subset only the 2000s
temp_era5_dailymax_2000s <- temp_era5_dailymax_19602024[[time_info >=
                                                        as.Date("2000-01-01")]]

#turn into C
temp_era5_dailymax_2000s <- temp_era5_dailymax_2000s-273.15

#Load Max. Heat Index (2000 - 2024)
heatindex_dailymax <- terra::rast(paste(here(),"Data", "intermediate","ERA5",
                                        "heatindex_dailymax_2000_2024.tif",sep = "/"))

time_vector_hi <- as.Date(names(heatindex_dailymax))

# assign it as time
time(heatindex_dailymax) <- time_vector_hi

#assign proper dates to temp. layers
temp_dates <- time(heatindex_dailymax)
names(heatindex_dailymax) <- as.character(as.Date(temp_dates))

#harmonise spatial grid
temp_dailymax_c <- resample(
  temp_era5_dailymax_2000s,
  heatindex_dailymax,
  method = "bilinear")

#Compute daily difference (HI - T)
hi_minus_t <- heatindex_dailymax - temp_dailymax_c
names(hi_minus_t) <- names(heatindex_dailymax)

####Abs. difference####
#Identify where diff. is largest
hi_gap_p95 <- app(
  abs(hi_minus_t),
  quantile,
  probs = 0.95,
  na.rm = TRUE
)

names(hi_gap_p95) <- "P95 |HI − T| (°C)"

#Plot
plot(
  hi_gap_p95,
  main = "Areas where heat index diverges most from measured temperature\nP95 of |HI − T| (°C)"
)

####Pos./neg. differences####
hi_gap_mean <- app(
  hi_minus_t,
  mean,
  na.rm = TRUE
)

names(hi_gap_mean) <- "Mean HI − T (°C)"

plot(
  hi_gap_mean,
  main = "Where heat index exceeds temperature (positive) \nvs. dry cooling (negative)"
)

####Seasonal decomposition####
dates <- time(hi_minus_t)

season <- case_when(
  month(dates) %in% c(12, 1, 2) ~ "Winter"
  month(dates) %in% c(3, 4, 5)  ~ "Spring",
  month(dates) %in% c(6, 7, 8)  ~ "Summer",
  month(dates) %in% c(9, 10,11) ~ "Autumn"
)

#split raster by season
hi_by_season <- lapply(
  unique(season),
  function(s) hi_minus_t[[season == s]]
)

names(hi_by_season) <- unique(season)

#seasonal mean difference
hi_gap_mean_seasonal <- lapply(
  hi_by_season,
  app,
  fun = mean,
  na.rm = TRUE
)

#combine into one object
hi_gap_mean_seasonal <- rast(hi_gap_mean_seasonal)
names(hi_gap_mean_seasonal) <- names(hi_by_season)

#plot
lims <- global(hi_gap_mean_seasonal, range, na.rm = TRUE)

plot(
  hi_gap_mean_seasonal,
  nc = 2,
  zlim = lims,
  main = names(hi_gap_mean_seasonal)
)

####Zoom in: Assiut & Suhag####
#map this for Assiut & Suhag
#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#crop to Suhag & Assiut only
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

#spat vector
egypt_2gov_v <- vect(egypt_2gov)

#crop the raster
crs(egypt_2gov_v) <- crs(hi_gap_mean)

hi_gap_mean_2gov <- hi_gap_mean |>
  crop(egypt_2gov_v) |>
  mask(egypt_2gov_v)

#####mean#####
#plot with outline of Governorates
plot(
  hi_gap_mean_2gov,
  main = "Where heat index exceeds temperature (positive) \nvs. dry cooling (negative)")
plot(egypt_2gov,add = TRUE,color = "black")
plot(hh_sf,add = TRUE)

#####seasonal#####
hi_gap_mean_seasonal_2gov <- hi_gap_mean_seasonal |>
  crop(egypt_2gov) |>
  mask(egypt_2gov)

plot(
  hi_gap_mean_seasonal_2gov,
  nc = 2,
  zlim = lims,
  main = names(hi_gap_mean_seasonal)
)

