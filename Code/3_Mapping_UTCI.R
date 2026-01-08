#Comparions of UTCI with Temp. values
#Read in Packages --------------------------------------------------------
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

#Load Data ---------------------------------------------------------------
####Households####
hh_data_imputedgps <- readRDS(file = paste(here(),"Data",
                                           "hhdata_imputedgps_fullsample.rds",sep = "/"))

# Convert to sf object
hh_sf <- hh_data_imputedgps |>
  #filter out the 208 households without GPS location/could also not be imputed
  filter(!is.na(hh_gpslongitude_imputed),
         !is.na(hh_gpslatitude_imputed)) |>
  #turn into SF object
  st_as_sf(coords = c("hh_gpslongitude_imputed", "hh_gpslatitude_imputed"),
           crs = 4326, remove = FALSE)

####Shapefiles####
#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#Select relevant governorates
egypt_gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag" ,"Alexandria","Cairo"))

#2govs only
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

####Gov. level####
utci_govs_long <- readRDS(file = paste(here(),
                          "Data", "UTCI","UTCI_daily",
                          "utci_govpanel_20002024",sep = "/"))

#Load temp. data for Alexandria, Cairo, Assiut and Sohag
era5_temp_df <- readRDS(file = paste(here(),"Data",
                                     "Temperature","ERA5","Panel",
                                     "era5_temp_19602024",sep = "/"))

####Raster: UTCI####
utci_daily_max_rast <- terra::rast(
                   paste(here(),"Data", "UTCI","UTCI_daily",
                  "utci_dailymax_rast_20002024.tif",sep = "/"))

####Raster: Measured temp.####
#Max temp.
#Load Daily max raster (1960 - 2024)
temp_era5_dailymax_19602024 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                                 "temp_era5_dailymax_19602024.tif",sep = "/"))

#check time dimension
time_info <- time(temp_era5_dailymax_19602024)

# subset only the 2000s
temp_era5_dailymax_2000s <- temp_era5_dailymax_19602024[[time_info >=
                                                           as.Date("2000-01-01")]]

#turn into Celsius
temp_era5_dailymax_2000s <- temp_era5_dailymax_2000s-273.15

# Time Series -------------------------------------------------------------
#filter ERA5 temp. df
era5_temp_df_filt <- era5_temp_df |>
  filter(date >= "2000-01-01")

#join
era5_joined <- era5_temp_df_filt |>
  left_join(utci_govs_long,by = c("name" = "ADM1_EN","date"))

#plot time series UTCI for Suhag
era5_joined |>
  filter(date > "2024-01-01") |>
  dplyr::select(name,date,mean_maxtemp,utci_dailymax) |>
  pivot_longer(cols = c(mean_maxtemp,utci_dailymax),
               names_to = "temp_measure",
               values_to = "value") |>
  ggplot(aes(x = date,y = value,group = temp_measure,color = temp_measure))+
  geom_line()+
  facet_wrap(~name)

#correlation between both measures
era5_joined |>
  group_by(name) |>
  summarise(cor = cor(mean_maxtemp,utci_dailymax))
#corr. of 0.93 and above

#Mapping -----------------------------------------------------------------
#Harmonise spatial grid
temp_dailymax_utcigrid <- resample(
  temp_era5_dailymax_2000s,
  utci_daily_max_rast,
  method = "bilinear")

#compute daily difference (UTCI - T)
utci_minus_t <- utci_daily_max_rast - temp_dailymax_utcigrid
names(utci_minus_t) <- names(utci_daily_max_rast)

####Abs. difference####
#Identify where abs. diff. is largest
utci_gap_p95 <- app(
  abs(utci_minus_t),
  quantile,
  probs = 0.95,
  na.rm = TRUE)

names(utci_gap_p95) <- "P95 |UTCI − T| (°C)"

#plot
plot(
  utci_gap_p95,
  main = "Areas where UTCI diverges most from measured temperature\nP95 of |UTCI − T| (°C)"
)
plot(egypt_gov,add = TRUE,color = "black")

####Pos./neg. differences####
utci_gap_mean <- app(
  utci_minus_t,
  mean,
  na.rm = TRUE
)

names(utci_gap_mean) <- "Mean UTCI − T (°C)"

plot(
  utci_gap_mean,
  main = "Where UTCI exceeds temperature (positive) \nvs. falls below (negative)"
)
plot(egypt_gov,add = TRUE,color = "black")

####Seasonal decomposition####
dates <- time(utci_minus_t)

season <- case_when(
  month(dates) %in% c(12, 1, 2) ~ "Winter",
  month(dates) %in% c(3, 4, 5)  ~ "Spring",
  month(dates) %in% c(6, 7, 8)  ~ "Summer",
  month(dates) %in% c(9, 10,11) ~ "Autumn"
)

#split raster by season
utci_by_season <- lapply(
  unique(season),
  function(s) utci_minus_t[[season == s]]
)

names(utci_by_season) <- unique(season)

#seasonal mean difference
utci_gap_mean_seasonal <- lapply(
  utci_by_season,
  app,
  fun = mean,
  na.rm = TRUE
)

#combine into one object
utci_gap_mean_seasonal <- rast(utci_gap_mean_seasonal)
names(utci_gap_mean_seasonal) <- names(utci_by_season)

#plot
lims <- global(utci_gap_mean_seasonal, range, na.rm = TRUE)

plot(
  utci_gap_mean_seasonal,
  nc = 2,
  zlim = lims,
  main = names(utci_gap_mean_seasonal)
)

####Assiut & Suhag####
#spat vector
egypt_2gov_v <- vect(egypt_2gov)

#crop the raster
crs(egypt_2gov_v) <- crs(utci_gap_mean)

utci_gap_mean_2gov <- utci_gap_mean |>
  crop(egypt_2gov_v) |>
  mask(egypt_2gov_v)

#####mean#####
#plot with outline of Governorates
plot(
  utci_gap_mean_2gov,
  main = "Assiut & Suhag: Gap between UTCI and measured Temperature")
plot(egypt_2gov,add = TRUE,color = "black")
plot(hh_sf,add = TRUE)

#####seasonal#####
#seasonal
utci_gap_mean_seasonal_2gov <- utci_gap_mean_seasonal |>
  crop(egypt_2gov) |>
  mask(egypt_2gov)

plot(
  utci_gap_mean_seasonal_2gov,
  nc = 2,
  zlim = lims,
  main = names(utci_gap_mean_seasonal)
)

