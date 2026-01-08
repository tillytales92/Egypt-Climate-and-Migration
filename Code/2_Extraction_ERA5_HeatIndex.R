#Heat Index (Temp. & Humidity)
#Heat Index calc. in "1_ERA5_heatindex_calculation"
#Build Governorate-level panel data
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
####Egypt GOV. shapefile####
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#Select relevant governorates
egypt_gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag" ,"Alexandria","Cairo"))

####Load Max. Heat Index (2000 - 2024) ####
heatindex_dailymax <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                        "Heatindex",
                                        "heatindex_dailymax_2000_2024.tif",sep = "/"))

#add hi_max to raster layer names
names(heatindex_dailymax) <- paste0("hi_max_", names(heatindex_dailymax))

####Load Mean Heat Index (2000 - 2024)####
heatindex_dailymean <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                        "Heatindex",
                                        "heatindex_dailymean_2000_2024.tif",sep = "/"))

#add hi_mean to raster layer names
names(heatindex_dailymean) <- paste0("hi_mean_", names(heatindex_dailymean))

####Extract Time Series####
#extract time series for Governorates
#####HI Daily Max#####
egypt_gov$gov_id <- seq_len(nrow(egypt_gov))

hi_govs_max <- terra::extract(
  heatindex_dailymax,
  vect(egypt_gov),
  fun = "mean",
  na.rm = TRUE,
  ID = FALSE)

#add gov. names
hi_govs_max <- bind_cols(
  tibble(gov_id = egypt_gov$gov_id,
         ADM1_EN = egypt_gov$ADM1_EN),
  as_tibble(hi_govs_max))

#turn into long format
hi_govs_max_long <- hi_govs_max |>
  dplyr::select(ADM1_EN, starts_with("hi_max_")) |>
  pivot_longer(
    cols = starts_with("hi_max_"),
    names_to = "date",
    values_to = "hi_dailymax"
  ) |>
  mutate(
    date = as.Date(sub("^hi_max_", "", date))
  ) |>
  arrange(ADM1_EN, date)

#####HI Daily mean#####
hi_govs_mean <- terra::extract(
  heatindex_dailymean,
  vect(egypt_gov),
  fun = "mean",
  na.rm = TRUE,
  ID = FALSE)

#add gov. names
hi_govs_mean <- bind_cols(
  tibble(gov_id = egypt_gov$gov_id,
         ADM1_EN = egypt_gov$ADM1_EN),
  as_tibble(hi_govs_mean))

#turn into long format
hi_govs_mean_long <- hi_govs_mean |>
  dplyr::select(ADM1_EN, starts_with("hi_mean_")) |>
  pivot_longer(
    cols = starts_with("hi_mean_"),
    names_to = "date",
    values_to = "hi_dailymean"
  ) |>
  mutate(
    date = as.Date(sub("^hi_mean_", "", date))
  ) |>
  arrange(ADM1_EN, date)

####Join####
heatindex_govs <- hi_govs_max_long |>
  left_join(hi_govs_mean_long,by = c("ADM1_EN","date"))

#Saving Gov. panel data --------------------------------------------------
####Panel Data####
#####R####
saveRDS(heatindex_govs,file = paste(here(),
                                    "Data", "UTCI","UTCI_daily",
                                    "heatindex_govpanel_20002024",sep = "/"))

#####CSV####
write_csv(heatindex_govs,file = paste(here(),
                                      "Data", "UTCI","UTCI_daily",
                                      "heatindex_govpanel_20002024.csv",sep = "/"))


