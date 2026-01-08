#Impute Household GPS information where missing
#Import libraries --------------------------------------------------------
# libraries we need
libs <- c("elevatr", "terra", "tidyverse","here",
          "sf", "giscoR", "marmap","haven","naniar",
          "leaflet")
#elevatr allows access to several web services in search of raster elevation data
#includes Amazon Web Services Terrian Tiles and Open Topography global datasets API
#marmap provides a nice colour palette for topography

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

####Load Shapefile and reproject####
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

plot(st_geometry(egypt_hdx))

#filter for governorates
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

#Households
hh_data_new <- read_dta(paste(here(),"Data",
                          "Full Sample HH Data.dta",sep = "/"))

# Checks ------------------------------------------------------------------
#skim
skimr::skim(hh_data_new)
#3001 unique HH-ID; no missings, no empties --> GOOD!

#agglom_id: 109 uniques, has no NA but 68 empties ("") (implicit NAs)
hh_data_new |>
  filter(is.na(agglom_id) | agglom_id == "")

#replace "" with NA
hh_data_new <- hh_data_new |>
  mutate(agglom_id = if_else(agglom_id == "",NA_character_,agglom_id))

skimr::skim(hh_data_new)
#character vars. OK; govern & village_eng have 68 NA (same as agglom_id)
#look at those
hh_data_new |>
  filter(is.na(agglom_id)) |>
  miss_var_summary()
#do not have govern or village_eng; 100% NA for GPS lat, 50% NA for ml_GPS lat.

#Step 1: Use Male/Female GPS ---------------------------------------------
#get Household GPS from female GPS, if missing from male GPS
#First step: Calc. share of missing obs. in male and female GPS locations
miss_var_summary(hh_data_new)
#68% of male GPS missing; 52.9% of female GPS missing

#Test: If female GPS missing, how many missing male GPS
hh_data_new |>
  filter(is.na(gpslatitude)) |>
  miss_var_summary()
#87% missing male GPS if no female GPS

#vice versa
hh_data_new |>
  filter(is.na(ml_gpslatitude)) |>
  miss_var_summary()
#67.7% missing female GPS if no male GPS

#create Household GPS: use female GPS, but if missing use male GPS
hh_data_new <- hh_data_new |>
  mutate(
    hh_gpslatitude = if_else(is.na(gpslatitude),ml_gpslatitude,gpslatitude),
    hh_gpslongitude = if_else(is.na(gpslongitude),ml_gpslongitude,gpslongitude),
    hh_gpsaltitude = if_else(is.na(gpsaltitude),ml_gpsaltitude,gpsaltitude),
    hh_gpsaccuracy = if_else(is.na(gpsaccuracy),ml_gpsaccuracy,gpsaccuracy))

miss_var_summary(hh_data_new)
#reduces missing obs. share to 46%

#Step 2: Impute based on village & agglom_id ------------------------------
#where HH-GPS latitude is missing, imputed based on village & agglom_id
imputed_gps <- hh_data_new |>
  filter(!is.na(govern),
         !is.na(village_eng),
         !is.na(agglom_id)) |>#take out 68 HH without governorate,village,agglom. info
  group_by(govern,village_eng,agglom_id) |>
  mutate(
    # Flag whether gpslatitude was missing
    gps_imputed = is.na(hh_gpslatitude),
    # Impute missing values with group means
    across(
      hh_gpslatitude:hh_gpsaccuracy,
      ~ if_else(is.na(.x), median(.x, na.rm = TRUE), .x),
      .names = "{.col}_imputed")) |>
  ungroup() |>
  dplyr::select(hhid,govern,agglom_id,village_eng,gps_imputed:last_col())

#skim
skimr::skim(imputed_gps)
#gives a 94% complete rate for Household GPS locations

#check missings
print(imputed_gps |>
  filter(is.na(hh_gpslatitude_imputed)) |>
  miss_var_summary(), n = 25)
#they have governorate, agglom_id and village_eng info

#count
imputed_gps |>
  filter(is.na(hh_gpslatitude_imputed)) |>
  count(govern,village_eng,agglom_id) |>
  arrange(desc(n))

#check 17 households where no GPS info could be imputed
hh_data_new |>
  filter(govern == 2,village_eng == 8,agglom_id == 18) |>
  miss_var_summary()
#confirm that none of the 17 households has any GPS info -> no imputation possible

#Join HH Data with imputed GPS Data
hh_data <- hh_data_new |>
  left_join(imputed_gps,by = c("hhid","agglom_id","govern","village_eng"))

#some among the 68 HH without agglom,govern.,village info do have GPS info
#using those for the HH_x_imputed vars.
hh_data <- hh_data |>
  mutate(
    gps_imputed = is.na(hh_gpslatitude),
#fill in the GPS info for those with missing obs.
  hh_gpslatitude_imputed = if_else(is.na(hh_gpslatitude_imputed),hh_gpslatitude,
                                   hh_gpslatitude_imputed),
  hh_gpslongitude_imputed = if_else(is.na(hh_gpslongitude_imputed),hh_gpslongitude,
                                   hh_gpslongitude_imputed),
  hh_gpsaltitude_imputed  = if_else(is.na(hh_gpsaltitude_imputed),hh_gpsaltitude,
                                    hh_gpsaltitude_imputed),
  hh_gpsaccuracy_imputed = if_else(is.na(hh_gpsaccuracy_imputed),hh_gpsaccuracy,
                                   hh_gpsaccuracy_imputed))

skimr::skim(hh_data)

#change order of columns
hh_data <- hh_data |>
  relocate(c(hh_gpslatitude_imputed:hh_gpsaccuracy_imputed),
           .after = gpsaccuracy)

# Convert to sf object
hh_data_sf <- hh_data |>
  filter(!is.na(hh_gpslongitude_imputed),
         !is.na(hh_gpslatitude_imputed)) |>
  st_as_sf(coords = c("hh_gpslongitude_imputed",
                      "hh_gpslatitude_imputed"),
           crs = 4326, remove = FALSE)

# Leaflet Map -------------------------------------------------------------
# Ensure gps_imputed is a factor (for discrete colors)
hh_data_sf <- hh_data_sf |>
  mutate(gps_imputed = as.factor(gps_imputed))

# Define color palette
pal <- colorFactor(
  palette = "Set2",
  domain = hh_data_sf$gps_imputed
)

# Create a popup text combining governorate, district, and village info
hh_data_sf <- hh_data_sf |>
  mutate(
    popup_info = paste0(
      "<b>Governorate:</b> ", govern, "<br>",
      "<b>Village:</b> ", village_eng, "<br>",
      "<b>Agglomeration:</b> ", agglom_id, "<br>",
      "<b>GPS Imputed:</b> ", gps_imputed
    )
  )

# Build leaflet map
leaflet(hh_data_sf) |>
  addProviderTiles(providers$Esri.WorldImagery) |>  # Satellite basemap
  addCircleMarkers(
    radius = 3,
    color = ~pal(gps_imputed),
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~gps_imputed,
    popup = ~popup_info  #how governorate, district, village info
  ) |>
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~gps_imputed,
    title = "GPS Imputed",
    opacity = 1)

#all looks okay

# Merging Columns ---------------------------------------------------------
#select only hhid and agglomid for writing st object
hh_data_sel_sf <- hh_data_sf |>
  dplyr::select(hhid,agglom_id)

# Save Data ---------------------------------------------------------------
#Save HH Data
saveRDS(hh_data,
        file = paste(here(),"Data",
                     "hhdata_imputedgps_fullsample.rds",sep = "/"))

#save SF object
st_write(hh_data_sel_sf,
         dsn = paste(here(),"Data","Shapefiles",
                     "hh_imputedgps_fullsample.shp",sep = "/"))

