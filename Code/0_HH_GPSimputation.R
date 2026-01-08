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
hh_data <- read_dta(paste(here(),"Data",
            "Batch 1 and 2 HH Female Survey.dta",sep = "/"))

#hh select first 20 columns
hh_data_sel <- hh_data[1:20]

#do all HH have gov, district and village info?
hh_data_sel |>
  summarise(all_complete = all(!is.na(governorate) & !is.na(district) & !is.na(village)))
#Yes they do

#How many HH without GPS info
hh_data_sel |>
  filter(is.na(gpslatitude)) |>
  nrow()
#609 without GPS Latitude

hh_data_sel |>
  filter(is.na(gpslongitude)) |>
  nrow()
#609 without GPS Longitude

#impute based on governorate, district, village
hh_data_sel <- hh_data_sel |>
  group_by(governorate, district, village) |>
  mutate(
    # Flag whether gpslatitude was missing
    gps_imputed = is.na(gpslatitude),
    # Impute missing values with group means
    across(
      gpslatitude:gpsaltitude,
      ~ if_else(is.na(.x), mean(.x, na.rm = TRUE), .x),
      .names = "{.col}_imputed"
    )) |>
  ungroup()

#change order of columns
hh_data_sel <- hh_data_sel |>
  relocate(c(gps_imputed:gpsaltitude_imputed),
           .after = gpsaccuracy)

# Convert to sf object
hh_data_sel_sf <- hh_data_sel |>
  st_as_sf(coords = c("gpslongitude_imputed",
                      "gpslatitude_imputed"),
           crs = 4326, remove = FALSE)

# Leaflet Map -------------------------------------------------------------
# Ensure gps_imputed is a factor (for discrete colors)
hh_data_sel_sf <- hh_data_sel_sf |>
  mutate(gps_imputed = as.factor(gps_imputed))

# Define color palette
pal <- colorFactor(
  palette = "Set2",
  domain = hh_data_sel_sf$gps_imputed
)

# Create a popup text combining governorate, district, and village info
hh_data_sel_sf <- hh_data_sel_sf |>
  mutate(
    popup_info = paste0(
      "<b>Governorate:</b> ", governorate, "<br>",
      "<b>District:</b> ", district_eng, "<br>",
      "<b>Village:</b> ", village_eng, "<br>",
      "<b>GPS Imputed:</b> ", gps_imputed
    )
  )

# Build leaflet map
leaflet(hh_data_sel_sf) |>
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
    opacity = 1
  )

#all looks okay


# Merging Columns ---------------------------------------------------------
hh_data_othercols <- hh_data |>
  dplyr::select(c(21:last_col()))

#bind cols
hh_data_imputedgps <- hh_data_sel |>
  bind_cols(hh_data_othercols)

#select only hhid and agglomid for writing st object
hh_data_sel_sf <- hh_data_sel_sf |>
  dplyr::select(hhid,agglom_id)

# Save Data ---------------------------------------------------------------
saveRDS(hh_data_imputedgps,
        file = paste(here(),"Data",
                     "hhdata_imputedgps.rds",sep = "/"))

st_write(hh_data_sel_sf,
         dsn = paste(here(),"Data","Shapefiles",
                     "hh_imputedgps.shp",sep = "/"))

