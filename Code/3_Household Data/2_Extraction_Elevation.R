#Extract elevation and slope data for HH Sample (with imputed GPS)
#Elevation Raster Data was downloaded in "2_Extraction_Elevation"
# Import libraries --------------------------------------------------------
# libraries we need
libs <- c("elevatr", "terra", "tidyverse","here",
          "sf", "giscoR", "marmap","haven")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Load Data ---------------------------------------------------------------
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

####Read Elevation raster####
elevation_raster_tile <- raster::raster(
  x = paste(here(),"Data","intermediate","Elevation",
            "elevation_dem_tile.tif",sep = "/"))

####Read Slope raster####
slope_raster_tile <- raster::raster(
  x = paste(here(),"Data","intermediate","Elevation",
            "slope_raster_tile.tif",sep = "/"))

#####Slope#####
# Convert raster::RasterLayer to terra::SpatRaster
slope_raster_terra <- terra::rast(slope_raster_tile)

# Ensure hh points are SpatVector
hh_vect <- vect(hh_sf)

#Ensure CRS match
if (!crs(hh_vect) == crs(slope_raster_terra)) {
  message("CRS mismatch detected — reprojecting hh_vect to match raster CRS.")
  hh_vect <- project(hh_vect, crs(slope_raster_terra))
}

# Extract slope values
hh_slope <- terra::extract(slope_raster_terra, hh_vect)
hh_sf$slope <- hh_slope[,2]

#Find obs without slope value
hh_sf |>
  dplyr::select(gps_imputed,slope) |>
  filter(is.na(slope))

#####Elevation#####
elev_raster_terra <- rast(elevation_raster_tile)

#Ensure CRS match
if (!crs(hh_vect) == crs(elev_raster_terra)) {
  message("CRS mismatch detected — reprojecting hh_vect to match raster CRS.")
  hh_vect <- project(hh_vect, crs(elev_raster_terra))
}

hh_elev <- terra::extract(elev_raster_terra, hh_vect)
hh_sf$elevation_dem <- hh_elev[,2]

#select columns (HH ID,Slope & Elevation)
hhdata_dem <- hh_sf |>
  st_drop_geometry() |>
  dplyr::select(hhid,slope,elevation_dem)

#look for NAs
hhdata_dem |>
  miss_var_summary()

#Save Data ---------------------------------------------------------------
saveRDS(hhdata_dem,
        file = paste(here(),"Data","intermediate","Household Data",
                     "hhdata_dem.rds",sep ="/"))

