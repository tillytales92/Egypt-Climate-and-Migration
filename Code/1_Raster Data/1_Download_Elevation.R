#Load Elevation Data for Egpyt
#use elevatr package
#see here:https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution
# Import libraries --------------------------------------------------------
# libraries we need
libs <- c("elevatr", "terra", "tidyverse","here",
          "sf", "giscoR", "marmap","haven")

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
                           "Data","raw","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

plot(st_geometry(egypt_hdx))

#filter for governorates
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

####Get Elevation Data####
####Governorate level####
#use of elevatr package to import elevation data
#we can feed the get-elev-raster() function 2 parameters:
#1.locations: where we can give a data.frame, a shp or a raster object to crop the elevation data accordingly so we don't have to download the entire thing
#2. we can set te z argument to determine the zoom level to retrn (ranges from 1 (lowest res) to 15 (highest res)
elevation_raster <- get_elev_raster(
  locations = egypt_2gov,
  z = 14,#this determines the resolution (zoom)
  clip = "tile",
  override_size_check = TRUE,
  ncpu = ifelse(future::availableCores() > 2, 2, 1),
  verbose = TRUE
)

#z = 14 at Suhag altitude --> resolution of around 8.5 metres

#####Map elevation#####
# Create place names (This is not part of the function)
# Places
place <- c("Assiut", "Suhag")

# Approximate coordinates (decimal degrees)
place.lon <- c(31.1837, 31.6957)   # longitudes
place.lat <- c(27.1801, 26.5560)   # latitudes

## Function that only adds cities, no polygons
eg.map <- function() {
  points(place.lon, place.lat, pch = 15, col = "red", cex = 1)
  text(place.lon, place.lat, labels = place, pos = 2, cex = 0.8)
}

# Plot DEM with places only
plot(elevation_raster,
     zlim = c(0,800),
     col = terrain.colors(100),
     main = "Elevation (m) in Assiut & Suhag",
     axes = FALSE)
plot(egypt_2gov,add = TRUE,color = "black")

# Add places
eg.map()

#get the slope in degrees
slope_raster <- terrain(elevation_raster, v = "slope", unit = "degrees")

# Plot Slope with places only
plot(slope_raster,
     zlim = c(0,30),
     col = terrain.colors(100),
     main = "Slope (in degrees) in Assiut & Suhag",
     axes = FALSE)

# Add places
eg.map()

####Extract Elevation Raster Tile####
elevation_raster_tile <- get_elev_raster(
  locations = egypt_2gov,
  z = 14,#this determines the resolution (zoom)
  clip = "tile",
  override_size_check = TRUE,
  ncpu = ifelse(future::availableCores() > 2, 2, 1),
  verbose = TRUE
)

plot(elevation_raster_tile)

#####Calculate Slope#####
slope_raster_tile <- terrain(elevation_raster_tile,
                             v = "slope", unit = "degrees")

# Save Data ---------------------------------------------------------------
####Save Elevation raster####
raster::writeRaster(elevation_raster,
                    filename = paste(here(),"Data", "intermediate","Elevation",
                                    "elevation_dem.tif",sep = "/"))

####Save Slope raster####
raster::writeRaster(slope_raster,
                    filename = paste(here(),"Data", "intermediate","Elevation",
                                     "slope_raster.tif",sep = "/"))

####Save Elevation raster tile####
raster::writeRaster(elevation_raster_tile,
                    filename = paste(here(),"Data", "intermediate","Elevation",
                                     "elevation_dem_tile.tif",sep = "/"))

####Save Slope raster tile####
raster::writeRaster(slope_raster_tile,
                    filename = paste(here(),"Data", "intermediate","Elevation",
                                     "slope_raster_tile.tif",sep = "/"))


