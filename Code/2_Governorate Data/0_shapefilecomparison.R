#This script compares shapefiles available for Egypt
#Compared shapefiles stem from GADM, rnaturalearthdata, Humanitarian Data Exchange
#and RA shapefile used in previous work
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","here","devtools",
          "rnaturalearthdata","rnaturalearth","leaflet",
          "terra","raster","geodata","sf")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read in Shapefiles --------------------------------------------------------
##Load Egypt shapefile from GADM
egypt_gadm <- gadm("Egypt", level = 1, path = tempdir()) |>
  st_as_sf()

#Shapefile for Egypt from natuaralearthdata
egypt_ne <- ne_states(country = "Egypt")

#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                      "Data","raw","Shapefiles","HDX_Egypt",
                      "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#read RA shapefile
egypt_ra <- st_read(paste(here(),"Data","raw","Shapefiles",
                          "prev_RA_files",
                          "POLYGON_egypt.shp",sep = "/"))

#check CRS
st_crs(egypt_gadm)
st_crs(egypt_ne)
st_crs(egypt_hdx)
st_crs(egypt_ra)

#bounding boxes
st_bbox(egypt_gadm)
st_bbox(egypt_ne)
st_bbox(egypt_hdx)
st_bbox(egypt_ra)


# Comparison Plot ---------------------------------------------------------
#plotoutlines to understand differences between stapefiles
#no plot of egypt_ra here since it does only show country outline
ggplot() +
  geom_sf(data = egypt_gadm, fill = NA, color = "red") +
  geom_sf(data = egypt_ne, fill = NA, color = "blue") +
  geom_sf(data = egypt_hdx, fill = NA, color = "green") +
  theme_minimal()
#GADM seems inaccurate in South East; HDX shapefile most detailed and
#excludes water bodies

# Leaflet Map -------------------------------------------------------------
#Creating leaflet map for better comparison
# Make sure everything is in the same CRS (WGS84 for leaflet)
target_crs <- 4326
egypt_gadm <- st_transform(egypt_gadm, target_crs)
egypt_ne   <- st_transform(egypt_ne, target_crs)
egypt_hdx   <- st_transform(egypt_hdx, target_crs)

# Create interactive map
leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(data = egypt_gadm,
              color = "red", weight = 2, fill = FALSE,
              group = "GADM") |>
  addPolygons(data = egypt_ne,
              color = "blue", weight = 2, fill = FALSE,
              group = "Natural Earth") |>
  addPolygons(data = egypt_hdx,
              color = "green", weight = 2, fill = FALSE,
              group = "HDX") |>
  addLayersControl(
    overlayGroups = c("GADM", "Natural Earth", "HDX"),
    options = layersControlOptions(collapsed = FALSE))

#Natural Earth Data shapefile appears incorrect for our target governorates
#GO with HDX for the governorate-level analysis