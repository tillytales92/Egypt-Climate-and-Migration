####Retrieve data from OSM
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c(
  "tidyverse","naniar","here","osmdata","ohsome",
  "terra","sf","operators","pbapply","mapview")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

####Shapefiles####
#SEZ shp
#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#crop to Suhag & Assiut only
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

####OSM####
print(available_tags("amenity"),n = 150)
print(available_tags("building"),n = 200)

#Define area of interest

# Query OpenStreetMap for railways
eg_amenities <- opq(bbox = st_bbox(egypt_2gov)) |>
  add_osm_feature(key = "amenity",value = "marketplace") |>
  osmdata_sf()

#only returns 3 poitns
# Extract points as sf object
ame_points <- eg_amenities$osm_points
ame_points |> st_drop_geometry() |> count(amenity)

#extract polygons
eg_landuse <- opq(bbox = st_bbox(egypt_2gov)) |>
  add_osm_feature(key = "landuse",value = "commercial") |>
  osmdata_sf()

eg_landuse_points <- eg_landuse$osm_points