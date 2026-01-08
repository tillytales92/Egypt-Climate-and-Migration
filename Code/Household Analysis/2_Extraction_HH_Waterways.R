#Rivers and Waterways Data
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","haven","here","devtools","ggmap",
          "terra","raster","geodata","sf","exactextractr",
          "tmap","leaflet","patchwork","tools")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load Data ----------------------------------------------------------------
#Loading the Shapefiles loaded from Humanitarian Data Exchange
egy_wways <- read_sf(paste(here(),"Data", "Nile","hotosm_egy_waterways_lines_shp",
                                      "hotosm_egy_waterways_lines_shp.shp",sep = "/"))

#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#crop to Suhag & Assiut only
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

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

# Filter waterways --------------------------------------------------------
#filter water ways data to Assiut & Suhag
wways_2gov <- st_filter(egy_wways, egypt_2gov)  # defaults to st_intersects

#Nile sf
nile <- wways_2gov |>
  filter(name_en == "Nile")

#base plot
plot(st_geometry(wways_2gov))

#show types
ggplot(wways_2gov)+
  geom_sf(aes(color = waterway))+
  theme_minimal()

#leaflet map
# Make sure your waterways have a color variable
pal <- colorFactor(
  palette = "Set2",
  domain = wways_2gov$waterway
)

####Leaflet map####
#####Show waterways#####
leaflet(wways_2gov) |>
  addProviderTiles(providers$Esri.WorldImagery) |>  # Satellite basemap
  addPolylines(data = wways_2gov,
               color = ~pal(waterway),
               weight = 2,
               opacity = 0.8,
               label = ~waterway) |>
  addLegend("bottomright",
            pal = pal,
            values = ~waterway,
            title = "Waterway type",
            opacity = 1)

#####Show waterways & Household Locations#####
leaflet(wways_2gov) |>
  addProviderTiles(providers$Esri.WorldImagery) |>  # Satellite basemap
  addPolylines(data = wways_2gov,
               color = ~pal(waterway),
               weight = 2,
               opacity = 0.8,
               label = ~waterway) |>
  # addPolygons(data = egypt_2gov,
  #             color = "yellow",
  #             weight = 2,
  #             opacity = 0.8,
  #             fill = FALSE,
  #             label = ~ADM1_EN) |>
  addCircleMarkers(data = hh_sf,
                   radius = 3,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   label = ~as.character(hhid)) |>
  addLegend("bottomright",
            pal = pal,
            values = ~waterway,
            title = "Waterway type",
            opacity = 1)

# HH Distance to Nile/Waterways ----------------------------------------------
#filter for canals, ditches & river; does not include dams and drains
#on OSM
wways_filtered <- wways_2gov |>
  filter(waterway %in% c("canal","ditch","river","drain"))

# Distance calculations (as the crow flies)
#need to switch off spherical geometry
sf_use_s2(FALSE)

#Calculate distances from each household to nile (as the crow flies)
dist_matrix_nile <- st_distance(hh_sf,nile)
dist_matrix_wways <- st_distance(hh_sf,wways_filtered)#incl. canal, ditches, rivers

# Get the minimum distance to the nearest waterway for each household
hh_distwaterways <- hh_sf |>
  mutate(
    distance_nile_metres = apply(dist_matrix_nile, 1, min),
    distance_waterway_metres = apply(dist_matrix_wways,1,min))

#test with leaflet if successful --> looks correct!
leaflet(wways_2gov) |>
  addProviderTiles(providers$Esri.WorldImagery) |>#Satellite basemap
  addPolylines(data = wways_2gov,
               color = ~pal(waterway),
               weight = 2,
               opacity = 0.8,
               label = ~waterway) |>
  addCircleMarkers(data = hh_distwaterways,
                   radius = 3,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   label = ~(distance_waterway_metres)) |>
  addLegend("bottomright",
            pal = pal,
            values = ~waterway,
            title = "Waterway type",
            opacity = 1)

#select only agglom_id, hhid, slope and elevation dem
hh_distwaterways <- hh_distwaterways |>
  st_drop_geometry() |>
  dplyr::select(hhid,agglom_id,distance_nile_metres,distance_waterway_metres)

####Create Static Map####
#try google maps background map
google_key <- readLines(paste(here(),"google_key.txt",sep = "/"))
register_google(key = google_key)

# # Define bounding box
# bbox <- st_bbox(egypt_2gov)
#
# # Get Google basemap
# bg_map <- get_googlemap(
#   center = c(lon = mean(c(bbox["xmin"], bbox["xmax"])),
#              lat = mean(c(bbox["ymin"], bbox["ymax"]))),
#   zoom = 8,
#   maptype = "satellite"  # or "roadmap", "terrain", "hybrid"
# )

#####Asyut waterways map#####
asyut_coords <- c(lon = 31.18368, lat = 27.18096)

asyut_map <- get_googlemap(
  center = asyut_coords,
  zoom = 11,
  maptype = "satellite"  # "roadmap", "terrain", "hybrid", or "satellite"
)

#map -> to adjust!
# Convert palette function output into a manual scale for ggplot
# Filter out "dam" from waterways
wways_filtered <- wways_2gov |>
  filter(tolower(waterway) != "dam")

# Waterway types for the palette
waterway_types <- unique(wways_filtered$waterway)
pal_values <- pal(waterway_types) |> as.vector()
names(pal_values) <- waterway_types

# Create capitalized labels for legend
waterway_labels <- setNames(toTitleCase(waterway_types), waterway_types)

# Map
waterways_map_asyut <- ggmap(asyut_map) +
  geom_sf(
    data = wways_filtered,
    aes(color = waterway),
    inherit.aes = FALSE,
    size = 0.8,
    alpha = 0.8
  ) +
  geom_sf(
    data = hh_sf,
    inherit.aes = FALSE,
    color = "red",
    size = 1,
    alpha = 0.9
  ) +
  scale_color_manual(
    values = pal_values,
    labels = waterway_labels,
    name = "Waterway type"
  ) +
  labs(
    title = "Waterways & Household Locations",
    subtitle = "Households within 30 km of Asyut City",
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

waterways_map_asyut

#####Suhag waterways map#####
suhag_coords <- c(lon = 31.6948, lat = 26.5560)

suhag_map <- get_googlemap(
  center = suhag_coords,
  zoom = 11,
  maptype = "satellite"  # "roadmap", "terrain", "hybrid", or "satellite"
)

#Waterways map Suahg
waterways_map_suhag <- ggmap(suhag_map) +
  geom_sf(
    data = wways_filtered,
    aes(color = waterway),
    inherit.aes = FALSE,
    size = 0.8,
    alpha = 0.8
  ) +
  geom_sf(
    data = hh_sf,
    inherit.aes = FALSE,
    color = "red",
    size = 1,
    alpha = 0.9
  ) +
  scale_color_manual(
    values = pal_values,
    labels = waterway_labels,
    name = "Waterway type"
  ) +
  labs(
    title = "Waterways & Household Locations",
    subtitle = "Households within 30 km of Suhag City",
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

waterways_map_suhag

# Save Data ---------------------------------------------------------------
saveRDS(hh_distwaterways,
        file = paste(here(),"Data","Nile",
                     "hh_distwaterways.rds",sep ="/"))

