#Identifying Travel Distances to Nearest Market Towns
#NOTE: this script uses the Google Distance Matrix API
#Requires API Key and costs money depending on number of requests
#Read in Packages ----------------------------------------------
#Define packages used
libs <- c(
  "tidyverse","naniar","here","terra","raster","sf",
  "leaflet","rvest","fuzzyjoin","ggmap","tibble",
  "googleway","osrm","RColorBrewer","haven")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

#Google Key
google_key <- readLines(paste(here(),"google_key.txt",sep = "/"))

#set key
set_key(key = google_key)
google_keys()

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

#Village level -----------------------------------------------------------
####Using village centroid to reduce number of API calls####
#Check in Leaflet map if hh within villages are sufficiently clustered
#count village eng
hh_sf |>
  st_drop_geometry() |>
  count(village_eng) |>
  arrange(desc(n))

#36 unique villages
hh_sf |>
  st_drop_geometry() |>
  count(village_eng)

  # Create a color palette
  pal <- colorFactor(
    palette = brewer.pal(min(8, nlevels(hh_sf$village_eng)), "Set2"),
    domain  = hh_sf$village_eng
  )

  # Leaflet map
  leaflet(hh_sf) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addCircleMarkers(
      radius = 4,
      stroke = FALSE,
      fillOpacity = 0.7,
      color = ~pal(village_eng),
      popup = ~paste(
        "<strong>Village:</strong>", village_eng,
        "<br><strong>HHID:</strong>", hhid
      )
    ) |>
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~village_eng,
      title = "Village",
      opacity = 1
    )

#create village centroid var.
village_centroids <- hh_sf |>
  #filter out HH without village info (~34 HH)
  filter(!is.na(village_eng)) |>
    group_by(village_eng) |>
    summarise(
      geometry = st_centroid(st_union(geometry)),
      .groups = "drop"
    ) |>
  mutate(village_eng = as.factor(village_eng)) |>
  ungroup()

village_coords <- st_coordinates(village_centroids)

#create lon/lat char. vector Google can read
village_centroids_df <- village_centroids |>
  st_drop_geometry() |>
  mutate(coord = paste(village_coords[, 2], village_coords[, 1], sep = ",")) |>
  dplyr::select(village_eng, coord)

# --- Define batch size (Google Distance API limits to 25 origins per request) ---
batch_size <- 10

# --- Split into batches keeping both ID and coord together ---
village_batches <- split(village_centroids_df,
                         ceiling(seq_along(village_centroids_df$village_eng)/
                                            batch_size))

#gives three lists with 10 villages, fourth with 5

#####Asyut#####
# Function to get distance for one batch
get_distances_asyut <- function(origins) {

  df <- google_distance(
    origins = origins,
    destinations = "Asyut, Egypt",
    mode = "driving",
    key = google_key
  )

  # extract into a tibble
  df$rows$elements |>
    map_df(~{
      tibble(
        distance_km = .[["distance"]][["value"]] / 1000,
        duration_min = .[["duration"]][["value"]] / 60
      )
    })
}

# Loop over batches, bind rows
df_clean_asyut <- map_dfr(village_batches,
                          ~get_distances_asyut(.x$coord))

#Bind back to villages
village_dist_asyut <- village_centroids_df |>
  mutate(distance_asyut_km = df_clean_asyut$distance_km,
         duration_asyut_min = df_clean_asyut$duration_min)

#leaflet map to verify
village_dist_asyut_sf <- village_dist_asyut |>
  mutate(
    lon = as.numeric(str_extract(coord, "^[^,]+")),
    lat = as.numeric(str_extract(coord, "(?<=,).*"))) |>
  st_as_sf(coords = c("lat", "lon"), crs = 4326, remove = FALSE)

#colour palette
pal <- colorNumeric(
  palette = "viridis",
  domain = village_dist_asyut_sf$distance_asyut_km)

#leaflet
leaflet(village_dist_asyut_sf) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    radius = 6,
    color = ~pal(distance_asyut_km),
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0(
     # "<strong>Village:</strong> ", village_eng, "<br>",
      "<strong>Distance to Asyut:</strong> ", round(distance_asyut_km, 1), " km<br>",
      "<strong>Travel time:</strong> ", round(duration_asyut_min, 1), " min")) |>
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~distance_asyut_km,
    title = "Distance to Asyut (km)")
#works fine!

#####Suhag#####
# Function to get distance for one batch
get_distances_suhag <- function(origins) {
  df <- google_distance(
    origins = origins,
    destinations = "Suhag, Egypt",
    mode = "driving",
    key = google_key
  )

  # extract into a tibble
  df$rows$elements |>
    map_df(~{
      tibble(
        distance_km = .[["distance"]][["value"]] / 1000,
        duration_min = .[["duration"]][["value"]] / 60
      )
    })
}

# Loop over batches, bind rows
df_clean_suhag  <- map_dfr(village_batches,
                          ~get_distances_suhag(.x$coord))

#Bind back to villages
village_dist_suhag  <- village_centroids_df |>
  mutate(distance_suhag_km = df_clean_suhag$distance_km,
         duration_suhag_min = df_clean_suhag$duration_min)

#leaflet map to verify
village_dist_suhag_sf <- village_dist_suhag |>
  mutate(
    lon = as.numeric(str_extract(coord, "^[^,]+")),
    lat = as.numeric(str_extract(coord, "(?<=,).*"))) |>
  st_as_sf(coords = c("lat", "lon"), crs = 4326, remove = FALSE)

pal <- colorNumeric(
  palette = "viridis",
  domain = village_dist_suhag_sf$distance_suhag_km)

leaflet(village_dist_suhag_sf) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    radius = 6,
    color = ~pal(distance_suhag_km),
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0(
      #"<strong>Village:</strong> ", village_eng, "<br>",
      "<strong>Distance to Suhag:</strong> ", round(distance_suhag_km, 1), " km<br>",
      "<strong>Travel time:</strong> ", round(duration_suhag_min, 1), " min")) |>
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~distance_suhag_km,
    title = "Distance to Suhag (km)")

####Distance to market towns (> 50k people)####
#Load Market Town Shapefile from "4_FindingMarketTowns.R"
mtowns <- st_read(paste(here(),"Data","intermediate","Market Towns",
                        "markettowns.shp",sep = "/"))

#filter to towns with at least 50,000 inhabitants
mtowns_50k <- mtowns |>
  filter(pop_sum > 50000)

#get town centroids
town_centroids_50k <- st_centroid(mtowns_50k)

#extract the town coords (to use in google_distance function)
town_coords_50k <- apply(st_coordinates(town_centroids_50k), 1,
                     function(x) paste(x[2], x[1], sep = ","))

#test the town locations by getting distance to Suhag
# df_townlocations <- google_distance(
#   origins = town_coords_50k,
#   destinations = "Suhag, Egypt",
#   mode = "driving",
#   key = google_key
# )
#
# df_townlocations
#this works: so the town coords are fine!

####Step 1: Create village centroid batches ####
#create small batch size (to respect Google API limits)
batch_size <- 3

village_centroid_batches <- split(
  village_centroids_df,
  ceiling(seq_len(nrow(village_centroids_df)) / batch_size)
)

####Step 2: Call google_distance()####
get_distance_batch_raw <- function(village_df, town_sample) {

  google_distance(
    origins = village_df$coord,
    destinations = town_sample,
    mode = "driving",
    key = google_key
  )
}

####Step 3: Run batch-wise and keep raw outputs in a list####
distance_raw <- purrr::map(
  village_centroid_batches,
  ~ get_distance_batch_raw(
    village_df = .x,
    town_sample = town_coords_50k
  )
)

#Inspect first batch
distance_raw[[1]]$rows$elements
#for each village (origin), there is a list of distances to each town (destination)
#distance.value has the distance in meters
#duration.value has the travel time in seconds

####Step 4: Extracting min. distance (km)####
#Extract min. distance (km) per village from one API response
min_from_one_batch_km <- function(distance_obj) {
  purrr::map_dbl(
    distance_obj$rows$elements,
    ~ min(.x$distance$value) / 1000#divide by 1000 to get km
  )
}

#Apply to all batches
min_distances_km <- purrr::map(
  distance_raw,
  min_from_one_batch_km
)

#unlist
min_distances_km <- unlist(min_distances_km, use.names = FALSE)

#bind with village centroids dataframe
village_centroids_df <- village_centroids_df |>
  dplyr::mutate(
    min_distance_town_km = min_distances_km
  )

####Step 5: Extracting min distance (min)####
min_from_one_batch_min <- function(distance_obj) {
  purrr::map_dbl(
    distance_obj$rows$elements,
    ~ min(.x$duration$value) / 60#divide by 60 to get min
  )
}

#Apply to all batches
min_distances_min <- purrr::map(
  distance_raw,
  min_from_one_batch_min
)

#unlist
min_distances_min <- unlist(min_distances_min, use.names = FALSE)

#bind with village centroids dataframe
village_centroids_df <- village_centroids_df |>
  dplyr::mutate(
    min_distance_town_min = min_distances_min
  )

####Leaflet map to verify####
village_centroids_sf <- village_centroids_df |>
  mutate(
    lon = as.numeric(str_extract(coord, "^[^,]+")),
    lat = as.numeric(str_extract(coord, "(?<=,).*"))) |>
  st_as_sf(coords = c("lat", "lon"), crs = 4326, remove = FALSE)

pal <- colorNumeric(
  palette = "viridis",
  domain = village_centroids_sf$min_distance_town_min)

#leaflet map
mtowns_50k_centroids <- mtowns_50k |>
  sf::st_centroid()

#village centroids & town centroids on leaflet map
leaflet(village_centroids_sf) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    radius = 6,
    color = ~pal(min_distance_town_min),
    fillOpacity = 0.8,
    popup = ~paste0(
      "<strong>Distance to nearest town (km):</strong> ",
      round(min_distance_town_km, 1), " km<br>",
      "<strong>Travel time to nearest town (min):</strong> ",
      round(min_distance_town_min, 1), " min"
    )
  ) |>
  addCircleMarkers(
    data = mtowns_50k_centroids,
    radius = 5,
    color = "black",
    fillColor = "red",
    fillOpacity = 0.9
  ) |>
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~min_distance_town_min,
    title = "Distance to nearest town with 50k inhabitants (km)"
  )

####Bind all together####
hh_df <- hh_sf |>
  st_drop_geometry() |>
  dplyr::select(hhid, village_eng) |>
  mutate(village_eng = as.factor(village_eng))

#create village dataframe
village_distances_df <- village_dist_suhag |>
  left_join(village_dist_asyut, by = c("village_eng")) |>
  left_join(village_centroids_df,by = c("village_eng")) |>
  dplyr::select(-c(contains("coord")))

#now use this info at village level for households
hh_traveldistances <- hh_df |>
  left_join(village_distances_df, by = "village_eng") |>
  dplyr::select(-c(village_eng))

# Save Data ---------------------------------------------------------------
saveRDS(hh_traveldistances,
        file = paste(here(),"Data","intermediate","Household Data",
                     "hh_traveldistances_markettowns.rds",sep = "/"))

