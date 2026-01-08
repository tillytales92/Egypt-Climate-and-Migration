#Identifying Travel Distances to Nearest Market Towns
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
hh_data_imputedgps <- readRDS(file = paste(here(),"Data",
                            "hhdata_imputedgps_fullsample.rds",sep = "/")) |>
  filter(!is.na(hh_gpslongitude_imputed),
         !is.na(hh_gpslatitude_imputed))

hh_sf <- hh_data_imputedgps |>
  st_as_sf(coords = c("hh_gpslongitude_imputed", "hh_gpslatitude_imputed"),
           crs = 4326, remove = FALSE)

# --- Extract coordinates ---
coords <- st_coordinates(hh_sf)

# --- Combine IDs with coordinate strings ---
hh_coords_df <- hh_sf |>
  st_drop_geometry() |>
  mutate(coord = paste(coords[, 2], coords[, 1], sep = ",")) |>
  dplyr::select(hhid, coord)

# --- Define batch size (Google Distance API usually limits to 25 origins per request) ---
batch_size <- 20

# --- Split into batches keeping both ID and coord together ---
hh_batches <- split(hh_coords_df, ceiling(seq_along(hh_coords_df$hhid)/
                                          batch_size))

# --- (Optional) sample 10 batches for testing ---
hh_batch_sample <- hh_batches[1:10]

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
df_clean_asyut <- map_dfr(hh_batch_sample, get_distances_asyut)

#Bind back to households
hh_dist_asyut <- hh_sf |>
  mutate(distance_asyut_km = df_clean_asyut$distance_km,
         duration_asyut_min = df_clean_asyut$duration_min)

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

#Loop over batches, bind rows
df_clean_suhag <- map_dfr(hh_batches, get_distances_suhag)

#Bind back to households
hh_dist_suhag <- hh_sf |>
  mutate(distance_suhag_km = df_clean_suhag$distance_km,
         duration_suhag_min = df_clean_suhag$duration_min)



#for testing
# #sample 5
# town_sample <- slice_sample(town_centroids,n = 5)
#
# #town sample coords
# town_sample_coords <- apply(st_coordinates(town_sample), 1,
#                             function(x) paste(x[2], x[1], sep = ","))

df_list <- list()

for (i in seq_along(hh_batches)) {
  cat("Processing batch", i, "...\n")

  origins <- hh_batches[[i]]$coord
  origin_ids <- hh_batches[[i]]$hhid

  df <- googleway::google_distance(
    origins = origins,
    destinations = town_coords,
    mode = "driving",
    key = google_key
  )

  res <- df$rows$elements |>
    bind_rows() |>
    transmute(
      hhid = rep(origin_ids, each = length(town_coords)),
      distance_km = distance$value / 1000,
      duration_min = duration$value / 60,
      batch_id = i
    )

  df_list[[i]] <- res
  Sys.sleep(1)
}

df_distances <- bind_rows(df_list)

#test function
origins = hh_batches[[1]] |>
  dplyr::select(coord)

df <- google_distance(
  origins = origins,
  destinations = "Suhag, Egypt",
  mode = "driving",
  key = google_key)

#Village level -----------------------------------------------------------
####Using village centroid####
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
batch_size <- 20

# --- Split into batches keeping both ID and coord together ---
village_batches <- split(village_centroids_df,
                         ceiling(seq_along(village_centroids_df$village_eng)/
                                            batch_size))

#gives first with 20 villages, second with 15

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
mtowns <- st_read(paste(here(),"Data","Shapefiles",
                        "markettowns.shp",sep = "/"))

#towns with at least 50,000 inhabitants
mtowns_50k <- mtowns |>
  filter(pop_sum > 50000)

#get town centroids
town_centroids_50k <- st_centroid(mtowns_50k)

#extract the town coords (to use in google_distance function)
town_coords_50k <- apply(st_coordinates(town_centroids_50k), 1,
                     function(x) paste(x[2], x[1], sep = ","))

#test the town locations by getting distance to Suhag
df_townlocations <- google_distance(
  origins = town_coords_50k,
  destinations = "Suhag, Egypt",
  mode = "driving",
  key = google_key
)

df_townlocations
#this works: so the town coords are fine

town_sample <- town_coords_50k |> head(n = 2)

#Step 1: Create village centroid batches
batch_size <- 25#max. 25 villages per list (to comply with Google API)

village_centroid_batches <- split(
  village_centroids_df,
  ceiling(seq_len(nrow(village_centroids_df)) / batch_size)
)

#Step 2: Function to get nearest town for each village centroid
get_nearest_town_for_batch <- function(village_df, town_sample) {

  df <- google_distance(
    origins = village_df$coord,
    destinations = town_sample,
    mode = "driving",
    key = google_key
  )

  nearest <- purrr::map_df(
    seq_len(nrow(df$rows)),
    function(i) {

      el <- df$rows$elements[[i]]

      distances <- ifelse(
        el$status == "OK",
        el$distance$value/1000,
        NA_real_)

      durations <- ifelse(
        el$status == "OK",
        el$duration$value/60,
        NA_real_)

      tibble(
        distance_km  = if (all(is.na(distances))) NA_real_ else min(distances,
                                                                    na.rm = TRUE),
        duration_min = if (all(is.na(durations))) NA_real_ else min(durations,
                                                                    na.rm = TRUE)
      )
    }
  )

  dplyr::bind_cols(
    village_df |> dplyr::select(village_eng),
    nearest
  )
}

#Step 3: Run google calls batch wise and recombine
#35 village centroids * 25 town coords = 875 element calls in Google API
nearest_town_test <- purrr::map_dfr(
  village_centroid_batches,
  ~ get_nearest_town_for_batch(
    village_df = .x,
    town_sample = town_sample
  )
)

####Test simple####
town_sample <- town_coords_50k |> head(n = 10)

# Step 1: Create village centroid batches
batch_size <- 25

village_centroid_batches <- split(
  village_centroids_df,
  ceiling(seq_len(nrow(village_centroids_df)) / batch_size)
)

# Step 2: Function that ONLY calls google_distance()
get_distance_batch_raw <- function(village_df, town_sample) {

  google_distance(
    origins = village_df$coord,
    destinations = town_sample,
    mode = "driving",
    key = google_key
  )
}

# Step 3: Run batch-wise and keep raw outputs in a list
distance_raw <- purrr::map(
  village_centroid_batches,
  ~ get_distance_batch_raw(
    village_df = .x,
    town_sample = town_sample
  )
)

#Inspect first batch
distance_raw[[2]]$rows$elements

#Step 4: Extract min. distance for each village
min_distances_km <- purrr::map_dbl(
  distance_raw[[2]]$rows$elements,
  ~ min(.x$distance$value) / 1000
)

