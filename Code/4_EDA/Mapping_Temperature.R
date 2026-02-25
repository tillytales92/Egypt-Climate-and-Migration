#Temperature mapping and visualization
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","haven","here","ggmap","terra","raster","sf",
          "exactextractr","tmap","leaflet","patchwork")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load Data ----------------------------------------------------------------
#Loading the Raster Stack: Data was downloaded in 1_ERA5_Download_krigr_temperature
#and combined in 1_ERA5_Combining Rasters
#NOTE: these are big files!
#Load Daily mean raster (1960 - 2024)
temp_era5_dailymean_19602024 <- terra::rast(paste(here(),"Data","intermediate","ERA5",
                                  "temp_era5_dailymean_19602024.tif",sep = "/"))

temp_era5_dailymax_19602024 <- terra::rast(paste(here(),
                                                 "Data","intermediate","ERA5",
                                                 "temp_era5_dailymax_19602024.tif",sep = "/"))

#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","raw","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#Households
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

# Inspect
print(hh_sf)
plot(st_geometry(hh_sf))

#crop to Governorates - 4 governorates
egypt_4gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag","Cairo","Alexandria"))

#crop to Suhag & Assiut only
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

#USE 2020 pop. raster
pop_2020 <- raster::raster(paste(here(),"Data","raw","Landscan",
                                 "2020_landscan_pop.tif",sep = "/"))

st_crs(temp_era5_dailymean_19602024) == st_crs(pop_2020)

#Cropland raster
modis_lc <- raster::raster(paste(here(),"Data","raw","Modis",
                                 "2021_modis_landcover.tif",sep = "/"))

st_crs(temp_era5_dailymean_19602024) == st_crs(modis_lc)

cropland_mask <- modis_lc == 12

#load google background map
#try google maps background map
google_key <- readLines(paste(here(),"google_key.txt",sep = "/"))

register_google(key = google_key)

# Define bounding box
bbox <- st_bbox(egypt_2gov)

# Get Google basemap
bg_map <- get_googlemap(
  center = c(lon = mean(c(bbox["xmin"], bbox["xmax"])),
             lat = mean(c(bbox["ymin"], bbox["ymax"]))),
  zoom = 8,
  maptype = "satellite"  # or "roadmap", "terrain", "hybrid"
)

#decadal means
# check time dimension
time_info <- time(temp_era5_dailymean_19602024)

# subset only the 1980s
temp_era5_dailymean_1980s <- temp_era5_dailymean_19602024[[time_info >= as.Date("1980-01-01") & time_info <= as.Date("1989-12-31")]]

#subset 2000s
temp_era5_dailymean_2000s <- temp_era5_dailymean_19602024[[time_info >= as.Date("2000-01-01") & time_info <= as.Date("2000-12-31")]]

#subset 2020s
temp_era5_dailymean_2020s <- temp_era5_dailymean_19602024[[time_info >= as.Date("2020-01-01") & time_info <= as.Date("2024-12-31")]]

# Average for each decade
dailymean_1980 <- mean(temp_era5_dailymean_1980s, na.rm = TRUE)
dailymean_2000 <- mean(temp_era5_dailymean_2000s, na.rm = TRUE)
dailymean_2020 <- mean(temp_era5_dailymean_2020s, na.rm = TRUE)

#difference in avg. temp. between 2020 - 2000 and 2020 and 1980
dailymean_diff_2020_1980 <- dailymean_2020 - dailymean_1980
dailymean_diff_2020_2000 <- dailymean_2020 - dailymean_2000

#turn into data frames for plotting
diff_2020_1980_df <- as.data.frame(dailymean_diff_2020_1980, xy = TRUE, na.rm = TRUE)
diff_2020_2000_df <- as.data.frame(dailymean_diff_2020_2000, xy = TRUE, na.rm = TRUE)

# Change in Decadal Averages ---------------------------------------
####All of Egypt####
#####Avg. temp. difference - 2020 vs. 1980 #####
diff_map_20201980 <- ggplot() +
  geom_raster(data = diff_2020_1980_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = egypt_gov,fill = NA, color = "seashell", size = 0.3) +
  scale_fill_viridis_c(
    option = "F",
    direction = -1,
    name = "Δ Temp (°C)"# legend title from before
  )+
  coord_sf() +
  labs(
    title = "Egypt: Change in Average Temperature: 2020s vs 1980s (Decadal Average)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

diff_map_20201980

#####Avg. temp. difference - 2020 vs. 2000 #####
diff_map_20202000 <- ggplot() +
  geom_raster(data = diff_2020_2000_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = egypt_gov,fill = NA, color = "seashell", size = 0.3) +
  scale_fill_viridis_c(
    option = "F",
    direction = -1,
    name = "Δ Temp (°C)"# legend title from before
  )+
  coord_sf() +
  labs(
    title = "Egypt: Change in Average Temperature: 2020s vs 2000s (Decadal Average)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

diff_map_20202000

####Suhag & Assiut####
#difference in avg. temp. between 2020 - 2000
dailymean_diff_2020_2000_cropped <- mask(dailymean_diff_2020_2000, egypt_2gov)
dailymean_diff_2020_2000_cropped_df <- as.data.frame(dailymean_diff_2020_2000_cropped,
                                                    xy = TRUE, na.rm = TRUE)

#####Change in Decadal Mean Temp.#####
######2020 vs. 2000######
ggplot() +
  geom_raster(data = dailymean_diff_2020_2000_cropped_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = egypt_2gov,fill = NA, color = "seashell", size = 0.3) +
  scale_fill_viridis_c(
    option = "F",
    direction = -1,
    name = "Δ Temp (°C)"# legend title from before
  )+
  coord_sf(crs = st_crs(4326))+
  labs(
    title = "Suhag & Assiut: Change in Average Temperature: 2020s vs 2000s (Decadal Average)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

######Average summer temp. (July & August) between 2000 - 2004######
#Extract July & August 2000,2001,2002,2003,2004
years  <- 2000:2004
months <- c(7, 8)  # July, August

# get indices for all July/August days in 2000-2004
idx <- which(format(time_info, "%Y") %in% years &
               format(time_info, "%m") %in% sprintf("%02d", months))

# subset raster for those days
temp_subset <- temp_era5_dailymean_19602024[[idx]]

# compute mean across all selected days (°C)
mean_temp_julaug_2000_2004 <- mean(temp_subset, na.rm = TRUE)

# crop/mask to governorates
mean_temp_julaug_2000_2004_crop <- mask(mean_temp_julaug_2000_2004, egypt_2gov)

# optional: convert to dataframe for plotting
mean_temp_julaug_2000_2004_df <- as.data.frame(mean_temp_julaug_2000_2004_crop,
                                               xy = TRUE, na.rm = TRUE)

#summer temp. 2000 - 2004
summertemp_20002004_2gov <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = hh_sf, inherit.aes = FALSE,
          aes(color = "Households"), size = 1) +
  geom_raster(data = mean_temp_julaug_2000_2004_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = mean), alpha = 0.7) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 32,
    name = "Temperature (°C)"
  ) +
  scale_color_manual(
    values = c(
      "Suhag" = "#E41A1C",       # red from Set1
      "Assiut" = "#377EB8",      # blue from Set1
      "Households" = "orange"     # custom color for points
    ),
    name = "Legend"
  ) +
  labs(title = "Suhag & Assiut: Mean Summer Temperatures 2000-2004") +
  theme_minimal() +
  theme(legend.position = "bottom")

summertemp_20002004_2gov

######Average July & Aug Temp. 2020 - 2024######
#Extract rasters
years_2020s  <- 2020:2024
months <- c(7, 8)  # July, August

# get indices for all July/August days in 2000-2004
idx_2020s <- which(format(time_info, "%Y") %in% years_2020s &
               format(time_info, "%m") %in% sprintf("%02d", months))

# subset raster for those days
temp_subset_2020s <- temp_era5_dailymean_19602024[[idx_2020s]]

# compute mean across all selected days (°C)
mean_temp_julaug_2020_2024 <- mean(temp_subset_2020s, na.rm = TRUE)

# crop/mask to governorates
mean_temp_julaug_2020_2024_crop <- mask(mean_temp_julaug_2020_2024, egypt_2gov)

# optional: convert to dataframe for plotting
mean_temp_julaug_2020_2024_df <- as.data.frame(mean_temp_julaug_2020_2024_crop,
                                               xy = TRUE, na.rm = TRUE)

#plot
summertemp_20202024_2gov <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = hh_sf, inherit.aes = FALSE,
          aes(color = "Households"), size = 1) +
  geom_raster(data = mean_temp_julaug_2020_2024_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = mean), alpha = 0.7) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 32,
    name = "Temperature (°C)") +
  scale_color_manual(
    values = c(
      "Suhag" = "#E41A1C",       # red from Set1
      "Assiut" = "#377EB8",      # blue from Set1
      "Households" = "orange"     # black for HH points
    ),
    name = "Legend") +
  labs(title = "Suhag & Assiut: Mean Summer Temperatures 2020-2024") +
  theme_minimal() +
  theme(legend.position = "bottom")

summertemp_20202024_2gov

#####Difference Summer Temperatures: 2020s vs. 2000s#####
diff_summers_meantemp <- mean_temp_julaug_2020_2024_df$mean - mean_temp_julaug_2000_2004_df$mean

diff_summers <- mean_temp_julaug_2020_2024_df[1:2] |>
  bind_cols(diff_summers_meantemp) |>
  rename("temp_diff" = "...3")

diff_summers_map <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_raster(data = diff_summers, inherit.aes = FALSE,
              aes(x = x, y = y, fill = temp_diff), alpha = 0.7) +
  scale_fill_viridis_c(
    option = "rocket",
    #direction = 1,
    name = "Δ Temp (°C)"# legend title from before
  )+
  scale_color_manual(
    values = c(
      "Suhag" = "#E41A1C",       # red from Set1
      "Assiut" = "#377EB8"     # blue from Set1
    ),
    name = "Legend"
  ) +
  labs(
    title = "Suhag & Assiut: Change in Mean Summer\n (July & August) Temperature",
    caption = "Source: ERA5 data"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14),       # smaller title
    plot.subtitle = element_text(size = 12),    # smaller subtitle
    plot.caption = element_text(size = 10),     # smaller caption
    axis.title = element_blank(),               # remove axis titles
    axis.text = element_blank(),                # remove axis tick labels
    axis.ticks = element_blank()                # remove axis ticks
)

diff_summers_map

#####Winter Days 2000s#####
# define years and months (November - January)
years_2000s  <- 2000:2004
months_nd <- c(11:12,1)  # November - January

# get indices for all Nov–Dec days in 2000–2004
idx_winter_2000s <- which(format(time_info, "%Y") %in% years_2000s &
                            format(time_info, "%m") %in% sprintf("%02d", months_nd))

# subset rasters for those days
temp_subset_winter_2000s <- temp_era5_dailymax_19602024[[idx_winter_2000s]]

# count number of days > 30 °C per cell
days_above30_winter_2000s <- sum(temp_subset_winter_2000s > 30, na.rm = TRUE)

# crop/mask to governorates
days_above30_winter_2000s_crop <- mask(days_above30_winter_2000s, egypt_2gov)

# convert to dataframe for plotting
days_above30_winter_2000s_df <- as.data.frame(days_above30_winter_2000s_crop,
                                              xy = TRUE, na.rm = TRUE) |>
  mutate(mean = sum/5)  # optional: average per year

# plot
daysabove30_winter_2000s_map <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = hh_sf, inherit.aes = FALSE,
          aes(color = "Households"), size = 1) +
  geom_raster(data = days_above30_winter_2000s_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = mean), alpha = 0.7) +
  scale_fill_viridis_c(option = "C", name = "Days > 30°C") +
  scale_color_manual(
    values = c(
      "Suhag" = "#E41A1C",
      "Assiut" = "#377EB8",
      "Households" = "orange"
    ),
    name = "Legend"
  ) +
  labs(title = "Suhag & Assiut: Winter 2000–2004\nNumber of Days Max Temp. > 30°C") +
  theme_minimal() +
  theme(legend.position = "bottom")

daysabove30_winter_2000s_map

#####Winter Days 2020s#####
# define years and month (October)
years_2020s  <- 2020:2024

# get indices for all October days in 2020–2024
idx_winter_2020s <- which(format(time_info, "%Y") %in% years_2020s &
                             format(time_info, "%m") %in% sprintf("%02d", months_nd))

# subset rasters for those days
temp_subset_winter <- temp_era5_dailymax_19602024[[idx_winter_2020s]]

# count number of days > 30 °C per cell
days_above30_winter <- sum(temp_subset_winter > 30, na.rm = TRUE)

# crop/mask to governorates
days_above30_winter_crop <- mask(days_above30_winter, egypt_2gov)

# convert to dataframe for plotting
days_above30_winter_crop_df <- as.data.frame(days_above30_winter_crop,
                                         xy = TRUE, na.rm = TRUE) |>
  mutate(mean = sum/5)

# plot
daysabove30_winter_map <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = hh_sf, inherit.aes = FALSE,
          aes(color = "Households"), size = 1) +
  geom_raster(data = days_above30_winter_crop_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = mean), alpha = 0.7) +
  scale_fill_viridis_c(option = "C", name = "Days > 30°C") +
  scale_color_manual(
    values = c(
      "Suhag" = "#E41A1C",
      "Assiut" = "#377EB8",
      "Households" = "orange"
    ),
    name = "Legend"
  ) +
  labs(title = "Suhag & Assiut: Winter 2020–2024\nNumber of Days Max Temp. > 30°C") +
  theme_minimal() +
  theme(legend.position = "bottom")

daysabove30_winter_map

#####Compare 2000 vs. 2020s#####
# Ensure both data frames have the same fill scale
fill_limits <- range(
  c(days_above30_winter_2000s_df$mean,
    days_above30_winter_crop_df$mean),
  na.rm = TRUE
)

p1 <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = hh_sf, inherit.aes = FALSE,
          aes(color = "Households"), size = 1) +
  geom_raster(data = days_above30_winter_2000s_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = mean), alpha = 0.7) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, 30),
    breaks = c(0, 10, 20, 30),
    name = "Days > 30°C"
  )+
  scale_color_manual(
    values = c("Suhag" = "#E41A1C",
               "Assiut" = "#377EB8",
               "Households" = "orange"),
    name = "Legend"
  ) +
  labs(title = "Winter 2000–2004") +
  theme_minimal() +
  theme(legend.position = "bottom")

p2 <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_sf(data = hh_sf, inherit.aes = FALSE,
          aes(color = "Households"), size = 1) +
  geom_raster(data = days_above30_winter_crop_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = mean), alpha = 0.7) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(0, 30),
    breaks = c(0, 10, 20, 30),
    name = "Days > 30°C"
  )+
  scale_color_manual(
    values = c("Suhag" = "#E41A1C",
               "Assiut" = "#377EB8",
               "Households" = "orange"),
    name = "Legend"
  ) +
  labs(title = "Winter 2020–2024") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combine plots with shared legend
combined_map <- p1 + p2 + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14),       # smaller title
    plot.subtitle = element_text(size = 12),    # smaller subtitle
    plot.caption = element_text(size = 10),     # smaller caption
    axis.title = element_blank(),               # remove axis titles
    axis.text = element_blank(),                # remove axis tick labels
    axis.ticks = element_blank()                # remove axis ticks
  )

combined_map <- combined_map +
  plot_annotation(
    title = "Suhag & Assiut: Number of Winter Days with Max. Temp > 30°C",
    subtitle = "Winter defined as November–January.\nNumber of days counted per year and averaged over 5-year periods.",
    caption = "Source: ERA5 data"
  )

combined_map

######Population - 2020 ######
pop_2020_cropped <- crop(pop_2020,egypt_4gov)
pop_2020_cropped <- mask(pop_2020_cropped, egypt_4gov)
pop_2020_cropped_df <- as.data.frame(pop_2020_cropped,
                                   xy = TRUE, na.rm = TRUE)

#with static background map
pop_map_2020_2gov <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_raster(data = pop_2020_cropped_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = b1), alpha = 0.7)+
  scale_fill_viridis_c(option = "C", trans = "log", name = "Population") +
  coord_sf() +
  labs(
    title = "Suhag & Assiut: Population in 2020",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

pop_map_2020_2gov

#leaflet map
pal_pop <- colorNumeric("viridis", domain = pop_2020_cropped_df$b1,
                        na.color = "transparent")

leaflet() |>
  addTiles() |>
  addRasterImage(pop_2020_cropped, colors = pal_pop, opacity = 0.7) |>
  addPolygons(data = egypt_2gov, fill = FALSE, color = "white", weight = 2) |>
  addLegend(pal = pal_pop, values = pop_2020_cropped_df$b1, title = "Population")

######Cropland - 2021 ######
cropland_2021_cropped <- crop(cropland_mask,egypt_4gov)
cropland_2021_cropped <- mask(cropland_2021_cropped, egypt_4gov)
cropland_2021_cropped_df <- as.data.frame(cropland_2021_cropped,
                                          xy = TRUE, na.rm = TRUE)

cropland_2021_cropped_df <- cropland_2021_cropped_df |>
  rename(Cropland = layer) |>
  mutate(Cropland = ifelse(Cropland, "Cropland", "Other land use"))

cropland_map_2020_2gov <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_raster(data = cropland_2021_cropped_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = Cropland), alpha = 0.7)+
  scale_fill_viridis_d() +
  coord_sf() +
  labs(
    title = "Suhag & Assiut: Cropland in 2020",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

cropland_map_2020_2gov

#Temp.changes Assiut & Sohag
tempchanges_2gov <- ggmap(bg_map) +
  geom_sf(data = egypt_2gov, inherit.aes = FALSE,
          aes(color = ADM1_EN),
          fill = NA, linewidth = 0.8, linetype = "dashed") +
  geom_raster(data = dailymean_diff_2020_1980_cropped_df, inherit.aes = FALSE,
              aes(x = x, y = y, fill = mean), alpha = 0.7)+
  scale_fill_viridis_c(
    option = "B",
    direction = -1,
    name = "Δ Temp (°C)")+
  scale_color_brewer(palette = "Set1", name = "Governorate")+
  labs(title = "Change in Average Temperature: 2020s vs 1980s (Decadal Average)") +
  theme_minimal() +
  theme(legend.position = "bottom")


tempchanges_2gov



#turn into data frames for plotting
diff_2020_1980_df <- as.data.frame(dailymean_diff_2020_1980, xy = TRUE, na.rm = TRUE)
diff_2020_2000_df <- as.data.frame(dailymean_diff_2020_2000, xy = TRUE, na.rm = TRUE)

####Population####
#show temp. changes and pop.
# Convert population raster to data.frame
#difference in avg. temp. between 2020 - 2000 and 2020 and 1980
pop_diff_2000_2000 <- pop_2020 - pop_2000

#turn into data frames for plotting
pop_diff_2000_2000_df <- as.data.frame(pop_diff_2000_2000, xy = TRUE, na.rm = TRUE)

pop_2000_df <- as.data.frame(pop_2000, xy = TRUE, na.rm = TRUE)
pop_2020_df <- as.data.frame(pop_2020, xy = TRUE, na.rm = TRUE)

# Rename the column for clarity
pop_2000_df <- pop_2000_df |>
  rename(pop = b1) |>    # if the column is named "layer"
  mutate(pop_log = log1p(pop))   # log(1+pop) to reduce skew

pop_2020_df <- pop_2020_df |>
  rename(pop = b1) |>    # if the column is named "layer"
  mutate(pop_log = log1p(pop))   # log(1+pop) to reduce skew

pop_diff_2000_2000_df <- pop_diff_2000_2000_df |>
  rename(pop_diff = layer)   # if the column is named "layer"
  #mutate(pop_diff_log = log1p(pop_diff))   # log(1+pop) to reduce skew

#####Pop in 2020#####
pop_map_2020 <- ggplot() +
  geom_raster(data = pop_2020_df,aes(x = x, y = y, fill = pop)) +
  geom_sf(data = egypt_gov, fill = NA, color = "seashell") +
  scale_fill_viridis_c(option = "C", trans = "log", name = "Population") +
  coord_sf() +
  labs(
    title = "Egypt: Population in 2020",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

pop_map_2020

#leaflet map
pal <- colorNumeric("RdBu", domain = values(diff_2020_2000), reverse = TRUE)

leaflet() |>
  addTiles() |>
  addRasterImage(diff_2020_2000, colors = pal, opacity = 0.7) |>
  addLegend(pal = pal, values = values(diff_2020_2000),
            title = "Δ Temp (°C)")


pal <- colorNumeric("RdBu", domain = values(temp_era5_dailymean_19602024[[23742]]),
                    reverse = TRUE)

#test
leaflet() |>
  addTiles() |>
  addRasterImage(temp_era5_dailymean_19602024[[23742]], colors = pal, opacity = 0.7) |>
  addLegend(pal = pal, values = values(temp_era5_dailymean_19602024[[23742]]),
            title = "Δ Temp (°C)")


# Save Data ---------------------------------------------------------------
####Population raster####
#save cropped raster image
terra::writeRaster(pop_2020_cropped,
                   filename = paste(here(), "Data", "intermediate", "Landscan",
                                    "pop_2020_cropped.tif", sep = "/"),
                   overwrite = TRUE)

#save cropped data frame
saveRDS(pop_2020_cropped_df,
        file = paste(here(),"Data","intermediate","Landscan",
                     "pop_2020_cropped_df.Rds",sep = "/"))


####Cropland raster####
#save cropped raster image
# make categorical
crop_2021_cropped_cat <- as.factor(crop_2021_cropped)

# define labels
levels(crop_2021_cropped_cat) <- data.frame(
  value = c(0, 1),
  landuse = c("Other land use", "Cropland"))

terra::writeRaster(crop_2021_cropped_cat,
                   filename = paste(here(), "Data", "intermediate", "Modis",
                                    "cropland_2021_cropped_cat.tif", sep = "/"),
                   overwrite = TRUE)

#save cropped data frame
saveRDS(cropland_2021_cropped_df,
        file = paste(here(),"Data","intermediate","Modis",
                     "cropland_2021_cropped_df.Rds",sep = "/"))
