#Identifying market towns using GHSL and World Pop data
#Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","haven","here","devtools","ggmap",
          "terra","raster","geodata","sf","exactextractr",
          "tmap","leaflet","patchwork")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

#Load Data ----------------------------------------------------------------
####GHSL data####
#Use GHSL data: used to detect built-up land cells
ghsl_2020_1 <- terra::rast(paste(here(),"Data","Landcover","GHS_BUILT_2020_100m",
              "GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R6_C21.tif",sep = "/"))

ghsl_2020_2 <- terra::rast(paste(here(),"Data","Landcover","GHS_BUILT_2020_100m",
              "GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R6_C22.tif",sep = "/"))

ghsl_2020_3 <- terra::rast(paste(here(),"Data","Landcover","GHS_BUILT_2020_100m",
               "GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R7_C21.tif",sep = "/"))

ghsl_2020_4 <- terra::rast(paste(here(),"Data","Landcover","GHS_BUILT_2020_100m",
               "GHS_BUILT_S_E2020_GLOBE_R2023A_54009_100_V1_0_R7_C21.tif",sep = "/"))

#merge the 4 rasters
ghsl_2020 <- merge(ghsl_2020_1,ghsl_2020_2,ghsl_2020_3,ghsl_2020_4)

#reproject
ghsl_2020 <- terra::project(ghsl_2020,"EPSG:4326")

####Population Data####
#USE 2020 pop. raster: to count pop. per cell
pop_2020_1 <- terra::rast(paste(here(),"Data","Population","GHS_POP_2020_100m",
                "GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R6_C21.tif",sep = "/"))

pop_2020_2 <- terra::rast(paste(here(),"Data","Population","GHS_POP_2020_100m",
                "GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R6_C22.tif",sep = "/"))

pop_2020_3 <- terra::rast(paste(here(),"Data","Population","GHS_POP_2020_100m",
                "GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R7_C21.tif",sep = "/"))

pop_2020_4 <- terra::rast(paste(here(),"Data","Population","GHS_POP_2020_100m",
              "GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0_R7_C22.tif",sep = "/"))

#merge all 4 rasters
pop_2020 <- terra::merge(pop_2020_1,pop_2020_2,
              pop_2020_3,pop_2020_4)

#reproject to match Egypt shapefile
pop_2020 <- terra::project(pop_2020,"EPSG:4326")

#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#crop to Suhag & Assiut only
egypt_2gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

#crop to 2gov
ghsl_2gov <- ghsl_2020 |>
  crop(vect(egypt_2gov),mask = TRUE)

#test plots
plot(pop_2020)
plot(egypt_2gov,color = "red",add = TRUE)

####Step 1: Threshold built-up area ####
# (GHSL built-up fractions are typically between 0 and 100)
built_mask <- ghsl_2gov > 2500#e.g. at least 25% built-up
built_mask <- mask(built_mask, ghsl_2gov)  # keep same extent

plot(built_mask)

#### Step 2: Identify contiguous patches ####
# 8-neighbour connectivity (diagonals count)
clusters <- patches(built_mask, directions = 8,
                    zeroAsNA = TRUE)

plot(clusters)

####Step 3: Convert patches to polygons ####
clusters_vect <- as.polygons(clusters, dissolve = TRUE) |>
  st_as_sf()

#### Step 4: Calculate area in hectares ####
clusters_vect$area_ha <- st_area(clusters_vect) / 1e4

#### Step 5: Add population info ####
pop_2gov <- pop_2020 |> crop(vect(egypt_2gov), mask = TRUE)
pop_sum <- terra::extract(pop_2gov, vect(clusters_vect), fun = sum, na.rm = TRUE)
clusters_vect$pop_sum <- pop_sum[,2]

#### Step 6:Filter for "market towns" ####
#(tune thresholds empirically)
towns_10k <- clusters_vect |>
  filter(pop_sum > 10000)#pop.sum > 10000 in 2020

plot(towns_10k)

#towns 50k
towns_50k <- clusters_vect |>
  filter(pop_sum > 50000)
#25 towns with >50k inhabitants

#### Step 7: Visualize ####
plot(ghsl_2gov, col = gray.colors(10, rev = TRUE))
plot(st_geometry(towns_50k), border = "red", add = TRUE, lwd = 1.5)

#leaflet
# Convert the raster to a format leaflet can handle
ghsl_2gov_rgb <- ghsl_2gov
ghsl_2gov_rgb[ghsl_2gov_rgb == 0] <- NA  # remove non-built pixels

# Define color palette for built-up intensity
pal <- colorNumeric(palette = "Greys", domain = values(ghsl_2gov_rgb),
                    na.color = "transparent")

#Create interactive map
leaflet() |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |>  # satellite basemap
  addRasterImage(ghsl_2gov_rgb, colors = pal, opacity = 0.7, project = TRUE) |>
  addPolygons(data = towns_50k,
              color = "blue",
              weight = 2,
              fillOpacity = 0,
              label = ~paste0("Area: ", round(area_ha, 1), " ha, Pop: ",
                              round(pop_sum)))
  addPolygons(data = clusters_vect,
              color = "red",
              weight = 2,
              fillOpacity = 0,
              label = ~paste0("Area: ", round(area_ha, 1), " ha, Pop: ",
                              round(pop_sum))) |>
  addScaleBar(position = "bottomleft") |>
  addLegend(pal = pal, values = values(ghsl_2gov_rgb),
            title = "Built-up fraction (%)",
            position = "bottomright")

# Save towns object -------------------------------------------------------
st_write(towns_10k,dsn =
           paste(here(),"Data","markettowns.shp",sep = "/"))

