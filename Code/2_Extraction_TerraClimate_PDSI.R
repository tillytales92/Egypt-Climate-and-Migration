#Precipitation extraction using exactextractr
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","exactextractr")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load Climate Data and Shapefile-------------------------------------------
#Loading the Raster Stack: Data was downloaded in "1_TerraClimate_Download.R"
#Load Monthly PDSI raster
tc_pdsi <- terra::rast(paste(here(),"Data", "Drought Indices","TerraClimate",
                             "tc_pdsi_19602023.tif",sep = "/"))

####Load Shapefile and reproject####
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

# Check CRS (convert terra crs to WKT for comparison)
st_crs(egypt_hdx)$wkt == crs(tc_pdsi)

# Reproject shapefile to raster CRS (safer than reprojecting raster)
egypt_hdx_proj <- st_transform(egypt_hdx, crs(tc_pdsi))

# Double-check
st_crs(egypt_hdx_proj)$wkt == crs(tc_pdsi)

####Test plots####
#PDSI January 1960, NOTE: high value --> high moisture, low value: drought risk
#see:https://climatedataguide.ucar.edu/climate-data/palmer-drought-severity-index-pdsi
plot(tc_pdsi[[1]])
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

####Cropping to Governorates and adj. layer names####
#Selecting Assiut, Suhag, Alexandria and Cairo from shapefile
egypt_gov <- egypt_hdx_proj |>
  filter(ADM1_EN %in% c("Assiut","Suhag" ,"Alexandria","Cairo"))

#####Daily Sum of rainfall (mm)#####
#crop and mask raster to Assiut and Suhag governorates using terra
tc_pdsi_cropped <- tc_pdsi |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: temperature (celsius) on January 1st, 1981
plot(tc_pdsi_cropped[[1]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
tc_pdsi_cropped_rasterobject <- raster::stack(tc_pdsi_cropped)

#rename layers
#Covers 1st of Jan 1960 - 31st of December 2023
dates_pdsi <- seq(as.Date("1960-01-01"), as.Date("2023-12-31"), by = "month")

# Assign the dates to the RasterStack
tc_pdsi_cropped_rasterobject <- setZ(tc_pdsi_cropped_rasterobject,
  dates_pdsi,name = "Date/time")

# You can also fix the layer names to be cleaner
names(tc_pdsi_cropped_rasterobject) <- format(dates_pdsi, "%Y-%m-%d")

#check if dates are adjusted
tc_pdsi_cropped_rasterobject

# Extraction: Preparing Pop. and Cropland weights -----------------------------
#Egypt DF
egypt_df <- egypt_gov |>
  st_drop_geometry() |>
  dplyr::select(ADM1_EN)

####Including Landscan Pop. weights####
#USE 2020 pop. raster
pop_2020 <- raster::raster(paste(here(),"Data","Population",
                                 "2020_landscan_pop.tif",sep = "/"))

st_crs(tc_pdsi_cropped_rasterobject) == st_crs(pop_2020)

#crop first
pop_2020 <- pop_2020 |>
  crop(egypt_gov)

#test plot: pop in 2020
plot(pop_2020)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
pop_2020_proj <- projectRaster(
  pop_2020,
  crs = crs(tc_pdsi_cropped_rasterobject),
  method = "bilinear")#bilinear for continuous values like population density

#2.Resample to match resolution & extent of temp raster
pop_2020_resamp <- resample(
  pop_2020_proj,
  tc_pdsi_cropped_rasterobject,
  method = "bilinear")

#test plot: resampled pop in 2020
plot(pop_2020_resamp)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#Pop mask (all pixels where there is pop = 1, others 0)
pop_mask <- pop_2020 > 0

####Include Cropland####
#USE cropland raster
#Here use: MODIS 2021 LandCover
modis_lc <- raster::raster(paste(here(),"Data","Landcover",
                                 "2021_modis_landcover.tif",sep = "/"))

st_crs(tc_pdsi_cropped_rasterobject) == st_crs(modis_lc)

#crop first
modis_lc <- modis_lc |>
  crop(egypt_gov)

#test plot: Land Cover
plot(modis_lc)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
modis_lc_proj <- projectRaster(
  modis_lc,
  crs = crs(tc_pdsi_cropped_rasterobject),
  method = "ngb"#nearest neighbour for cat. variables
)

#2.Resample to match resolution & extent of temp raster
modis_lc_resamp <- resample(
  modis_lc_proj,
  tc_pdsi_cropped_rasterobject,
  method = "ngb"#nearest neighbour for cat. variables
)

#test plot: resampled pop in 2020
plot(modis_lc_resamp)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

# Cropland mask (only cropland pixels = 1, others NA)
cropland_mask <- modis_lc_resamp == 12

#test plot: resampled pop in 2020
plot(cropland_mask)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

####PDSI means####
#Three measures: Simple mean, Pop. weighted mean, cropland mean
#####Pop weighted#####
#Compute mean PDSI & population-weighted PDSI
egypt_pdsi_mean <- cbind(
  egypt_df,
  exact_extract(
    tc_pdsi_cropped_rasterobject,
    egypt_gov,#polygons
    fun = c("mean","weighted_mean"),#mean and weighted mean
    weights = pop_2020_resamp #population raster, resampled to match resolution
  )
)

#long format
egypt_pdsi_mean_long <- egypt_pdsi_mean |>
  pivot_longer(
    cols = starts_with(c("mean.", "weighted_mean.")),
    names_to = c("stat", "date"),
    names_pattern = "(mean|weighted_mean)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    pdsi_mean = mean,
    pdsi_popweighted_mean = weighted_mean
  )

#####Cropland weighted#####
egypt_pdsi_cropland <- cbind(
  egypt_df,
  exact_extract(
    tc_pdsi_cropped_rasterobject,
    egypt_gov,
    fun = c("weighted_mean"),
    weights = cropland_mask
  )
)

#long format
egypt_pdsi_long_cropland <- egypt_pdsi_cropland |>
  pivot_longer(
    cols = starts_with(c("weighted_mean.")),
    names_to = c("stat", "date"),
    names_pattern = "(weighted_mean)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})") |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value) |>
  rename(pdsi_croplandweighted_mean = weighted_mean)

#Left join
pdsi_mean_longdf <- egypt_pdsi_mean_long |>
  left_join(egypt_pdsi_long_cropland,by = c("name","date"))

#add year, month, day columns
pdsi_mean_longdf <- pdsi_mean_longdf |>
  mutate(year = year(date),
         month = month(date),
         day = day(date))

# Save data  --------------------------------------------------------------
####Cropped raster stack####
raster::writeRaster(tc_pdsi_cropped_rasterobject,
                    filename = paste(here(),"Data", "Drought Indices",
                    "TerraClimate",
                    "tc_pdsi_19602023_cropped.tif",sep = "/"))

####Panel Data####
#####R####
saveRDS(pdsi_mean_longdf,file = paste(here(),"Data","Drought Indices",
                                  "TerraClimate","Panel",
                                  "pdsi_19602024",sep = "/"))

#####CSV####
write_csv(pdsi_mean_longdf,file = paste(here(),"Data","Drought Indices",
                                    "TerraClimate","Panel",
                                    "pdsi_19602024.csv",sep = "/"))
