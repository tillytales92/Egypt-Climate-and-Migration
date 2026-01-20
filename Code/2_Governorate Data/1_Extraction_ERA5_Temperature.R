#Temperature extraction using exactextractr
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

# Load Climate Data and Shapefile--------------------------------------------------------------
#Loading the Raster Stack: Data was downloaded in 1_ERA5download
#NOTE: these are big files!
#Load Daily mean raster (1960 - 2024)
temp_era5_dailymean_19602024 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                  "temp_era5_dailymean_19602024.tif",sep = "/"))

#Load Daily max raster (1960 - 2024)
temp_era5_dailymax_19602024 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                  "temp_era5_dailymax_19602024.tif",sep = "/"))

#Load Daily min raster (1961 - 2024)
temp_era5_dailymin_19612024 <- terra::rast(paste(here(),"Data", "Temperature","ERA5",
                                  "temp_era5_dailymin_19612024.tif",sep = "/"))

####Load Shapefile and reproject####
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","raw","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

# Check CRS (convert terra crs to WKT for comparison)
st_crs(egypt_hdx)$wkt == crs(temp_era5_dailymean_19602024)

# Reproject shapefile to raster CRS (safer than reprojecting raster)
egypt_hdx_proj <- st_transform(egypt_hdx, crs(temp_era5_dailymean_19602024))

# Double-check
st_crs(egypt_hdx_proj)$wkt == crs(temp_era5_dailymean_19602024)

####Test plots####
#plotting to check if max temp > than mean and min temp. < mean
#Mean temperature (celsius) on January 1st, 1961 (1960 had 366 days)
plot(temp_era5_dailymean_19602024[[367]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

#Max temperature (celsius) on January 1st, 1961 (1960 had 366 days)
plot(temp_era5_dailymax_19602024[[367]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

#Min temperature (celsius) on January 1st, 1961
plot(temp_era5_dailymin_19612024[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

#all adds up!

####Cropping to Governorates and adj. layer names####
#Selecting Assiut, Suhag, Alexandria and Cairo from shapefile
egypt_gov <- egypt_hdx_proj |>
  filter(ADM1_EN %in% c("Assiut","Suhag" ,"Alexandria","Cairo"))

#####Daily Mean#####
#crop and mask raster to Assiut and Suhag governorates using terra
temp_era5_dailymean_19602024_cropped <- temp_era5_dailymean_19602024 |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: temperature (celsius) on January 1st, 1960
plot(temp_era5_dailymean_19602024_cropped[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
temp_era5_dailymean_19602024_cropped_rasterobject <- raster::stack(temp_era5_dailymean_19602024_cropped)

#rename layers
#Covers 1st of Jan 2010 - 31st of December
dates_dailymean <- seq(as.Date("1960-01-01"), as.Date("2024-12-31"), by = "day")

# Assign the dates to the RasterStack
temp_era5_dailymean_19602024_cropped_rasterobject <- setZ(
  temp_era5_dailymean_19602024_cropped_rasterobject,
  dates_dailymean,name = "Date/time")

# You can also fix the layer names to be cleaner
names(temp_era5_dailymean_19602024_cropped_rasterobject) <- format(
  dates_dailymean, "%Y-%m-%d")

#check if dates are adjusted
temp_era5_dailymean_19602024_cropped_rasterobject

#####Daily Max#####
#crop and mask raster to Assiut and Suhag governorates using terra
temp_era5_dailymax_19602024_cropped <- temp_era5_dailymax_19602024 |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: temperature (celsius) on January 1st, 1960
plot(temp_era5_dailymax_19602024_cropped[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
temp_era5_dailymax_19602024_cropped_rasterobject <- raster::stack(temp_era5_dailymax_19602024_cropped)

#rename layers (for exactextract to extract the correct dates)
#Covers 1st of Jan 2010 - 31st of December
dates_dailymax <- seq(as.Date("1960-01-01"), as.Date("2024-12-31"), by = "day")

# Assign the dates to the RasterStack
temp_era5_dailymax_19602024_cropped_rasterobject <- setZ(
  temp_era5_dailymax_19602024_cropped_rasterobject,
  dates_dailymax,name = "Date/time")

# You can also fix the layer names to be cleaner
names(temp_era5_dailymax_19602024_cropped_rasterobject) <- format(
  dates_dailymax, "%Y-%m-%d")

#check if dates are adjusted
temp_era5_dailymax_19602024_cropped_rasterobject

#####Daily Min#####
#crop and mask raster to Assiut and Suhag governorates using terra
temp_era5_dailymin_19612024_cropped <- temp_era5_dailymin_19612024 |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: temperature (celsius) on January 1st, 1961
plot(temp_era5_dailymin_19612024_cropped[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
temp_era5_dailymin_19612024_cropped_rasterobject <- raster::stack(temp_era5_dailymin_19612024_cropped)

#rename layers
#Covers 1st of Jan 2010 - 31st of December
dates_dailymin <- seq(as.Date("1961-01-01"), as.Date("2024-12-31"), by = "day")

# Assign the dates to the RasterStack
temp_era5_dailymin_19612024_cropped_rasterobject <- setZ(
  temp_era5_dailymin_19612024_cropped_rasterobject,
  dates_dailymin,name = "Date/time")

# You can also fix the layer names to be cleaner
names(temp_era5_dailymin_19612024_cropped_rasterobject) <- format(
  dates_dailymin, "%Y-%m-%d")

#check if dates are adjusted
temp_era5_dailymin_19612024_cropped_rasterobject

# Extraction: Preparing Pop. and Cropland weights -----------------------------
#Egypt DF
egypt_df <- egypt_gov |>
  st_drop_geometry() |>
  dplyr::select(ADM1_EN)

####Including Landscan Pop. weights####
#USE 2020 pop. raster
pop_2020 <- raster::raster(paste(here(),"Data","raw","Landscan",
                                 "2020_landscan_pop.tif",sep = "/"))

st_crs(temp_era5_dailymean_19602024_cropped_rasterobject) == st_crs(pop_2020)

#crop first
pop_2020 <- pop_2020 |>
  crop(egypt_gov)

#test plot: pop in 2020
plot(pop_2020)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
pop_2020_proj <- projectRaster(
  pop_2020,
  crs = crs(temp_era5_dailymean_19602024_cropped_rasterobject),
  method = "bilinear")#bilinear for continuous values like population density

#2.Resample to match resolution & extent of temp raster
pop_2020_resamp <- resample(
  pop_2020_proj,
  temp_era5_dailymean_19602024_cropped_rasterobject,
  method = "bilinear")

#test plot: resampled pop in 2020
plot(pop_2020_resamp)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#Pop mask (all pixels where there is pop = 1, others 0)
pop_mask <- pop_2020 > 0

####Include Cropland####
#USE cropland raster
#Here use: MODIS 2021 LandCover
modis_lc <- raster::raster(paste(here(),"Data","raw","Modis",
                                 "2021_modis_landcover.tif",sep = "/"))

st_crs(temp_era5_dailymean_19602024_cropped_rasterobject) == st_crs(modis_lc)

#crop first
modis_lc <- modis_lc |>
  crop(egypt_gov)

#test plot: Land Cover
plot(modis_lc)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
modis_lc_proj <- projectRaster(
  modis_lc,
  crs = crs(temp_era5_dailymean_19602024_cropped_rasterobject),
  method = "ngb"#nearest neighbour for cat. variables
)

#2.Resample to match resolution & extent of temp raster
modis_lc_resamp <- resample(
  modis_lc_proj,
  temp_era5_dailymean_19602024_cropped_rasterobject,
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

# Extracting values -------------------------------------------------------
####Mean values####
#Three measures: Simple mean temp., pop. weighted mean temp. & cropland weighted
#####Pop weighted#####
# Compute mean temp. & population-weighted mean temperature
egypt_temp_mean <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymean_19602024_cropped_rasterobject - 273.15,#temperature raster (in °C)
    egypt_gov,#polygons
    fun = c("mean", "weighted_mean"),#both statistics
    weights = pop_2020_resamp        #population raster, resampled to match
  )
)

#long format
egypt_temp_mean_long <- egypt_temp_mean |>
  pivot_longer(
    cols = starts_with(c("mean.", "weighted_mean.")),
    names_to = c("stat", "date"),
    names_pattern = "(mean|weighted_mean)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value) |>
  rename(
    mean_temp = mean,
    mean_temp_pop_weighted = weighted_mean)

#####Cropland weighted#####
egypt_temp_cropland <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymean_19602024_cropped_rasterobject - 273.15,#temperature raster (in °C)
    egypt_gov,#polygons
    fun = c("weighted_mean"),#both statistics
    weights = cropland_mask#population raster, resampled to match
  )
)

#long format
egypt_temp_long_cropland <- egypt_temp_cropland |>
  pivot_longer(
    cols = starts_with(c("weighted_mean.")),
    names_to = c("stat", "date"),
    names_pattern = "(weighted_mean)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})")|>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value) |>
  rename(mean_temp_cropland_weighted = weighted_mean)

#left join
temp_mean_longdf <- egypt_temp_mean_long |>
  left_join(egypt_temp_long_cropland,by = c("name","date"))

####Max values####
#Three measures: simple max temp, max temp pop. cells and max temp cropland cells
#####Pop weighted#####
#Compute mean max. temp. & population-weighted mean max. temperature
egypt_temp_meanmax <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymax_19602024_cropped_rasterobject - 273.15,#temperature raster (in °C)
    egypt_gov,#polygons
    fun = c("mean", "weighted_mean"),#both statistics
    weights = pop_2020_resamp        #population raster, resampled to match
  )
)

#long format
egypt_temp_meanmax_long <- egypt_temp_meanmax |>
  pivot_longer(
    cols = starts_with(c("mean.", "weighted_mean.")),
    names_to = c("stat", "date"),
    names_pattern = "(mean|weighted_mean)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})") |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value) |>
  rename(
    mean_maxtemp = mean,
    mean_maxtemp_pop_weighted = weighted_mean)

#####Cropland weighted#####
egypt_temp_meanmax_cropland <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymax_19602024_cropped_rasterobject - 273.15,   # temperature raster (in °C)
    egypt_gov,             # polygons
    fun = c("weighted_mean"),# both statistics
    weights = cropland_mask        # population raster, resampled to match
  )
)

#long format
egypt_temp_meanmax_cropland_long <- egypt_temp_meanmax_cropland |>
  pivot_longer(
    cols = starts_with(c("weighted_mean.")),
    names_to = c("stat", "date"),
    names_pattern = "(weighted_mean)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value) |>
  rename(mean_maxtemp_cropland_weighted = weighted_mean)

#left join
temp_meanmax_longdf <- egypt_temp_meanmax_long |>
  left_join(egypt_temp_meanmax_cropland_long,by = c("name","date"))

####Min values####
#Three measures: simple min temp average and weighted by pop./cropland
#####Pop weighted#####
#Compute mean max. temp. & population-weighted mean max. temperature
#adjust pop_2020_resamp CRS
#1.Reproject population raster to same CRS as temp raster
pop_2020_proj_mintemp <- projectRaster(
  pop_2020_proj,
  crs = crs(temp_era5_dailymin_19612024_cropped_rasterobject),
  method = "bilinear")#bilinear for continuous values like population density

#2.Resample to match resolution & extent of temp raster
pop_2020_resamp_mintemp <- resample(
  pop_2020_proj_mintemp,
  temp_era5_dailymin_19612024_cropped_rasterobject,
  method = "bilinear")

egypt_temp_meanmin <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymin_19612024_cropped_rasterobject - 273.15,#temperature raster (in °C)
    egypt_gov,#polygons
    fun = c("mean", "weighted_mean"),#both statistics
    weights = pop_2020_resamp_mintemp        #population raster, resampled to match
  )
)

#long format
egypt_temp_meanmin_long <- egypt_temp_meanmin |>
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
    mean_mintemp = mean,
    mean_mintemp_pop_weighted = weighted_mean
  )

#####Cropland weighted#####
#1.Reproject population raster to same CRS as temp raster
modis_lc_proj_mintemp <- projectRaster(
  modis_lc,
  crs = crs(temp_era5_dailymin_19612024_cropped_rasterobject),
  method = "ngb"#nearest neighbour for cat. variables
)

#2.Resample to match resolution & extent of temp raster
modis_lc_resamp_mintemp <- resample(
  modis_lc_proj_mintemp,
  temp_era5_dailymin_19612024_cropped_rasterobject,
  method = "ngb"#nearest neighbour for cat. variables
)

# Cropland mask (only cropland pixels = 1, others NA)
cropland_mask_mintemp <- modis_lc_resamp_mintemp == 12


egypt_temp_meanmin_cropland <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymin_19612024_cropped_rasterobject - 273.15,   # temperature raster (in °C)
    egypt_gov,             # polygons
    fun = c("weighted_mean"),# both statistics
    weights = cropland_mask_mintemp        # population raster, resampled to match
  )
)

#long format
egypt_temp_meanmin_cropland_long <- egypt_temp_meanmin_cropland |>
  pivot_longer(
    cols = starts_with(c("weighted_mean.")),
    names_to = c("stat", "date"),
    names_pattern = "(weighted_mean)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value) |>
  rename(mean_mintemp_cropland_weighted = weighted_mean)

#left join
temp_meanmin_longdf <- egypt_temp_meanmin_long |>
  left_join(egypt_temp_meanmin_cropland_long,by = c("name","date"))

# Merging Data Frames -----------------------------------------------------
#here:merge mean, max and min value df
era5_temp_df <- temp_mean_longdf |>
  left_join(temp_meanmax_longdf,by = c("name","date")) |>
  left_join(temp_meanmin_longdf,by = c("name","date"))

#add year, month, day columns
era5_temp_df <- era5_temp_df |>
  mutate(year = year(date),
         month = month(date),
         day = day(date))

#order variables
era5_temp_df <- era5_temp_df |>
  relocate(year,.after = date) |>
  relocate(month,.after = year) |>
  relocate(day,.after = month)

# Save data  --------------------------------------------------------------
####Cropped raster stacks####
raster::writeRaster(temp_era5_dailymean_19602024_cropped_rasterobject,
                    filename = paste(here(),"Data", "intermediate","ERA5",
                      "temp_era5_dailymean_19602024_cropped_rasterobject.tif",sep = "/"))
raster::writeRaster(temp_era5_dailymax_19602024_cropped_rasterobject,
                    filename = paste(here(),"Data", "intermediate","ERA5",
                    "temp_era5_dailymax_19602024_cropped_rasterobject.tif",sep = "/"))
raster::writeRaster(temp_era5_dailymin_19612024_cropped_rasterobject,
                    filename = paste(here(),"Data", "intermediate","ERA5",
                    "temp_era5_dailymin_19612024_cropped_rasterobject.tif",sep = "/"))

####Panel Data####
#R
saveRDS(era5_temp_df,file = paste(here(),"Data","intermediate",
                                  "Governorate Data",
                                  "era5_temp_19602024.Rds",sep = "/"))


