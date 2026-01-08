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
                           "Data","Shapefiles","HDX_Egypt",
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

#####Create a yearly raster#####
#Extract dates
dates <- seq(as.Date("1960-01-01"), as.Date("2024-12-31"), by = "year")

# Group by year
year_index <- format(dates, "%Y")
meantemp_era5_yearly <- tapp(temp_era5_dailymean_19602024_cropped, year_index, mean)

# Assign year names
names(meantemp_era5_yearly) <- unique(year_index)

#plot: 1960 average temp
plot(meantemp_era5_yearly[[1]]-273.15)
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
meantemp_era5_yearly_rasterobject <- raster::stack(meantemp_era5_yearly)

#rename layers
#Covers 1st of Jan 1960- 31st of December 2024
dates_yearlymean <- seq(as.Date("1960-01-01"), as.Date("2024-12-31"), by = "year")

# Assign the dates to the RasterStack
meantemp_era5_yearly_rasterobject <- setZ(
  meantemp_era5_yearly_rasterobject,
  dates_yearlymean,name = "Date/time")

# You can also fix the layer names to be cleaner
names(meantemp_era5_yearly_rasterobject) <- format(dates_yearlymean,"%Y-%m-%d")

#check if dates are adjusted
meantemp_era5_yearly_rasterobject

#####Daily Max#####
#crop and mask raster to Assiut and Suhag governorates using terra
temp_era5_dailymax_19602024_cropped <- temp_era5_dailymax_19602024 |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: max. temperature (celsius) on January 1st, 1960
plot(temp_era5_dailymax_19602024_cropped[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#####Create a yearly raster#####
#Extract dates
dates <- seq(as.Date("1960-01-01"), as.Date("2024-12-31"), by = "year")

# Group by year
year_index <- format(dates, "%Y")
maxtemp_era5_yearly <- tapp(temp_era5_dailymax_19602024_cropped, year_index, max)

# Assign year names
names(maxtemp_era5_yearly) <- unique(year_index)

#plot: 1960 max. temp
plot(maxtemp_era5_yearly[[1]]-273.15)
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
maxtemp_era5_yearly_rasterobject <- raster::stack(maxtemp_era5_yearly)

#rename layers
#Covers 1st of Jan 1960- 31st of December 2024
dates_yearlymax <- seq(as.Date("1960-01-01"), as.Date("2024-12-31"), by = "year")

# Assign the dates to the RasterStack
maxtemp_era5_yearly_rasterobject <- setZ(
  maxtemp_era5_yearly_rasterobject,
  dates_yearlymax,name = "Date/time")

# You can also fix the layer names to be cleaner
names(maxtemp_era5_yearly_rasterobject) <- format(dates_yearlymax,"%Y-%m-%d")

#check if dates are adjusted
maxtemp_era5_yearly_rasterobject

#####Daily min#####
#crop and mask raster to Assiut and Suhag governorates using terra
temp_era5_dailymin_19612024_cropped <- temp_era5_dailymin_19612024 |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: min. temperature (celsius) on January 1st, 1961
plot(temp_era5_dailymin_19612024_cropped[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#####Create a yearly raster#####
#Extract dates
dates <- seq(as.Date("1961-01-01"), as.Date("2024-12-31"), by = "year")

# Group by year
year_index <- format(dates, "%Y")
mintemp_era5_yearly <- tapp(temp_era5_dailymin_19612024_cropped, year_index, min)

# Assign year names
names(mintemp_era5_yearly) <- unique(year_index)

#plot: 1961 min. temp
plot(mintemp_era5_yearly[[1]]-273.15)
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
mintemp_era5_yearly_rasterobject <- raster::stack(mintemp_era5_yearly)

#rename layers
#Covers 1st of Jan 1961- 31st of December 2024
dates_yearlymin <- seq(as.Date("1961-01-01"),
                       as.Date("2024-12-31"), by = "year")

# Assign the dates to the RasterStack
mintemp_era5_yearly_rasterobject <- setZ(
  mintemp_era5_yearly_rasterobject,
  dates_yearlymin,name = "Date/time")

# You can also fix the layer names to be cleaner
names(mintemp_era5_yearly_rasterobject) <- format(dates_yearlymin,"%Y-%m-%d")

#check if dates are adjusted
mintemp_era5_yearly_rasterobject

# Extraction: Preparing Pop. and Cropland weights -----------------------------
#Egypt DF
egypt_df <- egypt_gov |>
  st_drop_geometry() |>
  dplyr::select(ADM1_EN)

####Including Landscan Pop. weights####
#USE 2020 pop. raster
pop_2020 <- raster::raster(paste(here(),"Data","Population",
                                 "2020_landscan_pop.tif",sep = "/"))

st_crs(meantemp_era5_yearly_rasterobject) == st_crs(pop_2020)

#crop first
pop_2020 <- pop_2020 |>
  crop(egypt_gov)

#test plot: pop in 2020
plot(pop_2020)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
pop_2020_proj <- projectRaster(
  pop_2020,
  crs = crs(meantemp_era5_yearly_rasterobject),
  method = "bilinear")#bilinear for continuous values like population density

#2.Resample to match resolution & extent of temp raster
pop_2020_resamp <- resample(
  pop_2020_proj,
  meantemp_era5_yearly_rasterobject,
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

st_crs(meantemp_era5_yearly_rasterobject) == st_crs(modis_lc)

#crop first
modis_lc <- modis_lc |>
  crop(egypt_gov)

#test plot: Land Cover
plot(modis_lc)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
modis_lc_proj <- projectRaster(
  modis_lc,
  crs = crs(meantemp_era5_yearly_rasterobject),
  method = "ngb"#nearest neighbour for cat. variables
)

#2.Resample to match resolution & extent of temp raster
modis_lc_resamp <- resample(
  modis_lc_proj,
  meantemp_era5_yearly_rasterobject,
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
egypt_temp_mean_annual <- cbind(
  egypt_df,
  exact_extract(
    meantemp_era5_yearly_rasterobject - 273.15,#temperature raster (in °C)
    egypt_gov,#polygons
    fun = c("mean", "weighted_mean"),#both statistics
    weights = pop_2020_resamp        #population raster, resampled to match
  )
)

#long format
egypt_temp_mean_annual_long <- egypt_temp_mean_annual |>
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
    mean_temp = mean,
    mean_temp_pop_weighted = weighted_mean
  )

#####Cropland weighted#####
egypt_temp_cropland_annual <- cbind(
  egypt_df,
  exact_extract(
    meantemp_era5_yearly_rasterobject - 273.15,   # temperature raster (in °C)
    egypt_gov,             # polygons
    fun = c("weighted_mean"),# both statistics
    weights = cropland_mask        # population raster, resampled to match
  )
)

#long format
egypt_temp_cropland_annual_long <- egypt_temp_cropland_annual |>
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
  rename(mean_temp_cropland_weighted = weighted_mean)

#left join
temp_mean_longdf_annual <- egypt_temp_mean_annual_long |>
  left_join(egypt_temp_cropland_annual_long,by = c("name","date"))

#add year, month, day columns
temp_mean_longdf_annual <- temp_mean_longdf_annual |>
  mutate(year = year(date),
         month = month(date),
         day = day(date))

####Max values####
#Three measures: simple max temp, max temp pop. cells and max temp cropland cells
# Compute daily max temp for each governorate
temp_max <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymax_19602024_cropped_rasterobject - 273.15,# temperature raster (in °C)
    egypt_gov,# polygons
    fun = "max"#extract max. value
  )
)

#long format
egypt_temp_max_long <- temp_max |>
  pivot_longer(
    cols = starts_with(c("max.")),
    names_to = c("stat", "date"),
    names_pattern = "(max)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    max_temp = max
  )

#####Pop crop#####
#Align pop_mask to temp raster
pop_mask_aligned <- resample(pop_mask, temp_era5_dailymax_19602024_cropped_rasterobject,
                             method = "ngb")

# Mask temperature raster using aligned pop_mask
temp_era5_max_popcells <- mask(
  temp_era5_dailymax_19602024_cropped_rasterobject,
  pop_mask_aligned,
  maskvalue = 0  # keep cells where pop_mask == 1
)

#test plot
plot(temp_era5_max_popcells[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#extract with masked temp. raster
temp_max_pop <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_max_popcells - 273.15,# temperature raster (in °C)
    egypt_gov,# polygons
    fun = "max"#extract max. value
  )
)

#long format
temp_max_pop_long <- temp_max_pop |>
  pivot_longer(
    cols = starts_with(c("max.")),
    names_to = c("stat", "date"),
    names_pattern = "(max)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    max_temp_popcells = max
  )

#####Cropland crop#####
#use Cropland mask from above to limit temp. raster to cells with cropland
# Mask temperature raster using aligned pop_mask
temp_era5_max_cropcells <- mask(
  temp_era5_dailymax_19602024_cropped_rasterobject,
  cropland_mask,
  maskvalue = 0  # keep cells where pop_mask == 1
)

#test plot
plot(temp_era5_max_cropcells[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#extract with masked temp. raster
temp_max_cropland <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_max_cropcells - 273.15,# temperature raster (in °C)
    egypt_gov,# polygons
    fun = "max"#extract max. value
  )
)

#long format
temp_max_cropland_long <- temp_max_cropland |>
  pivot_longer(
    cols = starts_with(c("max.")),
    names_to = c("stat", "date"),
    names_pattern = "(max)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    max_temp_croplandcells = max
  )

#left join
temp_max_longdf <- egypt_temp_max_long |>
  left_join(temp_max_pop_long,by = c("name","date")) |>
  left_join(temp_max_cropland_long,by = c("name","date"))


####Min values####
#Three measures: simple min temp, min temp pop. cells and min temp cropland cells
# Compute simple daily min temp for each governorate
temp_min <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_dailymin_19612024_cropped_rasterobject - 273.15,# temperature raster (in °C)
    egypt_gov,# polygons
    fun = "min"#extract min. value
  )
)

#long format
egypt_temp_min_long <- temp_min |>
  pivot_longer(
    cols = starts_with(c("min.")),
    names_to = c("stat", "date"),
    names_pattern = "(min)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    min_temp = min
  )

#####Pop crop#####
#Align pop_mask to temp raster
pop_mask_aligned_mintemp <- resample(pop_mask,
                                     temp_era5_dailymin_19612024_cropped_rasterobject,
                                     method = "ngb")

# Mask temperature raster using aligned pop_mask
temp_era5_min_popcells <- mask(
  temp_era5_dailymin_19612024_cropped_rasterobject,
  pop_mask_aligned_mintemp,
  maskvalue = 0  # keep cells where pop_mask == 1
)

#test plot
plot(temp_era5_min_popcells[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#extract with masked temp. raster
temp_min_pop <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_min_popcells - 273.15,# temperature raster (in °C)
    egypt_gov,# polygons
    fun = "min"#extract min. value
  )
)

#long format
temp_min_pop_long <- temp_min_pop |>
  pivot_longer(
    cols = starts_with(c("min.")),
    names_to = c("stat", "date"),
    names_pattern = "(min)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    min_temp_popcells = min
  )

#####Cropland crop#####
#use Cropland mask from above to limit temp. raster to cells with cropland
# Mask temperature raster using aligned pop_mask
crop_mask_aligned_mintemp <- resample(cropland_mask,
                                      temp_era5_dailymin_19612024_cropped_rasterobject,
                                      method = "ngb")

# Mask temperature raster using aligned pop_mask
temp_era5_min_cropcells <- mask(
  temp_era5_dailymin_19612024_cropped_rasterobject,
  crop_mask_aligned_mintemp,
  maskvalue = 0  # keep cells where pop_mask == 1
)

#test plot
plot(temp_era5_min_cropcells[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#extract with masked temp. raster
temp_min_cropland <- cbind(
  egypt_df,
  exact_extract(
    temp_era5_min_cropcells - 273.15,# temperature raster (in °C)
    egypt_gov,# polygons
    fun = "min"#extract min. value
  )
)

#long format
temp_min_cropland_long <- temp_min_cropland |>
  pivot_longer(
    cols = starts_with(c("min.")),
    names_to = c("stat", "date"),
    names_pattern = "(min)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    min_temp_croplandcells = min
  )

#left join
temp_min_longdf <- egypt_temp_min_long |>
  left_join(temp_min_pop_long,by = c("name","date")) |>
  left_join(temp_min_cropland_long,by = c("name","date"))

# Merging Data Frames -----------------------------------------------------
#here:merge mean, max and min value df
era5_temp_df <- temp_mean_longdf |>
  left_join(temp_max_longdf,by = c("name","date")) |>
  left_join(temp_min_longdf,by = c("name","date"))

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
                    filename = paste(here(),"Data", "Temperature","ERA5",
                                     "temp_era5_dailymean_19602024_cropped_rasterobject.tif",sep = "/"))
raster::writeRaster(temp_era5_dailymax_19602024_cropped_rasterobject,
                    filename = paste(here(),"Data", "Temperature","ERA5",
                                     "temp_era5_dailymax_19602024_cropped_rasterobject.tif",sep = "/"))
raster::writeRaster(temp_era5_dailymin_19612024_cropped_rasterobject,
                    filename = paste(here(),"Data", "Temperature","ERA5",
                                     "temp_era5_dailymin_19612024_cropped_rasterobject.tif",sep = "/"))

####Panel Data####
#####R####
saveRDS(era5_temp_df,file = paste(here(),"Data",
                                  "Temperature","ERA5","Panel",
                                  "era5_temp_19602024",sep = "/"))

#####CSV####
write_csv(era5_temp_df,file = paste(here(),"Data",
                                    "Temperature","ERA5","Panel",
                                    "era5_temp_19602024.csv",sep = "/"))






