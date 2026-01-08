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

# Load Climate Data and Shapefile--------------------------------------------------------------
#Loading the Raster Stack: Data was downloaded in 1_ERA5download
#NOTE: these are big files!
#Load Daily sum raster (1981 - 2024)
#Read in combined raster
prec_era5_dailysum_19812024 <- terra::rast(paste(here(),"Data", "Precipitation","ERA5",
                                "prec_era5_dailysum_19812024.tif",sep = "/"))

####Load Shapefile and reproject####
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

# Check CRS (convert terra crs to WKT for comparison)
st_crs(egypt_hdx)$wkt == crs(prec_era5_dailysum_19812024)

# Reproject shapefile to raster CRS (safer than reprojecting raster)
egypt_hdx_proj <- st_transform(egypt_hdx, crs(prec_era5_dailysum_19812024))

# Double-check
st_crs(egypt_hdx_proj)$wkt == crs(prec_era5_dailysum_19812024)

####Test plots####
#Daily sum of rainfall (metres/day) on January 1st, 1981
plot(prec_era5_dailysum_19812024[[1]])
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

#Daily sum of rainfall (mm/day) on January 1st, 1981
plot(prec_era5_dailysum_19812024[[1]] * 1000)#to turn from metres to mm per day
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

#turn the entire raster into mm/day (more common measure)
prec_era5_dailysum_19812024 <- prec_era5_dailysum_19812024 * 1000

####Cropping to Governorates and adj. layer names####
#Selecting Assiut, Suhag, Alexandria and Cairo from shapefile
egypt_gov <- egypt_hdx_proj |>
  filter(ADM1_EN %in% c("Assiut","Suhag" ,"Alexandria","Cairo"))

#####Daily Sum of rainfall (mm)#####
#crop and mask raster to Assiut and Suhag governorates using terra
prec_era5_dailysum_19812024_cropped <- prec_era5_dailysum_19812024 |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: temperature (celsius) on January 1st, 1981
plot(prec_era5_dailysum_19812024_cropped[[1]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#now turn into raster object for exactextractr to work
prec_era5_dailysum_19812024_cropped_rasterobject <- raster::stack(prec_era5_dailysum_19812024_cropped)

#rename layers
#Covers 1st of Jan 1981 - 31st of December 2024
dates_dailysum <- seq(as.Date("1981-01-01"), as.Date("2024-12-31"), by = "day")

# Assign the dates to the RasterStack
prec_era5_dailysum_19812024_cropped_rasterobject <- setZ(
  prec_era5_dailysum_19812024_cropped_rasterobject,
  dates_dailysum,name = "Date/time")

# You can also fix the layer names to be cleaner
names(prec_era5_dailysum_19812024_cropped_rasterobject) <- format(
  dates_dailysum, "%Y-%m-%d")

#check if dates are adjusted
prec_era5_dailysum_19812024_cropped_rasterobject

#Extraction: Preparing Pop. and Cropland weights -----------------------------
#Egypt DF
egypt_df <- egypt_gov |>
  st_drop_geometry() |>
  dplyr::select(ADM1_EN)

####Including Landscan Pop. weights####
#USE 2020 pop. raster
pop_2020 <- raster::raster(paste(here(),"Data","Population",
                                 "2020_landscan_pop.tif",sep = "/"))

st_crs(prec_era5_dailysum_19812024_cropped_rasterobject) == st_crs(pop_2020)

#crop first
pop_2020 <- pop_2020 |>
  crop(egypt_gov)

#test plot: pop in 2020
plot(pop_2020)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
pop_2020_proj <- projectRaster(
  pop_2020,
  crs = crs(prec_era5_dailysum_19812024_cropped_rasterobject),
  method = "bilinear")#bilinear for continuous values like population density

#2.Resample to match resolution & extent of temp raster
pop_2020_resamp <- resample(
  pop_2020_proj,
  prec_era5_dailysum_19812024_cropped_rasterobject,
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

st_crs(prec_era5_dailysum_19812024_cropped_rasterobject) == st_crs(modis_lc)

#crop first
modis_lc <- modis_lc |>
  crop(egypt_gov)

#test plot: Land Cover
plot(modis_lc)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
modis_lc_proj <- projectRaster(
  modis_lc,
  crs = crs(prec_era5_dailysum_19812024_cropped_rasterobject),
  method = "ngb"#nearest neighbour for cat. variables
)

#2.Resample to match resolution & extent of temp raster
modis_lc_resamp <- resample(
  modis_lc_proj,
  prec_era5_dailysum_19812024_cropped_rasterobject,
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
####Rainfall sums####
#Three measures: Simple rainfall sum, Pop. weighted sum, cropland weighted sum
#####Pop weighted#####
# Compute mean temp. & population-weighted mean temperature
egypt_prec_sum <- cbind(
  egypt_df,
  exact_extract(
    prec_era5_dailysum_19812024_cropped_rasterobject,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("mean","weighted_mean","sum", "weighted_sum"),#means and sums
    weights = pop_2020_resamp #population raster, resampled to match
  )
)

#long format
egypt_prec_sum_long <- egypt_prec_sum |>
  pivot_longer(
    cols = starts_with(c("sum", "weighted_sum.")),
    names_to = c("stat", "date"),
    names_pattern = "(sum|weighted_sum)\\.X(\\d{4}\\.\\d{2}\\.\\d{2})"
  ) |>
  mutate(date = as.Date(date, format = "%Y.%m.%d")) |>
  rename(name = ADM1_EN) |>
  pivot_wider(
    names_from = stat,
    values_from = value
  ) |>
  rename(
    sum_prec_era5 = sum,
    sum_prec_popweighted_era5 = weighted_sum
  )

#####Cropland weighted#####
egypt_prec_sum_cropland <- cbind(
  egypt_df,
  exact_extract(
    prec_era5_dailysum_19812024_cropped_rasterobject,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("sum", "weighted_sum"),#both statistics
    weights = cropland_mask #population raster, resampled to match
  )
)

#long format
egypt_temp_long_cropland <- egypt_temp_cropland |>
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
temp_mean_longdf <- egypt_temp_mean_long |>
  left_join(egypt_temp_long_cropland,by = c("name","date"))

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



