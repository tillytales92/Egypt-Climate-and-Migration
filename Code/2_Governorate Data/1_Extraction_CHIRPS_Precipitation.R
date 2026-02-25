#Precipitation extraction using exactextractr
#Data: CHIRPS dataset (daily sum of rainfall)
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","here","terra","raster","sf","exactextractr")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load Climate Data and Shapefile--------------------------------------------------------------
#Loading the Raster Stack: Data was downloaded through GEE and combined in "1_CHIRPS..."
#NOTE: these are big files!
#Load Daily sum raster (1981 - 2024)
#Read in combined raster
prec_chirps <- terra::rast(paste(here(),"Data", "intermediate","CHIRPS",
                                 "prec_chirps_dailysum_19812024.tif",sep = "/"))

###Load Shapefile and reproject####
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","raw","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

# Check CRS (convert terra crs to WKT for comparison)
st_crs(egypt_hdx)$wkt == crs(prec_chirps)

# Reproject shapefile to raster CRS (safer than reprojecting raster)
egypt_hdx_proj <- st_transform(egypt_hdx, crs(prec_chirps))

# Double-check
st_crs(egypt_hdx_proj)$wkt == crs(prec_chirps)

####Test plots####
#Daily sum of rainfall (metres/day) on January 1st, 1981
plot(prec_chirps[[1]])
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

####Cropping to Governorates and adj.layer names####
egypt_gov <- egypt_hdx_proj

#Daily Sum of rainfall (mm)
#crop and mask raster to select governorates using terra
prec_chirps_cropped <- prec_chirps |>
  crop(vect(egypt_gov),mask = TRUE)

#test plot: #Daily sum of rainfall (metres/day) on January 1st, 1981
plot(prec_chirps_cropped[[1]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#####Create a yearly raster#####
#Extract dates
dates <- as.Date(names(prec_chirps_cropped), format = "%Y-%m-%d")

# Group by month
year_index <- format(dates, "%Y")
prec_chirps_cropped_yearly <- tapp(prec_chirps_cropped, year_index, sum)

# Assign month names
names(prec_chirps_cropped_yearly) <- unique(year_index)

#plot: 1981 rainfall sum
plot(prec_chirps_cropped_yearly[[6]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#####Create a monthly raster#####
#Group by month
month_index <- format(dates, "%Y-%m")
prec_chirps_cropped_monthly <- tapp(prec_chirps_cropped, month_index, sum)

# Assign month names
names(prec_chirps_cropped_monthly) <- unique(month_index)

#plot: June 1981
plot(prec_chirps_cropped_monthly[[6]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

####Turn into raster objects####
#now turn into raster object for exactextractr to work
#####Yearly sum raster####
prec_chirps_cropped_yearly <- raster::stack(prec_chirps_cropped_yearly)

#rename layers
#Covers Jan 1981 - December 2024
dates_yearlyysum <- seq(as.Date("1981-01-01"), as.Date("2024-12-01"), by = "year")

# Assign the dates to the RasterStack
prec_chirps_cropped_yearly <- setZ(prec_chirps_cropped_yearly,
                                   dates_yearlyysum,name = "Date/time")

# You can also fix the layer names to be cleaner
names(prec_chirps_cropped_yearly) <- format(dates_yearlyysum, "%Y-%m-%d")

#check if dates are adjusted
prec_chirps_cropped_yearly

#####Monthly sum raster####
prec_chirps_cropped_monthly <- raster::stack(prec_chirps_cropped_monthly)

#rename layers
#Covers Jan 1981 - December 2024
dates_monthlysum <- seq(as.Date("1981-01-01"), as.Date("2024-12-01"), by = "month")

# Assign the dates to the RasterStack
prec_chirps_cropped_monthly <- setZ(
  prec_chirps_cropped_monthly,dates_monthlysum,name = "Date/time")

# You can also fix the layer names to be cleaner
names(prec_chirps_cropped_monthly) <- format(dates_monthlysum, "%Y-%m-%d")

#check if dates are adjusted
prec_chirps_cropped_monthly

#####Daily sum raster#####
prec_chirps_cropped_rasterobject <- raster::stack(prec_chirps_cropped)

#rename layers
#Covers 1st of Jan 1981 - 31st of December 2024
dates_dailysum <- seq(as.Date("1981-01-01"), as.Date("2024-12-31"), by = "day")

# Assign the dates to the RasterStack
prec_chirps_cropped_rasterobject <- setZ(
  prec_chirps_cropped_rasterobject,dates_dailysum,name = "Date/time")

# You can also fix the layer names to be cleaner
names(prec_chirps_cropped_rasterobject) <- format(
  dates_dailysum, "%Y-%m-%d")

#check if dates are adjusted
prec_chirps_cropped_rasterobject

#Extraction: Preparing Pop. and Cropland weights -----------------------------
#Egypt DF
egypt_df <- egypt_gov |>
  st_drop_geometry() |>
  dplyr::select(ADM1_EN)

####Including Landscan Pop. weights####
#USE 2020 pop. raster
pop_2020 <- raster::raster(paste(here(),"Data","raw","Landscan",
                                 "2020_landscan_pop.tif",sep = "/"))

st_crs(prec_chirps_cropped_rasterobject) == st_crs(pop_2020)

#crop first
pop_2020 <- pop_2020 |>
  crop(egypt_gov)

#test plot: pop in 2020
plot(pop_2020)
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
pop_2020_proj <- projectRaster(
  pop_2020,
  crs = crs(prec_chirps_cropped_rasterobject),
  method = "bilinear")#bilinear for continuous values like population density

#2.Resample to match resolution & extent of temp raster
pop_2020_resamp <- resample(
  pop_2020_proj,
  prec_chirps_cropped_rasterobject,
  method = "bilinear")

#test plot: resampled pop in 2020
plot(pop_2020_resamp)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#Pop mask (all pixels where there is pop = 1, others 0)
pop_mask <- pop_2020 > 0

####Including Cropland####
#USE cropland raster
#Here use: MODIS 2021 LandCover
modis_lc <- raster::raster(paste(here(),"Data","raw","Modis",
                                 "2021_modis_landcover.tif",sep = "/"))

st_crs(prec_chirps_cropped_rasterobject) == st_crs(modis_lc)

#crop first
modis_lc <- modis_lc |>
  crop(egypt_gov)

#test plot: Land Cover
plot(modis_lc)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#1.Reproject population raster to same CRS as temp raster
modis_lc_proj <- projectRaster(
  modis_lc,
  crs = crs(prec_chirps_cropped_rasterobject),
  method = "ngb"#nearest neighbour for cat. variables
)

#2.Resample to match resolution & extent of temp raster
modis_lc_resamp <- resample(
  modis_lc_proj,
  prec_chirps_cropped_rasterobject,
  method = "ngb"#nearest neighbour for cat. variables
)

#test plot: resampled pop in 2020
plot(modis_lc_resamp)
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

# Cropland mask (only cropland pixels = 1, others NA)
cropland_mask <- modis_lc_resamp == 12

#test plot: resampled pop in 2020
plot(cropland_mask)
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#extract 2024 rasters for test
prec_2024 <- prec_chirps_cropped_rasterobject[[grep("2024", names(prec_chirps_cropped_rasterobject))]]

# Extracting values -------------------------------------------------------
####Yearly Rainfall means####
#Three measures: Simple rainfall mean, Pop. weighted mean, cropland weighted mean
#####Pop weighted#####
# Compute rainfall sum. & population-weighted sum
egypt_prec_yearly <- cbind(
  egypt_df,
  exact_extract(
    prec_chirps_cropped_yearly,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("mean","weighted_mean"),#mean & weighted mean
    weights = pop_2020_resamp #population raster, resampled to match
  )
)

#long format
egypt_prec_yearly_long <- egypt_prec_yearly |>
  pivot_longer(
    cols = starts_with(c("mean", "weighted_mean.")),
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
    mean_prec_chirps = mean,
    mean_prec_popweighted_chirps = weighted_mean
  )

#####Cropland weighted#####
#Compute cropland-weighted sum
egypt_prec_yearly_cropland <- cbind(
  egypt_df,
  exact_extract(
    prec_chirps_cropped_yearly,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("weighted_mean"),#weighted mean
    weights = cropland_mask #cropland mask (only cropland incl.)
  )
)

#long format
egypt_prec_yearly_cropland_long <- egypt_prec_yearly_cropland |>
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
  rename(mean_prec_croplandweighted_chirps = weighted_mean)

#join both DF together
egypt_prec_yearly <- egypt_prec_yearly_long |>
  left_join(egypt_prec_yearly_cropland_long,by = c("name","date"))

#add year and month column
egypt_prec_yearly<- egypt_prec_yearly |>
  mutate(year = year(date),
         month = month(date))

#relocate columns
egypt_prec_myearly <- egypt_prec_yearly |>
  relocate(year,.after = date) |>
  relocate(month,.after = year)

#rename df
chirps_prec_yearly <- egypt_prec_myearly

####Monthly Rainfall means####
#Three measures: Simple rainfall mean, Pop. weighted mean, cropland weighted mean
#####Pop weighted#####
# Compute rainfall sum. & population-weighted sum
egypt_prec_monthly <- cbind(
  egypt_df,
  exact_extract(
    prec_chirps_cropped_monthly,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("mean","weighted_mean"),#mean & weighted mean
    weights = pop_2020_resamp #population raster, resampled to match
  )
)

#long format
egypt_prec_monthly_long <- egypt_prec_monthly |>
  pivot_longer(
    cols = starts_with(c("mean", "weighted_mean.")),
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
    mean_prec_chirps = mean,
    mean_prec_popweighted_chirps = weighted_mean
  )

#####Cropland weighted#####
#Compute cropland-weighted sum
egypt_prec_monthly_cropland <- cbind(
  egypt_df,
  exact_extract(
    prec_chirps_cropped_monthly,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("weighted_mean"),#weighted mean
    weights = cropland_mask #cropland mask (only cropland incl.)
  )
)

#long format
egypt_prec_monthly_cropland_long <- egypt_prec_monthly_cropland |>
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
  rename(mean_prec_croplandweighted_chirps = weighted_mean)

#join both DF together
egypt_prec_monthly <- egypt_prec_monthly_long |>
  left_join(egypt_prec_monthly_cropland_long,by = c("name","date"))

#add year and month column
egypt_prec_monthly <- egypt_prec_monthly |>
  mutate(year = year(date),
         month = month(date))

#relocate columns
egypt_prec_monthly <- egypt_prec_monthly |>
  relocate(year,.after = date) |>
  relocate(month,.after = year)

#rename df
chirps_prec_monthly <- egypt_prec_monthly

####Daily Rainfall means####
#####Pop weighted#####
# Compute rainfall sum. & population-weighted sum
egypt_prec_daily <- cbind(
  egypt_df,
  exact_extract(
    prec_chirps_cropped_rasterobject,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("mean","weighted_mean"),#mean & weighted mean
    weights = pop_2020_resamp #population raster, resampled to match
  )
)

#long format
egypt_prec_daily_long <- egypt_prec_daily |>
  pivot_longer(
    cols = starts_with(c("mean", "weighted_mean.")),
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
    mean_prec_chirps = mean,
    mean_prec_popweighted_chirps = weighted_mean
  )

#####Cropland weighted#####
#Compute cropland-weighted sum
egypt_prec_daily_cropland <- cbind(
  egypt_df,
  exact_extract(
    prec_chirps_cropped_rasterobject,#rainfall raster (mm/d)
    egypt_gov,#polygons
    fun = c("weighted_mean"),#weighted mean
    weights = cropland_mask #cropland mask (only cropland incl.)
  )
)

#long format
egypt_prec_daily_cropland_long <- egypt_prec_daily_cropland |>
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
  rename(mean_prec_croplandweighted_chirps = weighted_mean)

#join both DF together
egypt_prec_daily <- egypt_prec_daily_long |>
  left_join(egypt_prec_daily_cropland_long,by = c("name","date"))

#add year and month column
egypt_prec_daily <- egypt_prec_daily |>
  mutate(year = year(date),
         month = month(date),
         day = day(date))

#relocate columns
egypt_prec_daily <- egypt_prec_daily |>
  relocate(year,.after = date) |>
  relocate(month,.after = year) |>
  relocate(day,.after = month)

#rename df
chirps_prec_daily <- egypt_prec_daily

# Save Data ---------------------------------------------------------------
####Cropped raster stacks####
####Daily stack#####
raster::writeRaster(prec_chirps_cropped_rasterobject,
                    filename = paste(here(),"Data", "intermediate","CHIRPS",
                    "prec_chirps_dailysum_19812024_cropped.tif",sep = "/"))

####Panel Data####
#Yearly data
saveRDS(chirps_prec_yearly,file = paste(here(),
                                  "Data","intermediate","Governorate Data",
                                  "chirps_prec_yearly_19812024",sep = "/"))

#Monthly data
saveRDS(chirps_prec_monthly,file = paste(here(),"Data","intermediate","Governorate Data",
                                   "chirps_prec_monthly_19812024",sep = "/"))

#Daily data
saveRDS(chirps_prec_daily,file = paste(here(),"Data","intermediate","Governorate Data",
                                     "chirps_prec_daily_19812024",sep = "/"))





