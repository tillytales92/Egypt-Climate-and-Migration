#Test stagg package
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","stagg")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

#install and load stagg package
# install.packages("devtools")
#devtools::install_github("tcarleton/stagg")

#Data was downloaded in 1_ERA5download
#now spatio-temporal aggregation using stagg
egypt_temp_2023 <- raster::stack(paste(here(),"Data", "ERA5","egyt_era_temp_2023.nc",sep = "/"))
target_crs <- crs(era5_grid)

egypt_temp_2023_proj <- projectRaster(egypt_temp_2023, crs = target_crs)

#egypt_temp_jan2016 <- egypt_temp_2016[[1:31]]

# Reproject raster to shapefile CRS
# egypt_temp_jan2016_proj <- projectRaster(
#   egypt_temp_jan2016,
#   crs = crs(era5_grid)   # match era5 crs
# )

#rename
#Covers 1st of Jan 2016 - 30th of March 2016
# dates <- seq(as.Date("2016-01-01"), as.Date("2016-01-31"), by = "day")
#
# # Assign the dates to the RasterStack
# egypt_temp_jan2016_proj <- setZ(egypt_temp_jan2016_proj, dates,name = "Date/time")
#
# # You can also fix the layer names to be cleaner
# names(egypt_temp_jan2016_proj) <- format(dates, "%Y-%m-%d")
#
# egypt_temp_jan2016_proj

#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

egypt_hdx_targetcrs   <- st_transform(egypt_hdx, target_crs)

# Check CRS
st_crs(egypt_hdx_targetcrs) == st_crs(egypt_temp_2023_proj)

#test plot: temperature (celsius) on January 1st, 2016
plot(egypt_temp_2023_proj[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_hdx_targetcrs), add = TRUE, border = "red")

# Step 1 (optional) Resample a secondary data input and generate secondary weights ------
#USE updated pop. raster
pop_2015 <- raster::raster(paste(here(),"Data",
                                 "Landscan_Pop","2015_landscan_pop.tif",sep = "/"))

# 1. Reproject population raster to same CRS as temp raster
pop_2015_proj <- projectRaster(
  pop_2015,
  crs = crs(egypt_temp_2023_proj),
  method = "bilinear"   # bilinear for continuous values like population density
)

# 2. Resample to match resolution & extent of temp raster
pop_2015_resamp <- resample(
  pop_2015_proj,
  egypt_temp_2023_proj,
  method = "bilinear"
)


#test plot
plot(pop_2015_resamp)
plot(st_geometry(egypt_hdx_targetcrs), add = TRUE, border = "red")

#SKIP FOR NOW
#later use population & cropland raster as weights

pop_weights <- secondary_weights(
  secondary_raster = pop_2015_resamp,
  grid = egypt_temp_2023_proj,
  extent = "full")
#Warning: secondary raster contains NA values. NAs will be returned for weights.

# Step 2: Overlay administrative regions onto the data’s grid -------------
#Create empty raster aligned to egypt_temp_2016
# era5_grid_egypt <- raster(
#   extent(era5_grid),
#   res = res(egypt_temp_2023_proj),
#   crs = crs(era5_grid)
# )
#
# #cropped version
# era5_grid_egypt_cropped <- era5_grid_egypt |>
#   crop(egypt_temp_2023)

#test stagg overlay_weights function
gov_weights_self <- overlay_weights(
  polygons = egypt_hdx,# A simple features object with the desired polygons
  polygon_id_col = "ADM1_EN",
  grid = egypt_temp_2023_proj,
  secondary_weights = pop_weights
  )

gov_weights <- overlay_weights(
  polygons = egypt_hdx_targetcrs,# A simple features object with the desired polygons
  polygon_id_col = "ADM1_EN")

print(gov_weights, digits = 4)
gov_weights |> glimpse()

#resample
egypt_temp_2023_resample <- raster::resample(egypt_temp_2023_proj,
                                             era5_grid,
                                            method = "bilinear")

egypt_temp_2023_resample

# Step 3: Transform and aggregate data ------------------------------------
polynomial_output <- staggregate_polynomial(
  data = egypt_temp_2023_proj - 273.15, # A raster brick of our primary data,
  overlay_weights = gov_weights_self, # Output from Step 2:Governorate weights
  daily_agg = "none",#data already at day
  time_agg = "day",#The temporal level to aggregate daily transformed values
  #start_date = "2023-01-01", # The start date of the supplied data,
  #time_interval = '1 day',# only required if the layer name format is not compatible with stagg
  degree = 3# The highest order of the polynomial.
)

polynomial_output |>
  glimpse()

t <- polynomial_output |>
  as_tibble()

#plot - Asyut
t |>
  filter(poly_id == "Asyut") |>
  ggplot()+
  geom_line(aes(x = ))

