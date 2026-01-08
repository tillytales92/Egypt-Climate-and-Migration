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

# Load Data ----------------------------------------------------------------
#Loading the Raster Stack: Data was downloaded in 1_ERA5download
egypt_temp_2010decade <- raster::stack(paste(here(),"Data", "ERA5",
                                             "egypt_era_temp_2010decade.tif",sep = "/"))

#select year 2010 only
egypt_temp_2010 <- egypt_temp_2010decade[[1:100]]

#shapefile
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

# Check CRS
st_crs(egypt_hdx) == st_crs(egypt_temp_2010)

#reproject CRS
#target_crs <- 4326
#egypt_temp_2010only <- projectRaster(egypt_temp_2010only, crs = target_crs)
egypt_hdx   <- st_transform(egypt_hdx, crs(egypt_temp_2010))

# Check CRS again
st_crs(egypt_hdx) == st_crs(egypt_temp_2010)

#test plot: temperature (celsius) on January 1st, 2016
plot(egypt_temp_2010[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

#crop to Assiut and Suhag
egypt_gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

# Cropping ----------------------------------------------------------------
####Approach 1: Faster with raster -> turn sf to sp object
# 0) Helpful runtime settings
rasterOptions(progress = "text")              # see progress
# rasterOptions(tmpdir = "/fast/ssd/tmp")     # optional: put temp files on a fast disk

# 1) Ensure same CRS first (avoid on-the-fly reprojection)
egypt_gov <- st_transform(egypt_gov, crs(egypt_temp_2010))  # if egypt_gov is sf
egypt_gov_sp <- as(egypt_gov, "Spatial")                    # raster::mask/crop prefer sp

# 2) Crop to bbox, then mask to geometry
t_start <- Sys.time()
temp_2010_crop  <- crop(egypt_temp_2010, egypt_gov_sp)
temp_2010_mask  <- mask(temp_2010_crop, egypt_gov_sp)
t_end <- Sys.time()
print(t_end - t_start)  # measure on your machine

####Approach 2: parallelize the masks step
beginCluster()  # uses available cores
temp_2010_crop <- crop(egypt_temp_2010, egypt_gov_sp)
temp_2010_mask <- clusterR(temp_2010_crop, fun = mask, args = list(mask = egypt_gov_sp))
endCluster()

#####Approach 3: use terra
egypt_temp_2010decade_terra <- terra::rast(paste(here(),"Data", "ERA5",
                                             "egypt_era_temp_2010decade.tif",sep = "/"))

#select year 2010 only
egypt_temp_2010_terra <- egypt_temp_2010decade_terra[[1:100]]

v <- vect(egypt_gov) |> project(crs(egypt_temp_2010_terra))    # match CRS
t0 <- Sys.time()
egypt_temp_2010_terra_crop <- crop(egypt_temp_2010_terra, v, mask = TRUE)              # crop + mask in one go
t1 <- Sys.time()
print(t1 - t0)

#indeed MUCH faster

#test plot: temperature (celsius) on January 1st, 2016
plot(egypt_temp_2010_terra_crop[[1]]-273.15)#transform kelvin to celsius
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#exactextract
# Compute mean temp. & population-weighted mean temperature
egypt <- cbind(
  egypt_df,
  exact_extract(
    egypt_temp_2010_terra_crop - 273.15,   # temperature raster (in °C)
    egypt_gov,             # polygons
    fun = c("mean", "weighted_mean"),# both statistics
    weights = pop_2015_resamp        # population raster, resampled to match
  )
)