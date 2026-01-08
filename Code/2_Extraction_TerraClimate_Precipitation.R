#Precipitation extraction using exactextractr
#Here:TerraClimate precipitation
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
tc_prec <- terra::rast(paste(here(),"Data", "Precipitation","TerraClimate",
                             "tc_prec_19602023.tif",sep = "/"))

####Load Shapefile and reproject####
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

# Check CRS (convert terra crs to WKT for comparison)
st_crs(egypt_hdx)$wkt == crs(tc_prec)

# Reproject shapefile to raster CRS (safer than reprojecting raster)
egypt_hdx_proj <- st_transform(egypt_hdx, crs(tc_prec))

# Double-check
st_crs(egypt_hdx_proj)$wkt == crs(tc_prec)

####Test plots####
#PDSI on January 1st,1960, NOTE: high value --> high moisture, low value: drought risk
#see:https://climatedataguide.ucar.edu/climate-data/palmer-drought-severity-index-pdsi
plot(tc_prec[[1]])
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")