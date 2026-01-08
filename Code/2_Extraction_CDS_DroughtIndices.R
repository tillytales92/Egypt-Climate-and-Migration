#Combining ERA 5 SPI raster
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

# Load Climate Data Rasters------------------------------------------------
#Loading the Raster Stack: Data was downloaded through Copernicus CDS
####SPI####
#6 months
#1960 raster
egypt_spi <- terra::rast(paste(here(),"Data", "Drought Indices","ERA5",
                        "spi6_ERA5_no-expt_mon_19400101-20241201.nc",sep = "/"))

africa_spi <- terra::rast(paste(here(),"Data", "Drought Indices","ERA5",
                               "spi6_era5_africa.nc",sep = "/"))

#Africa shapefile
get_africa_sf <- function(africa) {
  africa <- giscoR::gisco_get_countries(
    year = "2020",
    epsg = "4326",
    resolution = "10",
    region = "Africa"
  )
  return(africa)
}

africa <- get_africa_sf()

#Egypt GOV. shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#plot Africa SPI
plot(africa_spi[[900]])
plot(st_geometry(africa), add = TRUE, border = "red")

#plot egypt
plot(egypt_spi[[900]])
plot(st_geometry(egypt_hdx), add = TRUE, border = "red")

####SPEI####
#6months
africa_spei <- terra::rast(paste(here(),"Data", "Drought Indices","ERA5",
                                "spei6_era5_africa.nc",sep = "/"))

plot(africa_spei[[1020]])
plot(st_geometry(africa), add = TRUE, border = "red")
