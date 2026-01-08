#Downloading TerraClimate Data using climateR package
#see here for info: https://www.climatologylab.org/terraclimate.html
#install the packages from github, do it in the console not from here
# remotes::install_github("mikejohnson51/AOI") # suggested!
# remotes::install_github("mikejohnson51/climateR")

#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","stagg",
          "AOI","climateR","leaflet","RColorBrewer")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

#Load Egypt shapefile
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#Selecting Assiut, Suhag, Alexandria and Cairo from shapefile
egypt_gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Alexandria","Cairo","Assiut","Suhag"))

# Terraclimate Data -------------------------------------------------------
####Temperature####
#####Max temp######
#Monthly mean max. temperature
tc_tmax = getTerraClim(AOI = egypt_gov,
                       varname = "tmax",
                       #dryrun = TRUE,
                       startDate = "1960-01-01",
                       endDate = "2024-12-31")#for now only until end of 2024

tc_tmax <- raster::stack(tc_tmax[[1]])

#dates tmax
dates_tmax <- seq(as.Date("1960-01-01"), as.Date("2023-12-31"), by = "month")

# Assign the dates to the RasterStack
tc_tmax <- setZ(tc_tmax,dates_tmax,name = "Date/time")

# You can also fix the layer names to be cleaner
names(tc_tmax) <- format(tc_tmax, "%Y-%m-%d")

#check if dates are adjusted
tc_tmax

#Max temperature January 1960
plot(tc_tmax[[1]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

#####Min temp#####
#monthly mean min. temperature
tc_tmin = getTerraClim(AOI = egypt_gov,
                       varname = "tmin",
                       #dryrun = TRUE,
                       startDate = "1960-01-01",
                       endDate = "2024-12-31")#for now only until end of 2023

tc_tmin <- raster::stack(tc_tmin[[1]])

#dates tmax
dates_tmin <- seq(as.Date("1960-01-01"), as.Date("2023-12-31"), by = "month")

# Assign the dates to the RasterStack
tc_tmin <- setZ(tc_tmin,dates_tmin,name = "Date/time")

# You can also fix the layer names to be cleaner
names(tc_tmin) <- format(tc_tmin, "%Y-%m-%d")

#check if dates are adjusted
tc_tmin

#Min temperature January 1960
plot(tc_tmin[[1]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

####Precipitation####
tc_prcp = getTerraClim(AOI = egypt_gov,
                       varname = "ppt",
                       #dryrun = TRUE,
                       startDate = "1960-01-01",
                       endDate = "2024-12-31")#for now only until end of 2023

tc_prec <- raster::stack(tc_prcp[[1]])

#dates tmax
dates_prec <- seq(as.Date("1960-01-01"), as.Date("2023-12-31"), by = "month")

# Assign the dates to the RasterStack
tc_prec <- setZ(tc_prec,dates_prec,name = "Date/time")

# You can also fix the layer names to be cleaner
names(tc_prec) <- format(tc_prec, "%Y-%m-%d")

#check if dates are adjusted
tc_prec

#test plot: sum of rainfall January 1960
plot(tc_prec[[1]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

####Palmer Drought Severity Index####
tc_pdsi = getTerraClim(AOI = egypt_gov,
                       varname = "PDSI",
                       #dryrun = TRUE,
                       startDate = "1960-01-01",
                       endDate = "2024-12-31")#for now only until end of 2023

tc_pdsi <- raster::stack(tc_pdsi[[1]])

#dates tmax
dates_pdsi <- seq(as.Date("1960-01-01"), as.Date("2023-12-31"), by = "month")

# Assign the dates to the RasterStack
tc_pdsi <- setZ(tc_pdsi,dates_pdsi,name = "Date/time")

# You can also fix the layer names to be cleaner
names(tc_pdsi) <- format(tc_pdsi, "%Y-%m-%d")

#check if dates are adjusted
tc_pdsi

#test plot: PDSI in January 1960
plot(tc_pdsi[[765]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")

# Save Rasters ------------------------------------------------------------
#tmax avg. temp
raster::writeRaster(tc_tmax,
                    filename = paste(here(),"Data", "Temperature","TerraClimate",
                    "tc_tmax_19602023.tif",sep = "/"))

#tmin avg. temp
raster::writeRaster(tc_tmin,
                    filename = paste(here(),"Data", "Temperature","TerraClimate",
                                     "tc_tmin_19602023.tif",sep = "/"))

#Precipitation
raster::writeRaster(tc_prec,
                    filename = paste(here(),"Data", "Precipitation","TerraClimate",
                                     "tc_prec_19602023.tif",sep = "/"))

#PDSI
raster::writeRaster(tc_pdsi,
                    filename = paste(here(),"Data", "Drought Indices","TerraClimate",
                                     "tc_pdsi_19602023.tif",sep = "/"))