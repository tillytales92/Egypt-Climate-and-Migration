#This script uses the RGEE package to load ERA5 data from Google Earth Engine (GEE)
#Data is saved on Google Drive and then downloaded locally
#Note: setting up RGEE is specific to each user's system and Python environment
# Load Packages -----------------------------------------------------------
#pacman
library(pacman)

#Define rgee environment
rgee_env_dir = "C:\\Users\\ADMIN\\AppData\\Local\\r-miniconda\\envs\\rgee_py\\"

#set environment
Sys.setenv(RETICULATE_PYTHON = rgee_env_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_env_dir)

library(reticulate)

#install general packages
p_load(raster,terra,sf,leaflet,mapview,here,
       tidyverse,viridis,cptcity,googledrive,
       lubridate)

#rgee packages
p_load(rgee,geojsonio,remotes,devtools)

#install rgee from github
#install_github("r-spatial/rgee")

# reticulate::use_python(rgee_env_dir,required = TRUE)
#
# rgee::ee_install_set_pyenv(
#   py_path = rgee_env_dir,
#   py_env = "rgee_py")

#authenticate and initialize rgee
# ee_install(py_env = "rgee_py")
# rgee::ee_Authenticate()

# Initialise rgee ---------------------------------------------------------
rgee::ee_Initialize(drive = TRUE)
#potential issue down the line with Python version

#check if all works
ee_check()
#potential issue with Earth Engine Python API

#authorise connection with drive()
googledrive::drive_auth()

#Downloading Weather Data (ERA5) from GEE -----------------------------------
#Load Shapefiles
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","raw","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#Select relevant governorates
egypt_gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag" ,"Alexandria","Cairo")) |>
  select(ADM1_EN)

# Convert egypt_gov to ee object
egypt_gov_ee <- sf_as_ee(egypt_gov)

# Get geometry from the ee object
govs_geometry <- egypt_gov_ee$geometry()

# Create ImageCollection
ic <- ee$ImageCollection('ECMWF/ERA5_LAND/HOURLY')$
  filterDate('1992-10-05', '1992-10-06')$
  filterBounds(govs_geometry)$
  select('temperature_2m')
         # 'dewpoint_temperature_2m',
         # 'u_component_of_wind_10m',
         # 'v_component_of_wind_10m')

# Download as raster
ds <- ee_as_rast(
  image = ic$toBands(),
  region = govs_geometry,
  dsn = "temp_2m_egypt.tif",
  scale = 11000,
  via = "drive",
  container = "egypt_era5_data",
  lazy = TRUE)

#load into raw data folder
# Download Sheet as csv, explicit type
googledrive::drive_download(
  file = "egypt_era5_data/noid_image_2026_01_22_09_49_50.tif",
  path = paste(here(),
               "Data","raw","ERA5","temp2m_test.tif",sep = "/"))

#load into R
names_vec <- names(egypt_temp_2m)
datetime_str <- str_extract(names_vec, "\\d{8}T\\d{2}")
datetime <- ymd_h(datetime_str)
time(egypt_temp_2m) <- datetime

egypt_temp_2m <- egypt_temp_2m - 273.15 #convert from Kelvin to Celsius

#test plot
plot(egypt_temp_2m[[1:6]])
