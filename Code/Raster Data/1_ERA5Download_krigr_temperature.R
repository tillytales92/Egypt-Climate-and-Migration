#Test krigR package
#Use krigR package for data download
#follow this course:https://www.erikkusch.com/courses/krigr/prep/
#load krigR
# devtools::install_github("https://github.com/ErikKusch/KrigR", ref = "Development")
# library(KrigR)

# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","stagg",
          "cowplot","ggmap","gimms","rnaturalearth",
          "rnaturalearthdata","mapview","KrigR","leaflet",
          "stagg")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Setting up KrigR --------------------------------------------------------
####Register API
API_User <- "t.a.meissner@lse.ac.uk"
API_Key <- "380c0560-137f-43f9-a359-09f3c4ad0b9a"

#Egypt Shapefile
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

# Temperature Data --------------------------------------------------------
####Daily means####
#maybe better to do Singular DL and increase the cores for aggregation
#get daily average for every day 2015 - 2024
egypt_temp_1950_decade_daily <- CDownloadS(
  Variable = "2m_temperature", # the variable we want to obtain data for
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "1950-01-01 00:00", # the starting date of our time-window
  DateStop = "1959-12-31 23:00", # the final date of our time-window
  TResolution = "day",#temporal resolution: day
  #FUN = "mean",#mean daily value
  TStep = 1,#how many time-steps to aggregate into one layer of data each
  Extent = egypt_hdx, # the spatial preference we are after
  Dir = paste(here(),"Data","ERA5",sep = "/"), # where to store the downloaded data
  FileExtension = ".tif",
  FileName = "egypt_era_temp_1950_decade", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key, # your API User Key
  TChunkSize = 300000,
  Cores = 4
)

egypt_temp_1950_decade_daily

####Daily max.####
egypt_temp_daily_max <- CDownloadS(
  Variable = "2m_temperature", # the variable we want to obtain data for
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "1960-01-01 00:00", # the starting date of our time-window
  DateStop = "1979-12-31 23:00", # the final date of our time-window
  TResolution = "day",#temporal resolution: day
  FUN = "max",#max daily value
  TStep = 1,#how many time-steps to aggregate into one layer of data each
  Extent = egypt_hdx, # the spatial preference we are after
  Dir = paste(here(),"Data","Temperature","ERA5",
              sep = "/"), # where to store the downloaded data
  FileExtension = ".tif",
  FileName = "era5_MaxTemp_1960_1979", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key, # your API User Key
  TChunkSize = 600000,
  Cores = 4
)

egypt_temp_daily_max
