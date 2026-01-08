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

#crop to Assiut and Suhag
egypt_gov <- egypt_hdx |>
  filter(ADM1_EN %in% c("Assiut","Suhag"))

#maybe better to do Singular DL and increase the cores for aggregation
#get daily average for every day 2015 - 2024
t0 <- Sys.time()
egypt_prec_decade2000_allegypt <- CDownloadS(
  Variable = "total_precipitation", # the variable we want to obtain data for
  CumulVar = TRUE,
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "2000-01-01 00:00", # the starting date of our time-window
  DateStop = "2009-12-31 23:00", # the final date of our time-window
  TResolution = "day",#temporal resolution: month
  #FUN = "mean",#mean daily value
  TStep = 1,#how many time-steps to aggregatre into one layer of data each
  Extent = egypt_hdx, # the spatial preference we are after
  Dir = paste(here(),"Data","ERA5","Precipitation",sep = "/"),# where to store the downloaded data
  #FileExtension = ".tif",
  FileName = "egypt_era_precip_2000decade_allegypt", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key, # your API User Key
  TChunkSize = 300000,
  Cores = 4
)
t1 <- Sys.time()
print(t1 - t0)


t0 <- Sys.time()
egypt_era_precip_2010decade_allegypt <- CDownloadS(
  Variable = "total_precipitation", # the variable we want to obtain data for
  CumulVar = TRUE,
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "2010-01-01 00:00", # the starting date of our time-window
  DateStop = "2019-12-31 23:00", # the final date of our time-window
  TResolution = "day",#temporal resolution: month
  #FUN = "mean",#mean daily value
  TStep = 1,#how many time-steps to aggregatre into one layer of data each
  Extent = egypt_hdx, # the spatial preference we are after
  Dir = paste(here(),"Data","ERA5","Precipitation",sep = "/"),# where to store the downloaded data
  FileExtension = ".tif",
  FileName = "egypt_era_precip_2010decade_allegypt", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key, # your API User Key
  TChunkSize = 300000,
  Cores = 4
)
t1 <- Sys.time()
print(t1 - t0)

#decade 2020
t0 <- Sys.time()
egypt_prec_decade_2020 <- CDownloadS(
  Variable = "total_precipitation", # the variable we want to obtain data for
  CumulVar = TRUE,
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "2020-01-01 00:00", # the starting date of our time-window
  DateStop = "2024-12-31 23:00", # the final date of our time-window
  TResolution = "day",#temporal resolution: month
  #FUN = "mean",#mean daily value
  TStep = 1,#how many time-steps to aggregatre into one layer of data each
  Extent = egypt_hdx, # the spatial preference we are after
  Dir = paste(here(),"Data","ERA5","Precipitation",sep = "/"),# where to store the downloaded data
  #FileExtension = ".tif",
  FileName = "egypt_era_precip_2020decade", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key, # your API User Key
  TChunkSize = 300000,
  Cores = 4
)
t1 <- Sys.time()
print(t1 - t0)



egypt_prec_2000_daily <- terra::rast(paste(here(),"Data", "ERA5","Precipitation",
                                              "egypt_era_precip_2000.tif",sep = "/"))

#plot


#test plot: raster and shapefile outline
plot(egypt_prec_2000_daily[[3*1000]])
plot(st_geometry(egypt_gov), add = TRUE, border = "red")



