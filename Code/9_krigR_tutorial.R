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
          "rnaturalearthdata","mapview","KrigR","leaflet")

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

#Setting up Directories
Dir.Base <- getwd() # identifying the current directory
Dir.Data <- file.path(Dir.Base,"Data","krigR","Data") # folder path for data
Dir.Covariates <- file.path(Dir.Base,"Data","krigR", "Covariates") # folder path for covariates
Dir.Exports <- file.path(Dir.Base,"Data","krigR", "Exports") # folder path for exports
## create directories, if they don't exist yet
Dirs <- sapply(c(Dir.Data, Dir.Covariates, Dir.Exports),
               function(x) if(!dir.exists(x)) dir.create(x))


# Datasets and Variables --------------------------------------------------
#check which datasets are available
Meta.List()#ERA 5
Meta.Variables()#list of variables

# Defining Spatial Pref. --------------------------------------------------
####Spatial preferences in krigR
#krigR can use extent, SpatialPolygons and locations stored in a data.frame
#extent
Extent_ext <- extent(c(9.87, 15.03, 49.89, 53.06))

#NaturalEarthData
Shape_shp <- ne_states(country = c("Germany", "Czech Republic"))
Shape_shp <- Shape_shp[Shape_shp$name_en %in% c("Saxony", "Saxony-Anhalt",
                                                "Thuringia",
                                                "Ústí nad Labem", "Karlovy Vary"), ]

#Egypt shapefile
# egypt_shp <- ne_states(country = "Egypt")


#Points of Interest (!VERY relevant for the household locations!)
# Create data frame from locations of a couple of mountains in the region
mountains <- data.frame(
  Mountain = c("Fichtelberg", "Brocken", "Großer Beerberg", "Meluzína", "Milešovka"),
  Lon = c(12.95472, 10.61722, 10.74611, 13.00778, 13.93153),
  Lat = c(50.42861, 51.80056, 50.65944, 50.39028, 50.55523)
)

# Convert to sf object (WGS84 CRS)
mountains_sf <- st_as_sf(mountains, coords = c("Lon", "Lat"), crs = 4326)

mountains_sf

#Visualise study setting
leaflet() |>
  # Add a basemap (terrain-like)
  addProviderTiles(providers$Esri.WorldTopoMap) |>
  # Add polygon overlay (if Shape_shp exists)
  addPolygons(data = Shape_shp,
              color = "black", weight = 1,
              fillColor = "black",
              fillOpacity = 0.5,
              popup = ~name_en) |>
  # Add mountain points
  addCircleMarkers(data = mountains_sf,
                   color = "red", radius = 6,
                   popup = ~Mountain)

# Download & Processing ---------------------------------------------------
#Basic call to download_ERA() using the shapefile outline
#results in nlyr = 1 since TResolution = "month"
####Using polygons ####
first_dl <- CDownloadS(
  Variable = "2m_temperature", # the variable we want to obtain data for
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "1996-01-01 00:00", # the starting date of our time-window
  DateStop = "1996-01-31 23:00", # the final date of our time-window
  TResolution = "month",#temporal resolution: monthly
  TStep = 1,#how many time-steps to aggregatre into one layer of data each
  Extent = Shape_shp, # the spatial preference we are after
  Dir = Dir.Data, # where to store the downloaded data
  FileName = "secondDL", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key # your API User Key
)

#test plot: raster and shapefile outline
plot(first_dl - 273.15)
plot(st_geometry(Shape_shp), add = TRUE, border = "red")

####Using point data####
#data.frame in Extent needs columns called Lat and Lon
#need to specify a Buffer and ID column
points_dl <- CDownloadS(
  Variable = "2m_temperature", # the variable we want to obtain data for
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "1995-01-01 00:00", # the starting date of our time-window
  DateStop = "1995-01-31 23:00", # the final date of our time-window
  TResolution = "month",#temporal resolution: monthly
  TStep = 1,#how many time-steps to aggregatre into one layer of data each
  Extent = mountains, # the spatial preference we are after
  Buffer = 0.5, #half-degree buffer
  #ID = 'Mountain',
  Dir = Dir.Data, # where to store the downloaded data
  FileName = "points_dl", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key # your API User Key
)

#test plot
plot(points_dl - 273.15)
plot(st_geometry(mountains_sf), add = TRUE, colour = "red")


####Changing the Temporal Resolution####
#e.g. lets download hourly data from CDS and aggregate to 1-day intervals
#focus on the first four days of January 1995
polygons_timeseries <- CDownloadS(
  Variable = "2m_temperature", # the variable we want to obtain data for
  DataSet = "reanalysis-era5-land", # the data set we want to obtain data from
  DateStart = "1995-01-01 00:00", # the starting date of our time-window
  DateStop = "1995-01-04 23:00", # the final date of our time-window
  TResolution = "hour",#temporal resolution: hourly
  TStep = 1,#how many time-steps to aggregatre into one layer of data each
  Extent = Shape_shp, # the spatial preference we are after
  Dir = Dir.Data, # where to store the downloaded data
  FileExtension = ".tif",
  FileName = "First_Timeseries", # a name for our downloaded file
  API_User = API_User, # your API User Number
  API_Key = API_Key # your API User Key
)

era5_test <- raster::brick(paste(here(),"Data",
                                     "krigR","Data",
                                 "First_Timeseries.tif",sep = "/"),
                               values = TRUE)

era5_test
plot(polygons_timeseries)

####Plotting####
# Convert SpatRaster to data.frame for ggplot
df <- as.data.frame(polygons_timeseries, xy = TRUE)

df_long <- pivot_longer(df, -c(x, y), names_to = "time", values_to = "value")

df_long <- df_long |>
  mutate(value_c = value - 273.15)

ggplot(df_long, aes(x, y, fill = value_c)) +
  geom_raster() +
  facet_wrap(~time) +
  scale_fill_viridis_c(option = "C", name = "Temperature (°C)") +
  coord_equal() +
  theme_minimal()
