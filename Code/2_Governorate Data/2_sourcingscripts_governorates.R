#2.Source scripts --------------------------------------------------------
#Use this script to run governorate data extraction scripts
#Import libraries --------------------------------------------------------
# libraries we need
libs <- c("terra", "tidyverse","here","sf",
          "haven","naniar","leaflet")

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

# Sourcing Scripts --------------------------------------------------------
####2_Extraction Scripts####
#source CHIRPS prec. extraction script
source(paste(here(),"Code","2_Governorate Data",
             "1_Extraction_CHIRPS_Precipitation.R",sep = "/"))

#source ERA5 Heat Index Extraction script
source(paste(here(),"Code","2_Governorate Data",
             "1_Extraction_ERA5_HeatIndex.R",sep = "/"))

#source ERA5 Temp. Extraction script
source(paste(here(),"Code","2_Governorate Data",
             "1_Extraction_ERA5_Temperature.R",sep = "/"))

#source UTCI Extraction script
source(paste(here(),"Code","2_Governorate Data",
             "1_Extraction_UTCI.R",sep = "/"))



