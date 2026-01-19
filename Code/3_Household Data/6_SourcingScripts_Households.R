#7.Source scripts --------------------------------------------------------
#Use this script when Household Data changes to get updated HH Data & Indices
#NOTE:Check file path in "0_HH_GPSimputation.R"
#instead would be better if I can define HH file here!
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
#source GPS imputation script
source(paste(here(),"Code","3_Household Data",
             "0_HH_GPSimputation.R",sep = "/"))

#source Household Elevation script
source(paste(here(),"Code","3_Household Data",
             "2_Extraction_Elevation.R",sep = "/"))

#source ESI household panel script
source(paste(here(),"Code","3_Household Data",
             "2_Extraction_ESI_HHPanel.R",sep = "/"))

#source Travel Distance to nearest market town script
#Note: uses Google Distance Matrix API
# source(paste(here(),"Code","3_Household Data",
#              "2_Extraction_HH_TravelDistances.R",sep = "/"))

#source Distance to Waterways script
source(paste(here(),"Code","3_Household Data",
             "2_Extraction_HH_Waterways.R",sep = "/"))

#source NDVI household panel script
source(paste(here(),"Code","3_Household Data",
             "2_Extraction_NDVI_HHPanel.R",sep = "/"))

#source ERA5 Temperature script
source(paste(here(),"Code","3_Household Data",
             "2_Extraction_Temp_HHPanel.R",sep = "/"))

####3_Climate Shock Variables Script####
#source Climate Shock Variables script
source(paste(here(),"Code","3_Household Data",
             "3_ClimateShock_Variables_HHPanel.R",sep = "/"))

####4_Merging Household Data####
#source Merging Household Data script
source(paste(here(),"Code","3_Household Data",
             "4_Merging_HHDataset.R",sep = "/"))

####5_Vulnerability Indices####
#source Vulnerability Indices script
source(paste(here(),"Code","3_Household Data",
             "5_VulnerabilityIndices_HHPanel.R",sep = "/"))
