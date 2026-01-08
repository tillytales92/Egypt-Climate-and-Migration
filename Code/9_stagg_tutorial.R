#Test stagg package
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","stagg",
          "tigris")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

#install and load stagg package
# install.packages("devtools")
#devtools::install_github("tcarleton/stagg")


#follows this tutorial: "https://github.com/tcarleton/stagg"
nj_counties <- tigris::counties("NJ")
nj_counties

####STEP 1: Generate secondary weights
cropland_weights <- secondary_weights(
  secondary_raster = cropland_nj_2015,
  grid = era5_grid,
  extent = "full")

cropland_weights

####STEP 2: Overlay admin. regions onto data's grid
county_weights <- overlay_weights(
  polygons = nj_counties,
  polygon_id_col = "COUNTYFP",
  grid = era5_grid,
  secondary_weights = cropland_weights
)

county_weights

####STEP 3: Transform and aggregate using staggregate() functions
polynomial_output <- staggregate_polynomial(
  data = temp_nj_jun_2024_era5 - 273.15, # A raster brick of our primary data, typically but not necessarily climate data. We're converting from Kelvin to Celsius here.
  overlay_weights = county_weights, # Output from Step 2, determined here by
  # area-normalized cropland weights for grid
  # cells within each county in New Jersey
  daily_agg = "average",            # How to aggregate hourly values to the
  # daily level (options are "sum", "average",and "none"). Here we want average daily temperature
  time_agg = "month",               # The temporal level to aggregate daily transformed values to. Current options are "hour", day", "month", and "year". Note that "hour" is only available if daily_agg is set to "none"
  degree = 3                        # The highest order of the polynomial. Here
  # this will create variable 3 columns:
  # order_1, order_2, and order_3
)

polynomial_output |>
  as_tibble()
