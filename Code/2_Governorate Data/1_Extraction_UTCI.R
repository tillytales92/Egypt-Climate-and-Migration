#UTCI: Univ. Thermal Climate Index
#Thermal comfort indices derived from ERA5 reanalysis (download through CDS)
#Build raster stacks from daily raster files
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","here","terra","sf")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Load Shapefiles ---------------------------------------------------------
#HDX shapefile:see(https://data.humdata.org/dataset/cod-ab-egy)
egypt_hdx <- st_read(paste(here(),
                           "Data","raw","Shapefiles","HDX_Egypt",
                           "egy_admbnda_adm1_capmas_20170421.shp",sep = "/"))

#egypt gov
egypt_gov <- egypt_hdx

#Load Climate Data Rasters------------------------------------------------
utci_daily_max_rast <- terra::rast(paste(here(),"Data", "intermediate","UTCI",
                      "utci_dailymax_rast_20002024.tif",sep = "/"))

# Extracting Time Series --------------------------------------------------
#extract time series for Governorates
egypt_gov$gov_id <- seq_len(nrow(egypt_gov))

utci_govs <- terra::extract(
  utci_daily_max_rast,
  vect(egypt_gov),
  fun = "mean",
  ID = FALSE)

#add gov. names
utci_govs <- bind_cols(
  tibble(gov_id = egypt_gov$gov_id,
         ADM1_EN = egypt_gov$ADM1_EN),
  as_tibble(utci_govs)
)

#turn into long format
utci_govs_long <- utci_govs |>
  dplyr::select(ADM1_EN, starts_with("utci_")) |>
  pivot_longer(
    cols = starts_with("utci_"),
    names_to = "date",
    values_to = "utci_dailymax"
  ) |>
  mutate(
    date = as.Date(sub("^utci_", "", date))
  ) |>
  arrange(ADM1_EN, date)

#Saving Gov. panel data --------------------------------------------------
#Panel Data
saveRDS(utci_govs_long,file = paste(here(),
                                    "Data","intermediate","Governorate Data",
                                    "utci_govpanel_20002024",sep = "/"))

