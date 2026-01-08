#Mapping Time series: Drought Indices
#Measures inspired by:
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

# Read Data ---------------------------------------------------------------
#Load PDSI data
pdsi_gov <- readRDS(file = paste(here(),"Data",
                                     "Drought Indices","TerraClimate","Panel",
                                     "pdsi_19602024",sep = "/"))

####Palmer Drought Severity Index (PDSI)#####
#define severe drought months: PDSI < -3
pdsi_gov <- pdsi_gov |>
  mutate(drought_overall = if_else(pdsi_mean < -3,1,0),
         drought_cropland = if_else(pdsi_croplandweighted_mean < -3,1,0))

drought_months <- pdsi_gov |>
  filter(name %in% c("Assiut","Suhag")) |>
  group_by(name, year) |>
  summarise(
    n_months_drought = sum(drought_overall  == 1, na.rm = TRUE),
    n_months_drought_cropland   = sum(drought_cropland == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("n_months"),
    names_to = "type",
    values_to = "n_months_drought"
  ) |>
  mutate(
    type = recode(type,
                  "n_months_drought" = "Unweighted",
                  "n_months_drought_cropland"   = "Cropland-weighted"
    )
  )

drought_months |>
  #filter(year > 1979) |>
  ggplot(aes(x = year, y = n_months_drought, color = type)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE) +
  facet_wrap(~name) +
  theme_minimal() +
  labs(
    title = "Number of Days with Average Temp. > 30°C by Governorate",
    x = "Year",
    y = "Number of Days",
    color = "Measure"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9)
  )

