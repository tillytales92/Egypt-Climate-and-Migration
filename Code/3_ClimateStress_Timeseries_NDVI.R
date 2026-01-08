#Mapping Time series: NDVI
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
#Load NDVI data
#Landsat 8 images (2014 - 2024)
ndvi_l8 <- read_csv(file = paste(here(),"Data",
                                   "NDVI","NDVI_timeseries_2gov_median.csv",sep = "/")) |>
  dplyr::select(-c(`system:index`,.geo))

#Landsat 7 images (2001 - 2013)
ndvi_l7 <- read_csv(file = paste(here(),"Data",
                                       "NDVI","NDVI_timeseries_2gov_landsat7.csv",sep = "/")) |>
  dplyr::select(-c(`system:index`,.geo))

#date columns
ndvi_l8 <- ndvi_l8 |>
  mutate(date = as_date(date,format = "%Y-%m"),
         m_name = month(date, label = TRUE, abbr = TRUE))

#L7
ndvi_l7 <- ndvi_l7 |>
  mutate(date = as_date(date,format = "%Y-%m"),
         m_name = month(date, label = TRUE, abbr = TRUE))

#bind
ndvi <- ndvi_l8 |>
  bind_rows(ndvi_l7) |>
  arrange(year)

#add column to indicate satellite
ndvi <- ndvi |>
  mutate(satellite = if_else(year < 2014,"Landsat 7","Landsat 8"))

#base plot
ndvi |>
  ggplot(aes(x = date,y = NDVI_cropland))+
  geom_line()

#see if there is a jump?
lm1 <- lm(NDVI_cropland ~ as_factor(month) + as_factor(satellite),data = ndvi)

summary(lm1)
#yes, Landsat 8 NDVI values are on average 0.035 higher

# Landsat 8 analysis (2014 - 2024) ------------------------------------------------------
#monthly averages,sd
# 1. Compute 2014 - 2016 monthly baseline
mean_monthly_l8 <- ndvi_l8 |>
  filter(year >= 2014, year <= 2017) |>
  group_by(month,region) |>
  summarise(
    mean_ndvi_crop     = mean(NDVI_cropland, na.rm = TRUE),
    sd_ndvi_crop      = sd(NDVI_cropland, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Join baselines and compute deviations/z-scores/shocks
ndvi_l8 <- ndvi_l8 |>
  left_join(mean_monthly_l8, by = c("month", "region")) |>
  group_by(region,month) |>
  mutate(
    # mean temperature deviation
    ndvi_diff   = NDVI_cropland  - mean_ndvi_crop,
    z_score_ndvidiff = (NDVI_cropland - mean_ndvi_crop) / sd_ndvi_crop,
    shock_ndvidiff   = case_when(
      z_score_ndvidiff >  2  ~  1,
      z_score_ndvidiff < -2  ~ -1,
      TRUE                   ~  0)) |>
  ungroup()

#shocks
ndvi_l8 |>
  filter(shock_ndvidiff == -1)

#time series NDVI_cropland
####Assiut####
ndvi_l8 |>
  filter(region == "Assiut") |>
  ggplot(aes(x = date, y = NDVI_cropland)) +
  geom_point(size = 0.9)+
  geom_line(color = "darkgreen") +
  geom_point(
    data = ndvi_l8 |> filter(shock_ndvidiff == -1, region == "Assiut"),
    aes(x = date, y = NDVI_cropland),
    color = "red", size = 2, shape = 21, fill = "red") +
  facet_wrap(~m_name,nrow = 2) +
  scale_x_date(
    limits = c(as.Date("2014-01-01"), as.Date("2024-12-31")),
    breaks = c(
      seq(from = ceiling_date(as.Date("2014-01-01"), "month"),
          to   = floor_date(as.Date("2024-12-31"), "month"),
          by = "2 year")),
    date_labels = "%Y")+
  labs(
    x = "Date",
    y = "NDVI (Cropland)",
    title = "NDVI Time Series for Assiut (2014–2024) - Based on Landsat 8 images",
    subtitle = "Red points indicate shocks defined as 2 SD difference from 2014-2016 monthly mean."
  )+
  theme_minimal()+
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

####Suhag####
ndvi_l8 |>
  filter(region == "Suhag") |>
  ggplot(aes(x = date, y = NDVI_cropland)) +
  geom_point(size = 0.9)+
  geom_line(color = "darkgreen") +
  geom_point(
    data = ndvi_l8 |> filter(shock_ndvidiff == -1, region == "Suhag"),
    aes(x = date, y = NDVI_cropland),
    color = "red", size = 2, shape = 21, fill = "red"
  ) +
  facet_wrap(~m_name,nrow = 2) +
  scale_x_date(
    limits = c(as.Date("2014-01-01"), as.Date("2024-12-31")),
    breaks = c(
      seq(from = ceiling_date(as.Date("2014-01-01"), "month"),
          to   = floor_date(as.Date("2024-12-31"), "month"),
          by = "2 year")),
    date_labels = "%Y")+
  labs(
    x = "Date",
    y = "NDVI (Cropland)",
    title = "NDVI Time Series for Suhag (2014–2024) - Based on Landsat 8 images",
    subtitle = "Red points indicate shocks defined as 2 SD difference from 2014-2016 monthly mean."
  )+
  theme_minimal()+
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

# Landsat 7 analysis (2001-2013) ------------------------------------------------------
#monthly averages,sd
# 1. Compute 2014 - 2016 monthly baseline
mean_monthly_l7 <- ndvi_l7 |>
  filter(year >= 2001, year <= 2004) |>
  group_by(month,region) |>
  summarise(
    mean_ndvi_crop     = mean(NDVI_cropland, na.rm = TRUE),
    sd_ndvi_crop      = sd(NDVI_cropland, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Join baselines and compute deviations/z-scores/shocks
ndvi_l7 <- ndvi_l7 |>
  left_join(mean_monthly_l7, by = c("month", "region")) |>
  group_by(region,month) |>
  mutate(
    # mean temperature deviation
    ndvi_diff   = NDVI_cropland  - mean_ndvi_crop,
    z_score_ndvidiff = (NDVI_cropland - mean_ndvi_crop) / sd_ndvi_crop,
    shock_ndvidiff   = case_when(
      z_score_ndvidiff >  2  ~  1,
      z_score_ndvidiff < -2  ~ -1,
      TRUE                   ~  0)) |>
  ungroup()

#shocks
ndvi_l7 |>
  filter(shock_ndvidiff == -1)

#time series NDVI_cropland
####Assiut####
ndvi_l7 |>
  filter(region == "Assiut") |>
  ggplot(aes(x = date, y = NDVI_cropland)) +
  geom_point(size = 0.9)+
  geom_line(color = "darkgreen") +
  geom_point(
    data = ndvi_l7 |> filter(shock_ndvidiff == -1, region == "Assiut"),
    aes(x = date, y = NDVI_cropland),
    color = "red", size = 2, shape = 21, fill = "red"
  ) +
  facet_wrap(~m_name,nrow = 2) +
  scale_x_date(
    limits = c(as.Date("2001-01-01"), as.Date("2013-12-31")),
    breaks = c(
      seq(from = ceiling_date(as.Date("2001-01-01"), "month"),
          to   = floor_date(as.Date("2013-12-31"), "month"),
          by = "2 year")),
    date_labels = "%Y")+
  labs(
    x = "Date",
    y = "NDVI (Cropland)",
    title = "NDVI Time Series for Assiut (2001–2013) - Based on Landsat 7 images",
    subtitle = "Red points indicate shocks defined as 2 SD difference from 2001-2004 monthly mean."
  )+
  theme_minimal()+
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

####Suhag####
ndvi_l7 |>
  filter(region == "Suhag") |>
  ggplot(aes(x = date, y = NDVI_cropland)) +
  geom_point(size = 0.9)+
  geom_line(color = "darkgreen") +
  geom_point(
    data = ndvi_l7 |> filter(shock_ndvidiff == -1, region == "Suhag"),
    aes(x = date, y = NDVI_cropland),
    color = "red", size = 2, shape = 21, fill = "red") +
  facet_wrap(~m_name,nrow = 2) +
  scale_x_date(
    limits = c(as.Date("2001-01-01"), as.Date("2013-12-31")),
    breaks = c(
      seq(from = ceiling_date(as.Date("2001-01-01"), "month"),
          to   = floor_date(as.Date("2013-12-31"), "month"),
          by = "2 year")),
    date_labels = "%Y")+
  labs(
    x = "Date",
    y = "NDVI (Cropland)",
    title = "NDVI Time Series for Suhag (2001–2013) - Based on Landsat 7 images",
    subtitle = "Red points indicate shocks defined as 2 SD difference from 2001-2004 monthly mean."
  )+
  theme_minimal()+
  theme(
    legend.position    = "bottom",
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())




