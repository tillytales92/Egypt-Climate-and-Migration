#Mapping Rainfall patterns - Time Series
#Data: CHIRPS dataset (daily sum of rainfall)
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

# Load Data ---------------------------------------------------------------
#Load Precipiation data for Alexandria, Cairo, Assiut and Sohag
#yearly data
chirps_yearly <- readRDS(file = paste(here(),"Data","intermediate",
                                      "Governorate Data","chirps_prec_yearly_19812024",
                                      sep = "/"))

#monthly data
chirps_monthly <- readRDS(file = paste(here(),"Data","intermediate",
                                       "Governorate Data","chirps_prec_yearly_19812024",
                                       sep = "/"))

#daily data
chirps_daily <- readRDS(file = paste(here(),"Data","intermediate",
                                     "Governorate Data","chirps_prec_yearly_19812024",
                                     sep = "/"))

# Yearly Rainfall Data ---------------------------------------------------
####axis not free####
#plot yearly rainfall - axis not free
chirps_yearly |>
  ggplot(aes(x = date, y = mean_prec_popweighted_chirps)) +
  geom_col(fill = "steelblue", color = "black", alpha = 0.7, linewidth = 0.2)+
  geom_smooth(se = FALSE, color = "darkred", linetype = "dashed",linewidth = 0.8) +
  facet_wrap(~name) +
  scale_x_date(
    limits = c(min(chirps_monthly$date), max(chirps_monthly$date)),
    breaks = c(
      min(chirps_monthly$date),
      seq(from = ceiling_date(min(chirps_monthly$date), "5 years"),
          to   = floor_date(max(chirps_monthly$date), "5 years"),
          by = "5 years"),
      max(chirps_monthly$date)
    ),
    date_labels = "%Y"
  )+
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual rainfall (population weighted) by Governorate",
    subtitle = "CHIRPS data (1981–2024)",
    x = "Year",
    y = "Rainfall (mm)"
  ) +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#plot monthly rainfall - axis free
####axis free####
chirps_yearly |>
  ggplot(aes(x = date, y = mean_prec_popweighted_chirps)) +
  geom_col(fill = "steelblue", color = "black", alpha = 0.7, linewidth = 0.2)+
  geom_smooth(se = FALSE, color = "darkred", linetype = "dashed",linewidth = 0.8) +
  facet_wrap(~name,scales = "free_y") +
  scale_x_date(
    limits = c(min(chirps_monthly$date), max(chirps_monthly$date)),
    breaks = c(
      min(chirps_monthly$date),
      seq(from = ceiling_date(min(chirps_monthly$date), "5 years"),
          to   = floor_date(max(chirps_monthly$date), "5 years"),
          by = "5 years"),
      max(chirps_monthly$date)
    ),
    date_labels = "%Y"
  )+
  theme_minimal(base_size = 12) +
  labs(
    title = "Annual rainfall (population weighted) by Governorate",
    subtitle = "CHIRPS data (1981–2024)",
    x = "Year",
    y = "Rainfall (mm)"
  ) +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Monthly Rainfall Data ---------------------------------------------------
####axis not free####
#plot monthly rainfall - axis not free
chirps_monthly |>
  ggplot(aes(x = date, y = mean_prec_popweighted_chirps)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_smooth(se = FALSE, color = "darkred", linetype = "dashed",linewidth = 0.8) +
  facet_wrap(~name) +
  scale_x_date(
    limits = c(min(chirps_monthly$date), max(chirps_monthly$date)),
    breaks = c(
      min(chirps_monthly$date),
      seq(from = ceiling_date(min(chirps_monthly$date), "5 years"),
          to   = floor_date(max(chirps_monthly$date), "5 years"),
          by = "5 years"),
      max(chirps_monthly$date)
    ),
    date_labels = "%Y"
  )+
  theme_minimal(base_size = 12) +
  labs(
    title = "Monthly rainfall (population weighted) by Governorate",
    subtitle = "CHIRPS data (1981–2024)",
    x = "Year",
    y = "Rainfall (mm)"
  ) +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#plot monthly rainfall - axis free
####axis free####
chirps_monthly |>
  ggplot(aes(x = date, y = mean_prec_popweighted_chirps)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_smooth(se = FALSE, color = "darkred", linetype = "dashed",linewidth = 0.8) +
  facet_wrap(~name, scales = "free_y") +
  scale_x_date(
    limits = c(min(chirps_monthly$date), max(chirps_monthly$date)),
    breaks = c(
      min(chirps_monthly$date),
      seq(from = ceiling_date(min(chirps_monthly$date), "5 years"),
          to   = floor_date(max(chirps_monthly$date), "5 years"),
          by = "5 years"),
      max(chirps_monthly$date)
    ),
    date_labels = "%Y"
  )+
  theme_minimal(base_size = 12) +
  labs(
    title = "Monthly rainfall (population weighted) by Governorate",
    subtitle = "CHIRPS data (1981–2024)",
    x = "Year",
    y = "Rainfall (mm)"
  ) +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

####Highlight driest/wettest years####
# 1. Compute long-run mean and sd per governorate
stats_yearly <- chirps_yearly |>
  group_by(name) |>
  summarise(mean_popland_prec = mean(mean_prec_popweighted_chirps, na.rm = TRUE),
            sd_popland_prec   = sd(mean_prec_popweighted_chirps, na.rm = TRUE),
            .groups = "drop")

# 2. Merge back and calculate z-scores and shocks
chirps_yearly <- chirps_yearly |>
  left_join(stats_yearly, by = "name") |>
  mutate(z_score = (mean_prec_popweighted_chirps - mean_popland_prec) / sd_popland_prec,
         shock = case_when(
           z_score >  2 ~ 1,   # extreme wet year
           z_score < -2 ~ 1,   # extreme dry year
           TRUE ~ 0
         ))

#3. Add deciles (or quintiles if you prefer) of annual precipitation
chirps_yearly <- chirps_yearly |>
  group_by(name) |>
  mutate(prec_decile = ntile(mean_prec_popweighted_chirps, 10)) |>
  ungroup()

# Create shading ranges for lowest/highest years
shading <- chirps_yearly |>
  mutate(xmin = as.Date(paste0(year, "-01-01")),
         xmax = as.Date(paste0(year, "-12-31")),
         shade_type = case_when(
           prec_decile == 1 ~ "Lowest decile",
           prec_decile == 10 ~ "Highest decile",
           TRUE ~ NA_character_
         )) |>
  filter(!is.na(shade_type))

#####Plot - axis not free#####
chirps_monthly |>
  ggplot(aes(x = date, y = mean_prec_popweighted_chirps)) +
  geom_rect(data = shading,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf,
                fill = shade_type),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_smooth(se = FALSE, color = "darkred", linetype = "dashed",linewidth = 0.8)+
  facet_wrap(~name) +
  scale_fill_manual(values = c("Lowest decile" = "tomato",
                               "Highest decile" = "dodgerblue")) +
  scale_x_date(
    limits = c(min(chirps_monthly$date), max(chirps_monthly$date)),
    breaks = c(
      min(chirps_monthly$date),
      seq(from = ceiling_date(min(chirps_monthly$date), "5 years"),
          to   = floor_date(max(chirps_monthly$date), "5 years"),
          by = "5 years"),
      max(chirps_monthly$date)
    ),
    date_labels = "%Y"
  )+
  theme_minimal(base_size = 12) +
  labs(
    title = "Monthly rainfall (population weighted) by Governorate",
    subtitle = "CHIRPS data (1981–2024)\nShaded areas show driest/wettest years",
    x = "Year",
    y = "Rainfall (mm)",
    fill = "Rainfall extreme"
  ) +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#####Plot - axis free#####
chirps_monthly |>
  ggplot(aes(x = date, y = mean_prec_popweighted_chirps)) +
  geom_rect(data = shading,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf,
                fill = shade_type),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_smooth(se = FALSE, color = "darkred", linetype = "dashed",linewidth = 0.8)+
  facet_wrap(~name, scales = "free_y") +
  scale_fill_manual(values = c("Lowest decile" = "tomato",
                               "Highest decile" = "dodgerblue")) +
  scale_x_date(
    limits = c(min(chirps_monthly$date), max(chirps_monthly$date)),
    breaks = c(
      min(chirps_monthly$date),
      seq(from = ceiling_date(min(chirps_monthly$date), "5 years"),
          to   = floor_date(max(chirps_monthly$date), "5 years"),
          by = "5 years"),
      max(chirps_monthly$date)
    ),
    date_labels = "%Y"
  )+
  theme_minimal(base_size = 12) +
  labs(
    title = "Monthly rainfall (population weighted) by Governorate",
    subtitle = "CHIRPS data (1981–2024)\nShaded areas show driest/wettest years",
    x = "Year",
    y = "Rainfall (mm)",
    fill = "Rainfall extreme"
  ) +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Daily Rainfall Data -----------------------------------------------------
#count days per year without any precipitation
days_norainfall <- chirps_daily |>
  group_by(name, year) |>
  summarise(
    n_days_norainfall = sum(mean_prec_chirps == 0, na.rm = TRUE),
    n_days_norainfall_popland = sum(mean_prec_popweighted_chirps == 0,na.rm = TRUE),
    n_days_norainfall_cropland   = sum(mean_prec_croplandweighted_chirps == 0, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = starts_with("n_days"),
    names_to = "type",
    values_to = "n_days_norainfall"
  ) |>
  mutate(
    type = recode(type,
                  "n_days_norainfall" = "Unweighted",
                  "n_days_norainfall_cropland"   = "Cropland",
                  "n_days_norainfall_popland" = "Populated Land")
  )

#plot
days_norainfall |>
  filter(type == "Populated Land") |>
  ggplot(aes(x = year,y = n_days_norainfall))+
  geom_line(color = "steelblue", linewidth = 0.8)+
  geom_point(color = "steelblue", size = 1.2, alpha = 0.6)+
  geom_smooth(se = FALSE, color = "darkred", linetype = "dashed", linewidth = 0.8)+
  facet_wrap(~name) +
  theme_minimal() +
  labs(
    title = "Days per year without any rainfall (population weighted) by Governorate",
    subtitle = "CHIRPS data (1981–2024)",
    x = "Year",
    y = "Number of Days",
    color = "Measure"
  ) +
  theme(
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )+
  scale_x_continuous(
    limits = c(min(days_norainfall$year), max(days_norainfall$year)),
    breaks = c(
      min(days_norainfall$year),
      seq(from = ceiling(min(days_norainfall$year) / 5) * 5,
          to   = floor(max(days_norainfall$year) / 5) * 5,
          by = 5),
      max(days_norainfall$year)
    )
  )

