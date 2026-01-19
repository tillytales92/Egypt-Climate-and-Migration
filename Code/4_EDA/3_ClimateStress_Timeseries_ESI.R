#Mapping Time series: ESI
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "terra","raster","geodata","sf","exactextractr",
          "lubridate")

#install missing librarieshttp://127.0.0.1:14081/graphics/plot_zoom_png?width=1124&height=861
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read Data ---------------------------------------------------------------
#Load ESI data
#ESI 12 week timeseries (2001 - 2024)
esi_12week <- read_csv(file = paste(here(),"Data","Drought Indices","ESI",
                                "ESI_timeseries_Suhag_Assiut_12week.csv",sep = "/")) |>
  dplyr::select(-c(`system:index`,.geo)) |>
  #date columns
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         m_name = month(date, label = TRUE, abbr = TRUE))

#ESI 4 week timeseries (2001 - 2024)
esi_4week <- read_csv(file = paste(here(),"Data","Drought Indices","ESI",
                                    "ESI_timeseries_Suhag_Assiut_4week.csv",sep = "/")) |>
  dplyr::select(-c(`system:index`,.geo)) |>
  #date columns
  mutate(year = year(date),
         month = month(date),
         day = day(date),
         m_name = month(date, label = TRUE, abbr = TRUE))

# ESI 4 weeks plot --------------------------------------------------------
#base plot ESI 4 weeks
esi_4week |>
  filter(governorate == "Assiut") |>
  ggplot(aes(x = date,y = ESI))+
  geom_line()+
  geom_smooth()+
  facet_wrap(~month)

esi_plot <- esi_4week %>%
  mutate(drought_flag = case_when(
    ESI <= -2 ~ "Severe",
    ESI <= -1 ~ "Moderate",
    TRUE ~ "Normal/Wet"
  ))

####Heat map####
#####Continuous ESI values####
esi_4week |>
  mutate(year = factor(year),
         month = factor(month, levels = 1:12, labels = month.abb)) |>
  ggplot(aes(x = month, y = year, fill = ESI)) +
  geom_tile(color = "white") +
  facet_wrap(~governorate) +
  scale_fill_gradient2(low = "brown", mid = "white", high = "blue",
                       midpoint = 0, limits = c(-3.5, 3.5),
                       name = "ESI") +
  labs(title = "Assiut & Suhag: Monthly Evaporative Stress Index (4-week)\nbetween 2001- 2024",
       subtitle = "Red indicates drought stress, Blue indicates wet anomaly.\nESI captures temporal anomalies in evapotranspiration based on remotely sensed\nland-surface temperature (LST) time-change signals.",
       caption = "Source: Author's calculations based on NOAA ESI Data",
       x = "Month", y = "Year") +
  theme_minimal()+
  theme(
    # legend
    legend.position   = "bottom",
    legend.justification = "center",
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    legend.key.size   = unit(0.35, "cm"),
    legend.box.margin = margin(t = 0, b = 0),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8))

#####Categorical ESI values#####
esi_plot |>
  mutate(year = factor(year),
         month = factor(month, levels = 1:12, labels = month.abb)) |>
  ggplot(aes(x = month, y = year, fill = drought_flag)) +
  geom_tile(color = "white") +
  facet_wrap(~governorate) +
  scale_fill_discrete()

# ESI 12 weeks plot -------------------------------------------------------
####SPI classification table####
classify_spi <- function(x) {
  case_when(
    x >  2.0  ~ "Extremely wet",
    x >  1.5  ~ "Very wet",
    x >  1.0  ~ "Moderately wet",
    x >= -1.0 ~ "Near normal",
    x >  -1.5 ~ "Moderately dry",
    x >  -2.0 ~ "Very dry",
    TRUE      ~ "Extremely dry"
  )
}

esi_cat <- esi_12week |>
  mutate(category = classify_spi(ESI))

esi_prob <- esi_cat |>
  count(category) |>
  mutate(
    probability = n / sum(n),          # fraction
    probability_percent = 100 * probability
  ) |>
  arrange(desc(probability))           # sort largest first

esi_prob

#base plot ESI 12 weeks
esi_12week |>
  filter(governorate == "Assiut") |>
  ggplot(aes(x = date,y = ESI))+
  geom_line()+
  geom_smooth()+
  facet_wrap(~month)

#####Cont. ESI values#####
esi_12week |>
  mutate(year = factor(year),
           month = factor(month, levels = 1:12, labels = month.abb)) |>
  ggplot(aes(x = month, y = year, fill = ESI)) +
  geom_tile(color = "white") +
  facet_wrap(~governorate) +
    scale_fill_gradient2(low = "brown", mid = "white", high = "blue",
                         midpoint = 0, limits = c(-3.5, 3.5),
                         name = "ESI") +
    labs(title = "Assiut & Suhag: Monthly Evaporative Stress Index (12-week)\nbetween 2001- 2024",
         subtitle = "Red indicates drought stress, Blue indicates wet anomaly.\nESI captures temporal anomalies in evapotranspiration based on remotely sensed\nland-surface temperature (LST) time-change signals.",
         caption = "Source: Author's calculations based on NOAA ESI Data",
         x = "Month", y = "Year") +
    theme_minimal()+
    theme(
      # legend
      legend.position   = "bottom",
      legend.justification = "center",
      legend.title      = element_text(size = 9),
      legend.text       = element_text(size = 8),
      legend.key.size   = unit(0.35, "cm"),
      legend.box.margin = margin(t = 0, b = 0),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 8))

####Time Series ESI####
# Define manual breaks to ensure 2001 and 2024 are included
breaks_manual <- seq(ymd("2000-01-01"), ymd("2024-12-31"), by = "2 years")

#####Weekly mean#####
#prepare weekly mean data
esi_plotdata <- esi_12week |>
  group_by(governorate, date, year, month) |>
  summarise(weekly_meanESI = mean(ESI, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    ESI_cat = case_when(
      weekly_meanESI > 0     ~ "Wet",
      weekly_meanESI > -1    ~ "Moderately dry",
      weekly_meanESI > -2    ~ "Very dry",
      TRUE                    ~ "Extremely dry"
    ),
    ESI_cat = factor(ESI_cat,
                     levels = c("Wet", "Moderately dry",
                                "Very dry", "Extremely dry")))

#plot
esi_plotdata |>
  ggplot(aes(x = date, y = weekly_meanESI, fill = ESI_cat)) +
  # Highlight treatment period (2020–2024)
  geom_rect(
    inherit.aes = FALSE,
    aes(
      xmin = ymd("2021-01-01"),
      xmax = ymd("2024-12-31"),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey85",
    alpha = 0.8
  ) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Wet" = "steelblue",
      "Moderately dry"       = "#ffb3b3",  # light red
      "Very dry"      = "#ff6666",  # medium red
      "Extremely dry"         = "#b30000"   # dark red
    ),
    name = "ESI Regime"
  ) +
  facet_wrap(~ governorate, nrow = 2) +
  scale_x_date(
    breaks = breaks_manual,
    date_labels = "%Y",
    expand = c(0,0),
    limits = c(ymd("2000-01-01"), ymd("2024-12-31"))
  ) +
  labs(
    title = "Evaporative Stress Index (ESI) by Governorate",
    subtitle = "Weekly mean ESI (2001–2024)",
    x = "Year",
    y = "Mean ESI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "lightgray", color = NA),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#####Quarterly means#####
# Compute quarterly mean with a real date column
esi_quarterly <- esi_12week |>
  mutate(
    quarter = quarter(date),
    year = year(date),
    # get first day of the quarter
    quarter_date = make_date(year, (quarter - 1) * 3 + 1, 1)) |>
  group_by(governorate, year, quarter, quarter_date) |>
  summarise(quarterly_meanESI = mean(ESI, na.rm = TRUE), .groups = "drop")

# Define manual breaks to ensure 2001 and 2024 are included
breaks_manual <- seq(ymd("2000-01-01"), ymd("2024-12-31"), by = "2 years")

# Plot ESI quarterly means
esi_quarterly |>
  ggplot(aes(x = quarter_date, y = quarterly_meanESI,
             fill = quarterly_meanESI > 0)) +
  geom_col(width = 80, alpha = 0.6) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    labels = c("Positive ESI", "Negative ESI")) +
  facet_wrap(~ governorate, nrow = 2) +
  scale_x_date(
    breaks = breaks_manual,
    date_labels = "%Y",
    expand = c(0,0),
    limits = c(ymd("2000-01-01"), ymd("2024-12-31"))
  ) +
  labs(
    title = "Evaporative Stress Index (ESI) by Governorate",
    subtitle = "Quarterly mean ESI values 2001–2024",
    x = "Year",
    y = "Quarterly Mean ESI",
    fill = "ESI Regime"
  ) +
  theme_minimal(base_size = 12)

####Seasonal aggregation####
esi_seasonal <- esi_12week |>
  mutate(season = case_when(
    month %in% c(11,12, 1, 2,3,4) ~ "Winter",
    month %in% c(5,6,7,8,9,10) ~ "Summer",
  )) |>
  group_by(governorate, year, season) |>
  summarise(seasonal_meanESI = mean(ESI, na.rm = TRUE)) |>
  ungroup()

ggplot(esi_seasonal, aes(x = year, y = seasonal_meanESI, color = season)) +
  geom_line() +
  geom_point() +
  facet_wrap(~governorate+season) +
  labs(title = "Seasonal ESI Trends by Governorate", y = "Mean ESI", x = "Year") +
  theme_minimal()


####ESI shock incidents####
#count ESI shock incidents by year
gov_esi_droughtmonths_yearly <- esi_12week |>
  mutate(
    season = case_when(
      month %in% c(11, 12, 1, 2, 3, 4) ~ "winter",
      month %in% c(5, 6, 7, 8, 9, 10) ~ "summer")) |>
  group_by(governorate, year,season) |>
  summarise(
    esi_mdrought_incidents = sum(ESI < -1, na.rm = TRUE),
    esi_sdrought_incidents = sum(ESI  < -2, na.rm = TRUE),
    .groups = "drop")

####Bar Charts####
#####Moderate and Severe Drought Shocks#####
gov_esi_droughtmonths_yearly |>
  group_by(governorate,year) |>
  summarise(sum_m_droughts = sum(esi_mdrought_incidents),
            sum_s_droughts = sum(esi_sdrought_incidents)) |>
  ungroup() |>
  pivot_longer(
    cols = c(sum_m_droughts, sum_s_droughts),
    names_to = "drought_type",
    values_to = "count") |>
  ggplot(aes(x = year, y = count, fill = governorate)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "gray20",
    linewidth = 0.3) +
  facet_wrap(
    ~ drought_type,
    labeller = labeller(
      drought_type = c(
        sum_m_droughts = "Moderate Droughts",
        sum_s_droughts = "Severe Droughts")))+
  scale_x_continuous(
    limits = c(2000, 2024),
    breaks = seq(2000, 2024, by = 2),
    expand = c(0.1,0.1)) +
  scale_y_continuous(
    limits = c(0,35),
    breaks = seq(0,35,by = 5),
    expand = c(0.1,0.1))+
  labs(
    title = "Drought Shock Incidents by Governorate (2000 - 2024)",
    subtitle = "Moderate drought = ESI < -1; Severe drought = ESI < -2.",
    x = "Year",
    y = "Number of Drought Incidents",
    fill = "Governorate") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))

#####Severe drought incidents#####
gov_esi_droughtmonths_yearly |>
  pivot_longer(
    cols = c(esi_sdrought_incidents),
    names_to = "drought_type",
    values_to = "count") |>
  group_by(governorate,year) |>
  summarise(sum_sdroughts = sum(count,na.rm = TRUE)) |>
  ungroup() |>
  ggplot(aes(x = year, y = sum_sdroughts)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "gray20",
    linewidth = 0.3
  ) +
  facet_wrap(~ governorate) +
  scale_x_continuous(
    limits = c(2000, 2024),
    breaks = seq(2000, 2024, by = 2),
    expand = c(0.1,0.1)
  ) +
  scale_y_continuous(
    limits = c(0,15),
    breaks = seq(0,15,by = 5),
    expand = c(0.1,0.1))+
  labs(
    title = "Severe Drought Shock Incidents by Governorate (2000 - 2024)",
    subtitle = "Severe drought = ESI < -2.",
    x = "Year",
    y = "Number of Severe Drought Incidents",
    fill = "Governorate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#bar chart - drought months
esi_monthly <- esi_12week |>
  mutate(
    # Create a proper date: first day of each month
    year_month = as.Date(paste(year, month, "01", sep = "-"))) |>
  group_by(governorate, year_month) |>
  summarise(
    monthly_meanESI = mean(ESI, na.rm = TRUE),
    .groups = "drop") |>
  arrange(governorate, year_month)

#severe drought incidents
esi_monthly |>
  mutate(year = lubridate::year(year_month)) |>
  group_by(governorate, year) |>
  summarise(
    esi_mdrought_months = sum(monthly_meanESI < -1, na.rm = TRUE),
    esi_sdrought_months = sum(monthly_meanESI < -2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(esi_mdrought_months, esi_sdrought_months),
    names_to = "drought_type",
    values_to = "count") |>
  ggplot(aes(x = year, y = count, fill = governorate)) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    color = "gray20",
    linewidth = 0.3) +
  facet_wrap(
    ~ drought_type,
    labeller = labeller(
      drought_type = c(
        esi_mdrought_months = "Moderate Drought Months",
        esi_sdrought_months = "Severe Drought Months")))+
  scale_x_continuous(
    limits = c(2000, 2024),
    breaks = seq(2000, 2024, by = 2),
    expand = c(0.1,0.1)) +
  scale_y_continuous(
    limits = c(0,10),
    breaks = seq(0,10,by = 5),
    expand = c(0.1,0.1))+
  labs(
    title = "Drought Shock Months by Governorate (2000 - 2024)",
    subtitle = "Moderate drought = mean ESI < -1; Severe drought = mean ESI < -2.",
    x = "Year",
    y = "Number of Drought Months",
    fill = "Governorate") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))

# ESI Variance (Unpredictability) ------------------------------------------
# define meteorological seasons
esi12_seasonal <- esi_12week |>
  mutate(
    season = case_when(
      month %in% c(12,1,2) ~ "Winter",
      month %in% c(3,4,5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9,10,11) ~ "Fall"
    ),
    # ensure December is assigned to the *next year's* winter season
    season_year = if_else(month == 12, year + 1, year)
  )

# compute within-season temp variance
esi12_seasonal_var <- esi12_seasonal |>
  group_by(governorate, season_year, season) |>
  summarise(
    esi_sd = sd(ESI, na.rm = TRUE),
    .groups = "drop"
  )

esi12_seasonal_var |>
  ggplot(aes(x = season_year, y = esi_sd, color = season)) +
  geom_line(alpha = 0.3, aes(group = interaction(governorate, season))) +
  geom_smooth(se = FALSE, linewidth = 1.2) +
  labs(
    title = "Seasonal ESI Variability Over Time",
    subtitle = "Within-season SD of ESI",
    x = "Year",
    y = "SD of ESI",
    color = "Season"
  ) +
  theme_minimal()+
  facet_wrap(~governorate,nrow = 2)+
  scale_x_continuous(
    limits = c(2001, 2025),
    breaks = seq(2001, 2025, by = 4),
    expand = c(0, 0)
  ) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    strip.background   = element_rect(fill = "lightgray", color = NA),
    strip.text         = element_text(face = "bold"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

#does not indicate an increase in SD -> pretty stable and in recent years a decrease