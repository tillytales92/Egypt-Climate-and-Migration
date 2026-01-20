#Mapping Time series: Temperature data
#Measures inspired by: https://data360.worldbank.org/en/dataset/WB_CCKP
# Read in Packages --------------------------------------------------------
#Define packages used
libs <- c("tidyverse","naniar","here","devtools",
          "lubridate","patchwork","zoo")

#install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

#load libraries
invisible(lapply(libs,library,character.only = TRUE))

# Read Data ---------------------------------------------------------------
#Load temp. data for Alexandria, Cairo, Assiut and Sohag
era5_temp_df <- readRDS(file = paste(here(),"Data",
                                     "intermediate","Governorate Data",
                                     "era5_temp_19602024",sep = "/"))

####Create annual data####
era5_temp_annual <-  era5_temp_df |>
  group_by(name, year) |>
  summarise(
    annual_mean_temp       = mean(mean_temp, na.rm = TRUE),
    annual_mean_temp_pop   = mean(mean_temp_pop_weighted, na.rm = TRUE),
    annual_mean_maxtemp = mean(mean_maxtemp,na.rm = TRUE),
    annual_mean_maxtemp_pop = mean(mean_maxtemp_pop_weighted,na.rm = TRUE),
    annual_mean_mintemp = mean(mean_mintemp,na.rm = TRUE),
    annual_mean_mintemp_pop = mean(mean_mintemp_pop_weighted,na.rm = TRUE),
    .groups = "drop")

#create deviations from mean (1960-2010) measure
#plot annual mean unweighted and annual mean pop. weighted
mean_19602010 <- era5_temp_annual |>
  filter(year <= 2010) |>
  group_by(name) |>
  summarise(mean_temp19602010_pop = mean(annual_mean_temp_pop , na.rm = TRUE),
            sd_temp19602010_pop   = sd(annual_mean_temp_pop, na.rm = TRUE),
            mean_maxtemp19602010_pop = mean(annual_mean_maxtemp_pop , na.rm = TRUE),
            sd_maxtemp19602010_pop   = sd(annual_mean_maxtemp_pop, na.rm = TRUE),
            mean_mintemp19602010_pop = mean(annual_mean_mintemp_pop , na.rm = TRUE),
            sd_mintemp19602010_pop   = sd(annual_mean_mintemp_pop, na.rm = TRUE),
            .groups = "drop")

#join and take deciles
era5_temp_annual <- era5_temp_annual |>
  left_join(mean_19602010, by = "name") |>
  group_by(name) |>  # group by governorate
  mutate(
    # mean temperature
    temp_diff_mean       = annual_mean_temp_pop - mean_temp19602010_pop,
    temp_decile_mean     = ntile(annual_mean_temp_pop, 10),
    z_score_meantemp     = (annual_mean_temp_pop - mean_temp19602010_pop) / sd_temp19602010_pop,
    shock_meantemp       = case_when(
      z_score_meantemp >  2  ~  1,
      z_score_meantemp < -2  ~ -1,
      TRUE                    ~ 0
    ),
    # max temperature
    temp_diff_maxtemp    = annual_mean_maxtemp_pop - mean_maxtemp19602010_pop,
    z_score_maxtemp      = (annual_mean_maxtemp_pop - mean_maxtemp19602010_pop) / sd_maxtemp19602010_pop,
    shock_maxtemp        = case_when(
      z_score_maxtemp >  2  ~  1,
      z_score_maxtemp < -2  ~ -1,
      TRUE                    ~ 0
    ),
    # min temperature
    temp_diff_mintemp    = annual_mean_mintemp_pop - mean_mintemp19602010_pop,
    z_score_mintemp      = (annual_mean_mintemp_pop - mean_mintemp19602010_pop) / sd_mintemp19602010_pop,
    shock_mintemp        = case_when(
      z_score_mintemp >  2  ~  1,
      z_score_mintemp < -2  ~ -1,
      TRUE                    ~ 0
    )
  ) |>
  ungroup()


# Plot --------------------------------------------------------------------
library(shiny)

ui <- fluidPage(
  titlePanel("Temperature Deviation Explorer"),

  sidebarLayout(
    sidebarPanel(
      selectInput("governorates", "Select Governorates:",
                  choices = unique(era5_temp_annual$name),
                  selected = c("Assiut", "Suhag"),
                  multiple = TRUE),

      radioButtons("temp_var", "Temperature Variable:",
                   choices = c("Mean Temperature" = "temp_diff_mean",
                               "Maximum Temperature" = "temp_diff_maxtemp",
                               "Minimum Temperature" = "temp_diff_mintemp"),
                   selected = "temp_diff_mean")
    ),

    mainPanel(
      plotOutput("temp_plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  output$temp_plot <- renderPlot({
    req(input$governorates)

    var_labels <- c(
      "temp_diff_mean" = "Annual Mean Temperature (Pop. weighted) Changes (1960–2024)",
      "temp_diff_maxtemp" = "Annual Maximum Temperature (Pop. weighted) Changes (1960–2024)",
      "temp_diff_mintemp" = "Annual Minimum Temperature (Pop. weighted) Changes (1960–2024)"
    )

    era5_temp_annual |>
      filter(name %in% input$governorates) |>
      ggplot(aes(x = year, y = .data[[input$temp_var]], fill = .data[[input$temp_var]])) +
      geom_col(color = "black", width = 0.9) +
      facet_wrap(~name, nrow = 2) +
      scale_x_continuous(
        limits = c(1960, 2025),
        breaks = seq(1960, 2024, by = 4),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        limits = c(-1.5, 2.5),
        breaks = seq(-1.5, 2.5, by = 0.5),
        expand = c(0, 0)
      ) +
      scale_fill_gradient2(
        low = "#0571b0",
        mid = "white",
        high = "#ca0020",
        midpoint = 0,
        name = "Deviation (°C)",
        breaks = seq(-1.5, 2.5, by = 1),
        limits = c(-1.5, 2.5)
      ) +
      theme_minimal(base_size = 12) +
      labs(
        title = var_labels[input$temp_var],
        subtitle = "Relative to 1960-2010 average (°C) - ERA5 data",
        x = "Year",
        y = "Temperature Deviation (°C)"
      ) +
      theme(
        legend.position = "bottom",
        strip.background = element_rect(fill = "lightgray", color = NA),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)





