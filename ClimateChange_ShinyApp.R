#Climate Change in Egypt - Interactive Shiny App
#Temperature trends and deviations (1960-2024)

# Load Packages -----------------------------------------------------------
libs <- c("shiny","tidyverse","here","lubridate","plotly","scales")

installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  pak::pkg_install(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = TRUE))

# Load and Process Data ---------------------------------------------------
era5_temp_df <- readRDS(file = paste(here(),"Data","intermediate",
                                     "Governorate Data",
                                     "era5_temp_19602024.Rds",sep = "/"))

# Create annual data
era5_temp_annual <- era5_temp_df |>
  group_by(name, year) |>
  summarise(
    # Unweighted means
    annual_mean_temp = mean(mean_temp, na.rm = TRUE),
    annual_mean_maxtemp = mean(mean_maxtemp, na.rm = TRUE),
    annual_mean_mintemp = mean(mean_mintemp, na.rm = TRUE),
    # Population-weighted means
    annual_mean_temp_pop = mean(mean_temp_pop_weighted, na.rm = TRUE),
    annual_mean_maxtemp_pop = mean(mean_maxtemp_pop_weighted, na.rm = TRUE),
    annual_mean_mintemp_pop = mean(mean_mintemp_pop_weighted, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate baseline statistics (1960-2010)
baseline_stats <- era5_temp_annual |>
  filter(year <= 2010) |>
  group_by(name) |>
  summarise(
    # Unweighted baselines
    mean_temp_baseline = mean(annual_mean_temp, na.rm = TRUE),
    sd_temp_baseline = sd(annual_mean_temp, na.rm = TRUE),
    mean_maxtemp_baseline = mean(annual_mean_maxtemp, na.rm = TRUE),
    sd_maxtemp_baseline = sd(annual_mean_maxtemp, na.rm = TRUE),
    mean_mintemp_baseline = mean(annual_mean_mintemp, na.rm = TRUE),
    sd_mintemp_baseline = sd(annual_mean_mintemp, na.rm = TRUE),
    # Population-weighted baselines
    mean_temp_pop_baseline = mean(annual_mean_temp_pop, na.rm = TRUE),
    sd_temp_pop_baseline = sd(annual_mean_temp_pop, na.rm = TRUE),
    mean_maxtemp_pop_baseline = mean(annual_mean_maxtemp_pop, na.rm = TRUE),
    sd_maxtemp_pop_baseline = sd(annual_mean_maxtemp_pop, na.rm = TRUE),
    mean_mintemp_pop_baseline = mean(annual_mean_mintemp_pop, na.rm = TRUE),
    sd_mintemp_pop_baseline = sd(annual_mean_mintemp_pop, na.rm = TRUE),
    .groups = "drop"
  )

# Join and calculate deviations
era5_temp_annual <- era5_temp_annual |>
  left_join(baseline_stats, by = "name") |>
  mutate(
    # Unweighted deviations
    temp_diff_mean = annual_mean_temp - mean_temp_baseline,
    temp_diff_max = annual_mean_maxtemp - mean_maxtemp_baseline,
    temp_diff_min = annual_mean_mintemp - mean_mintemp_baseline,
    z_score_mean = (annual_mean_temp - mean_temp_baseline) / sd_temp_baseline,
    z_score_max = (annual_mean_maxtemp - mean_maxtemp_baseline) / sd_maxtemp_baseline,
    z_score_min = (annual_mean_mintemp - mean_mintemp_baseline) / sd_mintemp_baseline,
    # Population-weighted deviations
    temp_diff_mean_pop = annual_mean_temp_pop - mean_temp_pop_baseline,
    temp_diff_max_pop = annual_mean_maxtemp_pop - mean_maxtemp_pop_baseline,
    temp_diff_min_pop = annual_mean_mintemp_pop - mean_mintemp_pop_baseline,
    z_score_mean_pop = (annual_mean_temp_pop - mean_temp_pop_baseline) / sd_temp_pop_baseline,
    z_score_max_pop = (annual_mean_maxtemp_pop - mean_maxtemp_pop_baseline) / sd_maxtemp_pop_baseline,
    z_score_min_pop = (annual_mean_mintemp_pop - mean_mintemp_pop_baseline) / sd_mintemp_pop_baseline
  )

# Shiny UI ----------------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),

  titlePanel(
    div(
      h2("Climate Change in Egypt: Temperature Trends (1960-2024)"),
      p("Explore annual temperature deviations from 1960-2010 baseline",
        style = "font-size: 14px; color: #666;")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Select Parameters"),

      selectInput(
        "governorates",
        "Governorates:",
        choices = unique(era5_temp_annual$name),
        selected = c("Assiut", "Suhag"),
        multiple = TRUE
      ),

      radioButtons(
        "temp_var",
        "Temperature Variable:",
        choices = c(
          "Mean Temperature" = "mean",
          "Maximum Temperature" = "max",
          "Minimum Temperature" = "min"
        ),
        selected = "mean"
      ),

      radioButtons(
        "weighting",
        "Weighting Method:",
        choices = c(
          "Population-weighted" = "pop",
          "Unweighted" = "raw",
          "Compare Both" = "compare"
        ),
        selected = "pop"
      ),

      radioButtons(
        "plot_type",
        "Visualization:",
        choices = c(
          "Deviation from Baseline" = "deviation",
          "Absolute Temperature" = "absolute",
          "Z-Score (Standard Deviations)" = "zscore"
        ),
        selected = "deviation"
      ),

      checkboxInput(
        "show_trend",
        "Show Trend Line",
        value = TRUE
      ),

      sliderInput(
        "year_range",
        "Year Range:",
        min = 1960,
        max = 2024,
        value = c(1960, 2024),
        step = 1,
        sep = ""
      ),

      hr(),

      helpText(
        "Data Source: ERA5 reanalysis (1960-2024)",
        br(),
        "Baseline: 1960-2010 average",
        br(),
        "Population weights based on 2020 data"
      )
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        type = "tabs",

        tabPanel(
          "Temperature Trends",
          br(),
          plotlyOutput("temp_plot", height = "600px")
        ),

        tabPanel(
          "Summary Statistics",
          br(),
          h4("Temperature Statistics by Governorate"),
          tableOutput("summary_table"),
          br(),
          h4("Decadal Averages"),
          tableOutput("decade_table")
        ),

        tabPanel(
          "About",
          br(),
          h4("About This App"),
          p("This interactive application visualizes temperature trends across Egyptian governorates
            from 1960 to 2024 using ERA5 reanalysis data."),

          h5("Features:"),
          tags$ul(
            tags$li("Compare multiple governorates simultaneously"),
            tags$li("Toggle between population-weighted and unweighted measurements"),
            tags$li("View mean, maximum, and minimum temperatures"),
            tags$li("Examine deviations from historical baseline (1960-2010)"),
            tags$li("Interactive plots with zoom and hover capabilities")
          ),

          h5("Methodology:"),
          p("Temperature deviations are calculated relative to the 1960-2010 baseline period.
            Population-weighted measurements account for where people actually live within each governorate,
            providing a more accurate representation of human exposure to temperature changes."),

          h5("Z-Scores:"),
          p("Z-scores indicate how many standard deviations a given year's temperature is from the baseline mean.
            Values beyond ±2 are considered exceptional.")
        )
      )
    )
  )
)

# Shiny Server ------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive data filtering
  filtered_data <- reactive({
    req(input$governorates)

    era5_temp_annual |>
      filter(
        name %in% input$governorates,
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
  })

  # Main plot
  output$temp_plot <- renderPlotly({
    req(input$governorates)

    df <- filtered_data()

    # Determine which variable to plot
    if (input$plot_type == "deviation") {
      if (input$weighting == "pop") {
        y_var <- paste0("temp_diff_", input$temp_var, "_pop")
        y_label <- "Temperature Deviation (°C)"
      } else if (input$weighting == "raw") {
        y_var <- paste0("temp_diff_", input$temp_var)
        y_label <- "Temperature Deviation (°C)"
      } else {
        # Compare mode handled separately
        y_var <- NULL
      }
    } else if (input$plot_type == "absolute") {
      if (input$weighting == "pop") {
        y_var <- paste0("annual_", input$temp_var, "temp_pop")
      } else {
        y_var <- paste0("annual_", input$temp_var, "temp")
      }
      y_label <- "Temperature (°C)"
    } else {  # zscore
      if (input$weighting == "pop") {
        y_var <- paste0("z_score_", input$temp_var, "_pop")
      } else {
        y_var <- paste0("z_score_", input$temp_var)
      }
      y_label <- "Z-Score (Standard Deviations)"
    }

    # Create title
    temp_names <- c("mean" = "Mean", "max" = "Maximum", "min" = "Minimum")
    plot_names <- c("deviation" = "Deviation from Baseline",
                    "absolute" = "Absolute Temperature",
                    "zscore" = "Z-Score")
    weight_names <- c("pop" = "Population-weighted",
                      "raw" = "Unweighted",
                      "compare" = "Comparison")

    title_text <- paste0(
      temp_names[input$temp_var], " Temperature - ",
      plot_names[input$plot_type],
      " (", weight_names[input$weighting], ")"
    )

    # Handle comparison mode
    if (input$weighting == "compare") {
      # Create long format data for comparison
      if (input$plot_type == "deviation") {
        df_compare <- df |>
          select(name, year,
                 Unweighted = paste0("temp_diff_", input$temp_var),
                 `Population-weighted` = paste0("temp_diff_", input$temp_var, "_pop")) |>
          pivot_longer(cols = c(Unweighted, `Population-weighted`),
                      names_to = "Weighting", values_to = "value")
      } else if (input$plot_type == "absolute") {
        df_compare <- df |>
          select(name, year,
                 Unweighted = paste0("annual_", input$temp_var, "temp"),
                 `Population-weighted` = paste0("annual_", input$temp_var, "temp_pop")) |>
          pivot_longer(cols = c(Unweighted, `Population-weighted`),
                      names_to = "Weighting", values_to = "value")
      } else {
        df_compare <- df |>
          select(name, year,
                 Unweighted = paste0("z_score_", input$temp_var),
                 `Population-weighted` = paste0("z_score_", input$temp_var, "_pop")) |>
          pivot_longer(cols = c(Unweighted, `Population-weighted`),
                      names_to = "Weighting", values_to = "value")
      }

      p <- ggplot(df_compare, aes(x = year, y = value, color = Weighting, group = Weighting)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2, alpha = 0.6) +
        facet_wrap(~name, ncol = 2) +
        scale_color_manual(values = c("Unweighted" = "#E69F00",
                                      "Population-weighted" = "#0072B2")) +
        theme_minimal(base_size = 12) +
        labs(
          title = title_text,
          subtitle = "Comparison of weighting methods - ERA5 data (1960-2024)",
          x = "Year",
          y = y_label
        ) +
        theme(
          legend.position = "bottom",
          strip.background = element_rect(fill = "lightgray", color = NA),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )

      if (input$show_trend) {
        p <- p + geom_smooth(method = "loess", se = TRUE, alpha = 0.2, linewidth = 0.8)
      }

    } else {
      # Single weighting mode
      if (input$plot_type == "deviation") {
        p <- ggplot(df, aes(x = year, y = .data[[y_var]], fill = .data[[y_var]])) +
          geom_col(color = "black", width = 0.8, alpha = 0.9) +
          scale_fill_gradient2(
            low = "#0571b0",
            mid = "white",
            high = "#ca0020",
            midpoint = 0,
            name = "Deviation (°C)"
          )
      } else {
        p <- ggplot(df, aes(x = year, y = .data[[y_var]])) +
          geom_line(color = "#0072B2", linewidth = 1) +
          geom_point(color = "#0072B2", size = 2, alpha = 0.7)

        if (input$show_trend) {
          p <- p + geom_smooth(method = "loess", se = TRUE, color = "#D55E00",
                              fill = "#D55E00", alpha = 0.2)
        }
      }

      p <- p +
        facet_wrap(~name, ncol = 2) +
        theme_minimal(base_size = 12) +
        labs(
          title = title_text,
          subtitle = "Relative to 1960-2010 baseline - ERA5 data",
          x = "Year",
          y = y_label
        ) +
        theme(
          legend.position = "bottom",
          strip.background = element_rect(fill = "lightgray", color = NA),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    }

    ggplotly(p, tooltip = c("x", "y")) |>
      layout(hovermode = "closest")
  })

  # Summary statistics table
  output$summary_table <- renderTable({
    req(input$governorates)

    df <- filtered_data()

    if (input$weighting == "pop") {
      df |>
        group_by(name) |>
        summarise(
          `Mean (°C)` = round(mean(annual_mean_temp_pop, na.rm = TRUE), 2),
          `Max (°C)` = round(mean(annual_mean_maxtemp_pop, na.rm = TRUE), 2),
          `Min (°C)` = round(mean(annual_mean_mintemp_pop, na.rm = TRUE), 2),
          `Warming Trend (°C)` = round(
            coef(lm(annual_mean_temp_pop ~ year))[2] * (max(year) - min(year)), 2
          ),
          .groups = "drop"
        ) |>
        rename(Governorate = name)
    } else {
      df |>
        group_by(name) |>
        summarise(
          `Mean (°C)` = round(mean(annual_mean_temp, na.rm = TRUE), 2),
          `Max (°C)` = round(mean(annual_mean_maxtemp, na.rm = TRUE), 2),
          `Min (°C)` = round(mean(annual_mean_mintemp, na.rm = TRUE), 2),
          `Warming Trend (°C)` = round(
            coef(lm(annual_mean_temp ~ year))[2] * (max(year) - min(year)), 2
          ),
          .groups = "drop"
        ) |>
        rename(Governorate = name)
    }
  }, striped = TRUE, hover = TRUE)

  # Decadal averages table
  output$decade_table <- renderTable({
    req(input$governorates)

    df <- filtered_data() |>
      mutate(decade = paste0(floor(year / 10) * 10, "s"))

    if (input$weighting == "pop") {
      df |>
        group_by(name, decade) |>
        summarise(
          `Mean Temp (°C)` = round(mean(annual_mean_temp_pop, na.rm = TRUE), 2),
          `Deviation from Baseline (°C)` = round(mean(temp_diff_mean_pop, na.rm = TRUE), 2),
          .groups = "drop"
        ) |>
        pivot_wider(names_from = decade, values_from = c(`Mean Temp (°C)`,
                                                          `Deviation from Baseline (°C)`)) |>
        rename(Governorate = name)
    } else {
      df |>
        group_by(name, decade) |>
        summarise(
          `Mean Temp (°C)` = round(mean(annual_mean_temp, na.rm = TRUE), 2),
          `Deviation from Baseline (°C)` = round(mean(temp_diff_mean, na.rm = TRUE), 2),
          .groups = "drop"
        ) |>
        pivot_wider(names_from = decade, values_from = c(`Mean Temp (°C)`,
                                                          `Deviation from Baseline (°C)`)) |>
        rename(Governorate = name)
    }
  }, striped = TRUE, hover = TRUE)
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
