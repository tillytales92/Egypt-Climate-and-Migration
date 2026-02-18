# Egypt Climate and Migration

This repository shows the data work for a climate and migration research project for the STEG (Structural Transformation and Economic Growth) initiative, focusing on "Migration in the Face of Climate Change: Assessing the Potential of Ultra-Poor Graduation Programs" in Upper Egypt. The work encompasses data acquisition, processing, and aggregation of climate variables at both governorate and household levels to enable a rigorous analysis of climate vulnerability and its relationship to migration decisions.

## Project Overview

The project analyses climate vulnerability and stress in the Assiut and Suhag governorates of Upper Egypt by combining:

- **Satellite climate data** (1960-2024): Temperature, heat indices, precipitation, drought indicators
- **Household survey data**: Primary survey data from households in Assiut and Suhag
- **Population data**: Spatial population distribution (LandScan)

The analysis measures climate exposure at both governorate and household levels, identifying climate shocks, trends, and vulnerability patterns relevant to migration decisions.

## Repository Structure

### Code Directory

The code is organized into a **sequential processing pipeline** with numbered folders:

```
Code/
├── 1_Raster Data/           # Download and process satellite/climate rasters
├── 2_Governorate Data/      # Extract climate data at governorate level
├── 3_Household Data/        # Process household surveys and extract climate exposure
└── 4_EDA/                   # Exploratory data analysis, visualization, and reporting
```

#### Script Numbering Convention

Within each folder, scripts use numbered prefixes indicating their processing stage:

- **0_**: Preliminary/exploratory analyses (e.g., `0_HH_GPSimputation.R`)
- **1_**: Data download and initial processing (e.g., `1_Download_ERA5_temperature.R`)
- **2_**: Data extraction (e.g., `2_Extraction_ERA5_Temperature.R`)
- **3_**: Analysis and variable construction (e.g., `3_ClimateShock_Variables_HHPanel.R`)
- **4_**: Dataset merging (e.g., `4_Merging_HHDataset.R`)
- **5_**: Indices and derived measures (e.g., `5_VulnerabilityIndices_HHPanel.R`)
- **6_**: Master pipeline scripts (e.g., `6_SourcingScripts_Households.R`)

#### Code Folders Explained

**1_Raster Data/**
- Downloads raw climate data from APIs (ERA5, CHIRPS)
- Combines and crops rasters to study region
- Calculates derived measures (heat indices, UTCI)
- Creates intermediate raster files for extraction

**2_Governorate Data/**
- Extracts climate data for all Egyptian governorates
- Creates governorate-level time series (1960-2024)
- Calculates population-weighted and unweighted climate measures
- Used for regional climate trend analysis

**3_Household Data/**
- Processes household survey data
- Imputes missing GPS coordinates
- Extracts climate exposure at household locations
- Constructs climate shock variables
- Creates vulnerability indices
- Merges all data into final household datasets

**4_EDA/**
- Time series visualizations (temperature, precipitation, drought)
- Spatial mapping of climate variables
- Interactive Shiny application for data exploration
- RMarkdown reports for climate change analysis

### Data Directory

The `Data/` directory is **not tracked in git** (gitignored) but follows this structure locally:

```
Data/
├── raw/                      # Raw input data
│   ├── ERA5/                 # Temperature and dewpoint rasters
│   ├── CHIRPS/               # Precipitation rasters
│   ├── ESI/                  # Evaporative Stress Index
│   ├── NDVI/                 # Vegetation indices
│   ├── UTCI/                 # Universal Thermal Climate Index
│   ├── Landscan/             # Population distribution rasters
│   ├── Modis/                # Land cover data
│   ├── Household Data/       # Survey data (.dta files)
│   └── Shapefiles/           # Geographic boundaries
│       └── HDX_Egypt/        # Egypt governorate boundaries
├── intermediate/             # Processed intermediate datasets
│   ├── ERA5/                 # Combined/cropped temperature rasters
│   ├── CHIRPS/               # Combined/cropped precipitation rasters
│   ├── UTCI/                 # Processed UTCI rasters
│   ├── Household Data/       # Household-level climate extractions
│   └── Governorate Data/     # Governorate-level time series
└── final/                    # Final analysis-ready datasets
    └── hh_data_v*.Rds/dta    # Versioned household datasets
```

## Data Pipeline Workflow

The project follows a strict sequential workflow:

```
1. Download & Process Climate Rasters (1_Raster Data)
   ↓
2. Extract Governorate-Level Climate Data (2_Governorate Data)
   ↓
3. Process Household Surveys & Extract Climate Exposure (3_Household Data)
   ↓
4. Analyse, Visualize, and Report (4_EDA)
```

### Key Outputs

- **Governorate-level datasets**: Climate time series for all Egyptian governorates (1960-2024)
- **Household-level datasets**: Survey data merged with climate exposure variables
- **Visualizations**: Interactive Shiny app and RMarkdown reports
- **Climate indices**: Composite measures of heat stress, drought stress, and overall climate vulnerability

## Climate Variables

The project analyses multiple climate indicators:

### Temperature
- Daily mean, maximum, and minimum temperature (ERA5, 1960-2024)
- Heat indices accounting for humidity
- UTCI (Universal Thermal Climate Index) for thermal comfort

### Precipitation
- Daily rainfall (CHIRPS, 1981-2024)
- Dry spell duration
- Precipitation extremes

### Drought
- ESI (Evaporative Stress Index)
- NDVI/EVI (vegetation greenness)
- Compound drought indicators

### Climate Shocks
Climate shocks are defined as extreme deviations from historical baselines:
- Temperature: ±2 standard deviations from 1960-2010 mean
- Precipitation: ±2 standard deviations from 1981-2024 mean
- Drought: ESI < -1 (moderate) or < -2 (severe)

## Technical Requirements

### R Packages
Core dependencies:
- **Spatial data**: `terra`, `sf`, `raster`, `exactextractr`
- **Data manipulation**: `tidyverse`, `haven`, `naniar`
- **Visualization**: `ggplot2`, `plotly`, `shiny`, `leaflet`
- **Climate data**: `KrigR` (Copernicus CDS API)
- **Project management**: `here`

### API Credentials
- **Copernicus Climate Data Store**: Required for downloading ERA5 data (store key in `cds_key.txt`)
- **Google Distance Matrix API**: Optional, for market access calculations (store key in `google_key.txt`)

## Getting Started

### Viewing Climate Change Reports

1. Open `Code/4_EDA/ClimateChange_ShinyApp.R` in RStudio and run the app for interactive exploration
2. Knit `Code/4_EDA/ClimateChange_Report.Rmd` to generate an HTML report with temperature and precipitation analysis

### Regenerating Household Data

If raw household survey data is updated, run the master pipeline:

```r
source(here("Code", "3_Household Data", "6_SourcingScripts_Households.R"))
```

This will regenerate all household-level climate extractions and merge them into the final dataset.

### Creating New Analyses

Follow the sequential folder structure:
1. Place exploratory scripts in the appropriate numbered folder
2. Use the `here()` package for all file paths
3. Follow the existing naming conventions
4. Save intermediate outputs to `Data/intermediate/`
5. Save final outputs to `Data/final/`

## Documentation

- **CLAUDE.md**: Technical documentation for AI coding assistants (workflow details, variable naming, conventions)
- **README.md**: This file - project overview and structure
- **Script comments**: Each R script contains inline documentation of its purpose and methods

## Contact

This project is part of the STEG research initiative studying climate-induced migration and the effectiveness of ultra-poor graduation programs in climate-vulnerable regions.
For questions about the code or methodology, refer to the inline documentation in individual scripts or consult CLAUDE.md for technical details.
