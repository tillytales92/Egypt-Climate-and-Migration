# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a research project analyzing climate change and migration patterns in Egypt, part of the STEG project "Migration in the Face of Climate Change: Assessing the Potential of Ultra-Poor Graduation Programs". The codebase focuses on spatial data processing and GPS imputation for household survey data in the Assiut and Suhag governorates.

## R Environment Setup

This is an R-based project using RStudio. The project follows standard R conventions:
- Indentation: 2 spaces (not tabs)
- Encoding: UTF-8
- Project file: `Egypt-Climate-and-Migration.Rproj`

### Running Scripts

Open the R project file in RStudio or run R from the repository root:

```r
# Source scripts from the Code directory
source("Code/0_HH_GPSimputation_fullsample.R")
```

The script automatically installs missing packages from the required libraries list.

## Data Architecture

### Input Data Structure

- `Data/Full Sample HH Data.dta` - Stata format household survey data containing:
  - 3001 unique households (`hhid`)
  - GPS coordinates from female respondents (`gpslatitude`, `gpslongitude`, `gpsaltitude`, `gpsaccuracy`)
  - GPS coordinates from male respondents (`ml_gpslatitude`, `ml_gpslongitude`, etc.)
  - Administrative boundaries (`govern`, `village_eng`, `agglom_id`)

- `Data/Shapefiles/HDX_Egypt/` - Egypt administrative boundary shapefiles from HDX (Humanitarian Data Exchange)
  - Source: https://data.humdata.org/dataset/cod-ab-egy
  - File: `egy_admbnda_adm1_capmas_20170421.shp`

### Output Data Structure

- `Data/hhdata_imputedgps_fullsample.rds` - R data frame with imputed GPS coordinates
- `Data/Shapefiles/hh_imputedgps_fullsample.shp` - Spatial (sf) object with household locations

## GPS Imputation Logic

The core workflow in `0_HH_GPSimputation_fullsample.R` implements a two-stage GPS imputation strategy:

### Stage 1: Cross-Gender GPS Pooling
- Primary: Use female respondent GPS coordinates
- Fallback: If female GPS missing, use male respondent GPS coordinates
- Result: Reduces missing GPS from 68% to 46%

### Stage 2: Geographic Imputation
- For households still missing GPS: impute using median coordinates from households in the same `govern` + `village_eng` + `agglom_id` group
- Result: Achieves 94% GPS coverage (17 households remain unimputed due to no reference data)

### Key Variables Created
- `hh_gpslatitude_imputed`, `hh_gpslongitude_imputed` - Final GPS coordinates after both imputation stages
- `gps_imputed` - Boolean flag indicating whether GPS was imputed (TRUE) or original (FALSE)

## Required R Packages

Core spatial and data manipulation libraries:
- `elevatr` - Access to AWS Terrain Tiles and OpenTopography elevation data
- `terra`, `sf` - Spatial data handling
- `giscoR`, `marmap` - Geographic data and topography
- `tidyverse` - Data manipulation (dplyr, ggplot2, etc.)
- `haven` - Read Stata .dta files
- `naniar` - Missing data visualization
- `leaflet` - Interactive maps
- `here` - Path management

The script auto-installs missing packages.

## Code Conventions

### Path Construction
Use `here()` for cross-platform path compatibility:
```r
read_dta(paste(here(), "Data", "Full Sample HH Data.dta", sep = "/"))
```

### Pipe Operator
Code uses native R pipe `|>` (requires R >= 4.1.0)

### Spatial Data
- Input CRS: WGS 84 (EPSG:4326)
- Convert to sf objects using `st_as_sf()` with coordinate columns specified
- Shapefiles use HDX Egypt administrative boundaries filtered for target governorates

### Missing Data Handling
- Empty strings ("") in `agglom_id` are converted to explicit `NA_character_`
- Use `miss_var_summary()` from naniar package for missing data analysis
- Imputation based on grouped medians using `group_by()` + `mutate()` pattern
