# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a climate and migration research project for the STEG (Structural Transformation and Economic Growth) initiative, focusing on "Migration in the Face of Climate Change: Assessing the Potential of Ultra-Poor Graduation Programs" in Upper Egypt (Assiut and Suhag governorates).

The project analyzes climate stress indicators (temperature, heat indices, drought measures) at both governorate and household levels, combining satellite climate data with household survey data to measure climate vulnerability and stress.

## Project Structure

The repository is organized into a sequential workflow with numbered scripts:

### Code Organization

Scripts follow a numbered naming convention indicating processing stages:

- **0_**: Preliminary/exploratory analyses (e.g., `0_shapefilecomparison.R`, `0_HH_GPSimputation.R`)
- **1_**: Data download and combination (e.g., `1_ERA5Download_krigr_precipitation.R`, `1_TerraClimate_Download.R`)
- **2_**: Data extraction and processing (e.g., `2_Extraction_ERA5_Temperature.R`, `2_Extraction_CDS_UTCI.R`)
- **3_**: Climate stress analysis, time series creation, and mapping (e.g., `3_ClimateStress_Timeseries_Temperature.R`, `3_Mapping_UTCI.R`)
- **4_**: Final dataset construction (e.g., `4_HHdataset.R`)
- **5_**: Analysis outputs (e.g., `5_VulnerabilityIndices.R`)
- **9_**: Tutorial/reference scripts (e.g., `9_krigR_tutorial.R`, `9_stagg_tutorial.R`)

### Directory Structure

- `Code/`: All R analysis scripts
  - `Code/Raster Data/`: Scripts for downloading and combining raster climate data
  - `Code/Household Analysis/`: Household-level GPS imputation, data extraction, and analysis
- `Data/`: All data files (not tracked in git)
  - Raw household survey data (.dta files)
  - Processed household datasets (.rds files)
  - `Data/Shapefiles/`: Geographic boundary files
    - `Data/Shapefiles/HDX_Egypt/`: Egypt administrative boundaries from HDX (primary shapefile: `egy_admbnda_adm1_capmas_20170421.shp`)
  - `Data/Households/`: Processed household datasets by version date

## Working with R Scripts

### Common R Environment Setup

Most scripts share a common setup pattern:

```r
# Define required packages
libs <- c("tidyverse", "here", "terra", "raster", "sf", ...)

# Install missing packages
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)){
  install.packages(libs[!installed_libs])
}

# Load libraries
invisible(lapply(libs, library, character.only = TRUE))
```

### Key R Packages

- **Spatial/raster**: `terra`, `raster`, `sf`, `geodata`, `exactextractr`
- **Climate data**: `KrigR`, `stagg`
- **Data manipulation**: `tidyverse`, `here`, `naniar`, `haven`
- **Visualization**: `ggplot2`, `leaflet`, `mapview`, `cowplot`
- **Date handling**: `lubridate`

### Running R Scripts

Open the project in RStudio using `Egypt-Climate-and-Migration.Rproj` to ensure proper working directory and project settings. Scripts use the `here()` package for path resolution, so they should work regardless of where you run them from within the project.

To run an individual script:
```r
source("Code/[script-name].R")
```

Or open and run interactively in RStudio.

### Generating Reports

To generate the GPS imputation report:
```r
rmarkdown::render("GPS_Imputation_Report.Rmd")
```

This produces both HTML and markdown outputs documenting the GPS imputation methodology.

## Key Data Workflows

### 1. Climate Data Download and Processing

**Temperature data workflow:**
1. `1_ERA5Download_krigr_temperature.R` - Downloads ERA5 temperature data via KrigR package
2. `1_ERA5_Temperature_CombiningRasters.R` - Combines multi-year rasters
3. `2_Extraction_ERA5_Temperature.R` - Extracts values to governorate polygons using exactextractr
4. `3_ClimateStress_Timeseries_Temperature.R` - Creates climate stress metrics and time series

**Precipitation data workflow:**
1. `1_ERA5Download_krigr_precipitation.R` - Downloads ERA5 precipitation via KrigR
2. `1_ERA5_Precipitation_CombiningRasters.R` or `1_CHIRPS_Precipitation_CombiningRasters.R`
3. `2_Extraction_ERA5_Precipitation.R` or `2_Extraction_CHIRPS_Precipitation.R`
4. `3_ClimateStress_TimeSeries_Precipitation.R`

**Heat stress indices:**
- `1_ERA5_heatindex_calculation.R` - Calculates heat index from temperature + humidity
- `2_Extraction_CDS_UTCI.R` - Universal Thermal Climate Index extraction
- `3_Mapping_UTCI.R` and `3_Mapping_HeatIndex.R` - Visualization

**Drought indices:**
- `2_Extraction_CDS_DroughtIndices.R` - Various drought indices
- `2_Extraction_TerraClimate_PDSI.R` - Palmer Drought Severity Index
- `2_Extraction_ESI_HHPanel.R` - Evaporative Stress Index for households
- `2_Extraction_NDVI_HHPanel.R` - NDVI and EVI vegetation indices

### 2. Household-Level Analysis

**GPS imputation (required first step):**
- `0_HH_GPSimputation.R` - Imputes missing GPS coordinates using governorate/district/village means
- `0_HH_GPSimputation_fullsample.R` - Full sample version
- Creates: `hhdata_imputedgps.rds` and `hhdata_imputedgps_fullsample.rds`

**Household data extraction:**
- `2_Extraction_Temp_HHPanel.R` - Temperature variables at household locations
- `2_Extraction_ESI_HHPanel.R` - Evaporative stress at HH level
- `2_Extraction_NDVI_HHPanel.R` - Vegetation indices at HH level
- `2_Extraction_HH_Waterways.R` - Distance to Nile/waterways
- `2_Extraction_HH_TravelDistances.R` - Travel distances to cities/markets

**Climate stress variables:**
- `3_ClimateStress_Temp_HHvariables.R` - Calculates household heat stress metrics
- `3_ClimateStress_Temp_HH_Plots.R` - Visualization

**Final dataset construction:**
- `4_HHdataset.R` - Combines all household climate variables into master dataset
- Output: `hh_data_v[date].dta` and `hh_data_v[date].Rds` in `Data/Households/`

**Vulnerability indices:**
- `5_VulnerabilityIndices.R` - Creates vulnerability indices using inverse distance weighting
- `5_VulnerabilityIndices_Maps.R` - Maps vulnerability across space

### 3. Auxiliary Data

**Elevation/topography:**
- `2_Elevation_DataDownload.R` - Downloads DEM data
- `2_Elevation_Extraction.R` - Extracts slope and elevation values

**Market access:**
- `4_FindingMarketTowns.R` - Identifies market towns for distance calculations

## Data Sources and APIs

### ERA5 Climate Data (via KrigR)
- Requires Copernicus Climate Data Store (CDS) API credentials
- API User and Key stored in scripts (should be externalized for security)
- Downloads via `CDownloadS()` function with multi-core support

### Geographic Boundaries
- Primary shapefile: HDX Egypt administrative boundaries (Level 1)
- File: `Data/Shapefiles/HDX_Egypt/egy_admbnda_adm1_capmas_20170421.shp`
- Source: https://data.humdata.org/dataset/cod-ab-egy

### Study Area
- Focus governorates: **Assiut** and **Suhag** (Upper Egypt)
- Some scripts also include Alexandria and Cairo for comparison

## Key Climate Metrics

The project captures three types of heat stress:
1. **Measured Air Temperature**: Mean, max, min daily temperatures
2. **Heat Index**: Temperature + relative humidity combination
3. **UTCI (Universal Thermal Climate Index)**: Physiological comfort index accounting for temperature, humidity, wind, and radiation

Drought is measured through:
1. **ESI (Evaporative Stress Index)**: Agricultural drought/crop stress
2. **NDVI/EVI**: Vegetation health indices
3. **PDSI**: Palmer Drought Severity Index

Note: Traditional precipitation-based drought indices (SPI, SPEI) are not suitable for Upper Egypt due to historically low rainfall.

## Temporal Coverage and Aggregation

- **Raw daily data**: 1960-2024 for temperature, 2000-2024 for most other variables
- **Seasonal definitions**:
  - Winter: November–May (months 11, 12, 1, 2, 3, 4, 5)
  - Summer: May–October (months 5, 6, 7, 8, 9, 10)
  - November-December assigned to next year's winter season
- **Climate stress reference period**: 1960-2010 baseline for calculating deviations/z-scores
- **Treatment periods**: Scripts often separate "pretreatment" vs "2020-2024" periods

## Coordinate Reference Systems

- Rasters typically in WGS84 (EPSG:4326)
- Shapefiles reprojected to match raster CRS when needed using `st_transform()`
- Always verify CRS compatibility before spatial operations

## Key Functions and Operations

### Raster Extraction
Scripts use `exactextractr::exact_extract()` for extracting raster values to polygons:
- Mean extraction: `exact_extract(raster, polygons, 'mean')`
- Population-weighted: requires population raster as weight
- Cropland-weighted: requires cropland raster as weight

### Climate Stress Calculations
Common pattern for z-scores and shocks:
```r
z_score = (value - historical_mean) / historical_sd
shock = case_when(
  z_score > 2 ~ 1,    # positive shock
  z_score < -2 ~ -1,  # negative shock
  TRUE ~ 0            # no shock
)
```

### Time Aggregation
- Daily → Annual: group by year, calculate means/counts
- Daily → Seasonal: group by season_year + season
- Shock days: Count days exceeding thresholds

## Output Formats

- **R objects**: `.rds` files for intermediate R data
- **Stata datasets**: `.dta` files for final household datasets (with variable labels)
- **Shapefiles**: Household locations with imputed GPS
- **Visualizations**: PDFs and interactive HTML maps

## Important Notes

- **Large files**: Temperature rasters (1960-2024 daily) are extremely large; scripts handle them with `terra` package for efficiency
- **Processing time**: Climate data downloads and raster extractions can take hours; scripts include timing code (`t0 <- Sys.time()`)
- **Memory requirements**: Raster operations require substantial RAM
- **API credentials**: ERA5 downloads require valid CDS API credentials
- **GPS imputation**: Always run GPS imputation scripts before household-level extractions
- **Path management**: Use `here()` package for cross-platform path compatibility
- **Kelvin to Celsius**: ERA5 temperature data downloaded in Kelvin, converted to Celsius by subtracting 273.15

## Variable Naming Conventions

Household dataset variables follow patterns:
- `[metric]_[aggregation]_[timeperiod]`: e.g., `temp_shockdays_summer_pre`
- `mean` replaces `avg` in final datasets
- `pre` replaces `pretreatment` for Stata compatibility
- `wdays30` = winter days above 30°C
- `utci_heatstress` = extreme heat stress days
- Population-weighted variables have `_pop` suffix
- Cropland-weighted variables have `_crop` suffix

## Documentation Files

- `workdocumentation.tex/.pdf`: LaTeX documentation of work done, metrics used, and data considerations
- `GPS_Imputation_Report.Rmd/.html`: Detailed report on GPS imputation methodology and validation
- `README.md`: Brief project description
