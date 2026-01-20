# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a climate and migration research project for the STEG (Structural Transformation and Economic Growth) initiative, analyzing "Migration in the Face of Climate Change: Assessing the Potential of Ultra-Poor Graduation Programs" in Upper Egypt (Assiut and Suhag governorates).

The project combines satellite climate data (temperature, heat indices, drought measures) with household survey data to measure climate vulnerability and stress at both governorate and household levels.

## Key Architecture Concepts

### Sequential Data Pipeline

The project follows a strict numbered workflow that should be executed in order:

1. **Raster Data Processing** (Code/1_Raster Data/): Download and combine satellite/climate rasters
2. **Governorate-Level Extraction** (Code/2_Governorate Data/): Extract climate data for governorates
3. **Household-Level Processing** (Code/3_Household Data/): Process household surveys and extract household-level climate exposure
4. **Exploratory Data Analysis** (Code/4_EDA/): Time series analysis, mapping, and visualization

### Script Numbering Convention

Scripts use numbered prefixes indicating their processing stage:

- **0_**: Exploratory/preliminary analyses (e.g., `0_HH_GPSimputation.R`, `0_shapefilecomparison.R`)
- **1_**: Data download and combination (e.g., `1_Download_ERA5_temperature.R`, `2_CombiningRasters_ERA5.R`)
- **2_**: Data extraction and processing (e.g., `2_Extraction_ERA5_Temperature.R`, `2_Extraction_NDVI_HHPanel.R`)
- **3_**: Climate shock/stress variable construction and analysis (e.g., `3_ClimateShock_Variables_HHPanel.R`, `3_Mapping_UTCI.R`)
- **4_**: Final dataset merging (e.g., `4_Merging_HHDataset.R`)
- **5_**: Indices and derived measures (e.g., `5_VulnerabilityIndices_HHPanel.R`)
- **6_**: Master sourcing scripts (e.g., `6_SourcingScripts_Households.R`)

### Household Data Master Pipeline

The household analysis uses a master sourcing script: `Code/3_Household Data/6_SourcingScripts_Households.R`

This script orchestrates the entire household data pipeline by sequentially sourcing:
1. GPS imputation (0_HH_GPSimputation.R)
2. All extraction scripts (2_Extraction_*.R)
3. Climate shock variables (3_ClimateShock_Variables_HHPanel.R)
4. Dataset merging (4_Merging_HHDataset.R)
5. Vulnerability indices (5_VulnerabilityIndices_HHPanel.R)

**Important**: When household raw data changes, run `6_SourcingScripts_Households.R` to regenerate all downstream datasets.

### Data Flow

```
Raw Climate Rasters (Data/raw/)
  → Combined/Processed Rasters (Data/intermediate/)
  → Extracted Values (household/governorate-level)
  → Climate Shock Variables
  → Final Merged Dataset (Data/final/hh_data_v*.Rds|.dta)
  → Vulnerability Indices
```

## Data Structure

### Directory Organization

- **Data/raw/**: Raw input data (not in git)
  - ERA5/: Temperature and heat index rasters
  - CHIRPS/: Precipitation rasters
  - ESI/: Evaporative Stress Index time series
  - NDVI/: Vegetation index time series
  - Household Data/: Survey data (.dta files)
  - Shapefiles/: Geographic boundaries and features
    - HDX_Egypt/: Egypt administrative boundaries (primary: `egy_admbnda_adm1_capmas_20170421.shp`)
    - hotosm_egy_waterways_lines_shp/: Waterway features

- **Data/intermediate/**: Processed intermediate datasets
  - Household Data/: Intermediate household-level extractions
  - Governorate Data/: Governorate-level climate extractions
  - ERA5/, CHIRPS/: Combined climate rasters

- **Data/final/**: Final analysis-ready datasets
  - Household datasets versioned by date (e.g., `hh_data_v09012025.Rds`, `hh_data_v09012025.dta`)

### Climate Data Sources

- **ERA5**: Temperature (min, max, mean), dewpoint, heat indices (from Copernicus Climate Data Store)
- **CHIRPS**: Precipitation (daily)
- **ESI**: Evaporative Stress Index (drought measure)
- **NDVI/EVI**: Vegetation indices (from MODIS)
- **UTCI**: Universal Thermal Climate Index (thermal comfort/heat stress)
- **Elevation/Slope**: Digital elevation models

## Key Climate Variables

### Shock Definitions

Climate "shocks" are defined as deviations from historical baseline (2000-2015):
- **Temperature shocks**: Days where temperature is ±2 SD from historical monthly mean
- **UTCI shocks**: Days where UTCI is ±2 SD from historical monthly mean
- **NDVI/EVI shocks**: Months where vegetation index is -2 SD from historical mean
- **Drought**: ESI < -1 (moderate) or < -2 (severe)
- **Heatwaves**: Days above 85th percentile of historical summer temperature

### Temporal Periods

Standard analysis periods used throughout:
- **Pre-treatment/Historical**: 2000-2019 (or 2000-2015 for baselines)
- **Treatment**: 2015-2024
- **Sub-periods**: 2015-2019, 2020-2024

### Seasons

- **Summer**: May-October (months 5-10)
- **Winter**: November-April (months 11-12, 1-4)

## Working with the Code

### R Environment Setup

All scripts follow this pattern for package management:
```r
libs <- c("terra", "tidyverse", "here", "sf", ...)
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = TRUE))
```

### Common R Packages

Core packages used across scripts:
- **Spatial**: `terra`, `sf`, `raster`, `exactextractr`
- **Data manipulation**: `tidyverse`, `naniar`, `haven`
- **Project management**: `here`
- **Visualization**: `ggplot2`, `leaflet`, `mapview`
- **Climate data**: `KrigR` (for CDS API downloads)
- **Indices**: `stdidx` (for creating composite indices via inverse covariance weighting)

### API Keys and Credentials

- **Copernicus Climate Data Store (CDS)**: API key stored in `cds_key.txt` (not in git)
- **Google Distance Matrix API**: Key stored in `google_key.txt` (not in git)

### File Path Conventions

All scripts use `here()` for project-relative paths:
```r
paste(here(), "Data", "intermediate", "file.Rds", sep = "/")
```

### Output Versioning

Final household datasets are versioned by date in filenames:
- Format: `hh_data_vDDMMYYYY.Rds` and `.dta`
- Update version date in `4_Merging_HHDataset.R` (lines 261-264) when regenerating

### Variable Naming Conventions

Climate variables follow structured naming:
- `{source}_{measure}_{season}_{period}` (e.g., `temp_shockdays_summer_pre`)
- Sources: `temp`, `utci`, `esi`, `ndvi`, `evi`
- Measures: `shockdays`, `mean`, `sd`, `n`, `growth`
- Seasons: `summer`, `winter`
- Periods: `pre` (pretreatment), `20152019`, `20202024`, `20152024`

Common abbreviations:
- `wdays30`: Winter days with max temp > 30°C
- `mdrought`/`sdrought`: Moderate/severe drought
- `utci_heatstress`: Days with UTCI > 46

## Common Tasks

### Regenerating Household Data

When raw household survey data changes:
```r
source(paste(here(), "Code", "3_Household Data",
             "6_SourcingScripts_Households.R", sep = "/"))
```

### Downloading New Climate Data

1. Set up CDS API credentials (register at Copernicus Climate Data Store)
2. Modify date ranges in `Code/1_Raster Data/1_Download_ERA5_temperature.R`
3. Note: Downloads via KrigR are slow; Google Earth Engine is faster for large downloads

### Creating Climate Shock Variables

Climate shocks are calculated in `Code/3_Household Data/3_ClimateShock_Variables_HHPanel.R`:
1. Calculate baseline statistics (2000-2015 monthly means and SDs)
2. Compute z-scores for each observation
3. Flag shocks (|z| > 2)
4. Aggregate by season and time period

### Building Vulnerability Indices

Indices use inverse covariance weighting (`stdidx::idx_invcov()`):
- Heat Stress Index: Temperature-based measures
- Combined Heat Stress: Temperature + UTCI
- Drought Stress: ESI + vegetation indices
- Combined Climate Stress: Heat + Drought

Scripts apply standard scaling (`scale()`) to all input variables before index construction.

## Data Output Formats

- **R**: `.Rds` files for intermediate processing (preserves data types)
- **Stata**: `.dta` files for final analysis datasets (includes variable labels via `labelled::set_variable_labels()`)
- **Spatial**: Shapefiles (.shp, .shx, .dbf, .prj) for geographic data
- **Rasters**: GeoTIFF (.tif) for all climate rasters

## Important Notes

- The project uses long file paths due to OneDrive directory structure; be aware of Windows path length limitations
- GPS coordinates for households may be imputed (see `0_HH_GPSimputation.R`)
- Some scripts are commented out in sourcing script (e.g., Google Distance Matrix extraction) to avoid repeated API calls
- Climate data processing is computationally intensive; intermediate datasets are saved at each major step
