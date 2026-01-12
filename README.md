# Egypt-Climate-and-Migration
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
