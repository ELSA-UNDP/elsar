# File Naming Convention for Input and Output Datasets

## Overview
This document provides a standardised naming convention for both input and output datasets used in the ELSA (and related) analyses. The purpose of this naming convention is to ensure consistency, improve file organization, and facilitate the identification of datasets across different analyses, geographic scales, and tier of analysis.

## General Guidelines
- **All Lowercase**: File names should be in all lowercase letters.
- **Underscores for Spaces**: Use underscores (`_`) to represent spaces.
- **No Special Characters**: Avoid using hyphens, periods (except before the file extension), or other special characters in the file names.
- **Descriptive**: The file names should provide enough information to easily identify the content, geographic scope, and tier of analysis.

## Structure
The naming convention follows this general structure:

- **`<dataset_description>`**: A brief description of the dataset (e.g., `population_density`, `land_cover`), using underscores for spaces.
- **`<iso3>`**: The ISO 3166-1 alpha-3 country code, representing the country or region. For global datasets, use `global`.
- **`<tier>`**: Indicates the tier of analysis:
  - `t1` for Global
  - `t2` for National
  - `t3` for Subnational
- **`<file_extension>`**: The file type extension (e.g., `.tif`, `.csv`, `.shp`, `.gpkg`).

## Examples
Here are examples of how to name datasets following this convention:

1. **Global Analysis (Tier 1)**
   - **Input**: `population_density_global_t1.tif`
   - **Output**: `land_cover_global_t1.tif`

2. **National Analysis (Tier 2)**
   - **Input**: `population_density_dom_t2.tif` (for the Dominican Republic)
   - **Output**: `deforestation_risk_dom_t2.tif`

3. **Subnational Analysis (Tier 3)**
   - **Input**: `population_density_gha_t3.tif` (for a subnational analysis in Ghana)
   - **Output**: `protected_areas_gha_t3.shp`

## Additional Notes
- **Versioning**: If multiple versions of a dataset are needed, append a version number at the end of the dataset description (e.g., `population_density_v2_dom_t2.tif`).
- **Date Stamps**: If needed, append a date in `YYYYMMDD` format before the tier (e.g., `population_density_20230901_dom_t2.tif`).

## Folder Structures
- **TBD**

Draft