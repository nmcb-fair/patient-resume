# Automated Patient Resume Generation

This project automates the generation of patient summary Excel files from Castor EDC data for the NMCB study (ME/CFS screening).

## Project Structure

The project is organized into modular R scripts:

- `**01_load_libraries.R**` - Loads required R packages (readr, openxlsx)
- `**02_load_data.R**` - Loads CSV data from Castor exports
- `**03_mapping.R**` - Defines the variable-to-cell mapping configuration
- `**04_helper_functions.R**` - Helper functions for Excel operations and data transformations
- `**05_export_function.R**` - Main export function that generates Excel files
- `**run_all.R**` - Main script that sources all modules and runs the export

## Quick Start

### Option 1: Run everything with default settings

```r
source("run_all.R")
```

### Option 2: Customize and run

Edit `run_all.R` to modify:

- Participant IDs to process
- Template path
- Output directory
- Sheet name

Then run:

```r
source("run_all.R")
```

### Option 3: Run modules individually

```r
source("01_load_libraries.R")
source("02_load_data.R")
source("03_mapping.R")
source("04_helper_functions.R")
source("05_export_function.R")

# Then call the function with your parameters
export_participants_resume(
  df = df_me_cfs,
  ids = c("1000175", "0000005"),
  map = map_resume,
  template_path = "template/template.xlsx",
  sheet = "Resumé"
)
```

## Input Files

- **Main screening data**: latest `NMCB_Study_ME_CFS_Screener_*export_*.csv/.xlsx` in `input/screening/` (df_me_cfs)
- **Visit data**: latest `NMCB_Study_*export_*.csv/.xlsx` in `input/visit/` (df_visit) – hgs average → D26 and `visit_meds`* medication fields
- **Measurements/Questionnaires (legacy)**: latest `NMCB_Measurements_Questionnaires_*export_*.csv/.xlsx` in `input/visit/` or `input/` (df_measurements) – fallback source for older participants (including medication fields)
- **DSQ2 symptoms**: latest `NMCB_Study_Symptomen_frequentie_en_ernst_hiervan_*export_*.csv/.xlsx` in `input/dsq_2/` (df_dsq2) – Survey Progress → D30
- **Vragenlijsten (optional supplemental Castor source)**: latest `NMCB_Study_Vragenlijsten_*export_*.csv/.xlsx` in `input/vragenlijsten/` (df_vragenlijsten)
- **CRL admin**: `input/CRL admin/CRL_Admin.xlsx` (df_crl_admin) – Patient type → D32
- **NASA Lean test**: `input/Omron/NASA_LEAN_TEST.csv` (df_nasa_lean) – rows copied to "NASA test" sheet
- **CDL alert**: `input/CDL_alert/` (and subfolders) – CSV files whose filename contains the participant ID are copied to the "Laboratorium" sheet
- **Template**: `template/template.xlsx`

## Output

Excel files are generated in the `exports_resume/` directory with the naming pattern:
`Participant_<ID>.xlsx`

## How It Works

This project is an offline R pipeline that turns exported study data into one Excel resume per participant.

The main script is `run_all.R`. It loads all modules in order, defines which participant IDs to process, and calls `export_participants_resume()`.

### Data Flow

1. `01_load_libraries.R` loads the required R packages: `readr` and `openxlsx`.
2. `02_load_data.R` reads the Castor exports and supporting files from `input/`.
3. `03_mapping.R` defines `map_resume`, which maps source variables to Excel cells or output sheets.
4. `04_helper_functions.R` provides reusable helpers for:
  - Excel cell parsing (`C4` -> row/column)
  - value conversion (`1/0` -> `ja/nee`, sex code -> `vrouw/man`)
  - diagnosis text extraction
5. `05_export_function.R` loops over participant IDs and, for each participant:
  - loads the Excel template
  - copies mapped values into the main `Resumé` sheet
  - builds diagnosis, illness, and medication lists
  - fills additional fields from other datasets
  - writes extra sheets such as `NASA test` and `Laboratorium`
  - saves the result as `Participant_<ID>.xlsx`

### External Data Sources Used During Export

- `df_me_cfs`: main screener data used for most direct mappings
- `df_visit`: hand grip strength values for `D26`, plus `visit_meds01`/`visit_meds02` medication fields
- `df_measurements`: legacy fallback source for older participants when values are missing in visit exports
- `df_dsq2`: survey progress used for `D30`
- `df_vragenlijsten`: optional supplemental Castor source used when mapped values are missing in primary data
- `df_crl_admin`: patient type used for `D32`
- `df_nasa_lean`: participant rows copied to the `NASA test` sheet, plus pulse-derived summary values
- `input/CDL_alert/`: matching lab CSV copied to the `Laboratorium` sheet, also used to derive the lab priority flag

## File-by-File Walkthrough

### `01_load_libraries.R`

Loads the only two package dependencies used by the pipeline:

- `readr` for CSV/delimited input
- `openxlsx` for reading and writing Excel workbooks

### `02_load_data.R`

Reads all input files into data frames. This is the place to update when export filenames or dates change.

It loads:

- the main ME/CFS screener export
- the visit export
- the measurements/questionnaires export
- the DSQ2 symptom export
- the CRL admin workbook
- the NASA Lean CSV

### `03_mapping.R`

Defines the declarative mapping table `map_resume`.

Each row describes:

- `var`: the source variable name
- `cell`: the target Excel cell or logical target sheet
- `op`: an optional transformation or special behavior

Examples of supported operations:

- direct write to a cell
- `ja_nee`
- `vrouw_man`
- `no_decimal`
- `add_to_list`
- `add_to_sheet`
- `add_text_to_sheet`

This file is the best place to look when you want to understand or change what ends up where in the output workbook.

### `04_helper_functions.R`

Contains shared utility functions used by the exporter, including:

- converting Excel column letters to numeric indices
- converting Excel cell names to row/column coordinates
- converting values into display text for the workbook
- extracting readable diagnosis or medication labels from variable names

### `05_export_function.R`

Contains the main function `export_participants_resume()`, which performs the full export.

Key responsibilities:

- normalizes source column names
- fuzzy-matches map entries to actual dataframe columns
- matches participant IDs across datasets, including IDs with or without leading zeros
- writes direct mappings into the main sheet
- creates illness and medication lists in separate sheets
- calculates extra derived fields such as grip strength averages and screening outputs
- copies NASA and laboratory data into dedicated sheets
- saves one workbook per participant

This is the core implementation file of the project.

### `run_all.R`

The orchestration script. It:

- sources files `01` through `05` in order
- defines the participant IDs to process
- sets the template path, output folder, and sheet name
- calls `export_participants_resume()`

This is the normal entry point when running the project manually.

### `generate_patient_resume.Rmd`

An older R Markdown version of the workflow kept for reference. The current modular pipeline is based on the standalone `.R` scripts above.

## Features

- **Variable normalization**: Handles quoted column names and whitespace
- **Fuzzy matching**: Automatically matches variables that don't have exact names
- **Data transformations**: 
  - 1/0 → "ja"/"nee"
  - Sex codes → "vrouw"/"man"
  - BMI rounding (no decimals)
- **Multiple output types**:
  - Direct cell mappings
  - Concatenated diagnosis lists
  - Separate sheets for Illness and Medication data
- **Additional mappings from other sources**:
  - D26: Average of hgs_01, hgs_02, hgs_03 from df_visit
  - D30: Survey Progress from df_dsq2
  - D32: Patient type from df_crl_admin
  - NASA test sheet: All rows from df_nasa_lean for the participant
  - Laboratorium sheet: Full content from matching CSV in `input/CDL_alert/` (and subfolders) when filename contains the participant ID

## Notes

- The original R Markdown file (`generate_patient_resume.Rmd`) is preserved for reference
- All modules must be sourced in order (01 → 05)
- The `run_all.R` script provides a convenient way to run everything with one command

