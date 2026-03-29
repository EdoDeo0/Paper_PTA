# Paper_PTA — Environmental Provisions in Chinese PTAs

**Author:** Edoardo Vitella  
**Affiliation:** PhD Student, University of Trento & Free University of Bozen

![](https://img.shields.io/badge/status-ongoing-yellow)

> **Disclaimer:** This is an ongoing project. The README is updated frequently, but may not always fully reflect the current state of all analyses, data, and scripts. If you have any questions or need clarifications, feel free to contact me directly.

---

## Table of Contents

- [Paper\_PTA — Environmental Provisions in Chinese PTAs](#paper_pta--environmental-provisions-in-chinese-ptas)
  - [Table of Contents](#table-of-contents)
  - [1. Project Description](#1-project-description)
  - [2. Repository Structure](#2-repository-structure)
  - [3. Data Sources](#3-data-sources)
    - [3.1 World Bank Deep Trade Agreements (DTA) Database](#31-world-bank-deep-trade-agreements-dta-database)
    - [3.2 TREND Database (2022)](#32-trend-database-2022)
    - [3.3 Chinese Customs Data](#33-chinese-customs-data)
    - [3.4 OECD Environmental Goods](#34-oecd-environmental-goods)
    - [3.5 Country Codes](#35-country-codes)
  - [4. Software Requirements](#4-software-requirements)
    - [R (≥ 4.0)](#r--40)
    - [Stata](#stata)
  - [5. Building the Final Dataset — Step by Step](#5-building-the-final-dataset--step-by-step)
    - [Step 0 — Convert WB Data to Stata Format *(one-time)*](#step-0--convert-wb-data-to-stata-format-one-time)
    - [Step 1 — Build the EP Indices Dataset (R)](#step-1--build-the-ep-indices-dataset-r)
      - [Internal Step 1.1 — Extract WB Data](#internal-step-11--extract-wb-data)
      - [Internal Step 1.2 — Extract TREND Data](#internal-step-12--extract-trend-data)
      - [Internal Step 1.3 — Merge and Build Indices](#internal-step-13--merge-and-build-indices)
    - [Step 2 — Build the Full Analysis Dataset (Stata)](#step-2--build-the-full-analysis-dataset-stata)
    - [Step 3 — Convert to FST Format (R)](#step-3--convert-to-fst-format-r)
  - [6. Analysis Scripts](#6-analysis-scripts)
    - [6.1 Main OLS / HDFE Estimation (R)](#61-main-ols--hdfe-estimation-r)
    - [6.2 Main PPML Estimation (R)](#62-main-ppml-estimation-r)
    - [6.3 CEM-Based Estimation](#63-cem-based-estimation)
    - [6.4 Zero Fill-In PPML Specifications](#64-zero-fill-in-ppml-specifications)
  - [7. Outputs](#7-outputs)
    - [`Output/Analysis/OLS/`](#outputanalysisols)
    - [`Output/Analysis/PPML/`](#outputanalysisppml)
    - [`Output/Analysis/CEM/`](#outputanalysiscem)
    - [`Output/CEM/`](#outputcem)
  - [8. Exploratory / Supporting Scripts](#8-exploratory--supporting-scripts)
  - [9. Important Notes](#9-important-notes)
    - [Data not included in the repository](#data-not-included-in-the-repository)
    - [Paths](#paths)
    - [Agreements included (China, 2000–2015)](#agreements-included-china-20002015)

---

## 1. Project Description

This project studies the effect of **Environmental Provisions (EPs) in Preferential Trade Agreements (PTAs)** signed by China on Chinese trade flows. The analysis covers the period **2000–2015** and focuses on how the depth and composition of environmental clauses in PTAs affect both the volume and composition of trade (broken down between environmental goods and non-environmental goods).

The empirical strategy includes:
- **OLS with High-Dimensional Fixed Effects** (`reghdfe` in Stata), with firm-product-destination (FPD) and year fixed effects, clustering at the PDT level.
- **PPML estimation** (`fepois` in R via `fixest`), same specification, to account for zero trade flows.
<!-- - **Callaway & Sant'Anna (2021) staggered DiD** (`did` in R), exploiting the staggered timing of Chinese PTA entries into force. -->

The key treatment variables are indices of EP breadth/depth constructed from two complementary databases: the **World Bank Deep Trade Agreements (DTA)** database and the **TREND** database.

---

## 2. Repository Structure

```
Paper_PTA/
├── Code/
│   ├── Dataset_Creation/                       # Dataset construction pipeline
│   │   ├── 1_Build_Final_PTA_EP_Dataset.R      # Main pipeline (indices construction)
│   │   ├── 2_Build_Final_PTA_EP_Dataset.do     # Merge with Chinese customs data (Stata)
│   │   └── 3_Build_Final_PTA_EP_Dataset.R      # Convert/compress to .fst format
│   ├── Analysis/                               # Regression scripts
│   │   ├── pta_functions.R                     # Shared functions for OLS/PPML workflows
│   │   ├── OLS_HDFE.R                          # Main OLS/HDFE estimation (R/fixest)
│   │   ├── PPML.R                              # Main PPML estimation (R/fixest)
│   │   ├── CEM.R                               # CEM construction and diagnostics
│   │   ├── OLS_CEM.R                           # OLS on CEM sample
│   │   ├── PPML_CEM.R                          # PPML on CEM sample
│   │   ├── Zero_Fill_In.R                      # Aggregated PPML with zero fill-in
│   │   ├── ZFI_Green_Goods.R                   # Firm-level zero fill-in for green goods
│   │   ├── Reg_PTA_04_02_26.do                 # Legacy OLS regressions (Stata)
│   │   ├── Callaway_Sant'Anna.R                # Staggered DiD (exploratory)
│   │   └── Callaway_SantAnna_v2.R              # Staggered DiD (updated version)
│   ├── TREND/
│   │   └── Inspecting TREND Dataset.r          # Exploratory analysis of TREND data
│   └── WB/
│       ├── Inspecting WB Database.R            # Exploratory analysis of WB data
│       └── WB_Dataset_Conversion.do            # Convert WB Excel → .dta (run once)
│
├── Data/
│   ├── Country_Codes_Custom_Data.csv           # Country codes for Chinese customs data
│   ├── Env_Codes_HS.dta                        # OECD environmental goods codes (HS6)
│   ├── Final Dataset/
│   │   └── final_dataset_pta_env_indices_compressed.fst
│   ├── Matching/
│   │   ├── BACI_HS92_Y2000_V202601.csv
│   │   ├── country_codes_V202601.csv
│   │   ├── product_codes_HS92_V202601.csv
│   │   ├── mfn_tariffs_2000.csv
│   │   ├── wdi_data.csv
│   │   ├── CEM_full/data_cem_matched_full.fst
│   │   └── CEM_no_asia/data_cem_matched_no_asia.fst
│   ├── Merged/                                 # Output of the R pipeline
│   │   ├── Merged_TREND_WB_FULL_NAMES.csv      # Merged dataset (full variable names)
│   │   ├── Merged_TREND_WB.csv / .dta          # Merged dataset (short-coded variables)
│   │   └── Merged_TREND_WB_Indices_Only.csv / .dta  # Indices only (for Stata merge)
│   ├── TREND/
│   │   ├── trend2022.csv                       # TREND database (raw)
│   │   ├── TREND_2022_Description.csv          # Variable descriptions for TREND
│   │   ├── TREND_China_2000_2015.csv           # TREND filtered for China, 2000–2015
│   │   ├── TREND_Variable_Mapping.csv          # Short-code → full name mapping
│   │   └── codebook_trend_2.docx               # TREND codebook
│   └── WB/
│       ├── WB_DTA.dta                          # WB data converted to Stata format
│       ├── DTA 2.0 - Vertical Content (v2).xlsx    # WB raw data (agreement-level)
│       ├── DTA 1.0 - Horizontal Content (v2).xlsx  # WB horizontal content (WTO-X)
│       ├── DTA 2.0 - 18. Environmental Laws (v2).xlsx  # WB environmental laws sheet
│       ├── WB_China_2000_2015.csv              # WB filtered for China, 2000–2015
│       ├── WB_DTA_ENV_CHINA_2000_2015.csv
│       ├── WB_DTA_ENV_CHINA_2000_2015.RData
│       └── WB_Variable_Mapping.csv             # Short-code → full name mapping
│
└── Output/
    ├── Analysis/
    │   ├── OLS/                                # Main OLS (R)
    │   │   ├── Models_Output/
    │   │   └── Tables/
    │   ├── PPML/                               # Main PPML (R)
    │   │   ├── Models_Output/
    │   │   └── Tables/
    │   └── CEM/                                # OLS/PPML on matched samples
    │       ├── OLS/
    │       │   ├── Models_Output/
    │       │   └── Tables/
    │       └── PPML/
    │           ├── Models_Output/
    │           └── Tables/
    ├── CEM/                                    # Matching diagnostics
    └── WB/                                     # Graphs on WB data
```

---

## 3. Data Sources

### 3.1 World Bank Deep Trade Agreements (DTA) Database
- **Description:** Codes the content of PTAs across many policy areas. The relevant sheet is *"Environmental Laws"* under the vertical content module.
- **URL:** https://datatopics.worldbank.org/dta/table.html
- **Files needed:**
  - `DTA 2.0 - Vertical Content (v2).xlsx` — provision-level coding (one row per provision, one column per agreement).
  - `DTA 1.0 - Horizontal Content (v2).xlsx` — agreement-level characteristics, including the `EnvironmentalLaws` dummy from the WTO-X sheets (`WTO-X AC` and `WTO-X LE`).
- **Pre-processing:** Must be converted to `.dta` format via `Code/WB/WB_Dataset_Conversion.do` before running the R pipeline.

### 3.2 TREND Database (2022)
- **Description:** Trade and Environmental Database. Codes environmental provisions in 775 PTAs.
- **URL:** https://www.chaire-epi.ulaval.ca/en/trend
- **Files needed:**
  - `Data/TREND/trend2022.csv` — main database (semicolon-separated).
  - `Data/TREND/TREND_2022_Description.csv` — variable descriptions (used to attach labels in R).

### 3.3 Chinese Customs Data
- **Description:** Transaction-level Chinese export data. Unit of observation: HS6 product × destination country × year × firm.
- **Location:** **Not included in the repository** (file too large). Must be obtained separately and placed at the path specified in `2_Build_Final_PTA_EP_Dataset.do`.
- **File name:** `final_dataset_pta.dta`

### 3.4 OECD Environmental Goods
- **Description:** Combined List of Environmental Goods (CLEG) coded at the HS6 level.
- **File:** `Data/Env_Codes_HS.dta` — manually created from OECD data. Contains variable `env_good` (binary indicator).

### 3.5 Country Codes
- **File:** `Data/Country_Codes_Custom_Data.csv` — maps country names to the numeric country codes used in the Chinese customs dataset.

---

## 4. Software Requirements

### R (≥ 4.0)
The following packages are required:

| Package | Used in |
|---|---|
| `haven` | Reading/writing `.dta` files |
| `readxl` | Reading Excel files |
| `dplyr`, `tidyr`, `tidyverse` | Data manipulation |
| `labelled` | Attaching variable labels |
| `reshape2` | Data reshaping |
| `here` | Portable file paths |
| `fst` | Fast serialization of large datasets |
| `fixest` | PPML estimation (`fepois`) |
| `data.table` | Fast data loading |
| `did` | Callaway & Sant'Anna DiD estimator |
| `lubridate` | Date utilities |
| `ggplot2` | Graphs |
| `janitor` | Data cleaning |
| `stargazer` | Summary statistics |

Install all at once:
```r
install.packages(c(
  "haven", "readxl", "dplyr", "tidyr", "tidyverse", "labelled",
  "reshape2", "here", "fst", "fixest", "data.table", "did",
  "lubridate", "ggplot2", "janitor", "stargazer"
))
```

### Stata
The following user-written Stata packages are required:

| Package | Used in |
|---|---|
| `reghdfe` | OLS with high-dimensional fixed effects |
| `estout` / `esttab` | Regression tables in LaTeX format |
| `regsave` | Saving regression results as `.dta` |

Install from SSC:
```stata
ssc install reghdfe
ssc install estout
ssc install regsave
```

---

## 5. Building the Final Dataset — Step by Step

The full data pipeline proceeds in four sequential steps. Scripts must be run **in order**.

```
Step 0 (Stata, once)     →  Step 1 (R)     →  Step 2 (Stata)     →  Step 3 (R)
WB_Dataset_Conversion.do    1_Build_...R       2_Build_...do          3_FST_...R
         ↓                        ↓                   ↓                    ↓
     WB_DTA.dta         Merged_TREND_WB_      final_dataset_pta_   final_dataset_...
                         Indices_Only.dta      env_indices_         .fst
                                               compressed.dta
```

---

### Step 0 — Convert WB Data to Stata Format *(one-time)*

**Script:** `Code/WB/WB_Dataset_Conversion.do`

This script imports the WB raw Excel file and saves it as a `.dta` file. It only needs to be run once.

1. Open the script in Stata.
2. Update the file paths to point to your local copies of the WB Excel files.
3. Run the script. Output: `Data/WB/WB_DTA.dta`.

---

### Step 1 — Build the EP Indices Dataset (R)

**Script:** `Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R`

This is the **main R pipeline**. It processes both databases, merges them, and constructs all Environmental Provision indices. It is structured in three internal steps:

#### Internal Step 1.1 — Extract WB Data
- Loads `Data/WB/WB_DTA.dta` and `DTA 2.0 - Vertical Content (v2).xlsx`.
- Attaches variable labels (agreement name + entry year) to each agreement column.
- Filters to **Environmental Laws** provisions only.
- Keeps only agreements **involving China** and with entry into force between **2000 and 2015**.
- Removes incorrectly matched agreements (`agree_220`, `agree_190`, `agree_253`).
- **Output:** `Data/WB/WB_China_2000_2015.csv`

#### Internal Step 1.2 — Extract TREND Data
- Loads `Data/TREND/trend2022.csv` and `Data/TREND/TREND_2022_Description.csv`.
- Filters to agreements **involving China** with year between **2000 and 2015**.
- Manually adds the Bangkok Agreement (`100_Bangkok Agreement_1975`) and APTA (`62_Asia Pacific Trade Agreement (Bangkok Agreement amended)_2005`), which are not explicitly labeled as "China" agreements in the raw data but include China.
- Removes service-sector-only agreements (China–Pakistan Services, ASEAN–China Services).
- **Output:** `Data/TREND/TREND_China_2000_2015.csv`

#### Internal Step 1.3 — Merge and Build Indices

**WB processing:**
- Reshapes the WB dataset from wide (provisions as rows, agreements as columns) to long: one row per agreement, one column per provision.
- Joins WTO-X horizontal dummies (`Env_Laws_AC`, `Env_Laws_LE`) from `DTA 1.0 - Horizontal Content (v2).xlsx`.
- Manually assigns entry years and partner countries to each agreement.
- **Expands** the dataset to country × year panel (from entry year up to 2015). If a country has multiple agreements, takes the **maximum value** across agreements for each provision.

**TREND processing:**
- Assigns entry years and partner countries to each TREND agreement.
- **Expands** to country × year panel, taking the maximum value across agreements.

**Merge:**
- Inner join on `Country` and `Year`.
- Adds numeric country codes from `Data/Country_Codes_Custom_Data.csv`.
- Drops administrative columns (`Trade.Agreement`, `Number`, `US_Partners`, `EC_Partners`, `HBTypeCode`, `Language`).
- Creates variable name mapping files (`Data/WB/WB_Variable_Mapping.csv`, `Data/TREND/TREND_Variable_Mapping.csv`) and renames variables to short codes (`WB_1`…`WB_N`, `X_…`).

**Index construction** (see [Section 6](#6-environmental-provisions-indices) for details):
- Builds all TREND-based, WB-based, and normalized indices.

**Outputs:**
| File | Description |
|---|---|
| `Data/Merged/Merged_TREND_WB_FULL_NAMES.csv` | Merged dataset with full variable names |
| `Data/Merged/Merged_TREND_WB.csv` | Merged dataset with short-coded variable names |
| `Data/Merged/Merged_TREND_WB.dta` | Same, in Stata format |
| `Data/Merged/Merged_TREND_WB_Indices_Only.csv` | Indices only (country_code, year, all indices) |
| `Data/Merged/Merged_TREND_WB_Indices_Only.dta` | Same, in Stata format — **used in Step 2** |

**How to run:**
```r
# Set working directory to the repository root
setwd("/path/to/Paper_PTA")

# Run the script
source("Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R")
```

---

### Step 2 — Build the Full Analysis Dataset (Stata)

**Script:** `Code/Dataset_Creation/2_Build_Final_PTA_EP_Dataset.do`

This script merges the EP indices constructed in Step 1 with the large Chinese customs dataset and with OECD environmental goods codes.

**Prerequisites:**
- `final_dataset_pta.dta` — Chinese customs data (not in repository).
- `Data/Merged/Merged_TREND_WB_Indices_Only.dta` — output of Step 1.
- `Data/Env_Codes_HS.dta` — OECD environmental goods codes.

**What it does:**

1. **Loads** Chinese customs data (`final_dataset_pta.dta`).
2. **Merges EP indices** (`m:1` merge on `country_code` × `year`). Country × year pairs without an active Chinese PTA remain unmatched — this is expected and correct.
3. **Merges environmental goods codes** (`m:1` merge on `hs6`). Unmatched observations from master are goods not classified as environmental, unmatched from using are environmental HS6 codes not present in the sample.
4. **Generates analysis variables:**
   - `ln_export = ln(export)` — log export value
   - `tariffs = ln(1 + duty)` — log MFN tariff rate
   - `env_good = 0` if missing (non-environmental goods)
   - `ln_export_qua = ln(exp_qua)` — log export quantity
   - `ln_export_value = ln(uv_exp)` — log unit value
   - `pdt = group(hs6, country_code, year)` — numeric long ID
   - Sets `WB_EP_Depth = 0` and `TREND_EP_Count = 0` when missing (no active PTA)
5. **Compresses** and saves the final dataset.

**Paths to update before running:**
```stata
cd "C:\path\to\chinese_customs\data"    // where final_dataset_pta.dta is located
use final_dataset_pta.dta, clear

merge m:1 country_code year using "C:\path\to\Paper_PTA\Data\Merged\Merged_TREND_WB_Indices_Only.dta"
merge m:1 hs6 using "C:\path\to\Paper_PTA\Data\Env_Codes_HS.dta"

save "C:\path\to\output\final_dataset_pta_env_indices_compressed.dta", replace
```

**Output:** `final_dataset_pta_env_indices_compressed.dta`  
*(not in repository — very large file)*

---

### Step 3 — Convert to FST Format (R)

**Script:** `Code/Dataset_Creation/3_Build_Final_PTA_EP_Dataset.R`

Converts the compressed Stata dataset to the `.fst` format for fast column-selective loading in R (used by the PPML and DiD scripts).

```r
library(haven)
library(fst)

df <- read_dta("Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta")
write_fst(df, "Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
```

Update the paths to match your local setup before running.

**Output:** `final_dataset_pta_env_indices_compressed.fst`  
*(not in repository — very large file)*

<!-- --- -->

<!-- ## 6. Environmental Provisions Indices

All indices are computed inside `Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R` and are available in `Data/Merged/Merged_TREND_WB_Indices_Only.dta`.

### TREND-based Indices

| Variable | Description |
|---|---|
| `TREND_EP_Count` | Total count of all environmental provisions (breadth) |
| `TREND_EP_Count_Binary` | Count of provisions present (binary version) |
| `TREND_Soft` | Soft provisions (general principles, vague cooperation, non-binding obligations) |
| `TREND_Hard` | Hard provisions (standards, enforcement, dispute settlement, climate obligations) |
| `TREND_Hardness_Share` | Share of hard provisions over total |
| `TREND_EnforcementDSM` | Enforcement and dispute settlement provisions (sections X5, X11, X12, X13) |
| `TREND_RegulatorySpace` | Regulatory space, sovereignty, and exceptions (sections X1.07–X1.09, X8) |
| `TREND_GreenMarketAccess` | Green goods and services market access (X7.01, X8.09.04) |
| `TREND_ClimateEnergy` | Climate change and energy-environment provisions (X4.03, X10) |
| `TREND_BiodivForestsFisheries` | Biodiversity, forests, and fisheries provisions (X1.07.02, X1.07.03, X11) |
| `TREND_Hardness_Share_v2` | Alternative hardness classification (includes enforcement in hard provisions) |

### WB-based Indices

| Variable | Description |
|---|---|
| `WB_EP_Depth` | Overall depth: sum of all WB environmental provision dummies |
| `WB_EP_Depth_Binary` | Binary version |
| `WB_StandardsNonRegression` | High standards and non-regression provisions (WB_2, WB_8, WB_9) |
| `WB_EnforcementDSM` | Enforcement and dispute settlement (WB_13–WB_16) |
| `WB_RegulatorySpaceExceptions` | Right-to-regulate and general exceptions (WB_5, WB_6, WB_7) |
| `WB_GreenLiberalization` | Differential liberalization of environmental goods (WB_10) |
| `WB_Assistance` | Technical/financial assistance and capacity building (WB_17) |
| `WB_Hardness_Share` | Share of standards + non-regression provisions over total depth |
| `WB_Hardness_Share_v2` | Alternative: includes enforcement provisions in hard category |

### Normalized Comparison Indices

| Variable | Description |
|---|---|
| `TREND_Depth_Norm` | TREND_EP_Count normalized by available provisions |
| `WB_Depth_Norm` | WB_EP_Depth normalized by available provisions |
| `TREND_Enforcement_Share` | Share of enforcement provisions (TREND) |
| `WB_Enforcement_Share` | Share of enforcement provisions (WB) |
| `TREND_RegSpace_Share` | Share of regulatory space provisions (TREND) |
| `WB_RegSpace_Share` | Share of regulatory space provisions (WB) |
| `TREND_GreenLib_Share` | Share of green liberalization provisions (TREND) |
| `WB_GreenLib_Share` | Share of green liberalization provisions (WB) |

> **Variable mapping:** To recover full provision names from short codes (e.g., `WB_13`), consult `Data/WB/WB_Variable_Mapping.csv` and `Data/TREND/TREND_Variable_Mapping.csv`.

--- -->

---

## 6. Analysis Scripts

### 6.1 Main OLS / HDFE Estimation (R)

**Script:** `Code/Analysis/OLS_HDFE.R`

Main OLS workflow implemented with `fixest::feols` (high-dimensional FE), reusing common utilities from `Code/Analysis/pta_functions.R`.

Baseline reference specification:

$$\ln Y_{fpdt} = \beta \cdot \text{EPDepth}_{dt} + \theta_{fpd} + \theta_t + \varepsilon_{fpdt}$$

where $f$ = firm, $p$ = HS6 product, $d$ = destination country, $t$ = year.

The script estimates WB and TREND blocks with/without `env_good` interactions and with/without controls across four FE structures:
- `fpd + year` (cluster `pdt`)
- `fpd + pt` (cluster `dt`)
- `fpt + pd` (cluster `dt`)
- `fpt + fpd` (cluster `dt`)

Outputs:
- LaTeX tables in `Output/Analysis/OLS/Tables/`
- `.rds` model objects in `Output/Analysis/OLS/Models_Output/`

---

### 6.2 Main PPML Estimation (R)

**Script:** `Code/Analysis/PPML.R`

Main PPML workflow with `fixest::fepois`, parallel in structure to `OLS_HDFE.R`.

Reference multiplicative form:

$$\mathbb{E}[Y_{fpdt} \mid X] = \exp\left(\beta \cdot \text{EPDepth}_{dt} + \theta_{FE} + X'\gamma\right)$$

Implemented blocks:
- WB depth and TREND depth
- with/without `env_good` interaction
- with/without controls (`tariffs`, `ln_hhi_baci`)
- four FE structures (`fpd+year`, `fpd+pt`, `fpt+pd`, `fpt+fpd`)

Outputs:
- LaTeX tables in `Output/Analysis/PPML/Tables/`
- `.rds` models in `Output/Analysis/PPML/Models_Output/`

### 6.3 CEM-Based Estimation

- `Code/Analysis/CEM.R`: builds matched samples and diagnostics (balance table, love plot, summary files).
- `Code/Analysis/OLS_CEM.R`: applies the OLS workflow to CEM-matched data.
- `Code/Analysis/PPML_CEM.R`: applies the PPML workflow to CEM-matched data.

Main CEM diagnostics outputs are generated under `Output/CEM/`. Estimation outputs are generated under `Output/Analysis/CEM/`.

---

### 6.4 Zero Fill-In PPML Specifications

- `Code/Analysis/Zero_Fill_In.R`:
  - aggregates to product-destination-year level;
  - fills sampling zeros for active product-destination pairs;
  - estimates PPML with FE suited to aggregated structure.

- `Code/Analysis/ZFI_Green_Goods.R`:
  - firm-level zero fill-in restricted to green goods;
  - explicit covariate lookup by dimensionality (`d,t` or `p,d,t`);
  - PPML estimation on the resulting panel.

These scripts are robustness checks relative to the main firm-level PPML specification.

---

## 7. Outputs

### `Output/Analysis/OLS/`

| Directory | Contents |
|---|---|
| `Tables/` | OLS regression tables by FE specification and index:<br/>- `OLS_WB_No_Interaction_fpd_pt.tex`, `OLS_WB_Interaction_fpd_pt.tex`<br/>- `OLS_WB_No_Interaction_fpd_year.tex`, `OLS_WB_Interaction_fpd_year.tex`<br/>- `OLS_WB_No_Interaction_fpt_fpd.tex`, `OLS_WB_Interaction_fpt_fpd.tex`<br/>- `OLS_WB_No_Interaction_fpt_pd.tex`, `OLS_WB_Interaction_fpt_pd.tex`<br/>- `OLS_TREND_No_Interaction_*.tex`, `OLS_TREND_Interaction_*.tex` (all FE combinations) |
| `Models_Output/` | OLS model `.rds` files with full specification details:<br/>- `OLS_WB_No_Interaction_(fpd_+_year_FE)_1-6.rds` — WB depth, no interaction, 6 control variants<br/>- `OLS_WB_Interaction_*.rds` — WB depth × env_good interaction<br/>- `OLS_TREND_*.rds` — TREND depth (No_Interaction and Interaction variants)<br/>- Multiple FE combinations: `fpd_+_year`, `fpd_+_pt`, `fpt_+_pd`, `fpt_+_fpd`, etc. |

### `Output/Analysis/PPML/`

| Directory | Contents |
|---|---|
| `Tables/` | PPML regression tables by FE specification and index:<br/>- `PPML_WB_No_Interaction_fpd_pt.tex`, `PPML_WB_Interaction_fpd_pt.tex`<br/>- `PPML_WB_No_Interaction_fpd_year.tex`, `PPML_WB_Interaction_fpd_year.tex`<br/>- `PPML_WB_No_Interaction_fpt_fpd.tex`, `PPML_WB_Interaction_fpt_fpd.tex`<br/>- `PPML_WB_No_Interaction_fpt_pd.tex`, `PPML_WB_Interaction_fpt_pd.tex`<br/>- `PPML_TREND_No_Interaction_*.tex`, `PPML_TREND_Interaction_*.tex` (all FE combinations) |
| `Models_Output/` | PPML model `.rds` files with full specification details:<br/>- `PPML_WB_No_Interaction_(fpd_+_year_FE)_1-6.rds` — WB depth, no interaction, 6 control variants<br/>- `PPML_WB_Interaction_*.rds` — WB depth × env_good interaction<br/>- `PPML_TREND_*.rds` — TREND depth (No_Interaction and Interaction variants)<br/>- Multiple FE combinations: `fpd_+_year`, `fpd_+_pt`, `fpt_+_pd`, `fpt_+_fpd`, `firm-product-time`, etc. |

### `Output/Analysis/CEM/`

| Directory | Contents |
|---|---|
| `OLS/Tables/` | CEM OLS LaTeX tables (`CEM_OLS_*.tex`) |
| `OLS/Models_Output/` | CEM OLS model `.rds` files |
| `PPML/Tables/` | CEM PPML LaTeX tables (`CEM_PPML_*.tex`) |
| `PPML/Models_Output/` | CEM PPML model `.rds` files |

### `Output/CEM/`

Matching diagnostics and supporting files generated by `Code/Analysis/CEM.R`:

| File | Description |
|---|---|
| `CEM_Summary.txt` | Matching summary |
| `CEM_Balance_Table.tex` | Balance table |
| `CEM_LovePlot.pdf/.png` | Love plot |
| `CEM_Covariate_Diagnostics.pdf/.png` | Distribution diagnostics |
| `matched_countries.csv` | Treated/control matched countries |

---

## 8. Exploratory / Supporting Scripts

| Script | Description |
|---|---|
| `Code/TREND/Inspecting TREND Dataset.r` | Initial exploration of the TREND database: filtering, labeling, and saving the China 2000–2015 subset |
| `Code/WB/Inspecting WB Database.R` | Initial exploration of the WB DTA database: filtering environmental laws provisions for China 2000–2015 |
| `Code/WB/WB_Dataset_Conversion.do` | Converts WB Excel file to `.dta` format (run once before Step 1) |
| `Code/Analysis/pta_functions.R` | Shared utility library used by OLS/PPML scripts (`run_block`, table generation, selective loading) |

---

## 9. Important Notes

### Data not included in the repository
- **Chinese customs data** (`final_dataset_pta.dta` / `final_dataset_pta_env_indices_compressed.dta/.fst`): The raw and processed transaction-level datasets are not pushed to GitHub due to their large size. These must be obtained separately and stored locally.
- The `Data/WB/WB_DTA.dta` file generated by Step 0 may also be large; if not present, re-run `WB_Dataset_Conversion.do`.

### Paths
All scripts contain hardcoded absolute paths that must be updated before running on a different machine. Paths to update are marked with comments in each script. When running R scripts, set the working directory to the **repository root** (`Paper_PTA/`) so that relative paths (e.g., `Data/Merged/...`) resolve correctly.

### Agreements included (China, 2000–2015)
The analysis covers the following PTAs signed by China that entered into force between 2000 and 2015:

| Partner(s) | Entry into Force |
|---|---|
| ASEAN members (Brunei, Cambodia, Indonesia, Laos, Malaysia, Myanmar, Philippines, Singapore, Thailand, East Timor, Vietnam) | 2005 |
| Bangladesh, India, Korea Rep., Laos, Sri Lanka (Bangkok Agreement / APTA) | 2002 / 2005 |
| Australia | 2015 |
| Chile | 2006 |
| Costa Rica | 2011 |
| Hong Kong | 2003 |
| Iceland | 2014 |
| Korea Rep. | 2015 |
| Macau | 2003 |
| New Zealand | 2008 |
| Pakistan | 2007 |
| Peru | 2010 |
| Singapore | 2009 |
| Switzerland | 2014 |

<!-- ### Variable naming convention
- WB provision variables are renamed to `WB_1`, `WB_2`, …, `WB_N` in the merged dataset. The mapping to full provision names is in `Data/WB/WB_Variable_Mapping.csv`.
- TREND provision variables retain their original `X` codes (e.g., `X2_01_01`) with dots replaced by underscores. The mapping is in `Data/TREND/TREND_Variable_Mapping.csv`.
- The full-name version of the merged dataset (before renaming) is available in `Data/Merged/Merged_TREND_WB_FULL_NAMES.csv`. -->
