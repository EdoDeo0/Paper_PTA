# Paper_PTA — Environmental Provisions in Chinese PTAs

**Author:** Edoardo Vitella  
**Affiliation:** PhD Student, University of Trento & Free University of Bozen

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
  - [6. Environmental Provisions Indices](#6-environmental-provisions-indices)
    - [TREND-based Indices](#trend-based-indices)
    - [WB-based Indices](#wb-based-indices)
    - [Normalized Comparison Indices](#normalized-comparison-indices)
  - [7. Analysis Scripts](#7-analysis-scripts)
    - [7.1 OLS with High-Dimensional Fixed Effects (Stata)](#71-ols-with-high-dimensional-fixed-effects-stata)
    - [7.2 PPML Estimation (R)](#72-ppml-estimation-r)
    - [7.3 Callaway \& Sant'Anna Staggered DiD (R)](#73-callaway--santanna-staggered-did-r)
  - [8. Outputs](#8-outputs)
    - [`Output/Analysis/`](#outputanalysis)
    - [`Output/Analysis/PPML/`](#outputanalysisppml)
    - [`Output/Riordinare/`](#outputriordinare)
  - [9. Exploratory / Supporting Scripts](#9-exploratory--supporting-scripts)
  - [10. Important Notes](#10-important-notes)
    - [Data not included in the repository](#data-not-included-in-the-repository)
    - [Paths](#paths)
    - [Agreements included (China, 2000–2015)](#agreements-included-china-20002015)
    - [Variable naming convention](#variable-naming-convention)

---

## 1. Project Description

This project studies the effect of **Environmental Provisions (EPs) in Preferential Trade Agreements (PTAs)** signed by China on Chinese trade flows. The analysis covers the period **2000–2015** and focuses on how the depth and composition of environmental clauses in PTAs affect both the volume and composition of trade (broken down between environmental goods and non-environmental goods).

The empirical strategy includes:
- **OLS with High-Dimensional Fixed Effects** (`reghdfe` in Stata), with firm-product-destination (FPD) and year fixed effects, clustering at the PDT level.
- **PPML estimation** (`fepois` in R via `fixest`), same specification, to account for zero trade flows.
- **Callaway & Sant'Anna (2021) staggered DiD** (`did` in R), exploiting the staggered timing of Chinese PTA entries into force.

The key treatment variables are indices of EP breadth/depth constructed from two complementary databases: the **World Bank Deep Trade Agreements (DTA)** database and the **TREND** database.

---

## 2. Repository Structure

```
Paper_PTA/
├── Code/
│   ├── Merge/                                  # Dataset construction pipeline
│   │   ├── 1_Build_Final_PTA_EP_Dataset.R      # Main pipeline (Steps 1–3 of dataset build)
│   │   ├── 2_Build_Final_PTA_EP_Dataset.do     # Merge with Chinese customs data (Stata)
│   │   ├── 3_FST_Conversion_Final_PTA_EP_Dataset.R  # Convert to .fst format
│   │   └── Merge_TREND_WB.R                    # Standalone merge script (legacy)
│   ├── Analysis/                               # Regression scripts
│   │   ├── Reg_PTA_04_02_26.do                 # OLS regressions (Stata)
│   │   ├── PPML_Estimation.R                   # PPML regressions (R)
│   │   └── Callaway_Sant'Anna.R                # Staggered DiD (R)
│   ├── TREND/
│   │   └── Inspecting TREND Dataset.r          # Exploratory analysis of TREND data
│   └── WB/
│       ├── Inspecting WB Database.R            # Exploratory analysis of WB data
│       ├── Graphs for presentation.R           # Graphs for presentations
│       └── WB_Dataset_Conversion.do            # Convert WB Excel → .dta (run once)
│
├── Data/
│   ├── Country_Codes_Custom_Data.csv           # Country codes for Chinese customs data
│   ├── Env_Codes_HS.dta                        # OECD environmental goods codes (HS6)
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
│       └── WB_Variable_Mapping.csv             # Short-code → full name mapping
│
└── Output/
    ├── Analysis/                               # Regression output tables
    │   ├── *.ster                              # Stata stored estimates
    │   ├── *.tex / *.txt                       # LaTeX and text regression tables
    │   ├── *.dta                               # Regression results in Stata format
    │   └── PPML/
    │       ├── Models_Output/                  # Saved RDS model objects
    │       └── Tables/                         # PPML LaTeX tables
    ├── Riordinare/                             # Comparison plots & tables (TREND vs WB)
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

**Script:** `Code/Merge/1_Build_Final_PTA_EP_Dataset.R`

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
source("Code/Merge/1_Build_Final_PTA_EP_Dataset.R")
```

---

### Step 2 — Build the Full Analysis Dataset (Stata)

**Script:** `Code/Merge/2_Build_Final_PTA_EP_Dataset.do`

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

**Script:** `Code/Merge/3_FST_Conversion_Final_PTA_EP_Dataset.R`

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

---

## 6. Environmental Provisions Indices

All indices are computed inside `Code/Merge/1_Build_Final_PTA_EP_Dataset.R` and are available in `Data/Merged/Merged_TREND_WB_Indices_Only.dta`.

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

---

## 7. Analysis Scripts

### 7.1 OLS with High-Dimensional Fixed Effects (Stata)

**Script:** `Code/Analysis/Reg_PTA_04_02_26.do`

Uses `reghdfe` to estimate the following baseline specification:

$$\ln Y_{fpdt} = \beta \cdot \text{EPDepth}_{dt} + \theta_{fpd} + \theta_t + \varepsilon_{fpdt}$$

where $f$ = firm, $p$ = HS6 product, $d$ = destination country, $t$ = year.

**Dependent variables:**
- `ln_export` — log export value
- `ln_export_qua` — log export quantity
- `ln_export_value` — log export unit value

**Key regressors:**
- `WB_EP_Depth` — WB EP depth index (main specification)
- `TREND_EP_Count` — TREND EP count (robustness)
- Interaction with `env_good` (binary for OECD environmental goods)

**Controls:** `tariffs` (log MFN), `ln_hhi_baci` (log HHI)

**Fixed effects:** FPD (`fpd`) + Year (`year`)  
**Clustering:** PDT level (`pdt`)

The script estimates 12 models (m1–m12) with WB depth and 12 models (m1T–m12T) with TREND depth, across combinations of:
- Baseline vs. with controls
- No interaction vs. interaction with `env_good`
- Three outcome variables

Results are stored as `.ster` files and exported to LaTeX via `esttab`.

**Update the path before running:**
```stata
use "C:\path\to\final_dataset_pta_env_indices.dta", clear
cd "C:\path\to\Paper_PTA\Output\Analysis"
```

---

### 7.2 PPML Estimation (R)

**Script:** `Code/Analysis/PPML_Estimation.R`

Estimates the same specification using **Poisson Pseudo-Maximum Likelihood** via `fepois` (from `fixest`), which handles zero trade flows and is robust to heteroskedasticity. Uses the `.fst` version of the dataset for memory-efficient column-selective loading.

**Specifications estimated (4 blocks × 6 models):**
- Block 1: WB depth, no interaction
- Block 2: WB depth, interaction with `env_good`
- Block 3: TREND depth, no interaction
- Block 4: TREND depth, interaction with `env_good`

Each block produces a LaTeX table saved in `Output/Analysis/PPML/Tables/`.  
Individual model objects are saved as `.rds` files in `Output/Analysis/PPML/Models_Output/`.

**Update the data path before running:**
```r
data_file <- "/path/to/final_dataset_pta_env_indices_compressed.fst"
```

---

### 7.3 Callaway & Sant'Anna Staggered DiD (R)

**Script:** `Code/Analysis/Callaway_Sant'Anna.R`

Implements the **Callaway & Sant'Anna (2021)** estimator for staggered adoption, using the `did` package. The treatment timing is defined as the first year a Chinese PTA entered into force with each destination country.

- Treatment variable: `G` = first year of treatment (= 0 for never-treated)
- Unit identifier: `fpd_id` (firm-product-destination)
- Control group: never-treated units
- Results aggregated into an event-study plot via `aggte(..., type = "dynamic")`

> **Note:** The script is currently exploratory. The aggregation unit and the treatment variable definition may need to be finalized based on computational constraints (30M+ observations).

**Update the data path before running:**
```r
setwd("/path/to/directory/with/fst/file")
```

---

## 8. Outputs

### `Output/Analysis/`

| File | Description |
|---|---|
| `m1.ster` – `m12.ster` | Stored Stata estimates, WB depth models |
| `m1T.ster` – `m12T.ster` | Stored Stata estimates, TREND depth models |
| `Regression_Results_No_Int_5_Feb.dta/.tex` | No-interaction results (WB depth) |
| `Regression_Results_Int_5_Feb.dta/.tex` | Interaction results (WB depth × env_good) |
| `Regression_Results_No_Int_5_Feb_TEND_DEPTH.dta/.tex` | No-interaction results (TREND depth) |
| `Regression_Results_Int_5_Feb_TREND_DEPTH.dta/.tex` | Interaction results (TREND depth × env_good) |
| `Presentation_Feb_5*.tex` | Presentation-format regression tables |
| `Table_Final.tex`, `Table_baseline.tex` | Final publication tables |

### `Output/Analysis/PPML/`

| Directory | Contents |
|---|---|
| `Tables/` | `PPML_WB_No_Interaction.tex`, `PPML_WB_Interaction.tex`, `PPML_TREND_*.tex` |
| `Models_Output/` | Individual `.rds` model files (one per estimated specification) |

### `Output/Riordinare/`

Comparison tables and figures between TREND and WB indices, used during the index construction phase:

| File | Description |
|---|---|
| `Summary_Comparison_TREND_WB.csv` | Summary statistics comparison |
| `Table_Agreement_Coverage.csv` | Agreement coverage table |
| `Table_Correlations_TREND_WB.csv` | Correlation matrix |
| `Table_Depth_Categories_CrossTab.csv` | Cross-tabulation of depth categories |
| `Correlation_Matrix_TREND_WB.png` | Correlation heatmap |
| `Depth_BoxPlot_TREND_WB.png` | Box plots of depth indices |
| `EP_Depth_TimeSeries_*.png` | Time series of EP depth |
| `Violin_Comparison_TREND_WB.png` | Violin plots comparison |

---

## 9. Exploratory / Supporting Scripts

| Script | Description |
|---|---|
| `Code/TREND/Inspecting TREND Dataset.r` | Initial exploration of the TREND database: filtering, labeling, and saving the China 2000–2015 subset |
| `Code/WB/Inspecting WB Database.R` | Initial exploration of the WB DTA database: filtering environmental laws provisions for China 2000–2015 |
| `Code/WB/Graphs for presentation.R` | Generates graphs on Environmental Provision depth for presentations |
| `Code/WB/WB_Dataset_Conversion.do` | Converts WB Excel file to `.dta` format (run once before Step 1) |
| `Code/Merge/Merge_TREND_WB.R` | Standalone (legacy) version of the merge and index construction step — now fully integrated into `1_Build_Final_PTA_EP_Dataset.R` |

---

## 10. Important Notes

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

### Variable naming convention
- WB provision variables are renamed to `WB_1`, `WB_2`, …, `WB_N` in the merged dataset. The mapping to full provision names is in `Data/WB/WB_Variable_Mapping.csv`.
- TREND provision variables retain their original `X` codes (e.g., `X2_01_01`) with dots replaced by underscores. The mapping is in `Data/TREND/TREND_Variable_Mapping.csv`.
- The full-name version of the merged dataset (before renaming) is available in `Data/Merged/Merged_TREND_WB_FULL_NAMES.csv`.
