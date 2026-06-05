# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an academic research project studying the effect of **Environmental Provisions (EPs) in Preferential Trade Agreements (PTAs)** signed by China on Chinese trade flows (2000–2015). The analysis examines how the depth and composition of environmental clauses in PTAs affect trade volumes and composition (environmental vs. non-environmental goods).

**Author:** Edoardo Vitella (PhD Student, University of Trento & Free University of Bozen)

## Reading PDFs

Always use `markitdown` to convert PDFs before reading them:
```bash
markitdown path/to/file.pdf
```

## Environment Variables & External Resources

- `$RESEARCH_HOME` → `~/Documents/work`
- Global research wiki: `$RESEARCH_HOME/research-wiki`
- Global skills: `$RESEARCH_HOME/research-config/skills/`
- Local project wiki: `./wiki/` (create if it doesn't exist)
- Project-specific skills: `.claude/commands/`
- Zotero MCP tool is available for searching and managing papers

## Running the Analysis

All R scripts should be run with the working directory set to the **repository root** (`Paper_PTA/`). Scripts contain hardcoded absolute paths that must be updated when running on a different machine.

### Data pipeline (must run in order)

```bash
# Step 0 — Convert WB Excel to .dta (Stata, run once)
# Open Code/WB/WB_Dataset_Conversion.do in Stata and run it

# Step 1 — Build EP indices dataset (R)
Rscript Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R

# Step 2 — Merge with Chinese customs data (Stata)
# Open Code/Dataset_Creation/2_Build_Final_PTA_EP_Dataset.do in Stata and run it

# Step 3 — Convert to FST format (R)
Rscript Code/Dataset_Creation/3_Build_Final_PTA_EP_Dataset.R
```

### Analysis scripts

```bash
# Main OLS with High-Dimensional Fixed Effects
Rscript Code/Analysis/OLS_HDFE.R

# Main PPML estimation
Rscript Code/Analysis/PPML.R

# CEM matching construction and diagnostics
Rscript Code/Analysis/CEM.R

# OLS/PPML on CEM-matched samples
Rscript Code/Analysis/OLS_CEM.R
Rscript Code/Analysis/PPML_CEM.R
```

### Install R dependencies

```r
install.packages(c(
  "haven", "readxl", "dplyr", "tidyr", "tidyverse", "labelled",
  "reshape2", "here", "fst", "fixest", "data.table",
  "lubridate", "ggplot2", "janitor", "stargazer",
  "cem", "cobalt", "patchwork", "WDI", "wbstats"
))
```

### Install Stata dependencies

```stata
ssc install reghdfe
ssc install estout
ssc install regsave
```

## Architecture

### Data pipeline flow

```
WB_Dataset_Conversion.do    →   WB_DTA.dta
                                     ↓
1_Build_Final_PTA_EP_Dataset.R  →  Merged_TREND_WB_Indices_Only.dta
                                     ↓
2_Build_Final_PTA_EP_Dataset.do →  final_dataset_pta_env_indices_compressed.dta
                                     ↓
3_Build_Final_PTA_EP_Dataset.R  →  final_dataset_pta_env_indices_compressed.fst
```

The `.fst` format enables fast column-selective loading from large datasets — essential because the full panel (firm × HS6 product × destination × year) is very large and not stored in the repository.

### Shared utility library: `Code/Analysis/pta_functions.R`

Source this file at the top of every analysis script. Key functions:

- `run_block(formulas, block_name, estimator, data_file, models_dir, ...)` — runs a named block of fixest formulas, caches results as `.rds`, skips already-computed models
- `estimate_model(formula_str, estimator, data_file, vcov, ...)` — estimates a single OLS (`feols`) or PPML (`fepois`) model from an `.fst` file, loading only the columns required by the formula
- `load_formula_data(data_file, formula_str, vcov)` — parses a fixest formula string and loads only necessary columns
- `make_table(stats_list, coefmap, filename, tables_dir, ...)` — generates LaTeX regression tables from model stats objects

### Estimation specification

Unit of observation: HS6 product × destination country × year × firm (`fpdt`).

Baseline OLS:
```
ln_export ~ EPDepth | fpd + year
```

Baseline PPML (multiplicative, handles zero trade flows):
```
export ~ EPDepth | fpd + year
```

Both estimators are run across four FE structures (`fpd+year`, `fpd+pt`, `fpt+pd`, `fpt+fpd`) with and without `env_good` interactions (distinguishing environmental goods from non-environmental goods).

### CEM matching (`Code/Analysis/CEM.R`)

Constructs a control group (non-PTA destinations) balanced on pre-treatment covariates:
- `gdp_growth_2000` — pre-treatment economic trend
- `log_gdppc_2000` — development level
- `mfn_tariff_2000` — pre-PTA protection level

Two matched samples are produced: `CEM_full` (all countries) and `CEM_no_asia` (excluding Asian partners).

### Variable naming conventions

- WB provision variables → `WB_1`, `WB_2`, … `WB_N` (mapping: `Data/WB/WB_Variable_Mapping.csv`)
- TREND provision variables → `X` codes with dots replaced by underscores (mapping: `Data/TREND/TREND_Variable_Mapping.csv`)
- Full-name version of the merged dataset: `Data/Merged/Merged_TREND_WB_FULL_NAMES.csv`

### Data not in the repository

- Chinese customs transaction-level data (`final_dataset_pta.dta` and its processed versions) — too large for GitHub
- `Data/WB/WB_DTA.dta` — regenerate with `WB_Dataset_Conversion.do` if absent
