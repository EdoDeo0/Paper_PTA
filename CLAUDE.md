# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## Behavioral Guidelines

### 1. Think Before Coding

Before implementing anything: state assumptions explicitly, surface ambiguities, and ask if uncertain. If multiple interpretations exist, present them — don't pick silently. If a simpler approach exists, say so.

### 2. Simplicity First

Minimum code that solves the problem. No features beyond what was asked, no abstractions for single-use code, no speculative flexibility. If the result could be significantly shorter, rewrite it.

### 3. Surgical Changes

Modify only what directly addresses the request. Match existing code style. Don't refactor adjacent code, don't delete pre-existing dead code — mention it instead. Every changed line should trace directly to the request.

### 4. Goal-Driven Execution

Transform tasks into verifiable success criteria. For multi-step tasks, state a brief plan:
```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
```

---

## Project Overview

Academic research project on **Environmental Provisions (EPs) in Preferential Trade Agreements (PTAs)** signed by China (2000–2015).

**Author:** Edoardo Vitella (PhD Student, University of Trento & Free University of Bozen)

## Reading PDFs

Always use `pymupdf4llm` to convert PDFs before reading them (handles double-column layout correctly):
```bash
python3 -c "import pymupdf4llm; open('/tmp/<slug>.md','w').write(pymupdf4llm.to_markdown('<path>.pdf'))"
```

Save output to `/tmp/` using `AuthorYear_ShortTitle` naming. Check `/tmp/` first — if a converted `.md` already exists, read that instead of converting again.

## Environment Variables & External Resources

- `$RESEARCH_HOME` → `~/Documents/work`
- Global research wiki: `$RESEARCH_HOME/research-wiki`
- Global skills: `$RESEARCH_HOME/research-config/skills/`
- Local project wiki: `./wiki/` (create if it doesn't exist)
- Project-specific skills: `.claude/commands/`
- Zotero MCP tool available for searching and managing papers

## Data Pipeline

Scripts must run **in order**. All R scripts require the working directory set to the repository root. Hardcoded paths must be updated when running on a different machine.

```
Step 0 (Stata, once)          Step 1 (R)                    Step 2 (Stata)                      Step 3 (R)
WB_Dataset_Conversion.do  →  1_Build_Final_PTA_EP_Dataset.R  →  2_Build_Final_PTA_EP_Dataset.do  →  3_Build_Final_PTA_EP_Dataset.R
         ↓                            ↓                                   ↓                                   ↓
     WB_DTA.dta            Merged_TREND_WB_Indices_Only.dta    final_dataset_pta_env_indices_      ...compressed.fst
                                                                      compressed.dta
```

- Step 0 and Step 2 are run interactively in Stata.
- The `.fst` output of Step 3 enables fast column-selective loading in R — the full panel is too large to store in the repository.

### Analysis scripts

```bash
Rscript Code/Analysis/OLS_HDFE.R   # OLS with high-dimensional fixed effects
Rscript Code/Analysis/PPML.R       # PPML estimation
Rscript Code/Analysis/CEM.R        # CEM matching construction and diagnostics
Rscript Code/Analysis/OLS_CEM.R    # OLS on CEM-matched sample
Rscript Code/Analysis/PPML_CEM.R   # PPML on CEM-matched sample
```

## Shared Utility Library

**`Code/Analysis/pta_functions.R`** — source at the top of every analysis script. Provides:

- `run_block()` — runs a named block of fixest formulas, caches each result as `.rds`, skips already-computed models
- `estimate_model()` — estimates a single model from an `.fst` file, loading only the columns required by the formula
- `load_formula_data()` — parses a fixest formula string and loads only necessary columns from `.fst`
- `make_table()` — generates LaTeX regression tables from model stats objects

## Variable Naming Conventions

- WB provision variables → `WB_1`, `WB_2`, … `WB_N`; mapping in `Data/WB/WB_Variable_Mapping.csv`
- TREND provision variables → original `X` codes with dots replaced by underscores; mapping in `Data/TREND/TREND_Variable_Mapping.csv`
- Full-name merged dataset (before renaming): `Data/Merged/Merged_TREND_WB_FULL_NAMES.csv`

## Dependencies

### R

```r
install.packages(c(
  "haven", "readxl", "dplyr", "tidyr", "tidyverse", "labelled",
  "reshape2", "here", "fst", "fixest", "data.table",
  "lubridate", "ggplot2", "janitor", "stargazer",
  "cem", "cobalt", "patchwork", "WDI", "wbstats"
))
```

### Stata

```stata
ssc install reghdfe
ssc install estout
ssc install regsave
```

## Zotero Collection

This project corresponds to Zotero collection: Paper_PTA

## Data Not in the Repository

- Chinese customs transaction-level data (`final_dataset_pta.dta` and processed versions) — too large for GitHub
- `Data/WB/WB_DTA.dta` — regenerate with `Code/WB/WB_Dataset_Conversion.do` if absent
