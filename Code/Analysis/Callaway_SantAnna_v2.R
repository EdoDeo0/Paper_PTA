########################################################
###### Implementing Callaway and Sant'Anna (2021) ######
########################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## Implementation of Callaway and Sant'Anna (2021) DiD estimator
##
## This script uses the shared function library in pta_functions.R
## for data loading and output directory management.

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(did)
library(data.table)
library(dplyr)
library(ggplot2)
library(here)

source(here("Code/Analysis/pta_functions.R"))

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir <- here("Output/Analysis/CS")
dirs <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# DATA PREPARATION
# ─────────────────────────────────────────────────────────────────────

vars_needed <- c(
    "export", "exp_qua", "uv_exp", "WB_EP_Depth", "TREND_EP_Count",
    "env_good", "tariffs", "ln_hhi_baci", "fpd", "year", "pdt", "country_code"
)
data <- as.data.table(read_fst(data_file, columns = vars_needed))

# Build treatment cohort variable: first treatment year per destination (country_code).
# Never-treated units receive G = 0.
data <- data %>%
    group_by(country_code) %>%
    mutate(G = ifelse(
        any(WB_EP_Depth > 0),
        min(year[WB_EP_Depth > 0]),
        0
    )) %>%
    ungroup() %>%
    as.data.table()

# CS requires a numeric unit ID. With 30M+ obs at the fpd level, consider
# aggregating to pd or d before estimating to reduce computation.
# data[, fpd_id := .GRP, by = fpd]
# data[, pdt_id := .GRP, by = pdt]


# ─────────────────────────────────────────────────────────────────────
# CALLAWAY-SANT'ANNA ESTIMATION
# ─────────────────────────────────────────────────────────────────────

att_out <- att_gt(
    yname         = "export",
    tname         = "year",
    idname        = "fpd_id", # Numeric unit ID (create above before running)
    gname         = "G", # First-treatment year (0 = never treated)
    data          = data,
    control_group = "nevertreated", # or "notyettreated"
    est_method    = "reg", # or "ipw", "dr" (doubly robust)
    clustervars   = "pdt_id" # Numeric cluster ID (create above before running)
)

saveRDS(att_out, file.path(dirs$models, "CS_att_gt_WB.rds"))


# ─────────────────────────────────────────────────────────────────────
# EVENT STUDY AGGREGATION AND PLOT
# ─────────────────────────────────────────────────────────────────────

es <- aggte(att_out, type = "dynamic", min_e = -5, max_e = 5)
p <- ggdid(es)

ggsave(
    file.path(dirs$tables, "CS_EventStudy_WB.pdf"),
    plot   = p,
    width  = 8,
    height = 5
)

cat("\n=== COMPLETATO! ===\n")
cat("Grafici in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
