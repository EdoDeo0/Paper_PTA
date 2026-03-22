#################################
###### PPML CEM Robustness ######
#################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## PPML Estimation on CEM-matched data (only positive export trade flows)
## using fepois from the fixest package.
##
## PART A — CEM full      (matching on all covariates)
## PART B — CEM no_asia   (no asia dummy in matching)

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

# Load shared functions for estimation and table building

source(here("Code/Analysis/pta_functions.R"))

# Set your own data file path (dataset not tracked in the repo – file too large)
data_file <- here("Data/Matching/CEM_full/data_cem_matched_full.fst")
out_dir <- here("Output/Analysis/CEM/CEM_full/PPML")
dirs <- setup_output_dirs(out_dir)

stopifnot("Dataset CEM full not found!" = file.exists(data_file))

# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────
cm_wb <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend_int <- c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)

show_stats_ppml <- c("nobs", "r2", "n_clust")


##########################################################################
######   PART A — CEM FULL (PPML)                                   ######
##########################################################################

# ─────────────────────────────────────────────────────────────────────
# A - FIRM-PRODUCT-DESTINATION (fdp) + TIME (year) FIXED EFFECTS
# Cluster standard errors at the product-destination-time level (pdt)
# ─────────────────────────────────────────────────────────────────────
start <- now()

## BLOCK 1: WB No Interaction (fpd + year FE)
cat("\n=== [CEM FULL] WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
  "export  ~ WB_EP_Depth | fpd + year",
  "exp_qua ~ WB_EP_Depth | fpd + year",
  "uv_exp  ~ WB_EP_Depth | fpd + year",
  "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "CEM Full WB No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats1, cm_wb, "CEM_Full_PPML_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()

## BLOCK 2: WB Interaction (fpd + year FE)
cat("\n=== [CEM Full] WB Interaction (fpd + year FE) ===\n")
f2 <- c(
  "export  ~ WB_EP_Depth * env_good | fpd + year",
  "exp_qua ~ WB_EP_Depth * env_good | fpd + year",
  "uv_exp  ~ WB_EP_Depth * env_good | fpd + year",
  "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "CEM Full WB Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats2, cm_wb_int, "CEM_Full_PPML_WB_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()

## BLOCK 3: TREND No Interaction (fpd + year FE)
cat("\n=== [CEM Full] TREND No Interaction (fpd + year FE) ===\n")
f3 <- c(
  "export  ~ TREND_EP_Count | fpd + year",
  "exp_qua ~ TREND_EP_Count | fpd + year",
  "uv_exp  ~ TREND_EP_Count | fpd + year",
  "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "CEM Full TREND No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats3, cm_trend, "CEM_Full_PPML_TREND_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()

## BLOCK 4: TREND Interaction (fpd + year FE)
cat("\n=== [CEM Full] TREND Interaction (fpd + year FE) ===\n")
f4 <- c(
  "export  ~ TREND_EP_Count * env_good | fpd + year",
  "exp_qua ~ TREND_EP_Count * env_good | fpd + year",
  "uv_exp  ~ TREND_EP_Count * env_good | fpd + year",
  "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "CEM Full TREND Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats4, cm_trend_int, "CEM_Full_PPML_TREND_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== [CEM Full] DONE fpd + year FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_*.rds\n")
cat("Time for [CEM Full] fpd + year:", now() - start, "seconds\n")





# ────────────────────────────────────────────────────────────────────────────────────────
# A - FIRM-PRODUCT-TIME FIXED EFFECTS (fpt) + FIRM-PRODUCT-DESTINATION (fpd) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ────────────────────────────────────────────────────────────────────────────────────────
start_fpt <- now()

## BLOCK 1: WB No Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM Full] WB No Interaction (firm-product-time + firm-product-destination FE) ===\n")
f1_fpt <- c(
  "export  ~ WB_EP_Depth | fpt + fpd",
  "exp_qua ~ WB_EP_Depth | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth | fpt + fpd",
  "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd"
)
stats1_fpt <- run_block(f1_fpt, "CEM Full WB No Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats1_fpt, cm_wb, "CEM_Full_PPML_WB_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_fpt)
gc()

## BLOCK 2: WB Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM Full] WB Interaction (firm-product-time + firm-product-destination FE) ===\n")
f2_fpt <- c(
  "export  ~ WB_EP_Depth * env_good | fpt + fpd",
  "exp_qua ~ WB_EP_Depth * env_good | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth * env_good | fpt + fpd",
  "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats2_fpt <- run_block(f2_fpt, "CEM Full WB Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats2_fpt, cm_wb_int, "CEM_Full_PPML_WB_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_fpt)
gc()

## BLOCK 3: TREND No Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM Full] TREND No Interaction (firm-product-time + firm-product-destination FE) ===\n")
f3_fpt <- c(
  "export  ~ TREND_EP_Count | fpt + fpd",
  "exp_qua ~ TREND_EP_Count | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count | fpt + fpd",
  "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd"
)
stats3_fpt <- run_block(f3_fpt, "CEM Full TREND No Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats3_fpt, cm_trend, "CEM_Full_PPML_TREND_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_fpt)
gc()

## BLOCK 4: TREND Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM Full] TREND Interaction (firm-product-time + firm-product-destination FE) ===\n")
f4_fpt <- c(
  "export  ~ TREND_EP_Count * env_good | fpt + fpd",
  "exp_qua ~ TREND_EP_Count * env_good | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count * env_good | fpt + fpd",
  "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats4_fpt <- run_block(f4_fpt, "CEM Full TREND Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats4_fpt, cm_trend_int, "CEM_Full_PPML_TREND_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_fpt)
gc()

cat("\n=== [CEM Full] DONE fpt + fpd FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_fpt.rds\n")
cat("Time for [CEM full] fpt + fpd:", now() - start_fpt, "seconds\n")





# ─────────────────────────────────────────────────────────────────────
# A - FIRM-PRODUCT-DESTINATION (fdp) + PRODUCT-TIME (pt) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ─────────────────────────────────────────────────────────────────────
start <- now()

## BLOCK 1: WB No Interaction (fpd + pt FE)
cat("\n=== [CEM Full] WB No Interaction (fpd + pt FE) ===\n")
f1 <- c(
  "export  ~ WB_EP_Depth | fpd + pt",
  "exp_qua ~ WB_EP_Depth | fpd + pt",
  "uv_exp  ~ WB_EP_Depth | fpd + pt",
  "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt"
)
stats1 <- run_block(f1, "CEM Full WB No Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats1, cm_wb, "CEM_Full_PPML_WB_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()

## BLOCK 2: WB Interaction (fpd + pt FE)
cat("\n=== [CEM Full] WB Interaction (fpd + pt FE) ===\n")
f2 <- c(
  "export  ~ WB_EP_Depth * env_good | fpd + pt",
  "exp_qua ~ WB_EP_Depth * env_good | fpd + pt",
  "uv_exp  ~ WB_EP_Depth * env_good | fpd + pt",
  "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats2 <- run_block(f2, "CEM Full WB Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats2, cm_wb_int, "CEM_Full_PPML_WB_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()

## BLOCK 3: TREND No Interaction (fpd + pt FE)
cat("\n=== [CEM Full] TREND No Interaction (fpd + pt FE) ===\n")
f3 <- c(
  "export  ~ TREND_EP_Count | fpd + pt",
  "exp_qua ~ TREND_EP_Count | fpd + pt",
  "uv_exp  ~ TREND_EP_Count | fpd + pt",
  "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt"
)
stats3 <- run_block(f3, "CEM Full TREND No Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats3, cm_trend, "CEM_Full_PPML_TREND_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()

## BLOCK 4: TREND Interaction (fpd + pt FE)
cat("\n=== [CEM Full] TREND Interaction (fpd + pt FE) ===\n")
f4 <- c(
  "export  ~ TREND_EP_Count * env_good | fpd + pt",
  "exp_qua ~ TREND_EP_Count * env_good | fpd + pt",
  "uv_exp  ~ TREND_EP_Count * env_good | fpd + pt",
  "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats4 <- run_block(f4, "CEM Full TREND Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats4, cm_trend_int, "CEM_Full_PPML_TREND_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== [CEM Full] DONE fpd + pt FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_pt.rds\n")
cat("Time for [CEM Full] fpd + pt:", now() - start, "seconds\n")





##########################################################################
######   PART B — CEM NO_ASIA (PPML)                                 ######
##########################################################################

# Set your own data file path (dataset not tracked in the repo – file too large)
data_file <- here("Data/Matching/CEM_no_asia/data_cem_matched_no_asia.fst")
out_dir <- here("Output/Analysis/CEM/CEM_no_asia/PPML")
dirs <- setup_output_dirs(out_dir)

stopifnot("Dataset CEM no_asia not found!" = file.exists(data_file))

# ─────────────────────────────────────────────────────────────────────
# B - FIRM-PRODUCT-DESTINATION (fdp) + TIME (year) FIXED EFFECTS
# Cluster standard errors at the product-destination-time level (pdt)
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

## BLOCK 1: WB No Interaction (fpd + year FE)
cat("\n=== [CEM No Asia] WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
  "export  ~ WB_EP_Depth | fpd + year",
  "exp_qua ~ WB_EP_Depth | fpd + year",
  "uv_exp  ~ WB_EP_Depth | fpd + year",
  "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "CEM No Asia WB No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats1, cm_wb, "CEM_No_Asia_PPML_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()

## BLOCK 2: WB Interaction (fpd + year FE)
cat("\n=== [CEM No Asia] WB Interaction (fpd + year FE) ===\n")
f2 <- c(
  "export  ~ WB_EP_Depth * env_good | fpd + year",
  "exp_qua ~ WB_EP_Depth * env_good | fpd + year",
  "uv_exp  ~ WB_EP_Depth * env_good | fpd + year",
  "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "CEM No Asia WB Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats2, cm_wb_int, "CEM_No_Asia_PPML_WB_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()

## BLOCK 3: TREND No Interaction (fpd + year FE)
cat("\n=== [CEM No Asia] TREND No Interaction (fpd + year FE) ===\n")
f3 <- c(
  "export  ~ TREND_EP_Count | fpd + year",
  "exp_qua ~ TREND_EP_Count | fpd + year",
  "uv_exp  ~ TREND_EP_Count | fpd + year",
  "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "CEM No Asia TREND No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats3, cm_trend, "CEM_No_Asia_PPML_TREND_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()

## BLOCK 4: TREND Interaction (fpd + year FE)
cat("\n=== [CEM No Asia] TREND Interaction (fpd + year FE) ===\n")
f4 <- c(
  "export  ~ TREND_EP_Count * env_good | fpd + year",
  "exp_qua ~ TREND_EP_Count * env_good | fpd + year",
  "uv_exp  ~ TREND_EP_Count * env_good | fpd + year",
  "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "CEM No Asia TREND Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats4, cm_trend_int, "CEM_No_Asia_PPML_TREND_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== [CEM No Asia] DONE fpd + year FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_*.rds\n")
cat("Time for [CEM No Asia] fpd + year:", now() - start, "seconds\n")





# ────────────────────────────────────────────────────────────────────────────────────────
# B - FIRM-PRODUCT-TIME FIXED EFFECTS (fpt) + FIRM-PRODUCT-DESTINATION (fpd) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ────────────────────────────────────────────────────────────────────────────────────────
start_fpt <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

## BLOCK 1: WB No Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM No Asia] WB No Interaction (firm-product-time + firm-product-destination FE) ===\n")
f1_fpt <- c(
  "export  ~ WB_EP_Depth | fpt + fpd",
  "exp_qua ~ WB_EP_Depth | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth | fpt + fpd",
  "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd"
)
stats1_fpt <- run_block(f1_fpt, "CEM No Asia WB No Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats1_fpt, cm_wb, "CEM_No_Asia_PPML_WB_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_fpt)
gc()

## BLOCK 2: WB Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM No Asia] WB Interaction (firm-product-time + firm-product-destination FE) ===\n")
f2_fpt <- c(
  "export  ~ WB_EP_Depth * env_good | fpt + fpd",
  "exp_qua ~ WB_EP_Depth * env_good | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth * env_good | fpt + fpd",
  "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats2_fpt <- run_block(f2_fpt, "CEM No Asia WB Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats2_fpt, cm_wb_int, "CEM_No_Asia_PPML_WB_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_fpt)
gc()

## BLOCK 3: TREND No Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM No Asia] TREND No Interaction (firm-product-time + firm-product-destination FE) ===\n")
f3_fpt <- c(
  "export  ~ TREND_EP_Count | fpt + fpd",
  "exp_qua ~ TREND_EP_Count | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count | fpt + fpd",
  "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd"
)
stats3_fpt <- run_block(f3_fpt, "CEM No Asia TREND No Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats3_fpt, cm_trend, "CEM_No_Asia_PPML_TREND_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_fpt)
gc()

## BLOCK 4: TREND Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== [CEM No Asia] TREND Interaction (firm-product-time + firm-product-destination FE) ===\n")
f4_fpt <- c(
  "export  ~ TREND_EP_Count * env_good | fpt + fpd",
  "exp_qua ~ TREND_EP_Count * env_good | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count * env_good | fpt + fpd",
  "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
  "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats4_fpt <- run_block(f4_fpt, "CEM No Asia TREND Interaction (firm-product-time + firm-product-destination FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats4_fpt, cm_trend_int, "CEM_No_Asia_PPML_TREND_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_fpt)
gc()

cat("\n=== [CEM No Asia] DONE fpt + fpd FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_fpt.rds\n")
cat("Time for [CEM No Asia] fpt + fpd:", now() - start_fpt, "seconds\n")





# ─────────────────────────────────────────────────────────────────────
# B - FIRM-PRODUCT-DESTINATION (fdp) + PRODUCT-TIME (pt) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

## BLOCK 1: WB No Interaction (fpd + pt FE)
cat("\n=== [CEM No Asia] WB No Interaction (fpd + pt FE) ===\n")
f1 <- c(
  "export  ~ WB_EP_Depth | fpd + pt",
  "exp_qua ~ WB_EP_Depth | fpd + pt",
  "uv_exp  ~ WB_EP_Depth | fpd + pt",
  "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt"
)
stats1 <- run_block(f1, "CEM No Asia WB No Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats1, cm_wb, "CEM_No_Asia_PPML_WB_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()

## BLOCK 2: WB Interaction (fpd + pt FE)
cat("\n=== [CEM No Asia] WB Interaction (fpd + pt FE) ===\n")
f2 <- c(
  "export  ~ WB_EP_Depth * env_good | fpd + pt",
  "exp_qua ~ WB_EP_Depth * env_good | fpd + pt",
  "uv_exp  ~ WB_EP_Depth * env_good | fpd + pt",
  "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats2 <- run_block(f2, "CEM No Asia WB Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats2, cm_wb_int, "CEM_No_Asia_PPML_WB_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()

## BLOCK 3: TREND No Interaction (fpd + pt FE)
cat("\n=== [CEM No Asia] TREND No Interaction (fpd + pt FE) ===\n")
f3 <- c(
  "export  ~ TREND_EP_Count | fpd + pt",
  "exp_qua ~ TREND_EP_Count | fpd + pt",
  "uv_exp  ~ TREND_EP_Count | fpd + pt",
  "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt"
)
stats3 <- run_block(f3, "CEM No Asia TREND No Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats3, cm_trend, "CEM_No_Asia_PPML_TREND_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()

## BLOCK 4: TREND Interaction (fpd + pt FE)
cat("\n=== [CEM No Asia] TREND Interaction (fpd + pt FE) ===\n")
f4 <- c(
  "export  ~ TREND_EP_Count * env_good | fpd + pt",
  "exp_qua ~ TREND_EP_Count * env_good | fpd + pt",
  "uv_exp  ~ TREND_EP_Count * env_good | fpd + pt",
  "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
  "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats4 <- run_block(f4, "CEM No Asia TREND Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml, preload_block_data = TRUE)
make_table(stats4, cm_trend_int, "CEM_No_Asia_PPML_TREND_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== [CEM No Asia] DONE fpd + pt FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_pt.rds\n")
cat("Time for [CEM No Asia] fpd + pt:", now() - start, "seconds\n")
