#####################################
###### OLS / REGHDFE Estimation #####
#####################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## OLS with HDFE (feols) - equivalent to Stata reghdfe
##
## This script uses the shared function library in pta_functions.R.
## All estimation and table-building logic lives there.

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

# Warning: for large datasets, using multiple threads with fixest and fst can cause crashes on Windows. 
# If you experience this, uncomment the lines below to run with a single thread.
# threads_fst(1)
# setFixest_nthreads(1)

# Load shared functions for estimation and table building
source(here("Code/Analysis/pta_functions.R"))

# Set your own data file path (dataset not tracked in the repo – file too large)
data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir <- here("Output/Analysis/OLS")
dirs <- setup_output_dirs(out_dir)

stopifnot("Data file not found!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────

cm_wb <- c(
    "WB_EP_Depth"           = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"               = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"           = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
    "WB_EP_Depth" = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs" = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
    "TREND_EP_Count"              = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"                     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                 = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend_int <- c(
    "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs" = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-DESTINATION (fdp) + TIME (year) FIXED EFFECTS
# Cluster standard errors at the product-destination-time level (pdt)
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ols <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())

## BLOCK 1: WB No Interaction (fpd + year FE)
cat("\n=== WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1, cm_wb, "OLS_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1)
gc()


## BLOCK 2: WB Interaction (fpd + year FE)
cat("\n=== WB Interaction (fpd + year FE) ===\n")
f2 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "WB Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2, cm_wb_int, "OLS_WB_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2)
gc()


## BLOCK 3: TREND No Interaction (fpd + year FE)
cat("\n=== TREND No Interaction (fpd + year FE) ===\n")
f3 <- c(
    "ln_export       ~ TREND_EP_Count | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "TREND No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3, cm_trend, "OLS_TREND_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3)
gc()


## BLOCK 4: TREND Interaction (fpd + year FE)
cat("\n=== TREND Interaction (fpd + year FE) ===\n")
f4 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "TREND Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4, cm_trend_int, "OLS_TREND_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4)
gc()

cat("\n=== DONE fpd + year FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 OLS_*_*.rds\n")
cat("Time for fpd + year FE:", now() - start, "seconds\n")





# ──────────────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-TIME FIXED EFFECTS (fpt) + PRODUCT-DESTINATION (pd) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ──────────────────────────────────────────────────────────────────────────────
start_fpt <- now()
show_stats_ols <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())

## BLOCK 1: WB No Interaction (firm-product-time + product-destination FE)
cat("\n=== WB No Interaction (firm-product-time + product-destination FE) ===\n")
f1_fpt <- c(
    "ln_export       ~ WB_EP_Depth | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth | fpt + pd",
    "ln_export_value ~ WB_EP_Depth | fpt + pd",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd"
)
stats1_fpt <- run_block(f1_fpt, "WB No Interaction (firm-product-time + product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_fpt, cm_wb, "OLS_WB_No_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_fpt)
gc()

## BLOCK 2: WB Interaction (firm-product-time + product-destination FE)
cat("\n=== WB Interaction (firm-product-time + product-destination FE) ===\n")
f2_fpt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt + pd",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt + pd",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd"
)
stats2_fpt <- run_block(f2_fpt, "WB Interaction (firm-product-time + product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_fpt, cm_wb_int, "OLS_WB_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_fpt)
gc()


## BLOCK 3: TREND No Interaction (firm-product-time + product-destination FE)
cat("\n=== TREND No Interaction (firm-product-time + product-destination FE) ===\n")
f3_fpt <- c(
    "ln_export       ~ TREND_EP_Count | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count | fpt + pd",
    "ln_export_value ~ TREND_EP_Count | fpt + pd",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd"
)
stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (firm-product-time + product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_fpt, cm_trend, "OLS_TREND_No_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_fpt)
gc()


## BLOCK 4: TREND Interaction (firm-product-time + product-destination FE)
cat("\n=== TREND Interaction (firm-product-time + product-destination FE) ===\n")
f4_fpt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt + pd",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt + pd",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd"
)
stats4_fpt <- run_block(f4_fpt, "TREND Interaction (firm-product-time + product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_fpt, cm_trend_int, "OLS_TREND_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_fpt)
gc()

cat("\n=== DONE fpt + pd FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 OLS_*_fpt.rds\n")
cat("Time for fpt + pd:", now() - start_fpt, "seconds\n")





# ────────────────────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-TIME FIXED EFFECTS (fpt) + FIRM-PRODUCT-DESTINATION (fpd) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ────────────────────────────────────────────────────────────────────────────────────
start_fpt <- now()
show_stats_ols <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())
# threads_fst(1)
# setFixest_nthreads(1) # To avoid windows crash

## BLOCK 1: WB No Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== WB No Interaction (firm-product-time + firm-product-destination FE) ===\n")
f1_fpt <- c(
    "ln_export       ~ WB_EP_Depth | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth | fpt + fpd",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd"
)
stats1_fpt <- run_block(f1_fpt, "WB No Interaction (firm-product-time + firm-product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_fpt, cm_wb, "OLS_WB_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_fpt)
gc()


## BLOCK 2: WB Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== WB Interaction (firm-product-time + firm-product-destination FE) ===\n")
f2_fpt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt + fpd",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats2_fpt <- run_block(f2_fpt, "WB Interaction (firm-product-time + firm-product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_fpt, cm_wb_int, "OLS_WB_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_fpt)
gc()


## BLOCK 3: TREND No Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== TREND No Interaction (firm-product-time + firm-product-destination FE) ===\n")
f3_fpt <- c(
    "ln_export       ~ TREND_EP_Count | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count | fpt + fpd",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd"
)
stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (firm-product-time + firm-product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_fpt, cm_trend, "OLS_TREND_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_fpt)
gc()


## BLOCK 4: TREND Interaction (firm-product-time + firm-product-destination FE)
cat("\n=== TREND Interaction (firm-product-time + firm-product-destination FE) ===\n")
f4_fpt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt + fpd",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats4_fpt <- run_block(f4_fpt, "TREND Interaction (firm-product-time + firm-product-destination FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_fpt, cm_trend_int, "OLS_TREND_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_fpt)
gc()

cat("\n=== DONE fpt + fpd FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 OLS_*_fpt.rds\n")
cat("Time for fpt + fpd:", now() - start_fpt, "seconds\n")





# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-DESTINATION (fdp) + PRODUCT-TIME (pt) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ols <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())
# threads_fst(1)
setFixest_nthreads(1) # To avoid windows crash

## BLOCK 1: WB No Interaction (fpd + pt FE)
cat("\n=== WB No Interaction (fpd + pt FE) ===\n")
f1 <- c(
    "ln_export       ~ WB_EP_Depth | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth | fpd + pt",
    "ln_export_value ~ WB_EP_Depth | fpd + pt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt"
)
stats1 <- run_block(f1, "WB No Interaction (fpd + pt FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1, cm_wb, "OLS_WB_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1)
gc()


## BLOCK 2: WB Interaction (fpd + pt FE)
cat("\n=== WB Interaction (fpd + pt FE) ===\n")
f2 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + pt",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + pt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats2 <- run_block(f2, "WB Interaction (fpd + pt FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2, cm_wb_int, "OLS_WB_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2)
gc()


## BLOCK 3: TREND No Interaction (fpd + pt FE)
cat("\n=== TREND No Interaction (fpd + pt FE) ===\n")
f3 <- c(
    "ln_export       ~ TREND_EP_Count | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count | fpd + pt",
    "ln_export_value ~ TREND_EP_Count | fpd + pt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt"
)
stats3 <- run_block(f3, "TREND No Interaction (fpd + pt FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3, cm_trend, "OLS_TREND_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3)
gc()


## BLOCK 4: TREND Interaction (fpd + pt FE)
cat("\n=== TREND Interaction (fpd + pt FE) ===\n")
f4 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + pt",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + pt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats4 <- run_block(f4, "TREND Interaction (fpd + pt FE)", "ols", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4, cm_trend_int, "OLS_TREND_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4)
gc()

cat("\n=== DONE fpd + pt FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 OLS_*_*.rds\n")
cat("Time for fpd + pt FE:", now() - start, "seconds\n")
