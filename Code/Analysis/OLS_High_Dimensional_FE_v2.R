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

# Stability mode for large Windows runs: avoid multi-thread race issues
# Comment these two lines if you use a Unix-based system or if you want to leverage multi-threading (adjust threads_fst() as needed)
# Unfortunenately, multi-threading with fst cause R terminal to crash on Windows when running the full set of models, likely due to race conditions.
# Alternatively, comment these two lines and run one block at a time (instead of all blocks in sequence) to mitigate the issue while still benefiting from multi-threading.
# threads_fst(1)
# setFixest_nthreads(1)

# Set the number of threads for fst (adjust based on your CPU)
# threads_fst(8) # 8 threads is a good default for modern CPUs (especially for laptops), but adjust as needed

# Load shared functions for estimation and table building
source(here("Code/Analysis/pta_functions.R"))

# Set your own data file path (dataset not tracked in the repo – file too large)
data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir <- here("Output/Analysis/OLS")
dirs <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


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
# RUN ALL BLOCKS
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction
cat("\n=== WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols)
make_table(stats1, cm_wb, "OLS_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 2: WB Interaction
cat("\n=== WB Interaction (fpd + year FE) ===\n")
f2 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "WB Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats2, cm_wb_int, "OLS_WB_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 3: TREND No Interaction
cat("\n=== TREND No Interaction (fpd + year FE) ===\n")
f3 <- c(
    "ln_export       ~ TREND_EP_Count | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "TREND No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats3, cm_trend, "OLS_TREND_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 4: TREND Interaction
cat("\n=== TREND Interaction (fpd + year FE) ===\n")
f4 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "TREND Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats4, cm_trend_int, "OLS_TREND_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

cat("\n=== COMPLETATO! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_*.rds\n")
cat("Tempo totale:", now() - start, "secondi\n")


# ─────────────────────────────────────────────────────────────────────
# RUN ALL BLOCKS WITH FIRM-PRODUCT-TIME FIXED EFFECTS (fpt)
# ─────────────────────────────────────────────────────────────────────
start_fpt <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction - firm-product-time FE
cat("\n=== WB No Interaction (firm-product-time FE) ===\n")
f1_fpt <- c(
    "ln_export       ~ WB_EP_Depth | fpt",
    "ln_export_qua   ~ WB_EP_Depth | fpt",
    "ln_export_value ~ WB_EP_Depth | fpt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt"
)
stats1_fpt <- run_block(f1_fpt, "WB No Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols)
make_table(stats1_fpt, cm_wb, "OLS_WB_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 2: WB Interaction - firm-product-time FE
cat("\n=== WB Interaction (firm-product-time FE) ===\n")
f2_fpt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt"
)
stats2_fpt <- run_block(f2_fpt, "WB Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats2_fpt, cm_wb_int, "OLS_WB_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 3: TREND No Interaction - firm-product-time FE
cat("\n=== TREND No Interaction (firm-product-time FE) ===\n")
f3_fpt <- c(
    "ln_export       ~ TREND_EP_Count | fpt",
    "ln_export_qua   ~ TREND_EP_Count | fpt",
    "ln_export_value ~ TREND_EP_Count | fpt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt"
)
stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats3_fpt, cm_trend, "OLS_TREND_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 4: TREND Interaction - firm-product-time FE
cat("\n=== TREND Interaction (firm-product-time FE) ===\n")
f4_fpt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt"
)
stats4_fpt <- run_block(f4_fpt, "TREND Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats4_fpt, cm_trend_int, "OLS_TREND_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

cat("\n=== COMPLETATO fpt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_fpt.rds\n")
cat("Tempo totale fpt:", now() - start_fpt, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# RUN ALL BLOCKS WITH PRODUCT-TIME FIXED EFFECTS (pt)
# ─────────────────────────────────────────────────────────────────────
start_pt <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction - product-time FE
cat("\n=== WB No Interaction (product-time FE) ===\n")
f1_pt <- c(
    "ln_export       ~ WB_EP_Depth | pt",
    "ln_export_qua   ~ WB_EP_Depth | pt",
    "ln_export_value ~ WB_EP_Depth | pt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt"
)
stats1_pt <- run_block(f1_pt, "WB No Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols)
make_table(stats1_pt, cm_wb, "OLS_WB_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 2: WB Interaction - product-time FE
cat("\n=== WB Interaction (product-time FE) ===\n")
f2_pt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | pt",
    "ln_export_value ~ WB_EP_Depth * env_good | pt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt"
)
stats2_pt <- run_block(f2_pt, "WB Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats2_pt, cm_wb_int, "OLS_WB_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 3: TREND No Interaction - product-time FE
cat("\n=== TREND No Interaction (product-time FE) ===\n")
f3_pt <- c(
    "ln_export       ~ TREND_EP_Count | pt",
    "ln_export_qua   ~ TREND_EP_Count | pt",
    "ln_export_value ~ TREND_EP_Count | pt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt"
)
stats3_pt <- run_block(f3_pt, "TREND No Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats3_pt, cm_trend, "OLS_TREND_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

# BLOCK 4: TREND Interaction - product-time FE
cat("\n=== TREND Interaction (product-time FE) ===\n")
f4_pt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | pt",
    "ln_export_value ~ TREND_EP_Count * env_good | pt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt"
)
stats4_pt <- run_block(f4_pt, "TREND Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
make_table(stats4_pt, cm_trend_int, "OLS_TREND_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)

cat("\n=== COMPLETATO pt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_pt.rds\n")
cat("Tempo totale pt:", now() - start_pt, "secondi\n")
