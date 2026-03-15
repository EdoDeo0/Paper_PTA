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
# FIRM-PRODUCT-DESTINATION (fdp) + TIME (year) FIXED EFFECTS
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction
## BLOCCO 1: WB No Interaction (fpd + year FE)
cat("\n=== WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
# stats1 <- run_block(f1, "WB No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols)
stats1 <- run_block(f1, "WB No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1, cm_wb, "OLS_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1)
gc()

# BLOCK 2: WB Interaction
## BLOCCO 2: WB Interaction (fpd + year FE)
cat("\n=== WB Interaction (fpd + year FE) ===\n")
f2 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
# stats2 <- run_block(f2, "WB Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats2 <- run_block(f2, "WB Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2, cm_wb_int, "OLS_WB_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2)
gc()

# BLOCK 3: TREND No Interaction
## BLOCCO 3: TREND No Interaction (fpd + year FE)
cat("\n=== TREND No Interaction (fpd + year FE) ===\n")
f3 <- c(
    "ln_export       ~ TREND_EP_Count | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
# stats3 <- run_block(f3, "TREND No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats3 <- run_block(f3, "TREND No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3, cm_trend, "OLS_TREND_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3)
gc()

# BLOCK 4: TREND Interaction
## BLOCCO 4: TREND Interaction (fpd + year FE)
cat("\n=== TREND Interaction (fpd + year FE) ===\n")
f4 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
# stats4 <- run_block(f4, "TREND Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats4 <- run_block(f4, "TREND Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4, cm_trend_int, "OLS_TREND_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4)
gc()

cat("\n=== COMPLETATO! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_*.rds\n")
cat("Tempo totale:", now() - start, "secondi\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-TIME FIXED EFFECTS (fpt)
# ─────────────────────────────────────────────────────────────────────
start_fpt <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction (firm-product-time FE)
cat("\n=== WB No Interaction (firm-product-time FE) ===\n")
f1_fpt <- c(
    "ln_export       ~ WB_EP_Depth | fpt",
    "ln_export_qua   ~ WB_EP_Depth | fpt",
    "ln_export_value ~ WB_EP_Depth | fpt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt"
)
# stats1_fpt <- run_block(f1_fpt, "WB No Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols)
stats1_fpt <- run_block(f1_fpt, "WB No Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_fpt, cm_wb, "OLS_WB_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_fpt)
gc()

# BLOCK 2: WB Interaction (firm-product-time FE)
cat("\n=== WB Interaction (firm-product-time FE) ===\n")
f2_fpt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt"
)
# stats2_fpt <- run_block(f2_fpt, "WB Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats2_fpt <- run_block(f2_fpt, "WB Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_fpt, cm_wb_int, "OLS_WB_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_fpt)
gc()

# BLOCK 3: TREND No Interaction (firm-product-time FE)
cat("\n=== TREND No Interaction (firm-product-time FE) ===\n")
f3_fpt <- c(
    "ln_export       ~ TREND_EP_Count | fpt",
    "ln_export_qua   ~ TREND_EP_Count | fpt",
    "ln_export_value ~ TREND_EP_Count | fpt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt"
)
# stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_fpt, cm_trend, "OLS_TREND_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_fpt)
gc()

# BLOCK 4: TREND Interaction (firm-product-time FE)
cat("\n=== TREND Interaction (firm-product-time FE) ===\n")
f4_fpt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt"
)
# stats4_fpt <- run_block(f4_fpt, "TREND Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats4_fpt <- run_block(f4_fpt, "TREND Interaction (firm-product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_fpt, cm_trend_int, "OLS_TREND_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_fpt)
gc()

cat("\n=== COMPLETATO fpt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_fpt.rds\n")
cat("Tempo totale fpt:", now() - start_fpt, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# PRODUCT-TIME FIXED EFFECTS (pt)
# ─────────────────────────────────────────────────────────────────────
start_pt <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction (product-time FE)
cat("\n=== WB No Interaction (product-time FE) ===\n")
f1_pt <- c(
    "ln_export       ~ WB_EP_Depth | pt",
    "ln_export_qua   ~ WB_EP_Depth | pt",
    "ln_export_value ~ WB_EP_Depth | pt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt"
)
# stats1_pt <- run_block(f1_pt, "WB No Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols)
stats1_pt <- run_block(f1_pt, "WB No Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_pt, cm_wb, "OLS_WB_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_pt)
gc()

# BLOCK 2: WB Interaction (product-time FE)
cat("\n=== WB Interaction (product-time FE) ===\n")
f2_pt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | pt",
    "ln_export_value ~ WB_EP_Depth * env_good | pt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt"
)
# stats2_pt <- run_block(f2_pt, "WB Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats2_pt <- run_block(f2_pt, "WB Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_pt, cm_wb_int, "OLS_WB_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_pt)
gc()

# BLOCK 3: TREND No Interaction (product-time FE)
cat("\n=== TREND No Interaction (product-time FE) ===\n")
f3_pt <- c(
    "ln_export       ~ TREND_EP_Count | pt",
    "ln_export_qua   ~ TREND_EP_Count | pt",
    "ln_export_value ~ TREND_EP_Count | pt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt"
)
# stats3_pt <- run_block(f3_pt, "TREND No Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats3_pt <- run_block(f3_pt, "TREND No Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_pt, cm_trend, "OLS_TREND_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_pt)
gc()

# BLOCK 4: TREND Interaction (product-time FE)
cat("\n=== TREND Interaction (product-time FE) ===\n")
f4_pt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | pt",
    "ln_export_value ~ TREND_EP_Count * env_good | pt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt"
)
# stats4_pt <- run_block(f4_pt, "TREND Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols)
stats4_pt <- run_block(f4_pt, "TREND Interaction (product-time FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_pt, cm_trend_int, "OLS_TREND_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_pt)
gc()

cat("\n=== COMPLETATO pt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_pt.rds\n")
cat("Tempo totale pt:", now() - start_pt, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-DESTINATION (fpd) AND FIRM-PRODUCT-TIME (fpt) FIXED EFFECTS
# ─────────────────────────────────────────────────────────────────────
start_fpd_fpt <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction (fpd & fpt FE)
cat("\n=== WB No Interaction (fpd & fpt FE) ===\n")
f1_fpd_fpt <- c(
    "ln_export       ~ WB_EP_Depth | fpd + fpt",
    "ln_export_qua   ~ WB_EP_Depth | fpd + fpt",
    "ln_export_value ~ WB_EP_Depth | fpd + fpt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt"
)
stats1_fpd_fpt <- run_block(f1_fpd_fpt, "WB No Interaction (fpd & fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_fpd_fpt, cm_wb, "OLS_WB_No_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_fpd_fpt)
gc()

# BLOCK 2: WB Interaction (fpd & fpt FE)
cat("\n=== WB Interaction (fpd & fpt FE) ===\n")
f2_fpd_fpt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + fpt",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + fpt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt"
)
stats2_fpd_fpt <- run_block(f2_fpd_fpt, "WB Interaction (fpd & fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_fpd_fpt, cm_wb_int, "OLS_WB_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_fpd_fpt)
gc()

# BLOCK 3: TREND No Interaction (fpd & fpt FE)
cat("\n=== TREND No Interaction (fpd & fpt FE) ===\n")
f3_fpd_fpt <- c(
    "ln_export       ~ TREND_EP_Count | fpd + fpt",
    "ln_export_qua   ~ TREND_EP_Count | fpd + fpt",
    "ln_export_value ~ TREND_EP_Count | fpd + fpt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt"
)
stats3_fpd_fpt <- run_block(f3_fpd_fpt, "TREND No Interaction (fpd & fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_fpd_fpt, cm_trend, "OLS_TREND_No_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_fpd_fpt)
gc()

# BLOCK 4: TREND Interaction (fpd & fpt FE)
cat("\n=== TREND Interaction (fpd & fpt FE) ===\n")
f4_fpd_fpt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + fpt",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + fpt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt"
)
stats4_fpd_fpt <- run_block(f4_fpd_fpt, "TREND Interaction (fpd & fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_fpd_fpt, cm_trend_int, "OLS_TREND_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_fpd_fpt)
gc()

cat("\n=== COMPLETATO fpd & fpt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_fpd_fpt.rds\n")
cat("Tempo totale fpd & fpt:", now() - start_fpd_fpt, "secondi\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-TIME (fpt) + PRODUCT-TIME (pt) + PRODUCT-DESTINATION (pd) FIXED EFFECTS
# Adapted from Crowley et al 2021
# ─────────────────────────────────────────────────────────────────────
start_fpt_pt_pd <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction (fpt + pt + pd FE)
cat("\n=== Crowley et al 2021: fpt + pt + pd FE ===\n")
f1_fpt_pt_pd <- c(
    "ln_export       ~ WB_EP_Depth | fpt + pt + pd",
    "ln_export_qua   ~ WB_EP_Depth | fpt + pt + pd",
    "ln_export_value ~ WB_EP_Depth | fpt + pt + pd",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats1_fpt_pt_pd <- run_block(f1_fpt_pt_pd, "Crowley et al 2021 (fpt + pt + pd FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_fpt_pt_pd, cm_wb, "OLS_WB_No_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_fpt_pt_pd)
gc()

# BLOCK 2: WB Interaction (fpt + pt + pd FE)
cat("\n=== WB Interaction (fpt + pt + pd FE) ===\n")
f2_fpt_pt_pd <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpt + pt + pd",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt + pt + pd",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt + pt + pd",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats2_fpt_pt_pd <- run_block(f2_fpt_pt_pd, "Crowley et al 2021 (fpt + pt + pd FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_fpt_pt_pd, cm_wb_int, "OLS_WB_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_fpt_pt_pd)
gc()

# BLOCK 3: TREND No Interaction (fpt + pt + pd FE)
cat("\n=== TREND No Interaction (fpt + pt + pd FE) ===\n")
f3_fpt_pt_pd <- c(
    "ln_export       ~ TREND_EP_Count | fpt + pt + pd",
    "ln_export_qua   ~ TREND_EP_Count | fpt + pt + pd",
    "ln_export_value ~ TREND_EP_Count | fpt + pt + pd",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats3_fpt_pt_pd <- run_block(f3_fpt_pt_pd, "Crowley et al 2021 (fpt + pt + pd FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_fpt_pt_pd, cm_trend, "OLS_TREND_No_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_fpt_pt_pd)
gc()

# BLOCK 4: TREND Interaction (fpt + pt + pd FE)
cat("\n=== TREND Interaction (fpt + pt + pd FE) ===\n")
f4_fpt_pt_pd <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpt + pt + pd",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt + pt + pd",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt + pt + pd",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats4_fpt_pt_pd <- run_block(f4_fpt_pt_pd, "Crowley et al 2021 (fpt + pt + pd FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_fpt_pt_pd, cm_trend_int, "OLS_TREND_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_fpt_pt_pd)
gc()

cat("\n=== COMPLETATO fpt + pt + pd! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_fpt_pt_pd.rds\n")
cat("Tempo totale fpt + pt + pd:", now() - start_fpt_pt_pd, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# FIRM-TIME (ft) + TIME (year) + DESTINATION (country_code) FIXED EFFECTS
# Adapted from Neri-Leinè et al 2023
# ─────────────────────────────────────────────────────────────────────
start_ft_year_dest <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction (ft + year + destination FE)
cat("\n=== Neri-Leinè et al 2023: ft + year + destination FE ===\n")
f1_ft_year_dest <- c(
    "ln_export       ~ WB_EP_Depth | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth | ft + year + country_code",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats1_ft_year_dest <- run_block(f1_ft_year_dest, "Neri-Leinè et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_ft_year_dest, cm_wb, "OLS_WB_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_ft_year_dest)
gc()

# BLOCK 2: WB Interaction (ft + year + destination FE)
cat("\n=== WB Interaction (ft + year + destination FE) ===\n")
f2_ft_year_dest <- c(
    "ln_export       ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats2_ft_year_dest <- run_block(f2_ft_year_dest, "Neri-Leinè et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_ft_year_dest, cm_wb_int, "OLS_WB_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_ft_year_dest)
gc()

# BLOCK 3: TREND No Interaction (ft + year + destination FE)
cat("\n=== TREND No Interaction (ft + year + destination FE) ===\n")
f3_ft_year_dest <- c(
    "ln_export       ~ TREND_EP_Count | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count | ft + year + country_code",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats3_ft_year_dest <- run_block(f3_ft_year_dest, "Neri-Leinè et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_ft_year_dest, cm_trend, "OLS_TREND_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_ft_year_dest)
gc()

# BLOCK 4: TREND Interaction (ft + year + destination FE)
cat("\n=== TREND Interaction (ft + year + destination FE) ===\n")
f4_ft_year_dest <- c(
    "ln_export       ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats4_ft_year_dest <- run_block(f4_ft_year_dest, "Neri-Leinè et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_year_dest, cm_trend_int, "OLS_TREND_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_year_dest)
gc()

cat("\n=== COMPLETATO ft + year + destination! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_ft_year_dest.rds\n")
cat("Tempo totale ft + year + destination:", now() - start_ft_year_dest, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# FIRM-TIME (ft) + PRODUCT (hs6) FIXED EFFECTS
# Adapted from Neri-Leinè et al 2021
# ─────────────────────────────────────────────────────────────────────
start_ft_hs6 <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction (ft + hs6 FE)
cat("\n=== Neri-Leinè et al 2021: ft + hs6 FE ===\n")
f1_ft_hs6 <- c(
    "ln_export       ~ WB_EP_Depth | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth | ft + hs6",
    "ln_export_value ~ WB_EP_Depth | ft + hs6",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6"
)
stats1_ft_hs6 <- run_block(f1_ft_hs6, "Neri-Leinè et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_ft_hs6, cm_wb, "OLS_WB_No_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_ft_hs6)
gc()

# BLOCK 2: WB Interaction (ft + hs6 FE)
cat("\n=== WB Interaction (ft + hs6 FE) ===\n")
f2_ft_hs6 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export_value ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6"
)
stats2_ft_hs6 <- run_block(f2_ft_hs6, "Neri-Leinè et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_ft_hs6, cm_wb_int, "OLS_WB_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_ft_hs6)
gc()

# BLOCK 3: TREND No Interaction (ft + hs6 FE)
cat("\n=== TREND No Interaction (ft + hs6 FE) ===\n")
f3_ft_hs6 <- c(
    "ln_export       ~ TREND_EP_Count | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count | ft + hs6",
    "ln_export_value ~ TREND_EP_Count | ft + hs6",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6"
)
stats3_ft_hs6 <- run_block(f3_ft_hs6, "Neri-Leinè et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_ft_hs6, cm_trend, "OLS_TREND_No_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_ft_hs6)
gc()

# BLOCK 4: TREND Interaction (ft + hs6 FE)
cat("\n=== TREND Interaction (ft + hs6 FE) ===\n")
f4_ft_hs6 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export_value ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6"
)
stats4_ft_hs6 <- run_block(f4_ft_hs6, "Neri-Leinè et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_hs6, cm_trend_int, "OLS_TREND_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_hs6)
gc()

cat("\n=== COMPLETATO ft + hs6! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_ft_hs6.rds\n")
cat("Tempo totale ft + hs6:", now() - start_ft_hs6, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# FIRM-TIME (ft) + DESTINATION (country_code) FIXED EFFECTS
# Adapted from Lee et al 2021
# ─────────────────────────────────────────────────────────────────────
start_ft_dest <- now()
show_stats_ols <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction (ft + destination FE)
cat("\n=== Lee et al 2021: ft + destination FE ===\n")
f1_ft_dest <- c(
    "ln_export       ~ WB_EP_Depth | ft + country_code",
    "ln_export_qua   ~ WB_EP_Depth | ft + country_code",
    "ln_export_value ~ WB_EP_Depth | ft + country_code",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + country_code"
)
stats1_ft_dest <- run_block(f1_ft_dest, "Lee et al 2021 (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_ft_dest, cm_wb, "OLS_WB_No_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_ft_dest)
gc()

# BLOCK 2: WB Interaction (ft + destination FE)
cat("\n=== WB Interaction (ft + destination FE) ===\n")
f2_ft_dest <- c(
    "ln_export       ~ WB_EP_Depth * env_good | ft + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good | ft + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good | ft + country_code",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + country_code"
)
stats2_ft_dest <- run_block(f2_ft_dest, "Lee et al 2021 (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_ft_dest, cm_wb_int, "OLS_WB_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_ft_dest)
gc()

# BLOCK 3: TREND No Interaction (ft + destination FE)
cat("\n=== TREND No Interaction (ft + destination FE) ===\n")
f3_ft_dest <- c(
    "ln_export       ~ TREND_EP_Count | ft + country_code",
    "ln_export_qua   ~ TREND_EP_Count | ft + country_code",
    "ln_export_value ~ TREND_EP_Count | ft + country_code",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + country_code"
)
stats3_ft_dest <- run_block(f3_ft_dest, "Lee et al 2021 (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_ft_dest, cm_trend, "OLS_TREND_No_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_ft_dest)
gc()

# BLOCK 4: TREND Interaction (ft + destination FE)
cat("\n=== TREND Interaction (ft + destination FE) ===\n")
f4_ft_dest <- c(
    "ln_export       ~ TREND_EP_Count * env_good | ft + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good | ft + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good | ft + country_code",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + country_code"
)
stats4_ft_dest <- run_block(f4_ft_dest, "Lee et al 2021 (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_dest, cm_trend_int, "OLS_TREND_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_dest)
gc()

cat("\n=== COMPLETATO ft + destination! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_ft_dest.rds\n")
cat("Tempo totale ft + destination:", now() - start_ft_dest, "secondi\n")
