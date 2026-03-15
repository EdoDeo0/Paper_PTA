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
stats1_ft_year_dest <- run_block(f1_ft_year_dest, "Neri-Leinè et al 2023: WB No Interaction (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats2_ft_year_dest <- run_block(f2_ft_year_dest, "Neri-Leinè et al 2023: WB Interaction (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats3_ft_year_dest <- run_block(f3_ft_year_dest, "Neri-Leinè et al 2023: TREND No Interaction (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats4_ft_year_dest <- run_block(f4_ft_year_dest, "Neri-Leinè et al 2023: TREND Interaction (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats1_ft_hs6 <- run_block(f1_ft_hs6, "Neri-Leinè et al 2021: WB No Interaction (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats2_ft_hs6 <- run_block(f2_ft_hs6, "Neri-Leinè et al 2021: WB Interaction (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats3_ft_hs6 <- run_block(f3_ft_hs6, "Neri-Leinè et al 2021: TREND No Interaction (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats4_ft_hs6 <- run_block(f4_ft_hs6, "Neri-Leinè et al 2021: TREND Interaction (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats1_ft_dest <- run_block(f1_ft_dest, "Lee et al 2021: WB No Interaction (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats2_ft_dest <- run_block(f2_ft_dest, "Lee et al 2021: WB Interaction (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats3_ft_dest <- run_block(f3_ft_dest, "Lee et al 2021: TREND No Interaction (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
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
stats4_ft_dest <- run_block(f4_ft_dest, "Lee et al 2021: TREND Interaction (ft + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_dest, cm_trend_int, "OLS_TREND_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_dest)
gc()

cat("\n=== COMPLETATO ft + destination! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 OLS_*_ft_dest.rds\n")
cat("Tempo totale ft + destination:", now() - start_ft_dest, "secondi\n")
