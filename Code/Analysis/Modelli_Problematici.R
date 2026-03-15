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

































#############################
###### PPML Estimation ######
#############################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## PPML Estimation without zeros fill-in (only positive export flows)
## using fepois from the fixest package.
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

# Set the number of threads for fst (adjust based on your CPU)
# threads_fst(8) # 8 threads is a good default for modern CPUs (especially for laptops), but adjust as needed


source(here("Code/Analysis/pta_functions.R"))

# Set your own data file path (dataset not tracked in the repo – file too large)
data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst") ## On Windows
out_dir <- here("Output/Analysis/PPML")
dirs <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


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



# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-DESTINATION (fpd) + FIRM-PRODUCT-TIME (fpt) FIXED EFFECTS
# ─────────────────────────────────────────────────────────────────────
start_fpd_fpt <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction - fpd + fpt FE
cat("\n=== WB No Interaction (fpd + fpt FE) ===\n")
f1_fpd_fpt <- c(
    "export  ~ WB_EP_Depth | fpd + fpt",
    "exp_qua ~ WB_EP_Depth | fpd + fpt",
    "uv_exp  ~ WB_EP_Depth | fpd + fpt",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt"
)
stats1_fpd_fpt <- run_block(f1_fpd_fpt, "WB No Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1_fpd_fpt, cm_wb, "PPML_WB_No_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_fpd_fpt)
gc()

# BLOCK 2: WB Interaction - fpd + fpt FE
cat("\n=== WB Interaction (fpd + fpt FE) ===\n")
f2_fpd_fpt <- c(
    "export  ~ WB_EP_Depth * env_good | fpd + fpt",
    "exp_qua ~ WB_EP_Depth * env_good | fpd + fpt",
    "uv_exp  ~ WB_EP_Depth * env_good | fpd + fpt",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt"
)
stats2_fpd_fpt <- run_block(f2_fpd_fpt, "WB Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2_fpd_fpt, cm_wb_int, "PPML_WB_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_fpd_fpt)
gc()

# BLOCK 3: TREND No Interaction - fpd + fpt FE
cat("\n=== TREND No Interaction (fpd + fpt FE) ===\n")
f3_fpd_fpt <- c(
    "export  ~ TREND_EP_Count | fpd + fpt",
    "exp_qua ~ TREND_EP_Count | fpd + fpt",
    "uv_exp  ~ TREND_EP_Count | fpd + fpt",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt"
)
stats3_fpd_fpt <- run_block(f3_fpd_fpt, "TREND No Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3_fpd_fpt, cm_trend, "PPML_TREND_No_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_fpd_fpt)
gc()

# BLOCK 4: TREND Interaction - fpd + fpt FE
cat("\n=== TREND Interaction (fpd + fpt FE) ===\n")
f4_fpd_fpt <- c(
    "export  ~ TREND_EP_Count * env_good | fpd + fpt",
    "exp_qua ~ TREND_EP_Count * env_good | fpd + fpt",
    "uv_exp  ~ TREND_EP_Count * env_good | fpd + fpt",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt"
)
stats4_fpd_fpt <- run_block(f4_fpd_fpt, "TREND Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4_fpd_fpt, cm_trend_int, "PPML_TREND_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_fpd_fpt)
gc()


cat("\n=== COMPLETATO fpd + fpt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_fpd_fpt.rds\n")
cat("Tempo totale fpd + fpt:", now() - start_fpd_fpt, "secondi\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-TIME (fpt) + PRODUCT-TIME (pt) + PRODUCT-DESTINATION (pd) [Crowley et al 2021]
# ─────────────────────────────────────────────────────────────────────
start_fpt_pt_pd <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

cat("\n=== Crowley et al 2021: fpt + pt + pd FE ===\n")
f1_fpt_pt_pd <- c(
    "export  ~ WB_EP_Depth | fpt + pt + pd",
    "exp_qua ~ WB_EP_Depth | fpt + pt + pd",
    "uv_exp  ~ WB_EP_Depth | fpt + pt + pd",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats1_fpt_pt_pd <- run_block(f1_fpt_pt_pd, "Crowley et al 2021: WB No Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1_fpt_pt_pd, cm_wb, "PPML_WB_No_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_fpt_pt_pd)
gc()


f2_fpt_pt_pd <- c(
    "export  ~ WB_EP_Depth * env_good | fpt + pt + pd",
    "exp_qua ~ WB_EP_Depth * env_good | fpt + pt + pd",
    "uv_exp  ~ WB_EP_Depth * env_good | fpt + pt + pd",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats2_fpt_pt_pd <- run_block(f2_fpt_pt_pd, "Crowley et al 2021: WB Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2_fpt_pt_pd, cm_wb_int, "PPML_WB_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_fpt_pt_pd)
gc()


f3_fpt_pt_pd <- c(
    "export  ~ TREND_EP_Count | fpt + pt + pd",
    "exp_qua ~ TREND_EP_Count | fpt + pt + pd",
    "uv_exp  ~ TREND_EP_Count | fpt + pt + pd",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats3_fpt_pt_pd <- run_block(f3_fpt_pt_pd, "Crowley et al 2021: TREND No Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3_fpt_pt_pd, cm_trend, "PPML_TREND_No_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_fpt_pt_pd)
gc()


f4_fpt_pt_pd <- c(
    "export  ~ TREND_EP_Count * env_good | fpt + pt + pd",
    "exp_qua ~ TREND_EP_Count * env_good | fpt + pt + pd",
    "uv_exp  ~ TREND_EP_Count * env_good | fpt + pt + pd",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd"
)
stats4_fpt_pt_pd <- run_block(f4_fpt_pt_pd, "Crowley et al 2021: TREND Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4_fpt_pt_pd, cm_trend_int, "PPML_TREND_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_fpt_pt_pd)
gc()

cat("\n=== COMPLETATO fpt + pt + pd! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_fpt_pt_pd.rds\n")
cat("Tempo totale fpt + pt + pd:", now() - start_fpt_pt_pd, "secondi\n")
