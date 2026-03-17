#####################################
###### OLS / REGHDFE — CEM Robustness
#####################################
##
## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## Replica tutte le stime di OLS_High_Dimensional_FE_v2.R sui dataset
## filtrati prodotti da Matching_Alternative.R.
##
## PARTE A — CEM full      (tutte le covariate, asia_dummy inclusa)
## PARTE B — CEM no_asia   (senza asia_dummy)
##
## Per ciascuna parte:
##   - legge  Output/Analysis/CEM_Robustness/CEM_<label>/data_cem_matched_<label>.fst
##   - scrive Output/Analysis/CEM_Robustness/CEM_<label>/OLS/

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

source(here("Code/Analysis/pta_functions.R"))

# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS (identici all'originale)
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

show_stats_ols <- c("nobs", "r2", "n_clust")


###########################################################################
######   PARTE A — CEM FULL                                           ######
###########################################################################

data_file <- here("Output/Analysis/CEM_Robustness/CEM_full/data_cem_matched_full.fst")
out_dir <- here("Output/Analysis/CEM_Robustness/CEM_full/OLS")
dirs <- setup_output_dirs(out_dir)

stopifnot("Dataset CEM full non trovato!" = file.exists(data_file))

# ─────────────────────────────────────────────────────────────────────
# A — FIRM-PRODUCT-DESTINATION (fpd) + TIME (year) FE
# ─────────────────────────────────────────────────────────────────────
start <- now()

cat("\n=== [FULL] WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1, cm_wb, "OLS_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1)
gc()

cat("\n=== [FULL] WB Interaction (fpd + year FE) ===\n")
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

cat("\n=== [FULL] TREND No Interaction (fpd + year FE) ===\n")
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

cat("\n=== [FULL] TREND Interaction (fpd + year FE) ===\n")
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

cat("\n=== [FULL] COMPLETATO fpd + year! Tempo:", round(as.numeric(now() - start, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# A — FIRM-PRODUCT-TIME (fpt) FE
# ─────────────────────────────────────────────────────────────────────
start_fpt <- now()

cat("\n=== [FULL] WB No Interaction (fpt FE) ===\n")
f1_fpt <- c(
    "ln_export       ~ WB_EP_Depth | fpt",
    "ln_export_qua   ~ WB_EP_Depth | fpt",
    "ln_export_value ~ WB_EP_Depth | fpt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt"
)
stats1_fpt <- run_block(f1_fpt, "WB No Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_fpt, cm_wb, "OLS_WB_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_fpt)
gc()

cat("\n=== [FULL] WB Interaction (fpt FE) ===\n")
f2_fpt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt"
)
stats2_fpt <- run_block(f2_fpt, "WB Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_fpt, cm_wb_int, "OLS_WB_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_fpt)
gc()

cat("\n=== [FULL] TREND No Interaction (fpt FE) ===\n")
f3_fpt <- c(
    "ln_export       ~ TREND_EP_Count | fpt",
    "ln_export_qua   ~ TREND_EP_Count | fpt",
    "ln_export_value ~ TREND_EP_Count | fpt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt"
)
stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_fpt, cm_trend, "OLS_TREND_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_fpt)
gc()

cat("\n=== [FULL] TREND Interaction (fpt FE) ===\n")
f4_fpt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt"
)
stats4_fpt <- run_block(f4_fpt, "TREND Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_fpt, cm_trend_int, "OLS_TREND_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_fpt)
gc()

cat("\n=== [FULL] COMPLETATO fpt! Tempo:", round(as.numeric(now() - start_fpt, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# A — PRODUCT-TIME (pt) FE
# ─────────────────────────────────────────────────────────────────────
start_pt <- now()

cat("\n=== [FULL] WB No Interaction (pt FE) ===\n")
f1_pt <- c(
    "ln_export       ~ WB_EP_Depth | pt",
    "ln_export_qua   ~ WB_EP_Depth | pt",
    "ln_export_value ~ WB_EP_Depth | pt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt"
)
stats1_pt <- run_block(f1_pt, "WB No Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_pt, cm_wb, "OLS_WB_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_pt)
gc()

cat("\n=== [FULL] WB Interaction (pt FE) ===\n")
f2_pt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | pt",
    "ln_export_value ~ WB_EP_Depth * env_good | pt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt"
)
stats2_pt <- run_block(f2_pt, "WB Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_pt, cm_wb_int, "OLS_WB_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_pt)
gc()

cat("\n=== [FULL] TREND No Interaction (pt FE) ===\n")
f3_pt <- c(
    "ln_export       ~ TREND_EP_Count | pt",
    "ln_export_qua   ~ TREND_EP_Count | pt",
    "ln_export_value ~ TREND_EP_Count | pt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt"
)
stats3_pt <- run_block(f3_pt, "TREND No Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_pt, cm_trend, "OLS_TREND_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_pt)
gc()

cat("\n=== [FULL] TREND Interaction (pt FE) ===\n")
f4_pt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | pt",
    "ln_export_value ~ TREND_EP_Count * env_good | pt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt"
)
stats4_pt <- run_block(f4_pt, "TREND Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_pt, cm_trend_int, "OLS_TREND_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_pt)
gc()

cat("\n=== [FULL] COMPLETATO pt! Tempo:", round(as.numeric(now() - start_pt, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# A — FIRM-PRODUCT-DESTINATION (fpd) + FIRM-PRODUCT-TIME (fpt) FE
# ─────────────────────────────────────────────────────────────────────
start_fpd_fpt <- now()

cat("\n=== [FULL] WB No Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [FULL] WB Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [FULL] TREND No Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [FULL] TREND Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [FULL] COMPLETATO fpd & fpt! Tempo:", round(as.numeric(now() - start_fpd_fpt, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# A — FIRM-PRODUCT-TIME (fpt) + PRODUCT-TIME (pt) + PRODUCT-DESTINATION (pd) FE
# Adapted from Crowley et al 2021
# ─────────────────────────────────────────────────────────────────────
start_fpt_pt_pd <- now()

cat("\n=== [FULL] Crowley et al 2021: fpt + pt + pd FE ===\n")
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

cat("\n=== [FULL] WB Interaction (fpt + pt + pd FE) ===\n")
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

cat("\n=== [FULL] TREND No Interaction (fpt + pt + pd FE) ===\n")
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

cat("\n=== [FULL] TREND Interaction (fpt + pt + pd FE) ===\n")
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

cat("\n=== [FULL] COMPLETATO fpt + pt + pd! Tempo:", round(as.numeric(now() - start_fpt_pt_pd, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# A — FIRM-TIME (ft) + PRODUCT (hs6) FE
# Adapted from Neri-Leine et al 2021
# ─────────────────────────────────────────────────────────────────────
start_ft_hs6 <- now()

cat("\n=== [FULL] Neri-Leine et al 2021: ft + hs6 FE ===\n")
f1_ft_hs6 <- c(
    "ln_export       ~ WB_EP_Depth | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth | ft + hs6",
    "ln_export_value ~ WB_EP_Depth | ft + hs6",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6"
)
stats1_ft_hs6 <- run_block(f1_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_ft_hs6, cm_wb, "OLS_WB_No_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_ft_hs6)
gc()

cat("\n=== [FULL] WB Interaction (ft + hs6 FE) ===\n")
f2_ft_hs6 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export_value ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6"
)
stats2_ft_hs6 <- run_block(f2_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_ft_hs6, cm_wb_int, "OLS_WB_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_ft_hs6)
gc()

cat("\n=== [FULL] TREND No Interaction (ft + hs6 FE) ===\n")
f3_ft_hs6 <- c(
    "ln_export       ~ TREND_EP_Count | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count | ft + hs6",
    "ln_export_value ~ TREND_EP_Count | ft + hs6",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6"
)
stats3_ft_hs6 <- run_block(f3_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_ft_hs6, cm_trend, "OLS_TREND_No_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_ft_hs6)
gc()

cat("\n=== [FULL] TREND Interaction (ft + hs6 FE) ===\n")
f4_ft_hs6 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export_value ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6"
)
stats4_ft_hs6 <- run_block(f4_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_hs6, cm_trend_int, "OLS_TREND_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_hs6)
gc()

cat("\n=== [FULL] COMPLETATO ft + hs6! Tempo:", round(as.numeric(now() - start_ft_hs6, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# A — FIRM-TIME (ft) + DESTINATION (country_code) FE
# Adapted from Lee et al 2021
# ─────────────────────────────────────────────────────────────────────
start_ft_dest <- now()

cat("\n=== [FULL] Lee et al 2021: ft + destination FE ===\n")
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

cat("\n=== [FULL] WB Interaction (ft + destination FE) ===\n")
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

cat("\n=== [FULL] TREND No Interaction (ft + destination FE) ===\n")
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

cat("\n=== [FULL] TREND Interaction (ft + destination FE) ===\n")
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

cat("\n=== [FULL] COMPLETATO ft + destination! Tempo:", round(as.numeric(now() - start_ft_dest, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# A — FIRM-TIME (ft) + TIME (year) + DESTINATION (country_code) FE
# Adapted from Neri-Leine et al 2023
# ─────────────────────────────────────────────────────────────────────
start_ft_year_dest <- now()

cat("\n=== [FULL] Neri-Leine et al 2023: ft + year + destination FE ===\n")
f1_ft_year_dest <- c(
    "ln_export       ~ WB_EP_Depth | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth | ft + year + country_code",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats1_ft_year_dest <- run_block(f1_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_ft_year_dest, cm_wb, "OLS_WB_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_ft_year_dest)
gc()

cat("\n=== [FULL] WB Interaction (ft + year + destination FE) ===\n")
f2_ft_year_dest <- c(
    "ln_export       ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats2_ft_year_dest <- run_block(f2_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_ft_year_dest, cm_wb_int, "OLS_WB_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_ft_year_dest)
gc()

cat("\n=== [FULL] TREND No Interaction (ft + year + destination FE) ===\n")
f3_ft_year_dest <- c(
    "ln_export       ~ TREND_EP_Count | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count | ft + year + country_code",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats3_ft_year_dest <- run_block(f3_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_ft_year_dest, cm_trend, "OLS_TREND_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_ft_year_dest)
gc()

cat("\n=== [FULL] TREND Interaction (ft + year + destination FE) ===\n")
f4_ft_year_dest <- c(
    "ln_export       ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats4_ft_year_dest <- run_block(f4_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_year_dest, cm_trend_int, "OLS_TREND_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_year_dest)
gc()

cat("\n=== [FULL] COMPLETATO ft + year + destination! Tempo:", round(as.numeric(now() - start_ft_year_dest, units = "mins"), 1), "min ===\n")
cat("\n*** PARTE A (CEM FULL) COMPLETATA ***\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")


###########################################################################
######   PARTE B — CEM NO_ASIA                                        ######
###########################################################################

data_file <- here("Output/Analysis/CEM_Robustness/CEM_no_asia/data_cem_matched_no_asia.fst")
out_dir <- here("Output/Analysis/CEM_Robustness/CEM_no_asia/OLS")
dirs <- setup_output_dirs(out_dir)

stopifnot("Dataset CEM no_asia non trovato!" = file.exists(data_file))

# ─────────────────────────────────────────────────────────────────────
# B — FIRM-PRODUCT-DESTINATION (fpd) + TIME (year) FE
# ─────────────────────────────────────────────────────────────────────
start <- now()

cat("\n=== [NO_ASIA] WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction (fpd + year FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1, cm_wb, "OLS_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1)
gc()

cat("\n=== [NO_ASIA] WB Interaction (fpd + year FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND No Interaction (fpd + year FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND Interaction (fpd + year FE) ===\n")
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

cat("\n=== [NO_ASIA] COMPLETATO fpd + year! Tempo:", round(as.numeric(now() - start, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# B — FIRM-PRODUCT-TIME (fpt) FE
# ─────────────────────────────────────────────────────────────────────
start_fpt <- now()

cat("\n=== [NO_ASIA] WB No Interaction (fpt FE) ===\n")
f1_fpt <- c(
    "ln_export       ~ WB_EP_Depth | fpt",
    "ln_export_qua   ~ WB_EP_Depth | fpt",
    "ln_export_value ~ WB_EP_Depth | fpt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt"
)
stats1_fpt <- run_block(f1_fpt, "WB No Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_fpt, cm_wb, "OLS_WB_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_fpt)
gc()

cat("\n=== [NO_ASIA] WB Interaction (fpt FE) ===\n")
f2_fpt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt"
)
stats2_fpt <- run_block(f2_fpt, "WB Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_fpt, cm_wb_int, "OLS_WB_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_fpt)
gc()

cat("\n=== [NO_ASIA] TREND No Interaction (fpt FE) ===\n")
f3_fpt <- c(
    "ln_export       ~ TREND_EP_Count | fpt",
    "ln_export_qua   ~ TREND_EP_Count | fpt",
    "ln_export_value ~ TREND_EP_Count | fpt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt"
)
stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_fpt, cm_trend, "OLS_TREND_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_fpt)
gc()

cat("\n=== [NO_ASIA] TREND Interaction (fpt FE) ===\n")
f4_fpt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt"
)
stats4_fpt <- run_block(f4_fpt, "TREND Interaction (fpt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_fpt, cm_trend_int, "OLS_TREND_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_fpt)
gc()

cat("\n=== [NO_ASIA] COMPLETATO fpt! Tempo:", round(as.numeric(now() - start_fpt, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# B — PRODUCT-TIME (pt) FE
# ─────────────────────────────────────────────────────────────────────
start_pt <- now()

cat("\n=== [NO_ASIA] WB No Interaction (pt FE) ===\n")
f1_pt <- c(
    "ln_export       ~ WB_EP_Depth | pt",
    "ln_export_qua   ~ WB_EP_Depth | pt",
    "ln_export_value ~ WB_EP_Depth | pt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt"
)
stats1_pt <- run_block(f1_pt, "WB No Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_pt, cm_wb, "OLS_WB_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_pt)
gc()

cat("\n=== [NO_ASIA] WB Interaction (pt FE) ===\n")
f2_pt <- c(
    "ln_export       ~ WB_EP_Depth * env_good | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | pt",
    "ln_export_value ~ WB_EP_Depth * env_good | pt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt"
)
stats2_pt <- run_block(f2_pt, "WB Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_pt, cm_wb_int, "OLS_WB_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_pt)
gc()

cat("\n=== [NO_ASIA] TREND No Interaction (pt FE) ===\n")
f3_pt <- c(
    "ln_export       ~ TREND_EP_Count | pt",
    "ln_export_qua   ~ TREND_EP_Count | pt",
    "ln_export_value ~ TREND_EP_Count | pt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt"
)
stats3_pt <- run_block(f3_pt, "TREND No Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_pt, cm_trend, "OLS_TREND_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_pt)
gc()

cat("\n=== [NO_ASIA] TREND Interaction (pt FE) ===\n")
f4_pt <- c(
    "ln_export       ~ TREND_EP_Count * env_good | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | pt",
    "ln_export_value ~ TREND_EP_Count * env_good | pt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt"
)
stats4_pt <- run_block(f4_pt, "TREND Interaction (pt FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_pt, cm_trend_int, "OLS_TREND_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_pt)
gc()

cat("\n=== [NO_ASIA] COMPLETATO pt! Tempo:", round(as.numeric(now() - start_pt, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# B — FIRM-PRODUCT-DESTINATION (fpd) + FIRM-PRODUCT-TIME (fpt) FE
# ─────────────────────────────────────────────────────────────────────
start_fpd_fpt <- now()

cat("\n=== [NO_ASIA] WB No Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [NO_ASIA] WB Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND No Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND Interaction (fpd & fpt FE) ===\n")
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

cat("\n=== [NO_ASIA] COMPLETATO fpd & fpt! Tempo:", round(as.numeric(now() - start_fpd_fpt, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# B — FIRM-PRODUCT-TIME (fpt) + PRODUCT-TIME (pt) + PRODUCT-DESTINATION (pd) FE
# Adapted from Crowley et al 2021
# ─────────────────────────────────────────────────────────────────────
start_fpt_pt_pd <- now()

cat("\n=== [NO_ASIA] Crowley et al 2021: fpt + pt + pd FE ===\n")
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

cat("\n=== [NO_ASIA] WB Interaction (fpt + pt + pd FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND No Interaction (fpt + pt + pd FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND Interaction (fpt + pt + pd FE) ===\n")
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

cat("\n=== [NO_ASIA] COMPLETATO fpt + pt + pd! Tempo:", round(as.numeric(now() - start_fpt_pt_pd, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# B — FIRM-TIME (ft) + PRODUCT (hs6) FE
# Adapted from Neri-Leine et al 2021
# ─────────────────────────────────────────────────────────────────────
start_ft_hs6 <- now()

cat("\n=== [NO_ASIA] Neri-Leine et al 2021: ft + hs6 FE ===\n")
f1_ft_hs6 <- c(
    "ln_export       ~ WB_EP_Depth | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth | ft + hs6",
    "ln_export_value ~ WB_EP_Depth | ft + hs6",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + hs6"
)
stats1_ft_hs6 <- run_block(f1_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_ft_hs6, cm_wb, "OLS_WB_No_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_ft_hs6)
gc()

cat("\n=== [NO_ASIA] WB Interaction (ft + hs6 FE) ===\n")
f2_ft_hs6 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export_value ~ WB_EP_Depth * env_good | ft + hs6",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + hs6"
)
stats2_ft_hs6 <- run_block(f2_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_ft_hs6, cm_wb_int, "OLS_WB_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_ft_hs6)
gc()

cat("\n=== [NO_ASIA] TREND No Interaction (ft + hs6 FE) ===\n")
f3_ft_hs6 <- c(
    "ln_export       ~ TREND_EP_Count | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count | ft + hs6",
    "ln_export_value ~ TREND_EP_Count | ft + hs6",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + hs6"
)
stats3_ft_hs6 <- run_block(f3_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_ft_hs6, cm_trend, "OLS_TREND_No_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_ft_hs6)
gc()

cat("\n=== [NO_ASIA] TREND Interaction (ft + hs6 FE) ===\n")
f4_ft_hs6 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export_value ~ TREND_EP_Count * env_good | ft + hs6",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + hs6"
)
stats4_ft_hs6 <- run_block(f4_ft_hs6, "Neri-Leine et al 2021 (ft + hs6 FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_hs6, cm_trend_int, "OLS_TREND_Interaction_ft_hs6.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_hs6)
gc()

cat("\n=== [NO_ASIA] COMPLETATO ft + hs6! Tempo:", round(as.numeric(now() - start_ft_hs6, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# B — FIRM-TIME (ft) + DESTINATION (country_code) FE
# Adapted from Lee et al 2021
# ─────────────────────────────────────────────────────────────────────
start_ft_dest <- now()

cat("\n=== [NO_ASIA] Lee et al 2021: ft + destination FE ===\n")
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

cat("\n=== [NO_ASIA] WB Interaction (ft + destination FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND No Interaction (ft + destination FE) ===\n")
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

cat("\n=== [NO_ASIA] TREND Interaction (ft + destination FE) ===\n")
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

cat("\n=== [NO_ASIA] COMPLETATO ft + destination! Tempo:", round(as.numeric(now() - start_ft_dest, units = "mins"), 1), "min ===\n")

# ─────────────────────────────────────────────────────────────────────
# B — FIRM-TIME (ft) + TIME (year) + DESTINATION (country_code) FE
# Adapted from Neri-Leine et al 2023
# ─────────────────────────────────────────────────────────────────────
start_ft_year_dest <- now()

cat("\n=== [NO_ASIA] Neri-Leine et al 2023: ft + year + destination FE ===\n")
f1_ft_year_dest <- c(
    "ln_export       ~ WB_EP_Depth | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth | ft + year + country_code",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats1_ft_year_dest <- run_block(f1_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, save_mode = "stats", requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats1_ft_year_dest, cm_wb, "OLS_WB_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats1_ft_year_dest)
gc()

cat("\n=== [NO_ASIA] WB Interaction (ft + year + destination FE) ===\n")
f2_ft_year_dest <- c(
    "ln_export       ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good | ft + year + country_code",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats2_ft_year_dest <- run_block(f2_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats2_ft_year_dest, cm_wb_int, "OLS_WB_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats2_ft_year_dest)
gc()

cat("\n=== [NO_ASIA] TREND No Interaction (ft + year + destination FE) ===\n")
f3_ft_year_dest <- c(
    "ln_export       ~ TREND_EP_Count | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count | ft + year + country_code",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats3_ft_year_dest <- run_block(f3_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats3_ft_year_dest, cm_trend, "OLS_TREND_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats3_ft_year_dest)
gc()

cat("\n=== [NO_ASIA] TREND Interaction (ft + year + destination FE) ===\n")
f4_ft_year_dest <- c(
    "ln_export       ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good | ft + year + country_code",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + country_code"
)
stats4_ft_year_dest <- run_block(f4_ft_year_dest, "Neri-Leine et al 2023 (ft + year + destination FE)", "ols", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ols, preload_block_data = TRUE)
make_table(stats4_ft_year_dest, cm_trend_int, "OLS_TREND_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ols)
rm(stats4_ft_year_dest)
gc()

cat("\n=== [NO_ASIA] COMPLETATO ft + year + destination! Tempo:", round(as.numeric(now() - start_ft_year_dest, units = "mins"), 1), "min ===\n")
cat("\n*** PARTE B (CEM NO_ASIA) COMPLETATA ***\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")

cat("\n\n=== OLS CEM ROBUSTNESS — COMPLETATO! ===\n")
