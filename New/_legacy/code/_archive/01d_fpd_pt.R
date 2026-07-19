## Fase 1 — Section 4: FPD + PT  [cluster: ~country_code]
## Eseguire come processo separato via run_fase1.ps1

rm(list = ls())
library(fst); library(fixest); library(data.table); library(here); library(lubridate)
threads_fst(1); setFixest_nthreads(4)
source(here("Code/Analysis/pta_functions.R"))

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("New/Output/OLS")
dirs      <- setup_output_dirs(out_dir)
stopifnot("Data file not found!" = file.exists(data_file))

show_stats_ols <- c("nobs", "r2", "n_clust")

cm_wb      <- c("WB_EP_Depth"  = "\\textit{EPDepth\\textsubscript{dt}}", "tariffs" = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}", "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}")
cm_wb_int  <- c("WB_EP_Depth"  = "\\textit{EPDepth\\textsubscript{dt}}", "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}", "tariffs" = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}", "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}")
cm_trend   <- c("TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}", "tariffs" = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}", "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}")
cm_trend_int <- c("TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}", "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}", "tariffs" = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}", "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}")

start <- now()

run_block(
  c("ln_export ~ WB_EP_Depth | fpd + pt", "ln_export_qua ~ WB_EP_Depth | fpd + pt", "ln_export_value ~ WB_EP_Depth | fpd + pt",
    "ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt", "ln_export_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt", "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt"),
  "WB_NI_fpd_pt", "ols", data_file, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb, "OLS_WB_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export ~ WB_EP_Depth * env_good | fpd + pt", "ln_export_qua ~ WB_EP_Depth * env_good | fpd + pt", "ln_export_value ~ WB_EP_Depth * env_good | fpd + pt",
    "ln_export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt", "ln_export_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt", "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt"),
  "WB_Int_fpd_pt", "ols", data_file, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb_int, "OLS_WB_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export ~ TREND_EP_Count | fpd + pt", "ln_export_qua ~ TREND_EP_Count | fpd + pt", "ln_export_value ~ TREND_EP_Count | fpd + pt",
    "ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt", "ln_export_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt", "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt"),
  "TREND_NI_fpd_pt", "ols", data_file, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend, "OLS_TREND_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export ~ TREND_EP_Count * env_good | fpd + pt", "ln_export_qua ~ TREND_EP_Count * env_good | fpd + pt", "ln_export_value ~ TREND_EP_Count * env_good | fpd + pt",
    "ln_export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt", "ln_export_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt", "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt"),
  "TREND_Int_fpd_pt", "ols", data_file, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend_int, "OLS_TREND_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

cat(sprintf("\n=== DONE 01d fpd+pt | %.1f min ===\n", as.numeric(now() - start, units = "mins")))
