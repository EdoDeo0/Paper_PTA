##############################################################
## fpd + pt  —  struttura FE rimasta da 01_inference_fix.R  #
##############################################################
## Gira direttamente in sessione R (nessun callr), quindi un
## eventuale crash non si propaga a nessun processo padre.
##
## Dopo aver completato i 24 modelli, lancia bootstrap + ladder
## (che usano RDS già in cache per le altre 3 strutture FE).
##
## Uso:
##   Rscript New/Code/01c_fpd_pt.R
##
## Interrompibile e riavviabile: ogni modello viene salvato come
## .rds, quindi un riavvio riprende dall'ultimo completato.
##############################################################

library(fst); library(fixest); library(data.table)
library(here); library(lubridate); library(parallel)

threads_fst(1)
# fpd ha 26M+ gruppi: multi-thread OpenMP crashava con recursive gc invocation.
# nthreads=1 risolve il problema (diagnostica confermata su subset crescenti).
setFixest_nthreads(1L)

source(here("Code/Analysis/pta_functions.R"))

DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
OUT_DIR   <- here("New/Output/OLS")
NTHREADS  <- as.integer(detectCores() - 1)
SHOW      <- c("nobs", "r2", "n_clust")

CM_WB <- c(
  "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
  "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
CM_WB_INT <- c(
  "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
  "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
CM_TREND <- c(
  "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
  "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)
CM_TREND_INT <- c(
  "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
  "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)

FE  <- "fpd + pt"
LBL <- "fpd_pt"

dirs <- setup_output_dirs(OUT_DIR)

# ─── 4 blocchi in sessione diretta (nessun callr) ────────────────────────────
run_one <- function(block_label, cm, formulas, tex_name) {
  cat(sprintf("\n==================\n%s_%s\n==================\n", block_label, LBL))
  tex_path <- file.path(dirs$tables, tex_name)
  if (file.exists(tex_path)) {
    cat("  SKIP — tabella già presente\n"); return(invisible(NULL))
  }
  t0 <- now()
  # preload_block_data=FALSE: ogni formula carica solo le proprie colonne dall'FST.
  # Riduce il picco di RAM rispetto al caricamento bulk di tutte le colonne insieme.
  stats <- run_block(formulas, paste0(block_label, "_", LBL), "ols",
                     DATA_FILE, dirs$models,
                     vcov = ~country_code, requested_stats = SHOW,
                     preload_block_data = FALSE)
  make_table(stats, cm, tex_name, dirs$tables, digits = 5, show_stats = SHOW)
  gc(); gc()
  cat(sprintf("[OK] %s — %.1f min\n", tex_name, as.numeric(now() - t0, "mins")))
}

f_wb <- c(
  paste0("ln_export ~ WB_EP_Depth | ", FE),
  paste0("ln_export_qua ~ WB_EP_Depth | ", FE),
  paste0("ln_export_value ~ WB_EP_Depth | ", FE),
  paste0("ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", FE)
)
f_wb_int <- c(
  paste0("ln_export ~ WB_EP_Depth * env_good | ", FE),
  paste0("ln_export_qua ~ WB_EP_Depth * env_good | ", FE),
  paste0("ln_export_value ~ WB_EP_Depth * env_good | ", FE),
  paste0("ln_export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", FE)
)
f_trend <- c(
  paste0("ln_export ~ TREND_EP_Count | ", FE),
  paste0("ln_export_qua ~ TREND_EP_Count | ", FE),
  paste0("ln_export_value ~ TREND_EP_Count | ", FE),
  paste0("ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", FE)
)
f_trend_int <- c(
  paste0("ln_export ~ TREND_EP_Count * env_good | ", FE),
  paste0("ln_export_qua ~ TREND_EP_Count * env_good | ", FE),
  paste0("ln_export_value ~ TREND_EP_Count * env_good | ", FE),
  paste0("ln_export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", FE),
  paste0("ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", FE)
)

run_one("WB_NI",    CM_WB,       f_wb,       "OLS_WB_No_Interaction_fpd_pt.tex")
run_one("WB_Int",   CM_WB_INT,   f_wb_int,   "OLS_WB_Interaction_fpd_pt.tex")
run_one("TREND_NI", CM_TREND,    f_trend,    "OLS_TREND_No_Interaction_fpd_pt.tex")
run_one("TREND_Int",CM_TREND_INT,f_trend_int,"OLS_TREND_Interaction_fpd_pt.tex")

cat("\n========================================\n")
cat("fpd+pt completo. Avvia bootstrap + ladder:\n")
cat("  Rscript New/Code/01d_bootstrap_ladder.R\n")
cat("========================================\n")
