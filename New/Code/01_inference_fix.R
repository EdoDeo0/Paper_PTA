########################################################
###### Fase 1 — Inferenza e igiene delle specifiche ####
########################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## Obiettivi (ROADMAP §Fase 1):
##   1. Clustering uniforme a country_code su tutte e 4 le strutture FE
##      (corregge l'eccezione ~pdt della specifica fpd+year)
##   2. Campione comune: baseline e "con controlli" stimati sullo stesso N
##      (pre-filtraggio su !is.na(tariffs) & !is.na(ln_hhi_baci))
##   3. Wild cluster bootstrap sulla specifica principale (fpt+fpd)
##   4. Ladder table diagnostica: effetto EP × ln_export per ogni struttura FE
##
## Output: New/Output/OLS/
## Dataset originale: MAI modificato — usato solo in lettura.
## Campione comune: scritto una volta in New/Data/common_sample.fst.

rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

# Su Windows con dataset grandi: fst single-thread evita conflitti di allocatore con fixest.
# fixest usa tutti i core meno uno per il demeaning HDFE (parte CPU-bound).
threads_fst(1)
setFixest_nthreads(max(1L, parallel::detectCores() - 1L))

if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
  stop("Pacchetto mancante: install.packages('fwildclusterboot')")
}
library(fwildclusterboot)

# Usa la libreria condivisa originale (nessuna modifica necessaria per Fase 1)
source(here("Code/Analysis/pta_functions.R"))

data_file     <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
common_sample <- here("New/Data/common_sample.fst")
out_dir       <- here("New/Output/OLS")
dirs          <- setup_output_dirs(out_dir)

stopifnot("Data file not found!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# STEP 0: CAMPIONE COMUNE
# Filtra NA nei controlli: baseline e "con controlli" girano sullo stesso N.
# Scritto una sola volta; le run successive lo riutilizzano.
# ─────────────────────────────────────────────────────────────────────

if (!file.exists(common_sample)) {
  cat("Building common sample (filter !is.na(tariffs) & !is.na(ln_hhi_baci))...\n")
  cols_needed <- c(
    "ln_export", "ln_export_qua", "ln_export_value",
    "WB_EP_Depth", "TREND_EP_Count",
    "tariffs", "ln_hhi_baci", "env_good",
    "fpd", "fpt", "pd", "pt", "dt",
    "country_code", "year"
  )
  d <- as.data.table(read_fst(data_file, columns = cols_needed))
  n_before <- nrow(d)
  d <- d[!is.na(tariffs) & !is.na(ln_hhi_baci)]
  cat(sprintf("  Rows before: %s | after: %s | dropped: %s\n",
              format(n_before, big.mark = ","),
              format(nrow(d),  big.mark = ","),
              format(n_before - nrow(d), big.mark = ",")))
  write_fst(d, common_sample, compress = 50)
  rm(d, n_before); gc()
  cat("Common sample saved:", common_sample, "\n")
} else {
  cat("Common sample already exists, skipping.\n")
}


# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────

cm_wb <- c(
  "WB_EP_Depth"  = "\\textit{EPDepth\\textsubscript{dt}}",
  "tariffs"      = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"  = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
  "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
  "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
  "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
  "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend_int <- c(
  "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
  "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)

show_stats_ols <- c("nobs", "r2", "n_clust")


# ─────────────────────────────────────────────────────────────────────
# SECTION 1: FPD + YEAR  [cluster: ~country_code — era: ~pdt]
# ─────────────────────────────────────────────────────────────────────
start <- now()

run_block(
  c("ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"),
  "WB_NI_fpd_year", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb, "OLS_WB_No_Interaction_fpd_year.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"),
  "WB_Int_fpd_year", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb_int, "OLS_WB_Interaction_fpd_year.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"),
  "TREND_NI_fpd_year", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend, "OLS_TREND_No_Interaction_fpd_year.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"),
  "TREND_Int_fpd_year", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend_int, "OLS_TREND_Interaction_fpd_year.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

cat(sprintf("\n=== DONE fpd + year | Time: %.1f min ===\n", as.numeric(now() - start, units = "mins")))


# ─────────────────────────────────────────────────────────────────────
# SECTION 2: FPT + PD  [cluster: ~country_code — era: ~dt]
# ─────────────────────────────────────────────────────────────────────
start <- now()

run_block(
  c("ln_export       ~ WB_EP_Depth | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth | fpt + pd",
    "ln_export_value ~ WB_EP_Depth | fpt + pd",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd"),
  "WB_NI_fpt_pd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb, "OLS_WB_No_Interaction_fpt_pd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ WB_EP_Depth * env_good | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt + pd",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt + pd",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd"),
  "WB_Int_fpt_pd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb_int, "OLS_WB_Interaction_fpt_pd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count | fpt + pd",
    "ln_export_value ~ TREND_EP_Count | fpt + pd",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd"),
  "TREND_NI_fpt_pd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend, "OLS_TREND_No_Interaction_fpt_pd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count * env_good | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt + pd",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt + pd",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd"),
  "TREND_Int_fpt_pd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend_int, "OLS_TREND_Interaction_fpt_pd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

cat(sprintf("\n=== DONE fpt + pd | Time: %.1f min ===\n", as.numeric(now() - start, units = "mins")))


# ─────────────────────────────────────────────────────────────────────
# SECTION 3: FPT + FPD  [cluster: ~country_code — SPECIFICA PRINCIPALE]
# ─────────────────────────────────────────────────────────────────────
start <- now()

run_block(
  c("ln_export       ~ WB_EP_Depth | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth | fpt + fpd",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd"),
  "WB_NI_fpt_fpd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb, "OLS_WB_No_Interaction_fpt_fpd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ WB_EP_Depth * env_good | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth * env_good | fpt + fpd",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd"),
  "WB_Int_fpt_fpd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb_int, "OLS_WB_Interaction_fpt_fpd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count | fpt + fpd",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd"),
  "TREND_NI_fpt_fpd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend, "OLS_TREND_No_Interaction_fpt_fpd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count * env_good | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count * env_good | fpt + fpd",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd"),
  "TREND_Int_fpt_fpd", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend_int, "OLS_TREND_Interaction_fpt_fpd.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

cat(sprintf("\n=== DONE fpt + fpd | Time: %.1f min ===\n", as.numeric(now() - start, units = "mins")))


# ─────────────────────────────────────────────────────────────────────
# SECTION 4: FPD + PT  [cluster: ~country_code — era: ~dt]
# ─────────────────────────────────────────────────────────────────────
start <- now()

run_block(
  c("ln_export       ~ WB_EP_Depth | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth | fpd + pt",
    "ln_export_value ~ WB_EP_Depth | fpd + pt",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt"),
  "WB_NI_fpd_pt", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb, "OLS_WB_No_Interaction_fpd_pt.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ WB_EP_Depth * env_good | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + pt",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + pt",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt"),
  "WB_Int_fpd_pt", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_wb_int, "OLS_WB_Interaction_fpd_pt.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count | fpd + pt",
    "ln_export_value ~ TREND_EP_Count | fpd + pt",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt"),
  "TREND_NI_fpd_pt", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend, "OLS_TREND_No_Interaction_fpd_pt.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

run_block(
  c("ln_export       ~ TREND_EP_Count * env_good | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + pt",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + pt",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt"),
  "TREND_Int_fpd_pt", "ols", common_sample, dirs$models,
  vcov = ~country_code, requested_stats = show_stats_ols, preload_block_data = TRUE
) |> (\(s) make_table(s, cm_trend_int, "OLS_TREND_Interaction_fpd_pt.tex",
                       dirs$tables, digits = 5, show_stats = show_stats_ols))()
gc()

cat(sprintf("\n=== DONE fpd + pt | Time: %.1f min ===\n", as.numeric(now() - start, units = "mins")))


# ─────────────────────────────────────────────────────────────────────
# SECTION 5: WILD CLUSTER BOOTSTRAP — specifica fpt + fpd
#
# Solo ln_export, WB e TREND, baseline e con controlli (4 modelli).
# Richiede lean = FALSE: il modello completo resta in memoria per boottest.
# Con ~30-40M righe il peak di RAM sarà elevato — eseguire su Windows.
# I risultati sono salvati in New/Output/OLS/Bootstrap/ e ricaricabili.
# ─────────────────────────────────────────────────────────────────────
cat("\n=== Wild bootstrap (fpt + fpd, ln_export, B=9999) ===\n")

boot_dir <- file.path(out_dir, "Bootstrap")
if (!dir.exists(boot_dir)) dir.create(boot_dir, recursive = TRUE)

d_boot <- as.data.table(read_fst(common_sample, columns = c(
  "ln_export", "WB_EP_Depth", "TREND_EP_Count",
  "tariffs", "ln_hhi_baci",
  "fpt", "fpd", "country_code"
)))

boot_specs <- list(
  wb_baseline    = list(f = "ln_export ~ WB_EP_Depth | fpt + fpd",
                        param = "WB_EP_Depth"),
  wb_controls    = list(f = "ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
                        param = "WB_EP_Depth"),
  trend_baseline = list(f = "ln_export ~ TREND_EP_Count | fpt + fpd",
                        param = "TREND_EP_Count"),
  trend_controls = list(f = "ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
                        param = "TREND_EP_Count")
)

boot_results <- lapply(names(boot_specs), function(nm) {
  out_path <- file.path(boot_dir, paste0("boot_", nm, ".rds"))
  if (file.exists(out_path)) {
    cat(sprintf("  [SKIP] %s\n", nm))
    return(readRDS(out_path))
  }
  spec <- boot_specs[[nm]]
  cat(sprintf("  Estimating: %s\n", spec$f))
  m <- feols(as.formula(spec$f), data = d_boot,
             cluster = ~country_code, lean = FALSE)
  cat(sprintf("  Bootstrapping: %s (B=9999)...\n", nm))
  br <- boottest(m, param = spec$param, clustid = "country_code",
                 B = 9999, seed = 42)
  res <- list(
    coef  = coef(m)[spec$param],
    se    = se(m)[spec$param],
    pval  = pvalue(m)[spec$param],
    boot  = tidy(br)   # data.frame: estimate, p.value, conf.low, conf.high
  )
  saveRDS(res, out_path)
  rm(m, br); gc()
  res
})
names(boot_results) <- names(boot_specs)

rm(d_boot); gc()

# Stampa e salva summary
cat("\n--- Wild Bootstrap Results ---\n")
boot_summary <- do.call(rbind, lapply(names(boot_results), function(nm) {
  r <- boot_results[[nm]]
  b <- r$boot
  cat(sprintf("  %-20s | coef: %9.6f | SE(clust): %9.6f | p(OLS): %.4f | p(WCR): %.4f | 95%%CI: [%9.6f, %9.6f]\n",
              nm, r$coef, r$se, r$pval, b$p.value, b$conf.low, b$conf.high))
  data.frame(spec = nm, coef = r$coef, se_cluster = r$se, p_ols = r$pval,
             p_wcr = b$p.value, ci_lo = b$conf.low, ci_hi = b$conf.high,
             stringsAsFactors = FALSE)
}))
write.csv(boot_summary, file.path(boot_dir, "bootstrap_summary.csv"), row.names = FALSE)
cat("[OK] bootstrap_summary.csv\n")


# ─────────────────────────────────────────────────────────────────────
# SECTION 6: LADDER TABLE
# Coefficiente EP × ln_export per ogni struttura FE (baseline e controlli).
# Righe = 4 strutture FE; colonne = WB baseline / WB ctrl / TREND baseline / TREND ctrl.
# Mostra la "firma di selezione": l'effetto si azzera salendo in saturazione.
# ─────────────────────────────────────────────────────────────────────
cat("\n=== Building ladder table ===\n")

# I modelli .rds sono nominati: OLS_{block_name}_{i}.rds
# Formula 1 = ln_export baseline; formula 4 = ln_export con controlli
load_rds <- function(block_name, i) {
  path <- file.path(dirs$models, sprintf("OLS_%s_%d.rds", block_name, i))
  if (!file.exists(path)) { warning("Not found: ", path); return(NULL) }
  readRDS(path)
}

fe_specs <- data.frame(
  label      = c("\\textit{fpd} + \\textit{t}",
                 "\\textit{fpt} + \\textit{pd}",
                 "\\textit{fpt} + \\textit{fpd}",
                 "\\textit{fpd} + \\textit{pt}"),
  wb_block   = c("WB_NI_fpd_year", "WB_NI_fpt_pd", "WB_NI_fpt_fpd", "WB_NI_fpd_pt"),
  trend_block= c("TREND_NI_fpd_year","TREND_NI_fpt_pd","TREND_NI_fpt_fpd","TREND_NI_fpd_pt"),
  stringsAsFactors = FALSE
)

ladder_rows <- lapply(seq_len(nrow(fe_specs)), function(j) {
  wb_b <- load_rds(fe_specs$wb_block[j],    1)
  wb_c <- load_rds(fe_specs$wb_block[j],    4)
  tr_b <- load_rds(fe_specs$trend_block[j], 1)
  tr_c <- load_rds(fe_specs$trend_block[j], 4)
  list(
    label    = fe_specs$label[j],
    wb_coef_b = wb_b$coefs["WB_EP_Depth"],   wb_se_b = wb_b$se["WB_EP_Depth"],   wb_p_b = wb_b$pval["WB_EP_Depth"],
    wb_coef_c = wb_c$coefs["WB_EP_Depth"],   wb_se_c = wb_c$se["WB_EP_Depth"],   wb_p_c = wb_c$pval["WB_EP_Depth"],
    tr_coef_b = tr_b$coefs["TREND_EP_Count"],tr_se_b = tr_b$se["TREND_EP_Count"],tr_p_b = tr_b$pval["TREND_EP_Count"],
    tr_coef_c = tr_c$coefs["TREND_EP_Count"],tr_se_c = tr_c$se["TREND_EP_Count"],tr_p_c = tr_c$pval["TREND_EP_Count"]
  )
})

ladder_tex <- c(
  "{",
  "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{\\textit{WB EP Depth}} & \\multicolumn{2}{c}{\\textit{TREND EP Count}} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Fixed Effects & (1) Baseline & (2) Controls & (3) Baseline & (4) Controls \\\\",
  "\\midrule"
)
for (r in ladder_rows) {
  ladder_tex <- c(ladder_tex,
    paste0(r$label, " & ",
           fmt_coef(r$wb_coef_b, r$wb_p_b, 5), " & ",
           fmt_coef(r$wb_coef_c, r$wb_p_c, 5), " & ",
           fmt_coef(r$tr_coef_b, r$tr_p_b, 5), " & ",
           fmt_coef(r$tr_coef_c, r$tr_p_c, 5), " \\\\"),
    paste0(" & ",
           fmt_se(r$wb_se_b, 5), " & ", fmt_se(r$wb_se_c, 5), " & ",
           fmt_se(r$tr_se_b, 5), " & ", fmt_se(r$tr_se_c, 5), " \\\\"),
    "\\addlinespace"
  )
}
ladder_tex <- c(
  head(ladder_tex, -1),  # rimuove l'ultimo \addlinespace
  "\\midrule",
  paste0("\\multicolumn{5}{l}{\\footnotesize \\textit{Notes}: SEs clustered at the destination (country\\_code) level. Common sample.} \\\\"),
  paste0("\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)} \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "}"
)
writeLines(ladder_tex, file.path(dirs$tables, "OLS_Ladder_FE.tex"))
cat("[OK] OLS_Ladder_FE.tex\n")


# ─────────────────────────────────────────────────────────────────────
# DONE
# ─────────────────────────────────────────────────────────────────────
cat("\n=== 01_inference_fix.R completato ===\n")
cat("Output in:", out_dir, "\n\n")
cat("Checkpoint Fase 1:\n")
cat("  [ ] Tutte le tabelle clusterizzate a country_code (check N clusters nelle tabelle)\n")
cat("  [ ] Stesso N in baseline e con controlli (check colonne 1 vs 4 di ogni tabella)\n")
cat("  [ ] Bootstrap p-values: New/Output/OLS/Bootstrap/bootstrap_summary.csv\n")
cat("  [ ] Ladder: l'effetto si azzera salendo da fpd+year a fpt+fpd (OLS_Ladder_FE.tex)\n")
cat("  [ ] Le stelle di fpd+year sono sparite o fortemente ridotte rispetto all'originale\n")
