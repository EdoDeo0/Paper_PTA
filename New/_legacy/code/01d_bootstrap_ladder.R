##############################################################
## Bootstrap wild + ladder table                             #
## Richiede: tutti i 48 modelli RDS (01_inference_fix.R +   #
##           01c_fpd_pt.R completati)                        #
##############################################################
##
## Uso:
##   Rscript New/Code/01d_bootstrap_ladder.R
##
## Interrompibile e riavviabile: ogni spec bootstrap viene
## salvata come .rds separato.
##############################################################

library(fst); library(fixest); library(data.table)
library(here); library(fwildclusterboot); library(lubridate); library(parallel)

threads_fst(1)
# fpt+fpd ha gruppi ad alta dimensionalità: multi-thread OpenMP
# causa il crash "recursive gc invocation" anche in questa struttura.
setFixest_nthreads(1L)

source(here("Code/Analysis/pta_functions.R"))

DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
OUT_DIR   <- here("New/Output/OLS")

dirs     <- setup_output_dirs(OUT_DIR)
boot_dir <- file.path(OUT_DIR, "Bootstrap")
if (!dir.exists(boot_dir)) dir.create(boot_dir, recursive = TRUE)

# ─── Wild bootstrap (fpt+fpd, ln_export) ─────────────────────────────────────
cat("\n=== Wild bootstrap (fpt+fpd, ln_export, B=9999) ===\n")

d_boot <- as.data.table(read_fst(DATA_FILE, columns = c(
  "ln_export", "WB_EP_Depth", "TREND_EP_Count",
  "tariffs", "ln_hhi_baci", "fpt", "fpd", "country_code"
)))

boot_specs <- list(
  wb_baseline    = list(f = "ln_export ~ WB_EP_Depth | fpt + fpd",                            param = "WB_EP_Depth"),
  wb_controls    = list(f = "ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",    param = "WB_EP_Depth"),
  trend_baseline = list(f = "ln_export ~ TREND_EP_Count | fpt + fpd",                         param = "TREND_EP_Count"),
  trend_controls = list(f = "ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd", param = "TREND_EP_Count")
)

boot_results <- lapply(names(boot_specs), function(nm) {
  out_path <- file.path(boot_dir, paste0("boot_", nm, ".rds"))
  if (file.exists(out_path)) {
    cat(sprintf("  [SKIP] %s\n", nm))
    return(readRDS(out_path))
  }
  spec <- boot_specs[[nm]]
  cat(sprintf("  Estimating: %s\n", spec$f))
  # lean=TRUE: non include i dati nel modello (49M righe + lean=FALSE crashano).
  # boottest.fixest v0.14.3 supporta lean models direttamente.
  m  <- feols(as.formula(spec$f), data = d_boot,
              cluster = ~country_code, lean = TRUE)
  cat(sprintf("  Bootstrapping: %s (B=9999)...\n", nm))
  set.seed(42)
  br  <- boottest(m, param = spec$param, clustid = "country_code",
                  B = 9999)
  res <- list(coef = coef(m)[spec$param], se = se(m)[spec$param],
              pval = pvalue(m)[spec$param], boot = tidy(br))
  saveRDS(res, out_path)
  rm(m, br); gc()
  res
})
names(boot_results) <- names(boot_specs)
rm(d_boot); gc()

boot_summary <- do.call(rbind, lapply(names(boot_results), function(nm) {
  r <- boot_results[[nm]]; b <- r$boot
  cat(sprintf(
    "  %-20s | coef: %9.6f | SE: %9.6f | p(OLS): %.4f | p(WCR): %.4f | CI95: [%7.5f, %7.5f]\n",
    nm, r$coef, r$se, r$pval, b$p.value, b$conf.low, b$conf.high
  ))
  data.frame(spec = nm, coef = r$coef, se_cluster = r$se, p_ols = r$pval,
             p_wcr = b$p.value, ci_lo = b$conf.low, ci_hi = b$conf.high,
             stringsAsFactors = FALSE)
}))
write.csv(boot_summary, file.path(boot_dir, "bootstrap_summary.csv"), row.names = FALSE)
cat("[OK] bootstrap_summary.csv\n")

# ─── Ladder table ──────────────────────────────────────────────────────────────
cat("\n=== Ladder table ===\n")

load_rds <- function(block, i) {
  p <- file.path(dirs$models, sprintf("OLS_%s_%d.rds", block, i))
  if (!file.exists(p)) { warning("Not found: ", p); return(NULL) }
  readRDS(p)
}

fe_specs <- data.frame(
  label       = c("\\textit{fpd} + \\textit{t}",
                  "\\textit{fpt} + \\textit{pd}",
                  "\\textit{fpt} + \\textit{fpd}",
                  "\\textit{fpd} + \\textit{pt}"),
  wb_block    = c("WB_NI_fpd_year", "WB_NI_fpt_pd", "WB_NI_fpt_fpd", "WB_NI_fpd_pt"),
  trend_block = c("TREND_NI_fpd_year","TREND_NI_fpt_pd","TREND_NI_fpt_fpd","TREND_NI_fpd_pt"),
  stringsAsFactors = FALSE
)

ladder_rows <- lapply(seq_len(nrow(fe_specs)), function(j) {
  wb_b <- load_rds(fe_specs$wb_block[j], 1)
  wb_c <- load_rds(fe_specs$wb_block[j], 4)
  tr_b <- load_rds(fe_specs$trend_block[j], 1)
  tr_c <- load_rds(fe_specs$trend_block[j], 4)
  if (is.null(wb_b) || is.null(wb_c) || is.null(tr_b) || is.null(tr_c)) {
    cat(sprintf("  WARNING: modelli mancanti per %s — riga saltata\n", fe_specs$label[j]))
    return(NULL)
  }
  list(label = fe_specs$label[j],
       wb_coef_b = wb_b$coefs["WB_EP_Depth"],    wb_se_b = wb_b$se["WB_EP_Depth"],    wb_p_b = wb_b$pval["WB_EP_Depth"],
       wb_coef_c = wb_c$coefs["WB_EP_Depth"],    wb_se_c = wb_c$se["WB_EP_Depth"],    wb_p_c = wb_c$pval["WB_EP_Depth"],
       tr_coef_b = tr_b$coefs["TREND_EP_Count"], tr_se_b = tr_b$se["TREND_EP_Count"], tr_p_b = tr_b$pval["TREND_EP_Count"],
       tr_coef_c = tr_c$coefs["TREND_EP_Count"], tr_se_c = tr_c$se["TREND_EP_Count"], tr_p_c = tr_c$pval["TREND_EP_Count"])
})
ladder_rows <- Filter(Negate(is.null), ladder_rows)

ladder_tex <- c("{", "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lcccc}", "\\toprule",
  " & \\multicolumn{2}{c}{\\textit{WB EP Depth}} & \\multicolumn{2}{c}{\\textit{TREND EP Count}} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Fixed Effects & (1) Baseline & (2) Controls & (3) Baseline & (4) Controls \\\\", "\\midrule")

for (r in ladder_rows) {
  ladder_tex <- c(ladder_tex,
    paste0(r$label,
           " & ", fmt_coef(r$wb_coef_b, r$wb_p_b, 5),
           " & ", fmt_coef(r$wb_coef_c, r$wb_p_c, 5),
           " & ", fmt_coef(r$tr_coef_b, r$tr_p_b, 5),
           " & ", fmt_coef(r$tr_coef_c, r$tr_p_c, 5), " \\\\"),
    paste0(" & ", fmt_se(r$wb_se_b, 5), " & ", fmt_se(r$wb_se_c, 5),
           " & ", fmt_se(r$tr_se_b, 5), " & ", fmt_se(r$tr_se_c, 5), " \\\\"),
    "\\addlinespace")
}
ladder_tex <- c(head(ladder_tex, -1), "\\midrule",
  "\\multicolumn{5}{l}{\\footnotesize \\textit{Notes}: SEs clustered at destination (\\texttt{country\\_code}). N varies across specs.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)} \\\\",
  "\\bottomrule", "\\end{tabular}", "}")

writeLines(ladder_tex, file.path(dirs$tables, "OLS_Ladder_FE.tex"))
cat("[OK] OLS_Ladder_FE.tex\n")

cat("\n=== CHECKPOINT FASE 0 ===\n")
if (file.exists(file.path(boot_dir, "bootstrap_summary.csv"))) {
  boot_summary <- read.csv(file.path(boot_dir, "bootstrap_summary.csv"))
  cat("  bootstrap_summary.csv:\n")
  print(boot_summary[, c("spec", "coef", "p_ols", "p_wcr")])
} else {
  cat("  bootstrap_summary.csv: non trovato\n")
}
cat("  OLS_Ladder_FE.tex      → effetto si azzera a fpt+fpd?\n")
