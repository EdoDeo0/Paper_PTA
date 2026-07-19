## Fase 1 — Section 5+6: Wild Bootstrap + Ladder Table
## Da eseguire DOPO che 01a-01d sono completati (i .rds devono esistere).
## Eseguire come processo separato via run_fase1.ps1

rm(list = ls())
library(fst); library(fixest); library(data.table); library(here); library(lubridate)
library(fwildclusterboot)
threads_fst(1); setFixest_nthreads(4)
source(here("Code/Analysis/pta_functions.R"))

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("New/Output/OLS")
dirs      <- setup_output_dirs(out_dir)
stopifnot("Data file not found!" = file.exists(data_file))

# ── BOOTSTRAP ──────────────────────────────────────────────────────────
cat("\n=== Wild bootstrap (fpt + fpd, ln_export, B=9999) ===\n")

boot_dir <- file.path(out_dir, "Bootstrap")
if (!dir.exists(boot_dir)) dir.create(boot_dir, recursive = TRUE)

d_boot <- as.data.table(read_fst(data_file, columns = c(
  "ln_export", "WB_EP_Depth", "TREND_EP_Count",
  "tariffs", "ln_hhi_baci", "fpt", "fpd", "country_code"
)))

boot_specs <- list(
  wb_baseline    = list(f = "ln_export ~ WB_EP_Depth | fpt + fpd",                              param = "WB_EP_Depth"),
  wb_controls    = list(f = "ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",      param = "WB_EP_Depth"),
  trend_baseline = list(f = "ln_export ~ TREND_EP_Count | fpt + fpd",                           param = "TREND_EP_Count"),
  trend_controls = list(f = "ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",   param = "TREND_EP_Count")
)

boot_results <- lapply(names(boot_specs), function(nm) {
  out_path <- file.path(boot_dir, paste0("boot_", nm, ".rds"))
  if (file.exists(out_path)) { cat(sprintf("  [SKIP] %s\n", nm)); return(readRDS(out_path)) }
  spec <- boot_specs[[nm]]
  cat(sprintf("  Estimating: %s\n", spec$f))
  m <- feols(as.formula(spec$f), data = d_boot, cluster = ~country_code, lean = FALSE)
  cat(sprintf("  Bootstrapping: %s (B=9999)...\n", nm))
  br <- boottest(m, param = spec$param, clustid = "country_code", B = 9999, seed = 42)
  res <- list(coef = coef(m)[spec$param], se = se(m)[spec$param], pval = pvalue(m)[spec$param],
              boot = tidy(br))
  saveRDS(res, out_path)
  rm(m, br); gc()
  res
})
names(boot_results) <- names(boot_specs)
rm(d_boot); gc()

cat("\n--- Wild Bootstrap Results ---\n")
boot_summary <- do.call(rbind, lapply(names(boot_results), function(nm) {
  r <- boot_results[[nm]]; b <- r$boot
  cat(sprintf("  %-20s | coef: %9.6f | SE: %9.6f | p(OLS): %.4f | p(WCR): %.4f | 95%%CI: [%9.6f, %9.6f]\n",
              nm, r$coef, r$se, r$pval, b$p.value, b$conf.low, b$conf.high))
  data.frame(spec = nm, coef = r$coef, se_cluster = r$se, p_ols = r$pval,
             p_wcr = b$p.value, ci_lo = b$conf.low, ci_hi = b$conf.high, stringsAsFactors = FALSE)
}))
write.csv(boot_summary, file.path(boot_dir, "bootstrap_summary.csv"), row.names = FALSE)
cat("[OK] bootstrap_summary.csv\n")

# ── LADDER TABLE ───────────────────────────────────────────────────────
cat("\n=== Building ladder table ===\n")

load_rds <- function(block_name, i) {
  path <- file.path(dirs$models, sprintf("OLS_%s_%d.rds", block_name, i))
  if (!file.exists(path)) { warning("Not found: ", path); return(NULL) }
  readRDS(path)
}

fe_specs <- data.frame(
  label       = c("\\textit{fpd} + \\textit{t}", "\\textit{fpt} + \\textit{pd}",
                  "\\textit{fpt} + \\textit{fpd}", "\\textit{fpd} + \\textit{pt}"),
  wb_block    = c("WB_NI_fpd_year", "WB_NI_fpt_pd", "WB_NI_fpt_fpd", "WB_NI_fpd_pt"),
  trend_block = c("TREND_NI_fpd_year", "TREND_NI_fpt_pd", "TREND_NI_fpt_fpd", "TREND_NI_fpd_pt"),
  stringsAsFactors = FALSE
)

ladder_rows <- lapply(seq_len(nrow(fe_specs)), function(j) {
  wb_b <- load_rds(fe_specs$wb_block[j], 1);    wb_c <- load_rds(fe_specs$wb_block[j], 4)
  tr_b <- load_rds(fe_specs$trend_block[j], 1); tr_c <- load_rds(fe_specs$trend_block[j], 4)
  list(label = fe_specs$label[j],
       wb_coef_b = wb_b$coefs["WB_EP_Depth"],    wb_se_b = wb_b$se["WB_EP_Depth"],    wb_p_b = wb_b$pval["WB_EP_Depth"],
       wb_coef_c = wb_c$coefs["WB_EP_Depth"],    wb_se_c = wb_c$se["WB_EP_Depth"],    wb_p_c = wb_c$pval["WB_EP_Depth"],
       tr_coef_b = tr_b$coefs["TREND_EP_Count"], tr_se_b = tr_b$se["TREND_EP_Count"], tr_p_b = tr_b$pval["TREND_EP_Count"],
       tr_coef_c = tr_c$coefs["TREND_EP_Count"], tr_se_c = tr_c$se["TREND_EP_Count"], tr_p_c = tr_c$pval["TREND_EP_Count"])
})

ladder_tex <- c("{", "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lcccc}", "\\toprule",
  " & \\multicolumn{2}{c}{\\textit{WB EP Depth}} & \\multicolumn{2}{c}{\\textit{TREND EP Count}} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Fixed Effects & (1) Baseline & (2) Controls & (3) Baseline & (4) Controls \\\\", "\\midrule")
for (r in ladder_rows) {
  ladder_tex <- c(ladder_tex,
    paste0(r$label, " & ", fmt_coef(r$wb_coef_b, r$wb_p_b, 5), " & ", fmt_coef(r$wb_coef_c, r$wb_p_c, 5),
           " & ", fmt_coef(r$tr_coef_b, r$tr_p_b, 5), " & ", fmt_coef(r$tr_coef_c, r$tr_p_c, 5), " \\\\"),
    paste0(" & ", fmt_se(r$wb_se_b, 5), " & ", fmt_se(r$wb_se_c, 5), " & ",
           fmt_se(r$tr_se_b, 5), " & ", fmt_se(r$tr_se_c, 5), " \\\\"), "\\addlinespace")
}
ladder_tex <- c(head(ladder_tex, -1), "\\midrule",
  "\\multicolumn{5}{l}{\\footnotesize \\textit{Notes}: SEs clustered at destination (country\\_code). N varies across columns (fixest drops NAs).} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)} \\\\",
  "\\bottomrule", "\\end{tabular}", "}")
writeLines(ladder_tex, file.path(dirs$tables, "OLS_Ladder_FE.tex"))
cat("[OK] OLS_Ladder_FE.tex\n")

cat("\n=== DONE 01e bootstrap + ladder ===\n")
cat("Checkpoint Fase 1:\n")
cat("  bootstrap_summary.csv -> controlla p_wcr: null o significativo?\n")
cat("  OLS_Ladder_FE.tex     -> l'effetto si azzera salendo a fpt+fpd?\n")
