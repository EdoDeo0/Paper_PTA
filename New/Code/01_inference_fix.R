########################################################
###### Fase 1 — Inferenza e igiene delle specifiche ####
########################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## Obiettivi (ROADMAP §Fase 1):
##   1. Clustering uniforme a ~country_code su tutte e 4 le strutture FE
##   2. Wild cluster bootstrap sulla specifica principale (fpt+fpd)
##   3. Ladder table diagnostica: effetto EP per ogni struttura FE
##
## Workaround crash OpenMP di RStudio su Windows: ogni sezione viene eseguita
## come sottoprocesso separato via callr::r() — RAM completamente liberata
## tra una sezione e l'altra. Due livelli di caching:
##   - Sezione:  saltata se tutte le tabelle .tex esistono già
##   - Modello:  saltato se il file .rds esiste già (logica in run_block)

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr)
library(here)

# ─────────────────────────────────────────────────────────────────────
# CONFIGURAZIONE (definita una sola volta, passata a ogni sottoprocesso)
# ─────────────────────────────────────────────────────────────────────
SHARED <- list(
  data_file  = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  out_dir    = here("New/Output/OLS"),
  nthreads   = 10L,
  show_stats = c("nobs", "r2", "n_clust"),
  cm_wb = c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
  ),
  cm_wb_int = c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
  ),
  cm_trend = c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
  ),
  cm_trend_int = c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
  )
)

# ─────────────────────────────────────────────────────────────────────
# HELPER: genera liste di formule per una struttura FE
# ─────────────────────────────────────────────────────────────────────
make_formulas <- function(fe) list(
  f_wb = c(
    paste0("ln_export ~ WB_EP_Depth | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth | ", fe),
    paste0("ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", fe)
  ),
  f_wb_int = c(
    paste0("ln_export ~ WB_EP_Depth * env_good | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth * env_good | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth * env_good | ", fe),
    paste0("ln_export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", fe)
  ),
  f_trend = c(
    paste0("ln_export ~ TREND_EP_Count | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count | ", fe),
    paste0("ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", fe)
  ),
  f_trend_int = c(
    paste0("ln_export ~ TREND_EP_Count * env_good | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count * env_good | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count * env_good | ", fe),
    paste0("ln_export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", fe)
  )
)

# ─────────────────────────────────────────────────────────────────────
# HELPER: lancia UN SINGOLO BLOCCO come sottoprocesso, salta se già completato
# ─────────────────────────────────────────────────────────────────────
run_block_section <- function(block_label, fe_label, tex_name, cm, formulas, shared) {
  table_path <- file.path(shared$out_dir, "Tables", tex_name)
  if (file.exists(table_path)) {
    cat(sprintf("  SKIP %s_%s (tabella già presente)\n", block_label, fe_label))
    return(invisible(NULL))
  }
  label <- sprintf("%s_%s", block_label, fe_label)
  cat(sprintf("\n=== %s ===\n", label))
  args <- c(shared, list(
    cm = shared[[paste0("cm_", tolower(block_label))]],
    fe_label    = fe_label,
    block_label = block_label,
    formulas    = formulas,
    tex_name    = tex_name
  ))
  # rimuovi i cm_* extra (non servono al sottoprocesso)
  args[c("cm_wb", "cm_wb_int", "cm_trend", "cm_trend_int")] <- NULL
  result <- tryCatch(
    callr::r(section_ols_block, args = args, show = TRUE),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    cat(sprintf("[ERROR] %s: %s\n", label, conditionMessage(result)))
    stop("Errore in ", label, ": ", conditionMessage(result))
  }
  cat("Done:", label, "\n")
}

# Wrapper per lanciare tutti e 4 i blocchi di una struttura FE
run_fe_structure <- function(fe_label, fe_str, shared) {
  fms <- make_formulas(fe_str)
  cat(sprintf("\n\n========== FE: %s ==========\n", fe_str))
  run_block_section("WB_NI",    fe_label, sprintf("OLS_WB_No_Interaction_%s.tex",   fe_label), shared$cm_wb,       fms$f_wb,       shared)
  run_block_section("WB_Int",   fe_label, sprintf("OLS_WB_Interaction_%s.tex",      fe_label), shared$cm_wb_int,   fms$f_wb_int,   shared)
  run_block_section("TREND_NI", fe_label, sprintf("OLS_TREND_No_Interaction_%s.tex",fe_label), shared$cm_trend,    fms$f_trend,    shared)
  run_block_section("TREND_Int",fe_label, sprintf("OLS_TREND_Interaction_%s.tex",   fe_label), shared$cm_trend_int,fms$f_trend_int,shared)
  cat(sprintf("=== FE %s completata ===\n", fe_str))
}

# Versione diretta senza il passaggio tramite shared$cm_*
run_one_block <- function(block_label, fe_label, fe_str, tex_name, cm, shared) {
  fms <- make_formulas(fe_str)
  key <- switch(block_label,
    WB_NI    = "f_wb",
    WB_Int   = "f_wb_int",
    TREND_NI = "f_trend",
    TREND_Int= "f_trend_int"
  )
  table_path <- file.path(shared$out_dir, "Tables", tex_name)
  if (file.exists(table_path)) {
    cat(sprintf("  SKIP %s_%s (tabella già presente)\n", block_label, fe_label))
    return(invisible(NULL))
  }
  label <- sprintf("%s_%s", block_label, fe_label)
  cat(sprintf("\n=== %s ===\n", label))
  args <- list(
    data_file   = shared$data_file,
    out_dir     = shared$out_dir,
    nthreads    = shared$nthreads,
    show_stats  = shared$show_stats,
    cm          = cm,
    fe_label    = fe_label,
    block_label = block_label,
    formulas    = fms[[key]],
    tex_name    = tex_name
  )
  result <- tryCatch(
    callr::r(section_ols_block, args = args, show = TRUE),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    cat(sprintf("[ERROR] %s: %s\n", label, conditionMessage(result)))
    stop("Errore in ", label, ": ", conditionMessage(result))
  }
  cat("Done:", label, "\n")
}

# ─────────────────────────────────────────────────────────────────────
# SEZIONE OLS — UN BLOCCO PER SOTTOPROCESSO
# Ogni blocco (WB_NI, WB_Int, TREND_NI, TREND_Int) viene eseguito in un
# sottoprocesso separato: RAM completamente liberata tra un blocco e l'altro.
# Questo evita l'accumulo di memoria che causava il crash al 4° blocco.
# Self-contained: non dipende da nulla definito fuori da questa funzione.
# ─────────────────────────────────────────────────────────────────────
section_ols_block <- function(data_file, out_dir, nthreads, show_stats,
                               cm, fe_label, block_label, formulas, tex_name) {
  library(fst); library(fixest); library(data.table); library(here); library(lubridate)
  threads_fst(1); setFixest_nthreads(nthreads)
  source(here("Code/Analysis/pta_functions.R"))
  dirs  <- setup_output_dirs(out_dir)
  start <- now()

  stats <- run_block(formulas, paste0(block_label, "_", fe_label), "ols",
                     data_file, dirs$models, vcov = ~country_code,
                     requested_stats = show_stats, preload_block_data = TRUE)
  make_table(stats, cm, tex_name, dirs$tables, digits = 5, show_stats = show_stats)
  gc()
  cat(sprintf("\n=== DONE %s_%s | %.1f min ===\n",
              block_label, fe_label,
              as.numeric(now() - start, units = "mins")))
}

# ─────────────────────────────────────────────────────────────────────
# SEZIONE 5: WILD BOOTSTRAP + LADDER TABLE
# Self-contained: non dipende da nulla definito fuori da questa funzione.
# ─────────────────────────────────────────────────────────────────────
section_bootstrap_ladder <- function(data_file, out_dir, nthreads, ...) {
  library(fst); library(fixest); library(data.table); library(here); library(fwildclusterboot)
  threads_fst(1); setFixest_nthreads(nthreads)
  source(here("Code/Analysis/pta_functions.R"))
  dirs     <- setup_output_dirs(out_dir)
  boot_dir <- file.path(out_dir, "Bootstrap")
  if (!dir.exists(boot_dir)) dir.create(boot_dir, recursive = TRUE)

  cat("\n=== Wild bootstrap (fpt+fpd, ln_export, B=9999) ===\n")
  d_boot <- as.data.table(read_fst(data_file, columns = c(
    "ln_export", "WB_EP_Depth", "TREND_EP_Count", "tariffs", "ln_hhi_baci", "fpt", "fpd", "country_code"
  )))
  boot_specs <- list(
    wb_baseline    = list(f = "ln_export ~ WB_EP_Depth | fpt + fpd",                            param = "WB_EP_Depth"),
    wb_controls    = list(f = "ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",    param = "WB_EP_Depth"),
    trend_baseline = list(f = "ln_export ~ TREND_EP_Count | fpt + fpd",                         param = "TREND_EP_Count"),
    trend_controls = list(f = "ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd", param = "TREND_EP_Count")
  )
  boot_results <- lapply(names(boot_specs), function(nm) {
    out_path <- file.path(boot_dir, paste0("boot_", nm, ".rds"))
    if (file.exists(out_path)) { cat(sprintf("  [SKIP] %s\n", nm)); return(readRDS(out_path)) }
    spec <- boot_specs[[nm]]
    cat(sprintf("  Estimating: %s\n", spec$f))
    m  <- feols(as.formula(spec$f), data = d_boot, cluster = ~country_code, lean = FALSE)
    cat(sprintf("  Bootstrapping: %s (B=9999)...\n", nm))
    br <- boottest(m, param = spec$param, clustid = "country_code", B = 9999, seed = 42)
    res <- list(coef = coef(m)[spec$param], se = se(m)[spec$param], pval = pvalue(m)[spec$param], boot = tidy(br))
    saveRDS(res, out_path); rm(m, br); gc(); res
  })
  names(boot_results) <- names(boot_specs)
  rm(d_boot); gc()

  boot_summary <- do.call(rbind, lapply(names(boot_results), function(nm) {
    r <- boot_results[[nm]]; b <- r$boot
    cat(sprintf("  %-20s | coef: %9.6f | SE: %9.6f | p(OLS): %.4f | p(WCR): %.4f | CI: [%9.6f, %9.6f]\n",
                nm, r$coef, r$se, r$pval, b$p.value, b$conf.low, b$conf.high))
    data.frame(spec = nm, coef = r$coef, se_cluster = r$se, p_ols = r$pval,
               p_wcr = b$p.value, ci_lo = b$conf.low, ci_hi = b$conf.high, stringsAsFactors = FALSE)
  }))
  write.csv(boot_summary, file.path(boot_dir, "bootstrap_summary.csv"), row.names = FALSE)
  cat("[OK] bootstrap_summary.csv\n")

  cat("\n=== Building ladder table ===\n")
  load_rds <- function(block, i) {
    p <- file.path(dirs$models, sprintf("OLS_%s_%d.rds", block, i))
    if (!file.exists(p)) { warning("Not found: ", p); return(NULL) }
    readRDS(p)
  }
  fe_specs <- data.frame(
    label       = c("\\textit{fpd} + \\textit{t}", "\\textit{fpt} + \\textit{pd}",
                    "\\textit{fpt} + \\textit{fpd}", "\\textit{fpd} + \\textit{pt}"),
    wb_block    = c("WB_NI_fpd_year",    "WB_NI_fpt_pd",    "WB_NI_fpt_fpd",    "WB_NI_fpd_pt"),
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
      paste0(r$label, " & ", fmt_coef(r$wb_coef_b,r$wb_p_b,5), " & ", fmt_coef(r$wb_coef_c,r$wb_p_c,5),
             " & ", fmt_coef(r$tr_coef_b,r$tr_p_b,5), " & ", fmt_coef(r$tr_coef_c,r$tr_p_c,5), " \\\\"),
      paste0(" & ", fmt_se(r$wb_se_b,5), " & ", fmt_se(r$wb_se_c,5),
             " & ", fmt_se(r$tr_se_b,5), " & ", fmt_se(r$tr_se_c,5), " \\\\"),
      "\\addlinespace")
  }
  ladder_tex <- c(head(ladder_tex, -1), "\\midrule",
    "\\multicolumn{5}{l}{\\footnotesize \\textit{Notes}: SEs clustered at destination (country\\_code). N varies across columns.} \\\\",
    "\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)} \\\\",
    "\\bottomrule", "\\end{tabular}", "}")
  writeLines(ladder_tex, file.path(dirs$tables, "OLS_Ladder_FE.tex"))
  cat("[OK] OLS_Ladder_FE.tex\n")
}

# ─────────────────────────────────────────────────────────────────────
# ESECUZIONE — un sottoprocesso per blocco (4 blocchi × 4 strutture FE = 16)
# Ogni blocco ha RAM fresca: elimina l'accumulo che causava il crash al 4° blocco.
# ─────────────────────────────────────────────────────────────────────

for (cfg in list(
  list(fe_label = "fpd_year", fe_str = "fpd + year"),
  list(fe_label = "fpt_pd",   fe_str = "fpt + pd"),
  list(fe_label = "fpt_fpd",  fe_str = "fpt + fpd"),
  list(fe_label = "fpd_pt",   fe_str = "fpd + pt")
)) {
  fms <- make_formulas(cfg$fe_str)
  fe  <- cfg$fe_label
  cat(sprintf("\n\n========== FE: %s ==========\n", cfg$fe_str))
  run_one_block("WB_NI",    fe, cfg$fe_str, sprintf("OLS_WB_No_Interaction_%s.tex",   fe), SHARED$cm_wb,       SHARED)
  run_one_block("WB_Int",   fe, cfg$fe_str, sprintf("OLS_WB_Interaction_%s.tex",      fe), SHARED$cm_wb_int,   SHARED)
  run_one_block("TREND_NI", fe, cfg$fe_str, sprintf("OLS_TREND_No_Interaction_%s.tex",fe), SHARED$cm_trend,    SHARED)
  run_one_block("TREND_Int",fe, cfg$fe_str, sprintf("OLS_TREND_Interaction_%s.tex",   fe), SHARED$cm_trend_int,SHARED)
}

# Bootstrap + ladder (sottoprocesso singolo, già serializzato internamente)
{
  table_path <- file.path(SHARED$out_dir, "Tables", "OLS_Ladder_FE.tex")
  if (file.exists(table_path)) {
    cat("\nSKIP bootstrap + ladder (tabella già presente)\n")
  } else {
    cat("\n=== bootstrap + ladder ===\n")
    result <- tryCatch(
      callr::r(section_bootstrap_ladder, args = SHARED, show = TRUE),
      error = function(e) e
    )
    if (inherits(result, "error")) stop("Errore bootstrap: ", conditionMessage(result))
    cat("Done: bootstrap + ladder\n")
  }
}

cat("\n=== Fase 1 completata ===\n")
cat("  New/Output/OLS/Bootstrap/bootstrap_summary.csv  <- p_wcr: null o significativo?\n")
cat("  New/Output/OLS/Tables/OLS_Ladder_FE.tex         <- effetto si azzera a fpt+fpd?\n")
