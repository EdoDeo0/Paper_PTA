########################################################
###### 15 — Saturation ladder: 4 strutture FE x 4 blocchi ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 01_inference_fix.R + 01c_fpd_pt.R + 01d_bootstrap_ladder.R +
##              _gen_ladder_tex.R (in New/_legacy/code/, storia della loro
##              sovrapposizione documentata in verification/equivalence_log.md).
##              Run: OGNI MODELLO E' GIA' CACHATO su disco (.rds per modello,
##              .tex per blocco) - un run da zero rilancerebbe 96 modelli OLS
##              (4 strutture FE x 4 blocchi x 6 formule) sul pannello raw da
##              49,2M righe, ore di calcolo. Questo script SALTA tutto cio'
##              che e' gia' presente: verificare l'equivalenza via cache
##              esistente, non ri-lanciare i modelli, e' la strada
##              esplicitamente ammessa dal piano di riordino per questo script.
##
## Cosa fa: per ciascuna delle 4 strutture di fixed effects a 2 vie
## (fpd+year, fpt+pd, fpt+fpd, fpd+pt - diagnostica preliminare sulla scelta
## della struttura FE, PRIMA di arrivare alla tripla-diff a 3 FE del modulo
## principale), stima 4 blocchi di 6 formule ciascuno (WB/TREND x con/senza
## interazione env_good, su ln_export/ln_export_qua/ln_export_value, con e
## senza controlli tariffs+ln_hhi_baci) e produce una "ladder table" che
## mostra come il coefficiente EP si comporta al saturare la struttura FE.
##
## ATTENZIONE — env_good (audit 2026-08-23, W3): questo script usa la colonna
## `env_good` COME STA NEL .fst (definizione congelata alla costruzione del
## dataset), mentre tutto il resto della pipeline (16, stata/17, stata/18,
## stata/19b, stata/57) la RICALCOLA dalla lista green_codes_hs1996.csv.
## Conseguenze:
##   - blocco "NI" (livello, senza interazione): non dipende da env_good ed e'
##     verificato IDENTICO a stata/19b. E' l'unico blocco citato dal paper
##     (tab:ladder / OLS_Ladder_FE.tex) -> nessun problema.
##   - blocco "Int" (interazione EP x env_good): NON confrontabile con 19b/57,
##     che stimano la stessa spec con la definizione green ricalcolata
##     (es. fpt+fpd: -0.00223 qui, -0.00271 in 19b, a parita' di N).
## Se in futuro si vorra' pubblicare il blocco Int, la fonte da usare e'
## 19b/57 (definizione coerente col resto del paper), non questo script.
##
## NOTA STABILITA': le strutture fpd+pt e fpt+fpd hanno gruppi ad altissima
## cardinalita' (fpd: 26M+ gruppi) - il multi-thread OpenMP di fixest causa
## il crash "recursive gc invocation" anche con lean=TRUE; per queste due
## strutture il modello gira con nthreads=1 (fix verificato empiricamente
## nelle sessioni originali, preservato qui). Ogni blocco gira nel proprio
## sottoprocesso callr (RAM liberata tra un blocco e l'altro).
##
## WCB: il wild cluster bootstrap sulla ladder (fpt+fpd, B=9999) e' in
## 21_wcb_ladder_fullpanel.R (Fase D) via Frisch-Waugh, NON qui - il
## bootstrap diretto originariamente tentato in questo script (01d) non ha
## mai prodotto un bootstrap_summary.csv affidabile sul full panel ed e'
## stato superato dall'approccio Frisch-Waugh.
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
## Output: New/Output/OLS/Tables/OLS_{WB,TREND}_{No_Interaction,Interaction}_*.tex (16 tabelle)
##         New/Output/OLS/Tables/OLS_Ladder_FE.tex
##         New/Output/OLS/Models_Output/OLS_*.rds (96 modelli, cache)

##
## ATTENZIONE - VARIANTE DI CAMPIONE. Storicamente questa ladder girava sul
## panel COMPLETO, HK+MO INCLUSI. Ora la variante e' parametrizzata via
## _sample_config.R come nel resto della pipeline: il default (PTA_SAMPLE non
## impostata) e' HK+MO ESCLUSI, coerente con la specifica principale del paper.
## Gli output gia' presenti in New/Output/OLS/ senza suffisso provengono dal
## vecchio run INCLUSIVO: vanno rinominati in New/Output/OLS_inclHKMO/ prima
## di lanciare la variante esclusa, altrimenti la cache per nome file li fa
## rileggere come se fossero il campione escluso.

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(callr)
source(here("Code/Analysis/pta_functions.R"))
source(here("New/Code/_sample_config.R"))

## --- Parametri e percorsi --------------------------------------------------
DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
OUT_DIR   <- out_path(here("New/Output/OLS"))
SHOW_STATS <- c("nobs", "r2", "n_clust")
dirs <- setup_output_dirs(OUT_DIR)

CM_WB <- c(
  "WB_EP_Depth" = "\\textit{EPDepth\\textsubscript{dt}}",
  "tariffs"     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}"
)
CM_WB_INT <- c(
  "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
  "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
CM_TREND <- c(
  "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
  "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)
CM_TREND_INT <- c(
  "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
  "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)

# strutture FE ad alta cardinalita' che richiedono nthreads=1 (crash noto altrimenti)
# fpt_pd aggiunta empiricamente durante il rerun 2026-07-22: stesso crash
# "recursive gc invocation" di fpd_pt/fpt_fpd, ripetuto 3/3 volte a nthreads=10
HIGH_CARDINALITY_FE <- c("fpd_year", "fpd_pt", "fpt_fpd", "fpt_pd")

make_formulas <- function(fe) list(
  WB_NI = c(
    paste0("ln_export ~ WB_EP_Depth | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth | ", fe),
    paste0("ln_export ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | ", fe)
  ),
  WB_Int = c(
    paste0("ln_export ~ WB_EP_Depth * env_good | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth * env_good | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth * env_good | ", fe),
    paste0("ln_export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ", fe)
  ),
  TREND_NI = c(
    paste0("ln_export ~ TREND_EP_Count | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count | ", fe),
    paste0("ln_export ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | ", fe)
  ),
  TREND_Int = c(
    paste0("ln_export ~ TREND_EP_Count * env_good | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count * env_good | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count * env_good | ", fe),
    paste0("ln_export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", fe),
    paste0("ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ", fe)
  )
)

CM_BY_BLOCK <- list(WB_NI = CM_WB, WB_Int = CM_WB_INT, TREND_NI = CM_TREND, TREND_Int = CM_TREND_INT)

## --- Sezione 1: 96 modelli (4 FE x 4 blocchi x 6 formule) ------------------
# ogni blocco (4 FE x 4 blocchi = 16 chiamate) nel proprio sottoprocesso callr:
# RAM completamente liberata tra un blocco e l'altro. Ogni singolo modello e'
# cachato come .rds da run_block (skip automatico se gia' presente); l'intero
# blocco e' skippato se il .tex finale esiste gia'.
run_block_subprocess <- function(data_file, out_dir, nthreads, show_stats,
                                  cm, block_label, fe_label, formulas, tex_name,
                                  hkmo_drop) {
  library(fst); library(fixest); library(data.table); library(here); library(lubridate)
  threads_fst(1)
  setFixest_nthreads(nthreads)
  source(here("Code/Analysis/pta_functions.R"))

  # Il filtro HK+MO non si puo' passare a run_block(): pta_functions.R sta fuori
  # da New/ e non va modificato. Si maschera load_formula_data() nel global env
  # del sottoprocesso - estimate_model() la risolve lessicalmente li' e prende
  # questa. country_code e' sempre tra le colonne caricate perche' e' la
  # variabile di cluster (vcov = ~country_code).
  if (hkmo_drop) {
    .load_orig <- load_formula_data
    load_formula_data <<- function(data_file, formula_str, vcov = "HC1") {
      d <- .load_orig(data_file, formula_str, vcov)
      d[!country_code %in% c(110L, 121L)]
    }
  }

  dirs <- setup_output_dirs(out_dir)
  t0 <- now()
  stats <- run_block(formulas, paste0(block_label, "_", fe_label), "ols",
                     data_file, dirs$models, vcov = ~country_code,
                     requested_stats = show_stats, preload_block_data = FALSE)
  make_table(stats, cm, tex_name, dirs$tables, digits = 5, show_stats = show_stats)
  gc()
  cat(sprintf("[OK] %s_%s - %.1f min\n", block_label, fe_label, as.numeric(now() - t0, "mins")))
}

fe_structures <- list(
  fpd_year = "fpd + year",
  fpt_pd   = "fpt + pd",
  fpt_fpd  = "fpt + fpd",
  fpd_pt   = "fpd + pt"
)

for (fe_label in names(fe_structures)) {
  fe_str <- fe_structures[[fe_label]]
  fms <- make_formulas(fe_str)
  nthreads <- if (fe_label %in% HIGH_CARDINALITY_FE) 1L else 4L
  cat(sprintf("\n\n========== FE: %s (nthreads=%d) ==========\n", fe_str, nthreads))
  for (block_label in names(fms)) {
    tex_name <- sprintf("OLS_%s_%s.tex",
                        switch(block_label, WB_NI = "WB_No_Interaction", WB_Int = "WB_Interaction",
                               TREND_NI = "TREND_No_Interaction", TREND_Int = "TREND_Interaction"),
                        fe_label)
    tex_path <- file.path(dirs$tables, tex_name)
    if (file.exists(tex_path)) {
      cat(sprintf("  SKIP %s_%s (tabella gia' presente)\n", block_label, fe_label))
      next
    }
    cat(sprintf("\n=== %s_%s ===\n", block_label, fe_label))
    result <- tryCatch(
      callr::r(run_block_subprocess, args = list(
        data_file = DATA_FILE, out_dir = OUT_DIR, nthreads = nthreads, show_stats = SHOW_STATS,
        cm = CM_BY_BLOCK[[block_label]], block_label = block_label, fe_label = fe_label,
        formulas = fms[[block_label]], tex_name = tex_name, hkmo_drop = HKMO_DROP
      ), show = TRUE),
      error = function(e) e
    )
    if (inherits(result, "error")) stop(sprintf("Errore in %s_%s: %s", block_label, fe_label, conditionMessage(result)))
  }
}

## --- Sezione 2: ladder table ------------------------------------------------
# righe = strutture FE, colonne = WB/TREND x baseline/controlli. Coefficienti
# letti dai modelli 1 (baseline, no controlli) e 4 (con controlli) di ciascun
# blocco NI (No Interaction) gia' cachati sopra.
ladder_path <- file.path(dirs$tables, "OLS_Ladder_FE.tex")
if (file.exists(ladder_path)) {
  cat("\nSKIP ladder table (gia' presente)\n")
} else {
  cat("\n=== Building ladder table ===\n")
  load_rds <- function(block, i) {
    p <- file.path(dirs$models, sprintf("OLS_%s_%d.rds", block, i))
    if (!file.exists(p)) { warning("Not found: ", p); return(NULL) }
    readRDS(p)
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
    if (is.null(wb_b) || is.null(wb_c) || is.null(tr_b) || is.null(tr_c)) {
      cat(sprintf("  WARNING: modelli mancanti per %s - riga saltata\n", fe_specs$label[j]))
      return(NULL)
    }
    list(label = fe_specs$label[j],
         wb_coef_b = wb_b$coefs["WB_EP_Depth"],    wb_se_b = wb_b$se["WB_EP_Depth"],    wb_p_b = wb_b$pval["WB_EP_Depth"],
         wb_coef_c = wb_c$coefs["WB_EP_Depth"],    wb_se_c = wb_c$se["WB_EP_Depth"],    wb_p_c = wb_c$pval["WB_EP_Depth"],
         tr_coef_b = tr_b$coefs["TREND_EP_Count"], tr_se_b = tr_b$se["TREND_EP_Count"], tr_p_b = tr_b$pval["TREND_EP_Count"],
         tr_coef_c = tr_c$coefs["TREND_EP_Count"], tr_se_c = tr_c$se["TREND_EP_Count"], tr_p_c = tr_c$pval["TREND_EP_Count"])
  })
  ladder_rows <- Filter(Negate(is.null), ladder_rows)
  cat(sprintf("Righe ladder: %d\n", length(ladder_rows)))

  ladder_tex <- c("{", "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
    "\\begin{tabular}{lcccc}", "\\toprule",
    " & \\multicolumn{2}{c}{\\textit{WB EP Depth}} & \\multicolumn{2}{c}{\\textit{TREND EP Count}} \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    "Fixed Effects & (1) Baseline & (2) Controls & (3) Baseline & (4) Controls \\\\", "\\midrule")
  for (r in ladder_rows) {
    ladder_tex <- c(ladder_tex,
      paste0(r$label, " & ", fmt_coef(r$wb_coef_b, r$wb_p_b, 5), " & ", fmt_coef(r$wb_coef_c, r$wb_p_c, 5),
             " & ", fmt_coef(r$tr_coef_b, r$tr_p_b, 5), " & ", fmt_coef(r$tr_coef_c, r$tr_p_c, 5), " \\\\"),
      paste0(" & ", fmt_se(r$wb_se_b, 5), " & ", fmt_se(r$wb_se_c, 5),
             " & ", fmt_se(r$tr_se_b, 5), " & ", fmt_se(r$tr_se_c, 5), " \\\\"),
      "\\addlinespace")
  }
  ladder_tex <- c(head(ladder_tex, -1), "\\midrule",
    "\\multicolumn{5}{l}{\\footnotesize \\textit{Notes}: SEs clustered at destination (\\texttt{country\\_code}). N varies across specs.} \\\\",
    "\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)} \\\\",
    "\\bottomrule", "\\end{tabular}", "}")
  writeLines(ladder_tex, ladder_path)
  cat("[OK] OLS_Ladder_FE.tex\n")
}

cat("\n=== Fase 15 completata ===\n")
cat("  Il WCB sulla ladder (fpt+fpd) e' in 21_wcb_ladder_fullpanel.R.\n")
