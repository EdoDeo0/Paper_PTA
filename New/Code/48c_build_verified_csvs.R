########################################################
###### 48c — Costruisce i CSV verificati da Stata      ###
########################################################
## Author: Edoardo Vitella
##
## Cosa fa: legge stata_check_46_47_collapsed.csv (verità Stata) e
## riscrive tripledd_trimmed_collapsed.csv e tripledd_decomp_collapsed.csv
## con i valori corretti. Aggiunge colonna source="reghdfe_stata_48".
##
## Lanciare DOPO che 48_trim_check.do è completato.

rm(list = ls())
library(data.table)
library(here)

STATA_REF <- here("New/Output/TripleDiff/Tables/stata_check_46_47_collapsed.csv")
OUT_DIR   <- here("New/Output/TripleDiff/Tables")

stopifnot("stata_check non trovato — lanciare prima 48_trim_check.do" = file.exists(STATA_REF))
ref <- fread(STATA_REF)
stopifnot("righe attese 24" = nrow(ref) == 24)

## Mappa nomi variabili Stata -> nomi R (come appaiono nel CSV asintotico)
var_map <- c(
  wb_green = "WB_EP_Depth:env_good",
  wb_dirty = "WB_EP_Depth:dirty_p",
  td_green = "env_good:TotalDepth_nonEnv",
  td_dirty = "dirty_p:TotalDepth_nonEnv",
  tr_green = "TREND_EP_Count:env_good",
  tr_dirty = "TREND_EP_Count:dirty_p"
)

build_csv <- function(ds_label, out_file) {
  d <- ref[dataset == ds_label]
  stopifnot(nrow(d) == 8)

  wb   <- d[treat == "WB"]
  tr   <- d[treat == "TREND"]

  make_rows <- function(rows, treat_label) {
    rows[, .(
      treat  = treat_label,
      var    = var_map[var],
      coef   = coef,
      se     = se,
      pval   = pval,
      nobs   = nobs,
      nclust = nclust,
      source = "reghdfe_stata_48"
    )]
  }

  out <- rbind(make_rows(wb, "WB"), make_rows(tr, "TREND"))
  fwrite(out, out_file)
  cat(sprintf("Scritto: %s (%d righe)\n", basename(out_file), nrow(out)))
  invisible(out)
}

## --- tripledd_trimmed_collapsed.csv ---
build_csv("trim",
  file.path(OUT_DIR, "tripledd_trimmed_collapsed.csv"))

## --- tripledd_decomp_collapsed.csv ---
## Per la decomposizione non esiste ancora un CSV combinato con la stessa
## struttura — lo creiamo ex novo con colonna outcome.
decomp_qua <- ref[dataset == "decomp_qua"]
decomp_uv  <- ref[dataset == "decomp_uv"]

make_decomp_rows <- function(rows, outcome, treat_label) {
  rows[treat == treat_label, .(
    outcome = outcome,
    treat   = treat_label,
    var     = var_map[var],
    coef    = coef,
    se      = se,
    pval    = pval,
    nobs    = nobs,
    nclust  = nclust,
    source  = "reghdfe_stata_48"
  )]
}

decomp_out <- rbindlist(list(
  make_decomp_rows(decomp_qua, "ln_export_qua",   "WB"),
  make_decomp_rows(decomp_qua, "ln_export_qua",   "TREND"),
  make_decomp_rows(decomp_uv,  "ln_export_value", "WB"),
  make_decomp_rows(decomp_uv,  "ln_export_value", "TREND")
))
fwrite(decomp_out, file.path(OUT_DIR, "tripledd_decomp_collapsed.csv"))
cat(sprintf("Scritto: tripledd_decomp_collapsed.csv (%d righe)\n", nrow(decomp_out)))

cat("\n=== VERIFICHE FINALI ===\n")
trim <- fread(file.path(OUT_DIR, "tripledd_trimmed_collapsed.csv"))
cat("WB dirty coef:", trim[treat=="WB" & grepl("dirty_p", var)]$coef,
    "  (atteso ~-0.01159)\n")
cat("TREND dirty coef:", trim[treat=="TREND" & grepl("dirty_p", var)]$coef,
    "  (atteso ~+0.00025)\n")
cat("\nFATTO. I CSV asintotici sono ora basati su reghdfe Stata.\n")
cat("Passo successivo: lanciare il WCB in 46 (solo Part A2) con layer-2 vs Stata.\n")
