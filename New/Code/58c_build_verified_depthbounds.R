########################################################
###### 58c — CSV depthbounds verificati da Stata       ###
########################################################
## Author: Edoardo Vitella
##
## Cosa fa: riscrive i tre CSV della banda depthbounds usando le stime
## reghdfe di Stata (52_omnibus_collapsed.do) al posto delle run R del
## 2026-08-07 (notte), che l'audit del 23/08 ha trovato stantie rispetto
## agli input correnti (scarti ~0,1-0,4% relativo sui coefficienti).
##
## Nessuna stima qui dentro: solo lettura dei .dta di regsave e riscrittura
## nello stesso schema colonne dei CSV attuali, con l'aggiunta di `source`.
##
## Input:  New/Output/TripleDiff/Tables_Stata/OMNI_nodepth_{WB,TREND}.dta
##         New/Output/TripleDiff/Tables_Stata/OMNI_targeted_{WB,TREND}.dta
##         New/Output/TripleDiff/Tables_Stata/OMNI_epshare_WB.dta
## Output: New/Output/TripleDiff/Tables/tripledd_collapsed_nodepth.csv
##         New/Output/TripleDiff/Tables/tripledd_collapsed_targeted.csv
##         New/Output/TripleDiff/Tables/tripledd_collapsed_targeted_TREND.csv
##         New/Output/TripleDiff/Tables/tripledd_epshare_treatedonly.csv
##
## Esecuzione (da PowerShell, root progetto):
##   Rscript New/Code/58c_build_verified_depthbounds.R

rm(list = ls())
library(data.table)
library(haven)
library(here)

STATA_DIR <- here("New/Output/TripleDiff/Tables_Stata")
OUT_DIR   <- here("New/Output/TripleDiff/Tables")
SOURCE_TAG <- "reghdfe_stata_52"

rd_dta <- function(f) {
  p <- file.path(STATA_DIR, f)
  stopifnot("manca il .dta: lanciare prima stata/52_omnibus_collapsed.do" = file.exists(p))
  as.data.table(read_dta(p))[var != "_cons"]
}

## Nomi dei termini come li scrivono gli script R (e come li legge 44).
term_of <- function(v, treat, depth_label) {
  ep <- if (treat == "WB") "WB_EP_Depth" else "TREND_EP_Count"
  c(ep_green = paste0(ep, ":env_good"),
    ep_dirty = paste0(ep, ":dirty_p"),
    td_green = paste0("env_good:", depth_label),
    td_dirty = paste0("dirty_p:", depth_label))[v]
}

rows_for <- function(f, treat, depth_label) {
  d <- rd_dta(f)
  data.table(
    treat  = treat,
    term   = unname(term_of(d$var, treat, depth_label)),
    coef   = d$coef,
    se     = d$stderr,
    pval   = d$pval,
    nobs   = d$N,
    source = SOURCE_TAG)
}

## Confronto col CSV attuale prima di sovrascriverlo. Tolleranza: 1% del
## coefficiente, con pavimento assoluto 5e-4 per i coefficienti vicini a zero.
## Serve una soglia relativa perche' EP_share e' un rapporto e i suoi
## coefficienti stanno su una scala ~200x quella delle altre spec. Uno scarto
## sopra soglia significa spec non corrispondente: fermarsi invece di scrivere.
report_delta <- function(old_file, new_dt) {
  if (!file.exists(old_file)) { cat("  (nessun CSV precedente)\n"); return(invisible()) }
  old <- fread(old_file)
  key <- if ("treat" %in% names(old)) c("treat", "term") else "term"
  cmp <- merge(old[, c(key, "coef"), with = FALSE],
               new_dt[, c(key, "coef"), with = FALSE],
               by = key, suffixes = c("_old", "_new"))
  cmp[, delta := coef_new - coef_old]
  cmp[, tol   := pmax(5e-4, 0.01 * abs(coef_old))]
  for (i in seq_len(nrow(cmp)))
    cat(sprintf("  %-32s %+.8f -> %+.8f  (delta %+.2e, tol %.2e)\n",
                cmp$term[i], cmp$coef_old[i], cmp$coef_new[i], cmp$delta[i], cmp$tol[i]))
  if (any(abs(cmp$delta) > cmp$tol)) {
    bad <- cmp[abs(delta) > tol]
    stop(sprintf("scarto oltre tolleranza su %s: spec non corrispondente, non sovrascrivo.",
                 paste(bad$term, collapse = ", ")))
  }
  cat(sprintf("  scarto relativo massimo: %.3f%% (entro tolleranza)\n",
              100 * max(abs(cmp$delta) / pmax(abs(cmp$coef_old), 1e-12))))
}

write_verified <- function(new_dt, out_name) {
  out_file <- file.path(OUT_DIR, out_name)
  cat("\n==", out_name, "==\n")
  report_delta(out_file, new_dt)
  fwrite(new_dt, out_file)
  cat(sprintf("  scritto (%d righe, source=%s)\n", nrow(new_dt), SOURCE_TAG))
}

## --- nodepth (nessun controllo di profondita': solo le due interazioni EP) ---
nodepth <- rbind(
  rows_for("OMNI_nodepth_WB.dta",    "WB",    NA_character_),
  rows_for("OMNI_nodepth_TREND.dta", "TREND", NA_character_))
write_verified(nodepth, "tripledd_collapsed_nodepth.csv")

## --- targeted (TotalDepth ristretto alle 14 aree ad alta correlazione) ------
targeted <- rbind(
  rows_for("OMNI_targeted_WB.dta",    "WB",    "TotalDepth_targeted"),
  rows_for("OMNI_targeted_TREND.dta", "TREND", "TotalDepth_targeted"))
write_verified(targeted, "tripledd_collapsed_targeted.csv")
write_verified(targeted[treat == "TREND"], "tripledd_collapsed_targeted_TREND.csv")

## --- EP_share (solo trattati; schema senza colonna treat) -------------------
eps_raw <- rd_dta("OMNI_epshare_WB.dta")
epshare <- data.table(
  term   = c(sh_green = "EP_share:env_good",
             sh_dirty = "EP_share:dirty_p")[eps_raw$var],
  coef   = eps_raw$coef,
  se     = eps_raw$stderr,
  pval   = eps_raw$pval,
  nobs   = eps_raw$N,
  source = SOURCE_TAG)
epshare[, term := unname(term)]
write_verified(epshare, "tripledd_epshare_treatedonly.csv")

cat("\nFATTO. Rilanciare 44_make_tables_tex.R e confrontare ptab_depthbounds.tex.\n")
