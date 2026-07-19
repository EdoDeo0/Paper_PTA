########################################################################
###### Fase R3 — Chiusura pista dirty: leave-one-out + lista estesa  ###
########################################################################

## Author: Edoardo Vitella
##
## La pista: EP(WB)×dirty = -0,0089 (p asintotico 0,006; ma p_wcb = 0,18 da 15)
## sul panel collassato. Con ~25 paesi trattati un singolo paese può generare
## da solo il risultato. Due controlli:
##   1. LEAVE-ONE-OUT: ristima escludendo un paese trattato alla volta.
##   2. LISTA ESTESA: dirty_ext (con cemento/minerali non metallici) al posto
##      del core Mani-Wheeler.
##
## NOTA TECNICA: su questa macchina l'allocatore R crasha alla SECONDA feols
## nella stessa sessione ("recursive gc invocation", visto 2026-07-06 anche
## sul panel collassato). Pattern che funziona (come 01_inference_fix a
## giugno): OGNI stima nel proprio sottoprocesso callr, con tryCatch.
## Costo: ricarica la cache (~150MB) a ogni stima — accettabile.
##
## Output: New/Output/TripleDiff/Tables/dirty_leaveoneout.csv

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here); library(data.table)

## Una singola stima, self-contained: carica, filtra, stima, restituisce.
stima_una <- function(drop_cc, dirty_var) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(4)
  base <- "C:/Work/projects/Paper_PTA"
  cell <- as.data.table(read_fst(file.path(base, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[
    , .(hs6 = as.integer(hs6), dirty_p = dirty, dirty_ext = dirty_ext)]
  cell[dirty, on = "hs6", `:=`(dirty_p = i.dirty_p, dirty_ext = i.dirty_ext)]
  cell[is.na(dirty_p), dirty_p := 0L]; cell[is.na(dirty_ext), dirty_ext := 0L]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  if (!is.na(drop_cc)) cell <- cell[country_code != drop_cc]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  f <- sprintf("y ~ WB_EP_Depth:env_good + WB_EP_Depth:%s + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:%s | pd + dt + pt",
               dirty_var, dirty_var)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  key <- sprintf("WB_EP_Depth:%s", dirty_var)
  list(coef = coef(m)[[key]], pval = pvalue(m)[[key]])
}

## Lista dei paesi trattati (lettura leggera dalla cache, senza fixest)
library(fst)
treated <- sort(unique(as.data.table(
  read_fst(here("New/Data/Collapsed/panel_pdt_collapsed.fst"),
           columns = c("country_code", "WB_EP_Depth")))[WB_EP_Depth > 0, country_code]))
cat("Paesi trattati:", length(treated), "\n")

## Piano: baseline core, robustezza dirty_ext, poi leave-one-out per paese
piano <- rbind(
  data.table(drop_cc = NA_integer_, dirty_var = "dirty_p",   label = "baseline"),
  data.table(drop_cc = NA_integer_, dirty_var = "dirty_ext", label = "lista_estesa"),
  data.table(drop_cc = treated,     dirty_var = "dirty_p",   label = paste0("senza_", treated))
)

rows <- list()
for (i in seq_len(nrow(piano))) {
  p <- piano[i]
  r <- tryCatch(
    callr::r(stima_una, args = list(drop_cc = p$drop_cc, dirty_var = p$dirty_var)),
    error = function(e) { cat("[FALLITO]", p$label, "\n"); NULL })
  if (!is.null(r)) {
    cat(sprintf("%-14s: %+.5f (p=%.4f)\n", p$label, r$coef, r$pval))
    rows[[p$label]] <- data.table(spec = p$label, dropped_country = p$drop_cc,
                                  coef = r$coef, pval = r$pval)
  }
}
loo <- rbindlist(rows)
fwrite(loo, here("New/Output/TripleDiff/Tables/dirty_leaveoneout.csv"))

## Verdetto automatico (rispetto alla baseline core)
b0 <- loo[spec == "baseline", coef]
sub <- loo[grepl("^senza_", spec)]
cat("\n=== VERDETTO leave-one-out ===\n")
cat(sprintf("Cambi di segno: %d/%d | p>0.10: %d/%d\n",
            sub[sign(coef) != sign(b0), .N], nrow(sub), sub[pval > 0.10, .N], nrow(sub)))
if (sub[sign(coef) != sign(b0), .N] > 0)
  cat("Segno invertito togliendo:", sub[sign(coef) != sign(b0), dropped_country], "\n")
if (sub[pval > 0.10, .N] > 0)
  cat("Significativita' persa togliendo:", sub[pval > 0.10, dropped_country], "\n")
cat("[OK] dirty_leaveoneout.csv\n")
