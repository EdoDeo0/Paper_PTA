########################################################
###### 27 — Robustezza: leave-one-out sul coefficiente dirty ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 15b_dirty_leaveoneout.R. Run: ~10-15 min (nessuna cache: 27
##              stime, una per sottoprocesso callr).
##
## Cosa fa: EP(WB)xdirty e' negativo e asintoticamente significativo sul
## panel collassato (16). Con ~25 paesi trattati un singolo paese puo'
## generare da solo il risultato. Due controlli:
##   1. LEAVE-ONE-OUT: ristima escludendo un paese trattato alla volta.
##   2. LISTA ESTESA: dirty_ext (con cemento/minerali non metallici, 06) al
##      posto del core Mani-Wheeler.
##
## NOTA TECNICA: su questa macchina l'allocatore R crasha spesso alla
## SECONDA feols nella stessa sessione ("recursive gc invocation"). Pattern
## anti-crash: OGNI stima nel proprio sottoprocesso callr, con tryCatch.
## Costo: ricarica la cache del panel collassato (~55MB) a ogni stima -
## accettabile.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/dirty_leaveoneout.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)
library(fst)
source(here("New/Code/_sample_config.R"))

CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
## --- Funzione: una singola stima, self-contained ---------------------------
stima_una <- function(cache_fst, green_file, dirty_file, depth_file, drop_cc, dirty_var,
                      depth_var, depth_drop_unmeasured) {
  library(fst)
  library(fixest)
  library(data.table)
  threads_fst(1)
  setFixest_nthreads(4)

  cell <- as.data.table(read_fst(cache_fst))
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty, dirty_ext = dirty_ext)]
  cell[dirty, on = "hs6", `:=`(dirty_p = i.dirty_p, dirty_ext = i.dirty_ext)]
  cell[is.na(dirty_p), dirty_p := 0L]
  cell[is.na(dirty_ext), dirty_ext := 0L]
  dep <- fread(depth_file)[, .(country_code, year, dep_val__ = get(depth_var))]
  cell[dep, on = c("country_code", "year"), (depth_var) := i.dep_val__]
  if (depth_drop_unmeasured) {
    n0 <- nrow(cell)
    cell <- cell[!(is.na(get(depth_var)) & WB_EP_Depth > 0)]
    cat(sprintf("[depth] %s: %d celle trattate senza copertura escluse (%.3f%%)\n",
                depth_var, n0 - nrow(cell), 100 * (n0 - nrow(cell)) / n0))
  }
  cell[is.na(get(depth_var)), (depth_var) := 0]
  if (!is.na(drop_cc)) cell <- cell[country_code != drop_cc]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  f <- sprintf("y ~ WB_EP_Depth:env_good + WB_EP_Depth:%s + %s:env_good + %s:%s | pd + dt + pt",
               dirty_var, depth_var, depth_var, dirty_var)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  key <- sprintf("WB_EP_Depth:%s", dirty_var)
  list(coef = coef(m)[[key]], pval = pvalue(m)[[key]])
}

## --- Lista dei paesi trattati (lettura leggera, senza fixest) --------------
treated <- sort(unique(as.data.table(
  read_fst(CACHE_FST, columns = c("country_code", "WB_EP_Depth")))[WB_EP_Depth > 0, country_code]))
cat("Paesi trattati:", length(treated), "\n")

## --- Piano: baseline core, robustezza dirty_ext, poi leave-one-out --------
piano <- rbind(
  data.table(drop_cc = NA_integer_, dirty_var = "dirty_p",   label = "baseline"),
  data.table(drop_cc = NA_integer_, dirty_var = "dirty_ext", label = "lista_estesa"),
  data.table(drop_cc = treated,     dirty_var = "dirty_p",   label = paste0("senza_", treated))
)

## resumabilita': se un run precedente ha gia' scritto delle righe (alcuni
## sottoprocessi crashano quasi sempre almeno una volta su questa macchina),
## riparte da li' invece di rifare tutto - stesso spirito degli altri script
## con cache, applicato qui via CSV invece di .rds per-spec
OUT_FILE <- out_path(here("New/Output/TripleDiff/Tables/dirty_leaveoneout.csv"))
rows <- list()
if (file.exists(OUT_FILE)) {
  prev <- fread(OUT_FILE)
  for (lbl in prev$spec) rows[[lbl]] <- prev[spec == lbl]
  cat("Righe gia' presenti da un run precedente:", length(rows), "\n")
}

for (i in seq_len(nrow(piano))) {
  p <- piano[i]
  if (!is.null(rows[[p$label]])) { cat("[cache]", p$label, "\n"); next }
  r <- NULL
  for (tent in 1:4) {
    r <- tryCatch(
      callr::r(stima_una, args = list(
        cache_fst = CACHE_FST, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
        depth_file = DEPTH_FILE, drop_cc = p$drop_cc, dirty_var = p$dirty_var,
        depth_var = DEPTH_VAR, depth_drop_unmeasured = DEPTH_DROP_UNMEASURED
      )),
      error = function(e) { cat("[CRASH tentativo", tent, "]", p$label, "\n"); NULL })
    if (!is.null(r)) break
  }
  if (is.null(r)) cat("[FALLITO dopo 4 tentativi]", p$label, "\n")
  if (!is.null(r)) {
    cat(sprintf("%-14s: %+.5f (p=%.4f)\n", p$label, r$coef, r$pval))
    rows[[p$label]] <- data.table(spec = p$label, dropped_country = p$drop_cc,
                                  coef = r$coef, pval = r$pval)
    # salvataggio incrementale: se il PROCESSO PADRE stesso crasha, non si
    # perde il lavoro gia' fatto
    fwrite(rbindlist(rows), OUT_FILE)
  }
}
loo <- rbindlist(rows)
fwrite(loo, OUT_FILE)

## --- Verdetto automatico (rispetto alla baseline core) ---------------------
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
