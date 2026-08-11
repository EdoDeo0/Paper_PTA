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
  ## 2 thread come 20/29 (stabili): a 4 l'allocatore crasha quasi a ogni spec
  setFixest_nthreads(2)

  cell <- as.data.table(read_fst(cache_fst))
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty, dirty_ext = dirty_ext)]
  cell[dirty, on = "hs6", `:=`(dirty_p = i.dirty_p, dirty_ext = i.dirty_ext)]
  cell[is.na(dirty_p), dirty_p := 0L]
  cell[is.na(dirty_ext), dirty_ext := 0L]
  dep <- fread(depth_file)[, .(country_code, year, dep_val__ = as.numeric(get(depth_var)))]
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
  ## Interazioni esplicite + solo le colonne che servono a feols. Sul campione
  ## intero incl le colonne inutili portano l'allocatore oltre la soglia e la
  ## stima segfaulta sempre; potandole gira in ~6s (vedi memoria di progetto).
  ## Il prodotto esplicito e' identico alla sintassi `a:b` di fixest: verificato
  ## contro il baseline di Run 1 (scarto 3.6e-17) e contro l'output di 16.
  cell[, `:=`(ep_green = WB_EP_Depth    * env_good,
              ep_dirty = WB_EP_Depth    * get(dirty_var),
              td_green = get(depth_var) * env_good,
              td_dirty = get(depth_var) * get(dirty_var))]
  cell <- cell[, .(y, n, country_code, pd, dt, pt, ep_green, ep_dirty, td_green, td_dirty)]
  gc()
  m <- feols(y ~ ep_green + ep_dirty + td_green + td_dirty | pd + dt + pt,
             data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  list(coef = coef(m)[["ep_dirty"]], pval = pvalue(m)[["ep_dirty"]])
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
  ## In-process: dentro callr::r() l'allocatore crasha molto piu' spesso (il 29
  ## crashava 4 volte su 4, in-process gira in 54s). Qui il crash resta possibile
  ## ma e' intermittente (~1 su 4) e uccide il processo, quindi non e'
  ## catturabile da tryCatch: la protezione e' il salvataggio incrementale del
  ## CSV + il riavvio esterno, che riprende dalle righe gia' scritte.
  r <- tryCatch(
    stima_una(
      cache_fst = CACHE_FST, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
      depth_file = DEPTH_FILE, drop_cc = p$drop_cc, dirty_var = p$dirty_var,
      depth_var = DEPTH_VAR, depth_drop_unmeasured = DEPTH_DROP_UNMEASURED
    ),
    error = function(e) { cat("[CRASH]", p$label, ":", conditionMessage(e), "\n"); NULL })
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

## Un leave-one-out incompleto non deve passare in silenzio: senza questo
## controllo lo script esce con successo anche se meta' delle stime e' fallita.
mancanti <- setdiff(piano$label, loo$spec)
if (length(mancanti))
  stop(sprintf("leave-one-out incompleto: %d spec mancanti (%s)",
               length(mancanti), paste(mancanti, collapse = ", ")))

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
