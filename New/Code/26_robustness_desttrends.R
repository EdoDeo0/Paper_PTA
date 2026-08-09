########################################################
###### 22 — Robustezza: trend lineari destinazione x green/dirty ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 26_r79_desttrends.R. Run: ~1 min se cachato (come atteso).
##
## Cosa fa: se le destinazioni con EP profonde hanno una domanda green in
## crescita PRE-esistente, il confronto green-vs-neutro potrebbe raccogliere
## quella deriva invece dell'effetto del PTA. Qui si aggiunge alla spec
## collassata (16) un trend lineare destinazione-specifico del gap green e
## dirty:
##   y ~ EP:g + EP:b + TD:g + TD:b | pd + dt + pt + country[t*g] + country[t*b]
## Se il null regge anche al netto di derive lineari per-destinazione, il
## confondente "preferenze verdi in crescita" e' controllato (nella sua
## forma lineare - la piu' plausibile su 16 anni).
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/r79_desttrends.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)
source(here("New/Code/_sample_config.R"))

## --- Parametri e percorsi --------------------------------------------------
CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
CACHE_DIR  <- here("New/Output/TripleDiff/Models")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Funzione: una stima, un sottoprocesso (pattern anti-crash) -----------
stima_trend <- function(cache_fst, green_file, dirty_file, depth_file, treat_var,
                        depth_var, depth_drop_unmeasured) {
  library(fst)
  library(fixest)
  library(data.table)
  threads_fst(1)
  setFixest_nthreads(1)

  cell <- as.data.table(read_fst(cache_fst))
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]
  cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, dep_val__ = get(depth_var))]
  cell[dep, on = c("country_code", "year"), (depth_var) := i.dep_val__]
  if (depth_drop_unmeasured) {
    n0 <- nrow(cell)
    cell <- cell[!(is.na(get(depth_var)) & WB_EP_Depth > 0)]
    cat(sprintf("[depth] %s: %d celle trattate senza copertura escluse (%.3f%%)\n",
                depth_var, n0 - nrow(cell), 100 * (n0 - nrow(cell)) / n0))
  }
  cell[is.na(get(depth_var)), (depth_var) := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  # trend lineari per-destinazione del gap green/dirty
  cell[, trend_g := (year - 2000L) * env_good]
  cell[, trend_b := (year - 2000L) * dirty_p]

  f <- sprintf("y ~ %s:env_good + %s:dirty_p + %s:env_good + %s:dirty_p | pd + dt + pt + country_code[trend_g] + country_code[trend_b]",
               treat_var, treat_var, depth_var, depth_var)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  data.table(treat = treat_var, term = names(coef(m)), coef = coef(m),
             se = se(m), pval = pvalue(m), nobs = m$nobs)
}

## --- Esecuzione: cache per indice, retry -----------------------------------
res <- list()
for (tv in c("WB_EP_Depth", "TREND_EP_Count")) {
  rds <- file.path(CACHE_DIR, sprintf("r79_desttrends_%s%s.rds", tv, OUT_SUFFIX))
  if (file.exists(rds)) { res[[tv]] <- readRDS(rds); cat("[cache]", tv, "\n"); next }
  ok <- FALSE
  for (tent in 1:4) {
    cat(sprintf("Stima %s (tentativo %d)...\n", tv, tent))
    r <- tryCatch(callr::r(stima_trend, args = list(
      cache_fst = CACHE_FST, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
      depth_file = DEPTH_FILE, treat_var = tv,
      depth_var = DEPTH_VAR, depth_drop_unmeasured = DEPTH_DROP_UNMEASURED
    ), show = TRUE), error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
    if (!is.null(r)) { saveRDS(r, rds); res[[tv]] <- r; ok <- TRUE; break }
  }
  if (!ok) cat("[FALLITO dopo 4 tentativi]", tv, "\n")
}

out <- rbindlist(res)
print(out)
fwrite(out, out_path(here("New/Output/TripleDiff/Tables/r79_desttrends.csv")))
cat("[OK] r79_desttrends.csv\n")
