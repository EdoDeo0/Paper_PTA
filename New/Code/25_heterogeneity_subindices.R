########################################################
###### 21 — Eterogeneita' per sotto-indice EP           ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 18_subindices_collapsed.R. Run: ~1 min se tutti gli 8
##              modelli sono gia' cachati (come atteso), altrimenti ~15 min.
##
## Cosa fa: l'indice aggregato EP somma clausole CON meccanismo commerciale
## (green market access, standard) e clausole SENZA (cooperazione,
## regulatory space): il null aggregato potrebbe nascondere un canale
## specifico che "morde". Qui la triple-diff del collassato (16) viene
## ristimata sostituendo all'indice aggregato un sotto-indice alla volta:
##   - meccanismo GREEN:  WB_GreenLiberalization, TREND_GreenMarketAccess
##   - meccanismo DIRTY:  WB_StandardsNonRegression
##   - moderatori:        WB_EnforcementDSM, TREND_EnforcementDSM, TREND_Hard
##   - PLACEBO (nessun meccanismo commerciale - se "mordono" e' selezione):
##                        TREND_Soft, TREND_RegulatorySpace
## Fonte sotto-indici: Data/Merged/Merged_TREND_WB_Indices_Only.csv (dest-anno, root).
## Un sottoprocesso callr per modello (allocatore R fragile), cache .rds.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         Data/Merged/Merged_TREND_WB_Indices_Only.csv (root)
## Output: New/Output/TripleDiff/Tables/subindices_collapsed.csv

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
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
IDX_FILE   <- here("Data/Merged/Merged_TREND_WB_Indices_Only.csv")
CACHE_DIR  <- here("New/Output/TripleDiff/Models_Output")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

SUBS <- c("WB_GreenLiberalization", "TREND_GreenMarketAccess",
          "WB_StandardsNonRegression",
          "WB_EnforcementDSM", "TREND_EnforcementDSM", "TREND_Hard",
          "TREND_Soft", "TREND_RegulatorySpace")

## --- Funzione: una stima, un sottoprocesso ----------------------------------
stima_sub <- function(cache_fst, green_file, dirty_file, depth_file, idx_file, sub_var) {
  library(fst)
  library(fixest)
  library(data.table)
  threads_fst(1)
  setFixest_nthreads(4)

  cell <- as.data.table(read_fst(cache_fst))
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]
  cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  idx <- fread(idx_file)
  idx <- idx[, c("country_code", "year", sub_var), with = FALSE]
  setnames(idx, sub_var, "SUB")
  cell[idx, on = c("country_code", "year"), SUB := i.SUB]
  cell[is.na(SUB), SUB := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  m <- feols(y ~ SUB:env_good + SUB:dirty_p +
               TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt,
             data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  list(coefs = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs)
}

## --- Esecuzione: cache per modello ------------------------------------------
rows <- list()
for (s in SUBS) {
  cat("===", s, "===\n")
  rds <- file.path(CACHE_DIR, sprintf("SUBIDX_%s%s.rds", s, SAMPLE_SUFFIX))
  if (file.exists(rds)) {
    r <- readRDS(rds)
  } else {
    r <- tryCatch(callr::r(stima_sub, args = list(
      cache_fst = CACHE_FST, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
      depth_file = DEPTH_FILE, idx_file = IDX_FILE, sub_var = s
    )), error = function(e) { cat("[FALLITO]", s, "\n"); NULL })
    if (!is.null(r)) saveRDS(r, rds)
  }
  if (!is.null(r)) {
    for (term in c("SUB:env_good", "SUB:dirty_p")) {
      cat(sprintf("  %s %-12s: %+.5f (p=%.4f)\n", s,
                  sub("SUB:", "x ", term), r$coefs[[term]], r$pval[[term]]))
    }
    rows[[s]] <- data.table(sub_index = s, term = names(r$coefs), coef = r$coefs,
                            se = r$se, pval = r$pval, nobs = r$nobs)
  }
}
out <- rbindlist(rows)
fwrite(out, out_path(here("New/Output/TripleDiff/Tables/subindices_collapsed.csv")))
cat("\n[OK] subindices_collapsed.csv\n")
