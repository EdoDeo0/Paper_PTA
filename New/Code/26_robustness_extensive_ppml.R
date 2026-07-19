########################################################
###### 26 — Robustezza: PPML con zeri (margine estensivo) ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 20_ppml_extensive.R. Run: ~1 min se cachato (come atteso).
##
## Cosa fa: l'OLS su ln(export) vede solo i flussi positivi (margine
## intensivo). L'eventuale "green trade creation" - nuove combinazioni
## prodotto-mercato che nascono grazie all'accordo - vive nel margine
## estensivo: serve PPML su un panel con gli ZERI (Santos Silva & Tenreyro
## 2006). Usa il file gia' pronto Data/Final Dataset/ppml_agg_pdt_zerofill.fst
## (8,3M celle hs6 x dest x anno, zero-filled, FE precalcolate pd/dt/pt).
## Igiene consueta: env_good ricalcolato HS1996, dirty da 02, TotalDepth da
## 04; HK+MO esclusi.
##
## Input:  Data/Final Dataset/ppml_agg_pdt_zerofill.fst (root)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/ppml_extensive.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)

PPML_FILE  <- here("Data/Final Dataset/ppml_agg_pdt_zerofill.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
CACHE_DIR  <- here("New/Output/TripleDiff/Models_Output")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Funzione: una stima, un sottoprocesso (allocatore fragile) -----------
stima_ppml <- function(ppml_file, green_file, dirty_file, depth_file, tr) {
  library(fst)
  library(fixest)
  library(data.table)
  threads_fst(1)
  setFixest_nthreads(4)

  d <- as.data.table(read_fst(ppml_file, columns = c(
    "agg_export", "hs6", "country_code", "year",
    "WB_EP_Depth", "TREND_EP_Count", "pd", "dt", "pt")))
  d <- d[!country_code %in% c(110, 121)]
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  d[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  d[dirty, on = "hs6", dirty_p := i.dirty_p]
  d[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, TotalDepth_nonEnv)]
  d[, country_code := as.integer(country_code)]
  d[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  d[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cat(sprintf("[%s] celle: %s | quota zeri: %.1f%%\n", tr,
              format(nrow(d), big.mark = ","), 100 * mean(d$agg_export == 0)))
  f <- sprintf("agg_export ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt", tr, tr)
  m <- fepois(as.formula(f), data = d, cluster = ~country_code, lean = TRUE)
  list(coefs = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs)
}

## --- Esecuzione: cache per indice --------------------------------------------
rows <- list()
for (tr in c("WB_EP_Depth", "TREND_EP_Count")) {
  tr_name <- if (tr == "WB_EP_Depth") "WB" else "TREND"
  cat("=== PPML", tr_name, "===\n")
  rds <- file.path(CACHE_DIR, sprintf("PPML_ext_%s.rds", tr_name))
  if (file.exists(rds)) {
    r <- readRDS(rds)
  } else {
    r <- tryCatch(callr::r(stima_ppml, args = list(
      ppml_file = PPML_FILE, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
      depth_file = DEPTH_FILE, tr = tr
    )), error = function(e) { cat("[FALLITO]", tr_name, "\n"); NULL })
    if (!is.null(r)) saveRDS(r, rds)
  }
  if (!is.null(r)) {
    print(round(cbind(coef = r$coefs, pval = r$pval), 5))
    rows[[tr_name]] <- data.table(treat = tr_name, term = names(r$coefs),
                                  coef = r$coefs, se = r$se, pval = r$pval, nobs = r$nobs)
  }
}
if (length(rows)) {
  fwrite(rbindlist(rows), here("New/Output/TripleDiff/Tables/ppml_extensive.csv"))
  cat("[OK] ppml_extensive.csv\n")
}
