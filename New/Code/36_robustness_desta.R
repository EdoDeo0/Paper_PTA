########################################################
###### 36 — Robustezza: DESTA al posto di TotalDepth (§8.9, passo 3) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.9, passo 3 — la riduzione di corr. within (0,068) ha superato
## la soglia di rilevanza (0,05): si ristima la spec principale sostituendo
## TotalDepth_nonEnv con DESTA_depth_index come controllo di profondita'
## generale, e si confronta con la spec principale (16).
##
## DESTA_depth_index copre 212/223 country-year trattati (95,1%); le
## destinazioni mai coperte da DESTA (rest of world + East Timor) prendono
## DESTA_depth_index = 0, stessa convenzione di TotalDepth_nonEnv in 16.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 10)
##         New/Data/TotalDepth/desta_depth_country_year.csv (da 32)
## Output: New/Output/TripleDiff/Tables/tripledd_collapsed_desta.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fst)
library(callr)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DESTA_FILE <- here("New/Data/TotalDepth/desta_depth_country_year.csv")
OUT_DIR    <- here("New/Output/TripleDiff/Tables")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
stopifnot(file.exists(CACHE_FST), file.exists(DESTA_FILE))

## --- Caricamento dati ----------------------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
cat("Panel collassato:", format(nrow(cell), big.mark = ","), "celle\n")

green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]

desta <- fread(DESTA_FILE)[, .(country_code, year, DESTA_depth_index)]
cell[desta, on = c("country_code", "year"), DESTA_depth_index := i.DESTA_depth_index]
cell[is.na(DESTA_depth_index), DESTA_depth_index := 0]

cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]
cat(sprintf("green: %.1f%% celle | dirty: %.1f%% celle\n",
            100 * mean(cell$env_good), 100 * mean(cell$dirty_p)))

## --- Stima con DESTA al posto di TotalDepth --------------------------------
run_desta_model <- function(cell, tr, key) {
  library(fixest)
  library(data.table)
  f <- sprintf("y ~ %s:env_good + %s:dirty_p + DESTA_depth_index:env_good + DESTA_depth_index:dirty_p | pd + dt + pt", tr, tr)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)

  cell[, `:=`(ep_green = get(tr) * env_good, ep_dirty = get(tr) * dirty_p,
              ds_green = DESTA_depth_index * env_good, ds_dirty = DESTA_depth_index * dirty_p)]
  X <- as.matrix(fixest::demean(cell[, .(y, ep_green, ep_dirty, ds_green, ds_dirty)],
                                f = cell[, .(pd, dt, pt)], weights = cell$n))
  sw <- sqrt(cell$n)
  cf_check <- qr.solve(X[, -1] * sw, X[, "y"] * sw)
  cf_m <- coef(m)[c(sprintf("%s:env_good", tr), sprintf("%s:dirty_p", tr),
                    "env_good:DESTA_depth_index", "dirty_p:DESTA_depth_index")]
  if (max(abs(cf_check - cf_m)) > 1e-6) stop("Frisch-Waugh non riproduce feols: risultato non affidabile")

  data.table(treat = key, term = names(coef(m)), coef = coef(m),
            se = se(m), pval = pvalue(m), nobs = m$nobs)
}

res <- list()
for (tr in c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")) {
  key <- names(which(c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count") == tr))
  cat("Stima (DESTA):", key, "...\n")
  out <- NULL
  for (tent in 1:10) {
    out <- tryCatch(
      callr::r(run_desta_model, args = list(cell = cell, tr = tr, key = key), show = TRUE),
      error = function(e) { cat("[CRASH tentativo", tent, "]", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(out)) break
  }
  if (is.null(out)) stop(sprintf("Stima %s fallita dopo 10 tentativi", key))
  res[[key]] <- out
  print(res[[key]])
}
fwrite(rbindlist(res), out_path(file.path(OUT_DIR, "tripledd_collapsed_desta.csv")))
cat("[OK] tripledd_collapsed_desta.csv\n")
