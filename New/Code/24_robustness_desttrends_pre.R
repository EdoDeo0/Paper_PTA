########################################################
###### 24 — Trend destinazione x green stimati SOLO sul pre-periodo ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 28_r79c_pretrend_variant.R. Run: ~3-5 min (nessuna cache).
##
## Cosa fa: la spec con trend su tutto il campione (22) inverte il segno di
## TREND×green ed e' significativa sotto WCB (23) - firma della
## contaminazione alla Wolfers (2006): il trend stimato sull'intero periodo
## puo' assorbire dinamica POST-trattamento invece di essere un puro
## controllo di tendenza pre-esistente. Variante pulita per costruzione: i
## trend per-destinazione del gap green/dirty si stimano SOLO sugli anni
## pre-accordo (mai-trattati: tutti gli anni), si proiettano su tutto il
## periodo e si sottraggono dall'outcome; poi si ristima la spec principale
## sul y detrendizzato. Il trend non puo' rubare nulla del post.
##
## Step:
##  1. per ogni destinazione: lm pesato y ~ year*env_good + year*dirty_p sui
##     soli anni pre (>=2 anni; slope green/dirty = coef year:env_good ecc.)
##  2. y_adj = y - slope_g[d]*(year-2000)*g - slope_b[d]*(year-2000)*b
##  3. feols y_adj ~ EP:g + EP:b + TD:g + TD:b | pd+dt+pt (WB e TREND)
##  4. WCB (Frisch-Waugh come 16) sulle interazioni EP
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/r79c_pretrends.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)

CACHE_FST  <- here("New/Data/Collapsed/panel_pdt_collapsed.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")

## --- Funzione: tutto in un sottoprocesso ------------------------------------
run_all <- function(cache_fst, green_file, dirty_file, depth_file) {
  library(fst)
  library(fixest)
  library(data.table)
  library(fwildclusterboot)
  threads_fst(1)
  setFixest_nthreads(1)

  cell <- as.data.table(read_fst(cache_fst))
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]
  cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]

  # anno di entrata (WB, come 19); mai-trattati: pre = tutti gli anni
  entry <- cell[WB_EP_Depth > 0, .(entry_year = min(year)), by = country_code]
  cell[entry, on = "country_code", entry_year := i.entry_year]
  cell[is.na(entry_year), entry_year := 10000L]

  ## 1. slope pre-periodo per destinazione (lm pesato sui soli anni pre)
  slopes <- cell[year < entry_year,
    {
      if (uniqueN(year) >= 2L && sum(env_good) > 0L) {
        m <- lm(y ~ year * env_good + year * dirty_p, weights = n, data = .SD)
        cf <- coef(m)
        .(slope_g = ifelse(is.na(cf["year:env_good"]), 0, cf[["year:env_good"]]),
          slope_b = ifelse(is.na(cf["year:dirty_p"]) | !"year:dirty_p" %in% names(cf),
                           0, cf[["year:dirty_p"]]))
      } else .(slope_g = 0, slope_b = 0)
    }, by = country_code]
  cat(sprintf("Slope pre-periodo stimate per %d destinazioni (0 imposto alle altre)\n",
              nrow(slopes)))
  cell[slopes, on = "country_code", `:=`(slope_g = i.slope_g, slope_b = i.slope_b)]
  cell[is.na(slope_g), `:=`(slope_g = 0, slope_b = 0)]

  ## 2. detrend (proiezione del trend pre su tutto il periodo)
  cell[, y_adj := y - slope_g * (year - 2000L) * env_good - slope_b * (year - 2000L) * dirty_p]

  ## 3-4. stima + WCB per ciascun indice
  out <- list()
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    cell[, `:=`(ep_green = get(tr) * env_good, ep_dirty = get(tr) * dirty_p,
                td_green = TotalDepth_nonEnv * env_good,
                td_dirty = TotalDepth_nonEnv * dirty_p)]
    m <- feols(y_adj ~ ep_green + ep_dirty + td_green + td_dirty | pd + dt + pt,
               data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
    cat(sprintf("[%s] asintotico: ep_green %+.5f (p=%.3f) | ep_dirty %+.5f (p=%.3f)\n",
                tr_name, coef(m)[["ep_green"]], pvalue(m)[["ep_green"]],
                coef(m)[["ep_dirty"]], pvalue(m)[["ep_dirty"]]))

    X <- fixest::demean(cell[, .(y_adj, ep_green, ep_dirty, td_green, td_dirty)],
                        f = cell[, .(pd, dt, pt)], weights = cell$n)
    df <- as.data.frame(X)
    df$n_w <- cell$n
    df$country_code <- cell$country_code
    rm(X)
    gc()
    m_lm <- lm(y_adj ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)
    dg <- abs(coef(m_lm)[["ep_green"]] - coef(m)[["ep_green"]])
    if (dg > 1e-5) stop("FW non riproduce feols")
    for (param in c("ep_green", "ep_dirty")) {
      set.seed(42)
      bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
      cat(sprintf("  [%s] %s: p_wcb = %.4f\n", tr_name, param, bt$p_val))
      out[[paste(tr_name, param)]] <- data.table(
        treat = tr_name, term = param,
        coef = coef(m)[[param]], se_asy = se(m)[[param]], p_asy = pvalue(m)[[param]],
        p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2],
        nobs = m$nobs, B = 9999L)
    }
    rm(m, m_lm, df)
    gc()
  }
  rbindlist(out)
}

## --- Esecuzione con retry -----------------------------------------------
res <- NULL
for (tent in 1:4) {
  cat(sprintf("Variante pre-period trends (tentativo %d)...\n", tent))
  res <- tryCatch(callr::r(run_all, args = list(
    cache_fst = CACHE_FST, green_file = GREEN_FILE, dirty_file = DIRTY_FILE, depth_file = DEPTH_FILE
  ), show = TRUE), error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) break
}
if (is.null(res)) stop("Fallito dopo 4 tentativi")
print(res)
fwrite(res, here("New/Output/TripleDiff/Tables/r79c_pretrends.csv"))
cat("[OK] r79c_pretrends.csv\n")
