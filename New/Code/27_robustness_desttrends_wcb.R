########################################################
###### 23 — WCB sulla spec con trend destinazione x green/dirty ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 27_r79b_wcb_trends.R. Run: ~3-5 min (nessuna cache: B=9999
##              boottest x 2 termini x 2 indici, ogni volta).
##
## Cosa fa: la spec con trend (26) produce TREND×green con segno invertito
## rispetto alla baseline e p ASINTOTICO marginale. Il paper insegna che le
## stelle asintotiche con 23 cluster trattati vanno bootstrappate prima di
## crederci. Stesso trucco di 16 (Frisch-Waugh + boottest), esteso ai
## varying slopes: demean() con slope.vars/slope.flag per assorbire anche
## country[trend_g] e country[trend_b]. VERIFICA DI EQUIVALENZA: i coef
## dell'lm demeanato devono coincidere con quelli di 22 (stampati e
## confrontati) - se non coincidono lo script si ferma.
##
## NOTA: i p_wcb non sono esattamente riproducibili run-to-run (fwildcluster
## boot >=0.13 usa un RNG dqrng non seedato da set.seed(), vedi nota su 16) -
## i coefficienti (deterministici) si'.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         New/Output/TripleDiff/Tables/r79_desttrends.csv (da 22, per la verifica)
## Output: New/Output/TripleDiff/Tables/r79b_wcb_trends.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)
source(here("New/Code/_sample_config.R"))

CACHE_FST   <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE  <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE  <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DESTTRENDS_FILE <- out_path(here("New/Output/TripleDiff/Tables/r79_desttrends.csv"))

## --- Funzione: tutto in un sottoprocesso, con verifica FW interna ----------
run_wcb <- function(cache_fst, green_file, dirty_file, depth_file, desttrends_file,
                    depth_var, depth_drop_unmeasured) {
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
  dep <- fread(depth_file)[, .(country_code, year, dep_val__ = as.numeric(get(depth_var)))]
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
  cell[, trend_g := (year - 2000L) * env_good]
  cell[, trend_b := (year - 2000L) * dirty_p]

  # coefficienti attesi da 22 (per la verifica di equivalenza)
  attesi <- fread(desttrends_file)

  out <- list()
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    cell[, `:=`(ep_green = get(tr) * env_good, ep_dirty = get(tr) * dirty_p,
                td_green = get(depth_var) * env_good,
                td_dirty = get(depth_var) * dirty_p)]
    X <- fixest::demean(cell[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                        f = cell[, .(pd, dt, pt, country_code)],
                        slope.vars = cell[, .(trend_g, trend_b)],
                        slope.flag = c(0L, 0L, 0L, -2L),
                        weights = cell$n)
    df <- as.data.frame(X)
    df$n_w <- cell$n
    df$country_code <- cell$country_code
    rm(X)
    gc()
    m_lm <- lm(y ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)

    # verifica: FW deve riprodurre i coef di 22
    att_g <- attesi[treat == tr & grepl(":env_good$", term), coef]
    att_b <- attesi[treat == tr & grepl(":dirty_p$", term), coef]
    dg <- abs(coef(m_lm)[["ep_green"]] - att_g); db <- abs(coef(m_lm)[["ep_dirty"]] - att_b)
    cat(sprintf("[%s] FW ep_green %+.7f (22: %+.7f, diff %.1e) | ep_dirty %+.7f (22: %+.7f, diff %.1e)\n",
                tr_name, coef(m_lm)[["ep_green"]], att_g, dg,
                coef(m_lm)[["ep_dirty"]], att_b, db))
    if (dg > 1e-5 || db > 1e-5) stop("Frisch-Waugh NON riproduce i coefficienti di 22: fermo qui.")

    for (param in c("ep_green", "ep_dirty")) {
      set.seed(42)
      bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
      cat(sprintf("  [%s] %s: p_wcb = %.4f\n", tr_name, param, bt$p_val))
      out[[paste(tr_name, param)]] <- data.table(
        treat = tr_name, term = param, coef = coef(m_lm)[[param]],
        p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L)
    }
    rm(m_lm, df)
    gc()
  }
  rbindlist(out)
}

## --- Esecuzione con retry -----------------------------------------------
res <- NULL
for (tent in 1:4) {
  cat(sprintf("WCB spec con trend (tentativo %d)...\n", tent))
  res <- tryCatch(callr::r(run_wcb, args = list(
    cache_fst = CACHE_FST, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
    depth_file = DEPTH_FILE, desttrends_file = DESTTRENDS_FILE,
    depth_var = DEPTH_VAR, depth_drop_unmeasured = DEPTH_DROP_UNMEASURED
  ), show = TRUE), error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) break
}
if (is.null(res)) stop("Fallito dopo 4 tentativi")
print(res)
fwrite(res, out_path(here("New/Output/TripleDiff/Tables/r79b_wcb_trends.csv")))
cat("[OK] r79b_wcb_trends.csv\n")
