########################################################################
###### R7.9b — WCB sulla spec con trend destinazione×green/dirty     ###
########################################################################

## Author: Edoardo Vitella
##
## La spec con trend (26) produce TREND×green = -0,0022 con p ASINTOTICO
## 0,0015 (segno invertito vs baseline +0,0018 n.s.). Il paper insegna che
## le stelle asintotiche con 23 cluster trattati vanno bootstrappate prima
## di crederci. Stesso trucco di 15 (Frisch-Waugh + boottest), esteso ai
## varying slopes: demean() con slope.vars/slope.flag per assorbire anche
## country[trend_g] e country[trend_b]. VERIFICA DI EQUIVALENZA: i coef
## dell'lm demeanato devono coincidere con quelli di 26 (stampati e
## confrontati) — se non coincidono lo script si ferma.
##
## Output: New/Output/TripleDiff/Tables/r79b_wcb_trends.csv

library(callr); library(here); library(data.table)

run_wcb <- function() {
  library(fst); library(fixest); library(data.table); library(fwildclusterboot)
  threads_fst(1); setFixest_nthreads(1)
  base <- "C:/Work/projects/Paper_PTA"
  cell <- as.data.table(read_fst(file.path(base, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  cell[, trend_g := (year - 2000L) * env_good]
  cell[, trend_b := (year - 2000L) * dirty_p]

  ## coefficienti attesi da 26 (per la verifica di equivalenza)
  attesi <- fread(file.path(base, "New/Output/TripleDiff/Tables/r79_desttrends.csv"))

  out <- list()
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    cell[, `:=`(ep_green = get(tr) * env_good, ep_dirty = get(tr) * dirty_p,
                td_green = TotalDepth_nonEnv * env_good,
                td_dirty = TotalDepth_nonEnv * dirty_p)]
    X <- fixest::demean(cell[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                        f = cell[, .(pd, dt, pt, country_code)],
                        slope.vars = cell[, .(trend_g, trend_b)],
                        slope.flag = c(0L, 0L, 0L, -2L),
                        weights = cell$n)
    df <- as.data.frame(X); df$n_w <- cell$n; df$country_code <- cell$country_code
    rm(X); gc()
    m_lm <- lm(y ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)

    ## verifica: FW deve riprodurre i coef di 26
    att_g <- attesi[treat == tr & grepl(":env_good$", term), coef]
    att_b <- attesi[treat == tr & grepl(":dirty_p$", term), coef]
    dg <- abs(coef(m_lm)[["ep_green"]] - att_g); db <- abs(coef(m_lm)[["ep_dirty"]] - att_b)
    cat(sprintf("[%s] FW ep_green %+.7f (26: %+.7f, diff %.1e) | ep_dirty %+.7f (26: %+.7f, diff %.1e)\n",
                tr_name, coef(m_lm)[["ep_green"]], att_g, dg,
                coef(m_lm)[["ep_dirty"]], att_b, db))
    if (dg > 1e-5 || db > 1e-5) stop("Frisch-Waugh NON riproduce i coefficienti di 26: fermo qui.")

    for (param in c("ep_green", "ep_dirty")) {
      set.seed(42)
      bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
      cat(sprintf("  [%s] %s: p_wcb = %.4f\n", tr_name, param, bt$p_val))
      out[[paste(tr_name, param)]] <- data.table(
        treat = tr_name, term = param, coef = coef(m_lm)[[param]],
        p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L)
    }
    rm(m_lm, df); gc()
  }
  rbindlist(out)
}

res <- NULL
for (tent in 1:4) {
  cat(sprintf("WCB spec con trend (tentativo %d)...\n", tent))
  res <- tryCatch(callr::r(run_wcb, show = TRUE),
                  error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) break
}
if (is.null(res)) stop("Fallito dopo 4 tentativi")
print(res)
fwrite(res, here("New/Output/TripleDiff/Tables/r79b_wcb_trends.csv"))
cat("[OK] r79b_wcb_trends.csv\n")
