########################################################
###### 25 — Robustezza: dirty continuo (intensita' CO2)  ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 31_r711_shapiro_intensity.R (SOLO la parte di stima, righe
##              96-167; la costruzione del crosswalk e' in 07_co2_intensity.R).
##              Run: ~2-3 min.
##
## Cosa fa: sostituisce il dirty_p binario (02, 6 settori Mani-Wheeler) con
## la misura CONTINUA di intensita' CO2 per HS6 costruita in 07, standardizzata
## (z-score), e ristima la spec principale (16) con EP x intensita' al posto
## di EP x dirty_p. Copertura crosswalk ~90,5% degli HS6 del pannello; i non
## concordati (WITS non li mappa a nessun ISIC3) prendono la MEDIA campionaria
## (z=0, "intensita' media", assunzione neutra) invece di 0 (che sarebbe
## sotto persino la media dei prodotti verdi e distorcerebbe il gradiente).
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/co2_intensity_hs6.csv (da 03)
##         New/Data/Classifications/green_codes_hs1996.csv (da 01)
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/r711_shapiro_intensity.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)

CACHE_FST  <- here("New/Data/Collapsed/panel_pdt_collapsed.fst")
CO2_FILE   <- here("New/Data/Classifications/co2_intensity_hs6.csv")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")

## --- Funzione: tutto in un sottoprocesso ------------------------------------
run_estimate <- function(cache_fst, co2_file, green_file, depth_file) {
  library(fst)
  library(fixest)
  library(data.table)
  library(fwildclusterboot)
  threads_fst(1)
  setFixest_nthreads(2)

  cell <- as.data.table(read_fst(cache_fst))
  co2 <- fread(co2_file)
  cell[co2, on = c("hs6" = "hs6_int"), co2_total := i.co2_total]
  # copertura crosswalk ~90,5% degli HS6 del pannello; i non concordati
  # prendono la MEDIA campionaria (z=0, "intensita' media", assunzione
  # neutra) invece di 0 (che sarebbe sotto persino la media dei prodotti
  # verdi e distorcerebbe il gradiente)
  mu <- mean(cell$co2_total, na.rm = TRUE)
  sdv <- sd(cell$co2_total, na.rm = TRUE)
  cell[is.na(co2_total), co2_total := mu]
  cell[, co2_z := (co2_total - mu) / sdv]

  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dep <- fread(depth_file)[, .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]

  out <- list()
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    cell[, `:=`(ep_co2 = get(tr) * co2_z, ep_green = get(tr) * env_good,
                td_co2 = TotalDepth_nonEnv * co2_z, td_green = TotalDepth_nonEnv * env_good)]
    m <- feols(y ~ ep_green + ep_co2 + td_green + td_co2 | pd + dt + pt,
               data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
    cat(sprintf("[%s] asintotico: ep_co2 %+.5f (p=%.3f) | ep_green %+.5f (p=%.3f)\n",
                tr_name, coef(m)[["ep_co2"]], pvalue(m)[["ep_co2"]],
                coef(m)[["ep_green"]], pvalue(m)[["ep_green"]]))

    X <- as.matrix(fixest::demean(cell[, .(y, ep_green, ep_co2, td_green, td_co2)],
                                  f = cell[, .(pd, dt, pt)], weights = cell$n))
    df <- as.data.frame(X); df$n_w <- cell$n; df$country_code <- cell$country_code
    sw <- sqrt(cell$n)
    cf_check <- qr.solve(as.matrix(df[, 1:5])[, -1] * sw, df$y * sw)
    if (max(abs(cf_check - coef(m)[c("ep_green", "ep_co2", "td_green", "td_co2")])) > 1e-5)
      stop("FW non riproduce feols")
    m_lm <- lm(y ~ 0 + ep_green + ep_co2 + td_green + td_co2, data = df, weights = n_w)

    for (param in c("ep_green", "ep_co2")) {
      set.seed(42)
      bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
      cat(sprintf("  [%s] %s: p_wcb = %.4f\n", tr_name, param, bt$p_val))
      out[[paste(tr_name, param)]] <- data.table(
        treat = tr_name, term = param,
        coef = coef(m)[[param]], se_asy = se(m)[[param]], p_asy = pvalue(m)[[param]],
        p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2],
        nobs = m$nobs, B = 9999L)
    }
  }
  rbindlist(out)
}

## --- Esecuzione con retry -----------------------------------------------
res <- NULL
for (tent in 1:4) {
  cat(sprintf("Stima CO2 continuo (tentativo %d)...\n", tent))
  res <- tryCatch(callr::r(run_estimate, args = list(
    cache_fst = CACHE_FST, co2_file = CO2_FILE, green_file = GREEN_FILE, depth_file = DEPTH_FILE
  ), show = TRUE), error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) break
}
if (is.null(res)) stop("Fallito dopo 4 tentativi")
print(res)
fwrite(res, here("New/Output/TripleDiff/Tables/r711_shapiro_intensity.csv"))
cat("[OK] r711_shapiro_intensity.csv\n")
