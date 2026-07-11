########################################################################
###### Fase R3 — Wild Cluster Bootstrap sul panel collassato         ###
########################################################################

## Author: Edoardo Vitella
##
## PERCHÉ: le stime di 14 usano p-value asintotici con cluster ~country_code,
## ma i paesi TRATTATI sono ~25 → serve il wild cluster bootstrap (CGM 2008).
## Chiude il terzo livello di inferenza (asintotico ✓, permutation ✓, WCB ← qui)
## e decide la PISTA DIRTY: EP(WB)×dirty = -0,0089 (p asintotico 0,006).
##
## COME (nota tecnica importante): boottest() vuole un modello NON-lean, ma
## feols non-lean crasha l'allocatore R su questa macchina anche a 3,7M celle
## (provato 2026-07-06: "recursive gc invocation"). Soluzione: FRISCH-WAUGH —
## si demeanano outcome e regressori rispetto alle 3 FE con fixest::demean()
## (pesato), poi si stima lm() sui dati demeanati: i COEFFICIENTI sono identici
## a 14 per costruzione (verifica stampata), e boottest lavora sull'lm leggero.
## I p WCB sono invarianti alla piccola differenza di dof (t-stat riscalato).
##
## Output: New/Output/TripleDiff/Tables/wcb_collapsed.csv

library(here); library(data.table); library(fixest); library(fst)
if (!requireNamespace("fwildclusterboot", quietly = TRUE))
  install.packages("fwildclusterboot", repos = "https://cloud.r-project.org")
library(fwildclusterboot)
threads_fst(1); setFixest_nthreads(4)

## ── Panel di 14 (dalla cache), stessi merge ───────────────────────────
cell <- as.data.table(read_fst(here("New/Data/Collapsed/panel_pdt_collapsed.fst")))
green <- fread(here("New/Data/Concordance/Env_Codes_HS1996.csv"),
               colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(here("New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
dep <- fread(here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[, .(country_code, year, TotalDepth_nonEnv)]
cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]
cat("Panel:", format(nrow(cell), big.mark = ","), "celle |",
    uniqueN(cell$country_code), "cluster (paesi)\n")

## ── WCB per ciascun indice ────────────────────────────────────────────
res <- list()
for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]

  ## 1. interazioni e demeaning Frisch-Waugh (pesato) rispetto a pd+dt+pt
  cell[, `:=`(ep_green = get(tr) * env_good,       ep_dirty = get(tr) * dirty_p,
              td_green = TotalDepth_nonEnv * env_good, td_dirty = TotalDepth_nonEnv * dirty_p)]
  X <- fixest::demean(cell[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                      f = cell[, .(pd, dt, pt)], weights = cell$n)
  df <- as.data.frame(X)
  df$n_w <- cell$n; df$country_code <- cell$country_code
  rm(X); gc()

  ## 2. lm sui dati demeanati (coefficienti = feols di 14, verifica stampata)
  m_lm <- lm(y ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)
  cat(sprintf("\n[%s] coef lm demeanato: ep_green %+.6f | ep_dirty %+.6f (attesi = 14)\n",
              tr_name, coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))

  ## 3. boottest sulle due interazioni EP
  for (param in c("ep_green", "ep_dirty")) {
    cat("  boottest:", param, "... ")
    set.seed(42)
    bt <- tryCatch(boottest(m_lm, param = param, clustid = "country_code", B = 9999),
                   error = function(e) { cat("ERRORE:", conditionMessage(e), "\n"); NULL })
    if (!is.null(bt)) {
      cat(sprintf("p_wcb = %.4f\n", bt$p_val))
      res[[paste(tr_name, param)]] <- data.table(
        treat = tr_name, term = param, coef = coef(m_lm)[[param]],
        p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L)
    }
  }
  rm(m_lm, df); gc()
}

out <- rbindlist(res)
fwrite(out, here("New/Output/TripleDiff/Tables/wcb_collapsed.csv"))
cat("\n[OK] wcb_collapsed.csv\n"); print(out)
