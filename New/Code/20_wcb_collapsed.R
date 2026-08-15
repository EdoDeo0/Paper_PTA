########################################################
###### 16 — Wild Cluster Bootstrap (panel collassato)   ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 15_wcb_collapsed.R. Run: ~2-3 min (B=9999 su 3,7M celle).
##
## Cosa fa: le stime di 12 usano p-value asintotici con cluster ~country_code,
## ma i paesi TRATTATI sono ~25 - serve il wild cluster bootstrap (Cameron,
## Gelbach & Miller 2008) per un'inferenza robusta ai pochi cluster. Decide
## in particolare la PISTA DIRTY (EP(WB)xdirty asintoticamente significativo).
##
## COME (nota tecnica importante): boottest() vuole un modello NON-lean, ma
## feols non-lean crasha l'allocatore R su questa macchina anche a 3,7M celle
## ("recursive gc invocation"). Soluzione: FRISCH-WAUGH - si demeanano
## outcome e regressori rispetto alle 3 FE con fixest::demean() (pesato),
## poi si stima lm() sui dati demeanati: i coefficienti sono identici a
## quelli di 12 per costruzione (verifica stampata), e boottest lavora
## sull'lm leggero invece che su un feols non-lean.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/wcb_collapsed.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fixest)
library(fst)
library(fwildclusterboot)
source(here("New/Code/_sample_config.R"))
threads_fst(1)
setFixest_nthreads(4)

## --- Parametri e percorsi --------------------------------------------------
CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
OUT_FILE   <- out_path(here("New/Output/TripleDiff/Tables/wcb_collapsed.csv"))

## --- Caricamento dati (stesso panel e stessi merge di 12) ------------------
cell <- as.data.table(read_fst(CACHE_FST))
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
dep <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]
cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
if (DEPTH_DROP_UNMEASURED) {
  n0 <- nrow(cell)
  cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
  cat(sprintf("[depth] %s: %d celle trattate senza copertura escluse (%.3f%%)\n",
              DEPTH_VAR, n0 - nrow(cell), 100 * (n0 - nrow(cell)) / n0))
}
cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]
cat("Panel:", format(nrow(cell), big.mark = ","), "celle |",
    uniqueN(cell$country_code), "cluster (paesi)\n")

## --- WCB per ciascun indice (WB, TREND) ------------------------------------
## Riproducibilita': da fwildclusterboot 0.13 boottest() campiona con dqrng e
## NON accetta piu' un argomento `seed`. set.seed() da solo non basta (testato:
## p_wcb oscilla ~1pp); serve dqrng::dqset.seed(). Il pacchetto raccomanda di
## fissare ENTRAMBI i generatori una volta, prima delle chiamate.
set.seed(42)
dqrng::dqset.seed(42)
res <- list()
for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]

  # interazioni e demeaning Frisch-Waugh (pesato) rispetto a pd+dt+pt
  cell[, `:=`(ep_green = get(tr) * env_good,            ep_dirty = get(tr) * dirty_p,
              td_green = get(DEPTH_VAR) * env_good, td_dirty = get(DEPTH_VAR) * dirty_p)]
  X <- fixest::demean(cell[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                      f = cell[, .(pd, dt, pt)], weights = cell$n)
  df <- as.data.frame(X)
  df$n_w <- cell$n
  df$country_code <- cell$country_code
  rm(X)
  gc()

  # lm sui dati demeanati (coefficienti = feols di 12, verifica stampata)
  m_lm <- lm(y ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)
  cat(sprintf("\n[%s] coef lm demeanato: ep_green %+.6f | ep_dirty %+.6f (attesi = 12)\n",
              tr_name, coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))

  # boottest sulle due interazioni EP
  for (param in c("ep_green", "ep_dirty")) {
    cat("  boottest:", param, "... ")
    bt <- tryCatch(boottest(m_lm, param = param, clustid = "country_code", B = 9999),
                   error = function(e) { cat("ERRORE:", conditionMessage(e), "\n"); NULL })
    if (!is.null(bt)) {
      cat(sprintf("p_wcb = %.4f\n", bt$p_val))
      ## nobs/nclust/fe esportati (prima vivevano solo nel log): la nota della
      ## tabella principale cita "236 clusters" e senza queste colonne il CSV
      ## non basta a ricostruirla mesi dopo (ROADMAP §10 punti 1 e 3).
      res[[paste(tr_name, param)]] <- data.table(
        treat = tr_name, term = param, coef = coef(m_lm)[[param]],
        p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L,
        nobs = nrow(df), nclust = uniqueN(df$country_code), fe = "pd+dt+pt")
    }
  }
  rm(m_lm, df)
  gc()
}

out <- rbindlist(res)
fwrite(out, OUT_FILE)
cat("\n[OK] wcb_collapsed.csv\n")
print(out)
