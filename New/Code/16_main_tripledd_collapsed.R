########################################################
###### 12 — Triple-diff principale (panel collassato)   ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: sezioni 2-3 di 14_tripledd_collapsed.R (la costruzione del
##              panel e' ora in 10_collapsed_panel.R; la sezione 4,
##              permutazione grezza, e' in 22_permutation_inference.R).
##              Run: ~1 min (il panel collassato e' gia' cachato).
##
## Cosa fa: la triple-diff full-panel (Stata, fpd+fdt+pt su 45,8M righe)
## crasha l'allocatore R su questa macchina - tre FE ad alta dimensionalita'
## insieme non ci stanno. La domanda di COMPOSIZIONE non richiede pero' il
## livello impresa: sul panel collassato a cella hs6 x destinazione x anno
## (10) si stima l'analogo diretto:
##
##   y_pdt ~ EP:green_p + EP:dirty_p + TotalDepth:green_p + TotalDepth:dirty_p
##         | pd + dt + pt,   weights = n. imprese-osservazioni, cluster ~dest
##
##   - dt (dest x anno)  assorbe il PTA stesso e tutto cio' che varia a
##     livello destinazione-anno (analogo del fdt del full panel)
##   - pd (hs6 x dest)   assorbe il livello della relazione prodotto-mercato
##   - pt (hs6 x anno)   assorbe gli shock globali di prodotto
##
## y = MEDIA di ln_export nella cella (flusso log medio per impresa), NON
## ln(somma): evita il bias di Jensen nel confronto col full panel. Il
## margine perso rispetto al full panel (variazione within-firm) e'
## riservato al modulo full-panel (Stata).
##
## Segue l'event study TWFE differenziale (green/dirty vs neutri) sullo
## stesso panel collassato, come diagnostica di pre-trend.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/tripledd_collapsed.csv
##         New/Output/TripleDiff/Diagnostics/eventstudy_collapsed.csv
##         New/Output/TripleDiff/Diagnostics/eventstudy_collapsed.png
##
## NOTA STABILITA': anche con lean=TRUE, feols su questa macchina crasha
## l'allocatore ("recursive gc invocation") in modo NON deterministico
## (~50% dei tentativi, causa nota, vedi memoria di progetto). Ogni stima
## gira quindi nel proprio sottoprocesso callr con retry - non e' una
## modifica dell'algoritmo, solo della modalita' di esecuzione.

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fst)
library(callr)
library(ggplot2)
threads_fst(1)

## --- Parametri e percorsi --------------------------------------------------
CACHE_FST  <- here("New/Data/Collapsed/panel_pdt_collapsed.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
OUT_DIR    <- here("New/Output/TripleDiff")
dir.create(file.path(OUT_DIR, "Tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "Diagnostics"), recursive = TRUE, showWarnings = FALSE)
stopifnot("panel_pdt_collapsed.fst non trovato - eseguire prima 10_collapsed_panel.R" = file.exists(CACHE_FST))

## --- Caricamento dati ----------------------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
cat("Panel collassato:", format(nrow(cell), big.mark = ","), "celle\n")

# green / dirty / TotalDepth (stesse fonti di tutta la pipeline)
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
dep <- fread(DEPTH_FILE)[, .(country_code, year, TotalDepth_nonEnv)]
cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]

# ID delle fixed effects (interi compatti)
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]
cat(sprintf("green: %.1f%% celle | dirty: %.1f%% celle\n",
            100 * mean(cell$env_good), 100 * mean(cell$dirty_p)))

## --- Sezione 1: stime principali (WB e TREND) ------------------------------
# ogni stima nel proprio sottoprocesso callr, con retry (vedi nota stabilita' sopra).
# VERIFICA INTERNA: un sottoprocesso crashato e ritentato puo', in rari casi,
# restituire un risultato silenziosamente CORROTTO invece di un errore pulito
# (osservato empiricamente in fase di test: un tentativo su piu' ha dato un
# coefficiente con segno e ordine di grandezza sbagliati, senza sollevare
# alcun errore). Per questo ogni stima feols viene ricontrollata con un
# Frisch-Waugh indipendente (demean + lm sui dati demeanati, stesso pattern
# gia' usato in 27/29/31): se i coefficienti non coincidono a 1e-6, la
# funzione si ferma con un errore esplicito invece di restituire un risultato
# non affidabile - il retry esterno riparte da capo.
run_main_model <- function(cell, tr, key) {
  library(fixest)
  library(data.table)
  f <- sprintf("y ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt", tr, tr)
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)

  cell[, `:=`(ep_green = get(tr) * env_good, ep_dirty = get(tr) * dirty_p,
              td_green = TotalDepth_nonEnv * env_good, td_dirty = TotalDepth_nonEnv * dirty_p)]
  X <- as.matrix(fixest::demean(cell[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                                f = cell[, .(pd, dt, pt)], weights = cell$n))
  sw <- sqrt(cell$n)
  cf_check <- qr.solve(X[, -1] * sw, X[, "y"] * sw)
  cf_m <- coef(m)[c(sprintf("%s:env_good", tr), sprintf("%s:dirty_p", tr),
                    "env_good:TotalDepth_nonEnv", "dirty_p:TotalDepth_nonEnv")]
  if (max(abs(cf_check - cf_m)) > 1e-6) stop("Frisch-Waugh non riproduce feols: risultato non affidabile")

  data.table(treat = key, term = names(coef(m)), coef = coef(m),
            se = se(m), pval = pvalue(m), nobs = m$nobs)
}

res <- list()
for (tr in c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")) {
  key <- names(which(c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count") == tr))
  cat("Stima:", key, "...\n")
  out <- NULL
  for (tent in 1:10) {
    out <- tryCatch(
      callr::r(run_main_model, args = list(cell = cell, tr = tr, key = key), show = TRUE),
      error = function(e) { cat("[CRASH tentativo", tent, "]", conditionMessage(e), "\n"); NULL }
    )
    if (!is.null(out)) break
  }
  if (is.null(out)) stop(sprintf("Stima %s fallita dopo 10 tentativi", key))
  res[[key]] <- out
  print(res[[key]])
}
fwrite(rbindlist(res), file.path(OUT_DIR, "Tables", "tripledd_collapsed.csv"))
cat("[OK] tripledd_collapsed.csv\n")

## --- Sezione 2: event study differenziale (green e dirty vs neutri) --------
entry <- cell[WB_EP_Depth > 0, .(entry_year = min(year)), by = country_code]
cell[entry, on = "country_code", entry_year := i.entry_year]
cell[, rel_time := year - entry_year]
cell[, rel_time := pmax(pmin(rel_time, 5L), -6L)]
cell[is.na(entry_year), rel_time := -1L]  # never-treated nel riferimento

run_eventstudy <- function(cell) {
  library(fixest)
  library(data.table)
  m_es <- feols(y ~ i(rel_time, env_good, ref = -1) + i(rel_time, dirty_p, ref = -1) | pd + dt + pt,
                data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  cf <- data.table(term = names(coef(m_es)), b = coef(m_es), se = se(m_es))
  cf <- cf[grepl("rel_time", term)]
  cf[, t := as.integer(gsub(".*rel_time::(-?\\d+).*", "\\1", term))]
  cf[, quale := fifelse(grepl("env_good", term), "green", "dirty")]
  cf
}

cf <- NULL
for (tent in 1:10) {
  cf <- tryCatch(callr::r(run_eventstudy, args = list(cell = cell), show = TRUE),
                 error = function(e) { cat("[CRASH tentativo", tent, "]", conditionMessage(e), "\n"); NULL })
  if (!is.null(cf)) break
}
if (is.null(cf)) stop("Event study fallito dopo 10 tentativi")
fwrite(cf, file.path(OUT_DIR, "Diagnostics", "eventstudy_collapsed.csv"))

p <- ggplot(cf, aes(t, b, colour = quale)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = -0.5, colour = "grey60") +
  geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se),
                  position = position_dodge(width = 0.4)) +
  scale_colour_manual(values = c(green = "forestgreen", dirty = "firebrick")) +
  labs(x = "Anni dall'entrata in vigore del PTA", y = "Effetto differenziale vs prodotti neutri",
       title = "Event study (panel collassato): green e dirty vs neutri", colour = NULL) +
  theme_minimal()
ggsave(file.path(OUT_DIR, "Diagnostics", "eventstudy_collapsed.png"), p, width = 9, height = 5)
cat("[OK] eventstudy_collapsed.png - controllare pre-trend differenziali\n")

cat("\n=== DONE 12 (triple-diff collassata) ===\n")
