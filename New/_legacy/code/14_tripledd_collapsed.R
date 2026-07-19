########################################################################
###### Fase R3 — Triple-diff su panel COLLASSATO (prodotto x dest x anno)
########################################################################

## Author: Edoardo Vitella
##
## PERCHÉ ESISTE: la triple-diff full-panel (07, FE fpd+fdt+pt su 45,8M righe)
## crasha l'allocatore R su questa macchina ("recursive gc invocation",
## 2026-07-06) — tre FE ad alta dimensionalità insieme non ci stanno.
## Questo script implementa la strada del REPORT_Ripartire_Da_Zero.md §3.5:
## la domanda di COMPOSIZIONE non richiede il livello impresa. Si collassa a
## cella hs6 x destinazione x anno (~2,9M celle) e si stima l'analogo diretto:
##
##   y_pdt ~ EP:green_p + EP:dirty_p + TotalDepth:green_p + TotalDepth:dirty_p
##         | pd + dt + pt,   weights = n. imprese-osservazioni, cluster ~dest
##
##   - dt (dest x anno)  assorbe il PTA stesso e tutto ciò che varia a livello
##     destinazione-anno  (analogo del fdt del full panel)
##   - pd (hs6 x dest)   assorbe il livello della relazione prodotto-mercato
##   - pt (hs6 x anno)   assorbe gli shock globali di prodotto
##
## NOTA outcome: y = MEDIA di ln_export nella cella (flusso log medio per
## impresa), NON ln(somma) — evita il bias di Jensen nel confronto col full
## panel ed è coerente con la sezione C di 07. Il margine perso rispetto al
## full panel: la variazione within-firm (riservata al modulo R4).
##
## Output: New/Output/TripleDiff/Tables/tripledd_collapsed.csv
##         New/Output/TripleDiff/Diagnostics/eventstudy_collapsed.png
##         New/Output/TripleDiff/Diagnostics/permutation_collapsed.csv

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here); library(data.table); library(fixest); library(ggplot2)

DATA_FST   <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
GREEN_FILE <- here("New/Data/Concordance/Env_Codes_HS1996.csv")
DIRTY_FILE <- here("New/Data/Dirty/dirty_goods_hs6.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
CACHE_FST  <- here("New/Data/Collapsed/panel_pdt_collapsed.fst")
OUT_DIR    <- here("New/Output/TripleDiff")
for (p in c(dirname(CACHE_FST), file.path(OUT_DIR, "Tables"), file.path(OUT_DIR, "Diagnostics")))
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)

# ── 1. Collasso (pesante: in sottoprocesso callr; cache su disco) ─────
build_collapsed <- function(data_file, cache_file) {
  library(fst); library(data.table)
  threads_fst(1)
  d <- as.data.table(read_fst(data_file, columns = c(
    "ln_export", "hs6", "country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
  d <- d[!country_code %in% c(110L, 121L)]          # HK+MO fuori (come 07)
  cell <- d[!is.na(ln_export),
            .(y = mean(ln_export), n = .N,
              WB_EP_Depth = first(WB_EP_Depth), TREND_EP_Count = first(TREND_EP_Count)),
            by = .(hs6, country_code, year)]
  write_fst(cell, cache_file)
  nrow(cell)
}

if (!file.exists(CACHE_FST)) {
  cat("Collasso del panel (una tantum, poi cache)...\n")
  n <- callr::r(build_collapsed, args = list(data_file = DATA_FST, cache_file = CACHE_FST), show = TRUE)
  cat("Celle:", format(n, big.mark = ","), "\n")
}
library(fst); threads_fst(1)
cell <- as.data.table(read_fst(CACHE_FST))
cat("Panel collassato:", format(nrow(cell), big.mark = ","), "celle\n")

## green / dirty / TotalDepth (stesse fonti di 07)
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
dep <- fread(DEPTH_FILE)[, .(country_code, year, TotalDepth_nonEnv)]
cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]

## ID delle FE (interi compatti)
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]
cat(sprintf("green: %.1f%% celle | dirty: %.1f%% celle\n",
            100 * mean(cell$env_good), 100 * mean(cell$dirty_p)))

# ── 2. Stime principali (WB e TREND) ──────────────────────────────────
res <- list()
for (tr in c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")) {
  f <- sprintf("y ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt", tr, tr)
  cat("Stima:", f, "\n")
  m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  key <- names(which(c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count") == tr))
  res[[key]] <- data.table(treat = key, term = names(coef(m)), coef = coef(m),
                           se = se(m), pval = pvalue(m), nobs = m$nobs)
  print(res[[key]])
}
fwrite(rbindlist(res), file.path(OUT_DIR, "Tables", "tripledd_collapsed.csv"))
cat("[OK] tripledd_collapsed.csv\n")

# ── 3. Event study differenziale (green e dirty vs neutri) ────────────
entry <- cell[WB_EP_Depth > 0, .(entry_year = min(year)), by = country_code]
cell[entry, on = "country_code", entry_year := i.entry_year]
cell[, rel_time := year - entry_year]
cell[, rel_time := pmax(pmin(rel_time, 5L), -6L)]
cell[is.na(entry_year), rel_time := -1L]            # never-treated nel riferimento

m_es <- feols(y ~ i(rel_time, env_good, ref = -1) + i(rel_time, dirty_p, ref = -1) | pd + dt + pt,
              data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
cf <- data.table(term = names(coef(m_es)), b = coef(m_es), se = se(m_es))
cf <- cf[grepl("rel_time", term)]
cf[, t := as.integer(gsub(".*rel_time::(-?\\d+).*", "\\1", term))]
cf[, quale := fifelse(grepl("env_good", term), "green", "dirty")]
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
cat("[OK] eventstudy_collapsed.png — controllare pre-trend differenziali\n")

# ── 4. Permutation inference (come 07-C: profili EP rimescolati) ──────
## Collasso ulteriore a dest x anno x green (celle ~7k): 1000 permutazioni in minuti.
cg <- cell[, .(y = weighted.mean(y, n), n = sum(n), EP = first(WB_EP_Depth)),
           by = .(country_code, year, env_good)]
cg[, dt_id := .GRP, by = .(country_code, year)]
cg[, dg_id := .GRP, by = .(country_code, env_good)]
cg[, tg_id := .GRP, by = .(year, env_good)]
est <- function(dat) coef(feols(y ~ EP:env_good | dt_id + dg_id + tg_id,
                                data = dat, weights = ~n, lean = TRUE))[["EP:env_good"]]
b_obs <- est(cg)
treated <- unique(cg[EP > 0, country_code])
prof <- unique(cg[country_code %in% treated, .(country_code, year, EP)])
set.seed(42)
b_perm <- replicate(1000L, {
  remap <- setNames(sample(treated), treated)
  pp <- copy(prof)[, country_code := remap[as.character(country_code)]]
  cc <- copy(cg)[, EP := NULL][pp, on = c("country_code", "year"), EP := i.EP][is.na(EP), EP := 0]
  tryCatch(est(cc), error = function(e) NA_real_)
})
pval <- mean(abs(b_perm) >= abs(b_obs), na.rm = TRUE)
cat(sprintf("Permutation: coeff osservato %.6f | p-value %.4f (n=1000)\n", b_obs, pval))
fwrite(data.table(b_obs = b_obs, p_perm = pval, n_perm = 1000L),
       file.path(OUT_DIR, "Diagnostics", "permutation_collapsed.csv"))

cat("\n=== DONE 14 (triple-diff collassata) ===\n")
