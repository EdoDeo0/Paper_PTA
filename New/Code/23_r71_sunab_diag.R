########################################################################
###### R7.1 — Diagnosi del gap Sun-Abraham dirty a t=-6              ###
########################################################################

## Author: Edoardo Vitella
##
## Il run di 19_sunab_gap.R mostra gap_dirty a t=-6 = +0.047 (p=0.001),
## in tensione con "pre-trend piatti" (par. 4.2 del paper). Ma il log dello
## stesso run avverte: VCOV non semidefinita positiva e "fixed" -> con 23
## cluster e ~28 coefficienti di lead/lag i p-value sui singoli lead sono
## gia' inaffidabili. Qui si scompone il coefficiente:
##   1. quali coorti identificano ogni lead (t=-6 solo entrate >=2006);
##   2. coefficienti coorte-specifici (sunab no_agg) a t=-6;
##   3. sensibilita': finestra troncata [-6,+5] come il TWFE; senza coorti
##      tardive (2014-15, quasi solo pre-periodo); leave-one-cohort-out;
##   4. confronto col TWFE di 14 (t=-6 dirty +0.027, n.s.).
##
## Output: New/Output/TripleDiff/Diagnostics/r71_sunab_diag.csv
##         New/Output/TripleDiff/Diagnostics/r71_sunab_diag.md

library(here); library(data.table); library(fixest); library(fst)
threads_fst(1); setFixest_nthreads(1)

## ── gap di composizione, identico a 19 ────────────────────────────────
cell <- as.data.table(read_fst(here("New/Data/Collapsed/panel_pdt_collapsed.fst")))
green <- fread(here("New/Data/Concordance/Env_Codes_HS1996.csv"),
               colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(here("New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]

gap <- cell[, .(
  y_green   = weighted.mean(y[env_good == 1], n[env_good == 1]),
  y_dirty   = weighted.mean(y[dirty_p == 1],  n[dirty_p == 1]),
  y_neutral = weighted.mean(y[env_good == 0 & dirty_p == 0], n[env_good == 0 & dirty_p == 0]),
  n_tot     = sum(n),
  EP        = first(WB_EP_Depth)
), by = .(country_code, year)]
gap[, gap_dirty := y_dirty - y_neutral]

entry <- gap[EP > 0, .(entry_year = min(year)), by = country_code]
gap[entry, on = "country_code", entry_year := i.entry_year]
gap[is.na(entry_year), entry_year := 10000L]
gap[, rel_time := fifelse(entry_year == 10000L, NA_integer_, year - entry_year)]

## ── 1. chi identifica t=-6 (e i lead profondi) ────────────────────────
coorti <- entry[, .N, by = entry_year][order(entry_year)]
espos  <- gap[!is.na(rel_time) & rel_time < 0,
              .(coorti = paste(sort(unique(entry_year)), collapse = " "),
                n_dest = uniqueN(country_code)), by = rel_time][order(rel_time)]

rows <- list()
stima <- function(dat, label, sun_extra = "") {
  f <- as.formula(sprintf("gap_dirty ~ sunab(entry_year, year%s) | country_code + year", sun_extra))
  m <- feols(f, data = dat, weights = ~n_tot, cluster = ~country_code)
  cf <- data.table(spec = label, term = names(coef(m)), coef = coef(m),
                   se = se(m), pval = pvalue(m))
  agg <- summary(m, agg = "ATT")
  rbind(cf, data.table(spec = label, term = "ATT",
                       coef = coef(agg)[["ATT"]], se = se(agg)[["ATT"]],
                       pval = pvalue(agg)[["ATT"]]))
}

## baseline (replica 19)
rows$base <- stima(gap, "baseline")

## ── 2. coefficienti coorte-specifici a t=-6 (no aggregazione) ─────────
m_raw <- feols(gap_dirty ~ sunab(entry_year, year, no_agg = TRUE) | country_code + year,
               data = gap, weights = ~n_tot, cluster = ~country_code)
cf_raw <- data.table(spec = "per_coorte", term = names(coef(m_raw)), coef = coef(m_raw),
                     se = se(m_raw), pval = pvalue(m_raw))
rows$raw <- cf_raw[grepl("::-6:", term) | grepl(":-6$", term) | grepl("year::-6", term)]
if (nrow(rows$raw) == 0) rows$raw <- cf_raw   # fallback: tieni tutto se il pattern non matcha

## ── 3a. finestra troncata [-6, +5] come il TWFE di 14 ─────────────────
gap_win <- gap[is.na(rel_time) | (rel_time >= -6 & rel_time <= 5)]
rows$win <- stima(gap_win, "finestra_-6_+5")

## ── 3b. senza coorti 2014-2015 (Islanda, Svizzera, Australia) ─────────
tardive <- entry[entry_year >= 2014, country_code]
rows$no_late <- stima(gap[!country_code %in% tardive], "senza_coorti_2014_15")

## ── 3c. leave-one-cohort-out sul coefficiente t=-6 ────────────────────
loo <- list()
for (cy in coorti$entry_year) {
  dat <- gap[entry_year != cy]
  m <- tryCatch(feols(gap_dirty ~ sunab(entry_year, year) | country_code + year,
                      data = dat, weights = ~n_tot, cluster = ~country_code),
                error = function(e) NULL)
  if (is.null(m)) next
  cf <- coef(m); nm <- grep("year::-6$", names(cf), value = TRUE)
  if (length(nm) == 1)
    loo[[as.character(cy)]] <- data.table(spec = sprintf("loo_coorte_%d", cy),
                                          term = "year::-6", coef = cf[[nm]],
                                          se = se(m)[[nm]], pval = pvalue(m)[[nm]])
}
rows$loo <- rbindlist(loo)

out <- rbindlist(rows, use.names = TRUE)
fwrite(out, here("New/Output/TripleDiff/Diagnostics/r71_sunab_diag.csv"))

## ── report ─────────────────────────────────────────────────────────────
sink(here("New/Output/TripleDiff/Diagnostics/r71_sunab_diag.md"))
cat("# R7.1 — Diagnosi Sun-Abraham gap_dirty t=-6\n\n")
cat("## Coorti di entrata (23 trattate, HK+MO esclusi)\n\n")
print(coorti)
cat("\n## Coorti che identificano ogni lead\n\n")
print(espos)
cat("\n## Baseline t=-6:", sprintf("%+.4f (p=%.4f)\n",
    out[spec == "baseline" & term == "year::-6", coef],
    out[spec == "baseline" & term == "year::-6", pval]))
cat("\n## Coefficienti coorte-specifici contenenti t=-6\n\n")
print(out[spec == "per_coorte"])
cat("\n## Finestra [-6,+5] t=-6:", {
  v <- out[spec == "finestra_-6_+5" & term == "year::-6"]
  if (nrow(v)) sprintf("%+.4f (p=%.4f)\n", v$coef, v$pval) else "n/d\n"})
cat("\n## Senza coorti 2014-15 t=-6:", {
  v <- out[spec == "senza_coorti_2014_15" & term == "year::-6"]
  if (nrow(v)) sprintf("%+.4f (p=%.4f)\n", v$coef, v$pval) else "n/d\n"})
cat("\n## Leave-one-cohort-out t=-6\n\n")
print(out[grepl("^loo_", spec)])
cat("\n## ATT nelle varie specifiche\n\n")
print(out[term == "ATT"])
sink()
cat("[OK] r71_sunab_diag.csv + r71_sunab_diag.md\n")
