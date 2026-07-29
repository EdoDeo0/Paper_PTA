########################################################
###### 19 — Event study Sun-Abraham + diagnosi t=-6 e   ###
###### grafico TWFE migliorato                          ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 19_sunab_gap.R + 23_r71_sunab_diag.R + 14c_eventstudy_plot.R.
##              Run: ~3-5 min complessivi.
##
## Cosa fa: TRE sezioni sull'event study, in ordine di produzione:
##
## A) SUN-ABRAHAM SUL GAP DI COMPOSIZIONE. L'event study TWFE di 12 e'
##    esposto all'eterogeneita' di coorte con timing scaglionato
##    (Goodman-Bacon 2021): il bin >=+5 e' identificato solo dalle coorti
##    precoci. Il disegno triple-diff non si presta direttamente a sunab();
##    il trucco: costruire per ogni destinazione-anno il GAP di composizione
##      gap_green_dt = media pesata di y sui prodotti green - media sui neutri
##    (idem dirty). Il gap E' il differenziale della triple-diff; su questo
##    panel destinazione-anno il trattamento "entrata PTA con EP" e' un
##    normale DiD scaglionato -> sunab() si applica direttamente, con i
##    never-treated come controllo.
##
## B) DIAGNOSI DEL GAP A t=-6 (dirty). Il run di (A) mostra gap_dirty a
##    t=-6 positivo e marginalmente significativo, in tensione con "pre-
##    trend piatti". Scompone il coefficiente: quali coorti identificano
##    ogni lead; coefficienti coorte-specifici (sunab no_agg) a t=-6;
##    sensibilita' (finestra troncata come il TWFE; senza coorti tardive
##    2014-15; leave-one-cohort-out).
##
## C) GRAFICO TWFE MIGLIORATO. Riprende le stime GIA' fatte da 12
##    (eventstudy_collapsed.csv) e rifa' solo il grafico (faccette separate
##    green/dirty, riferimento t=-1 esplicito, bande 90%/95%, bin
##    accumulati "<=-6"/">=+5"). Nessuna ri-stima.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Output/TripleDiff/Diagnostics/eventstudy_collapsed.csv (da 12, solo sezione C)
## Output: New/Output/TripleDiff/Tables/sunab_gap.csv
##         New/Output/TripleDiff/Diagnostics/eventstudy_sunab.png (citato nel paper)
##         New/Output/TripleDiff/Diagnostics/r71_sunab_diag.csv, r71_sunab_diag.md
##         New/Output/TripleDiff/Diagnostics/eventstudy_collapsed_v2.png (citato nel paper)

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fixest)
library(fst)
library(ggplot2)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
TAB_DIR    <- here("New/Output/TripleDiff/Tables")
DIAG_DIR   <- here("New/Output/TripleDiff/Diagnostics")
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DIAG_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Caricamento dati (comune a sezione A e B) -----------------------------
cell <- as.data.table(read_fst(CACHE_FST))
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]

# gap di composizione per destinazione-anno (neutri = ne' green ne' dirty)
gap <- cell[, .(
  y_green   = weighted.mean(y[env_good == 1], n[env_good == 1]),
  y_dirty   = weighted.mean(y[dirty_p == 1],  n[dirty_p == 1]),
  y_neutral = weighted.mean(y[env_good == 0 & dirty_p == 0], n[env_good == 0 & dirty_p == 0]),
  n_tot     = sum(n),
  EP        = first(WB_EP_Depth)
), by = .(country_code, year)]
gap[, gap_green := y_green - y_neutral]
gap[, gap_dirty := y_dirty - y_neutral]

# coorte = primo anno con EP > 0; never-treated = 10000 (convenzione sunab)
entry <- gap[EP > 0, .(entry_year = min(year)), by = country_code]
gap[entry, on = "country_code", entry_year := i.entry_year]
gap[is.na(entry_year), entry_year := 10000L]
cat("Destinazioni:", uniqueN(gap$country_code), "| trattate:", nrow(entry), "\n")

## ============================================================================
## SEZIONE A — Sun-Abraham sul gap di composizione (green e dirty)
## ============================================================================
setFixest_nthreads(2)
rows <- list()
for (g in c("gap_green", "gap_dirty")) {
  m_sa <- feols(as.formula(sprintf("%s ~ sunab(entry_year, year) | country_code + year", g)),
                data = gap, weights = ~n_tot, cluster = ~country_code)
  agg <- summary(m_sa, agg = "ATT")  # ATT medio post
  cat(sprintf("%s - ATT Sun-Abraham: %+.4f (p=%.3f)\n", g,
              coef(agg)[["ATT"]], pvalue(agg)[["ATT"]]))
  cf <- data.table(outcome = g, term = names(coef(m_sa)), coef = coef(m_sa),
                   se = se(m_sa), pval = pvalue(m_sa))
  rows[[g]] <- rbind(cf, data.table(outcome = g, term = "ATT_aggregato",
                                    coef = coef(agg)[["ATT"]], se = se(agg)[["ATT"]],
                                    pval = pvalue(agg)[["ATT"]]))
}
sunab_out <- rbindlist(rows)
fwrite(sunab_out, out_path(file.path(TAB_DIR, "sunab_gap.csv")))

# grafico: coefficienti sunab per anno relativo
cf_plot <- sunab_out[grepl("year::", term)]
cf_plot[, t := as.integer(gsub(".*year::(-?\\d+).*", "\\1", term))]
cf_plot[, quale := fifelse(outcome == "gap_green", "green", "dirty")]
p_sunab <- ggplot(cf_plot[t >= -6 & t <= 5], aes(t, coef, colour = quale)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = -0.5, colour = "grey60") +
  geom_pointrange(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                  position = position_dodge(width = 0.4)) +
  scale_colour_manual(values = c(green = "forestgreen", dirty = "firebrick")) +
  labs(x = "Anni dall'entrata in vigore", y = "Gap di composizione vs neutri",
       title = "Event study Sun-Abraham sul gap di composizione (dest-anno)",
       colour = NULL) + theme_minimal()
ggsave(out_path(file.path(DIAG_DIR, "eventstudy_sunab.png")), p_sunab, width = 9, height = 5)
cat("[OK] sunab_gap.csv + eventstudy_sunab.png\n")

## ============================================================================
## SEZIONE B — diagnosi del gap dirty a t=-6
## ============================================================================
setFixest_nthreads(1)
gap[, rel_time := fifelse(entry_year == 10000L, NA_integer_, year - entry_year)]

# chi identifica t=-6 (e i lead profondi)
coorti <- entry[, .N, by = entry_year][order(entry_year)]
espos  <- gap[!is.na(rel_time) & rel_time < 0,
              .(coorti = paste(sort(unique(entry_year)), collapse = " "),
                n_dest = uniqueN(country_code)), by = rel_time][order(rel_time)]

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

diag_rows <- list()
diag_rows$base <- stima(gap, "baseline")  # replica sezione A

# coefficienti coorte-specifici a t=-6 (no aggregazione)
m_raw <- feols(gap_dirty ~ sunab(entry_year, year, no_agg = TRUE) | country_code + year,
               data = gap, weights = ~n_tot, cluster = ~country_code)
cf_raw <- data.table(spec = "per_coorte", term = names(coef(m_raw)), coef = coef(m_raw),
                     se = se(m_raw), pval = pvalue(m_raw))
diag_rows$raw <- cf_raw[grepl("::-6:", term) | grepl(":-6$", term) | grepl("year::-6", term)]
if (nrow(diag_rows$raw) == 0) diag_rows$raw <- cf_raw  # fallback: tieni tutto se il pattern non matcha

# finestra troncata [-6, +5] come il TWFE di 12
gap_win <- gap[is.na(rel_time) | (rel_time >= -6 & rel_time <= 5)]
diag_rows$win <- stima(gap_win, "finestra_-6_+5")

# senza coorti 2014-2015 (Islanda, Svizzera, Australia)
tardive <- entry[entry_year >= 2014, country_code]
diag_rows$no_late <- stima(gap[!country_code %in% tardive], "senza_coorti_2014_15")

# leave-one-cohort-out sul coefficiente t=-6
loo <- list()
for (cy in coorti$entry_year) {
  dat <- gap[entry_year != cy]
  m <- tryCatch(feols(gap_dirty ~ sunab(entry_year, year) | country_code + year,
                      data = dat, weights = ~n_tot, cluster = ~country_code),
                error = function(e) NULL)
  if (is.null(m)) next
  cf <- coef(m); nm <- grep("year::-6$", names(cf), value = TRUE)
  if (length(nm) == 1) {
    loo[[as.character(cy)]] <- data.table(spec = sprintf("loo_coorte_%d", cy),
                                          term = "year::-6", coef = cf[[nm]],
                                          se = se(m)[[nm]], pval = pvalue(m)[[nm]])
  }
}
diag_rows$loo <- rbindlist(loo)

diag_out <- rbindlist(diag_rows, use.names = TRUE)
fwrite(diag_out, out_path(file.path(DIAG_DIR, "r71_sunab_diag.csv")))

sink(out_path(file.path(DIAG_DIR, "r71_sunab_diag.md")))
cat("# 19 (sezione B) - Diagnosi Sun-Abraham gap_dirty t=-6\n\n")
cat("## Coorti di entrata (23 trattate, HK+MO esclusi)\n\n")
print(coorti)
cat("\n## Coorti che identificano ogni lead\n\n")
print(espos)
cat("\n## Baseline t=-6:", sprintf("%+.4f (p=%.4f)\n",
    diag_out[spec == "baseline" & term == "year::-6", coef],
    diag_out[spec == "baseline" & term == "year::-6", pval]))
cat("\n## Coefficienti coorte-specifici contenenti t=-6\n\n")
print(diag_out[spec == "per_coorte"])
cat("\n## Finestra [-6,+5] t=-6:", {
  v <- diag_out[spec == "finestra_-6_+5" & term == "year::-6"]
  if (nrow(v)) sprintf("%+.4f (p=%.4f)\n", v$coef, v$pval) else "n/d\n"})
cat("\n## Senza coorti 2014-15 t=-6:", {
  v <- diag_out[spec == "senza_coorti_2014_15" & term == "year::-6"]
  if (nrow(v)) sprintf("%+.4f (p=%.4f)\n", v$coef, v$pval) else "n/d\n"})
cat("\n## Leave-one-cohort-out t=-6\n\n")
print(diag_out[grepl("^loo_", spec)])
cat("\n## ATT nelle varie specifiche\n\n")
print(diag_out[term == "ATT"])
sink()
cat("[OK] r71_sunab_diag.csv + r71_sunab_diag.md\n")

## ============================================================================
## SEZIONE C — grafico TWFE migliorato (nessuna ri-stima, solo presentazione)
## ============================================================================
cf_es <- fread(out_path(file.path(DIAG_DIR, "eventstudy_collapsed.csv")))

# punto di riferimento esplicito (t = -1, effetto 0 per definizione)
rif <- data.table(term = "rif", b = 0, se = 0, t = -1L,
                  quale = c("green", "dirty"), riferimento = TRUE)
cf_es[, riferimento := FALSE]
cf_es <- rbind(cf_es, rif)

cf_es[, quale := factor(quale, levels = c("green", "dirty"),
                        labels = c("Prodotti green (vs neutri)", "Prodotti dirty (vs neutri)"))]

p_twfe <- ggplot(cf_es, aes(t, b)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
  geom_vline(xintercept = -1, linetype = 3, colour = "grey55") +
  # bande: 95% chiara, 90% piu' scura (se una banda 90% esclude lo zero si vede)
  geom_ribbon(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se, fill = quale),
              alpha = 0.15, data = cf_es[riferimento == FALSE]) +
  geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se, fill = quale),
              alpha = 0.25, data = cf_es[riferimento == FALSE]) +
  geom_line(aes(colour = quale), linewidth = 0.4, data = cf_es[riferimento == FALSE]) +
  geom_point(aes(colour = quale), size = 2.2, data = cf_es[riferimento == FALSE]) +
  geom_point(shape = 21, size = 2.6, fill = "white", colour = "grey30",
             data = cf_es[riferimento == TRUE]) +
  annotate("text", x = -1, y = 0, label = "rif.", vjust = -1.1, size = 3, colour = "grey30") +
  facet_wrap(~quale, ncol = 2) +
  scale_x_continuous(breaks = -6:5,
                     labels = c("≤-6", as.character(-5:4), "≥+5")) +
  scale_colour_manual(values = c("forestgreen", "firebrick"), guide = "none") +
  scale_fill_manual(values = c("forestgreen", "firebrick"), guide = "none") +
  labs(x = "Anni dall'entrata in vigore del PTA (t = -1 riferimento)",
       y = "Effetto differenziale su ln(export medio)",
       title = "Event study — composizione dell'export attorno all'entrata del PTA",
       subtitle = "Panel collassato hs6×dest×anno; FE pd+dt+pt; cluster destinazione. Ombre: IC 90% (scuro) e 95% (chiaro).",
       caption = "Bin accumulati agli estremi (≤-6, ≥+5). Never-treated nel gruppo di riferimento.") +
  theme_minimal(base_size = 11) +
  theme(strip.text = element_text(face = "bold"))

ggsave(out_path(file.path(DIAG_DIR, "eventstudy_collapsed_v2.png")), p_twfe, width = 11, height = 4.8, dpi = 200)
cat("[OK] eventstudy_collapsed_v2.png\n")
