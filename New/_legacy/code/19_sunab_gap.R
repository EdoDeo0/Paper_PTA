########################################################################
###### Fase R5 — Sun-Abraham sul differenziale green/dirty           ###
########################################################################

## Author: Edoardo Vitella
##
## L'event study TWFE di 14 e' esposto all'eterogeneita' di coorte con timing
## scaglionato (Goodman-Bacon 2021): il bin >=+5 e' identificato solo dalle
## coorti precoci. Risposta standard: Sun & Abraham (2021).
## Il disegno triple-diff non si presta direttamente a sunab(); il trucco
## pulito: costruire per ogni destinazione-anno il GAP di composizione
##   gap_green_dt = media pesata di y sui prodotti green - media sui neutri
## (idem dirty). Il gap E' il differenziale della triple-diff; su questo panel
## destinazione-anno (236 x 16) il trattamento "entrata PTA con EP" e' un
## normale DiD scaglionato -> sunab() si applica direttamente, con i
## never-treated come controllo.
##
## Output: New/Output/TripleDiff/Tables/sunab_gap.csv
##         New/Output/TripleDiff/Diagnostics/eventstudy_sunab.png

library(here); library(data.table); library(fixest); library(fst); library(ggplot2)
threads_fst(1); setFixest_nthreads(2)

cell <- as.data.table(read_fst(here("New/Data/Collapsed/panel_pdt_collapsed.fst")))
green <- fread(here("New/Data/Concordance/Env_Codes_HS1996.csv"),
               colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(here("New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]

## gap di composizione per destinazione-anno (neutri = ne' green ne' dirty)
gap <- cell[, .(
  y_green   = weighted.mean(y[env_good == 1], n[env_good == 1]),
  y_dirty   = weighted.mean(y[dirty_p == 1],  n[dirty_p == 1]),
  y_neutral = weighted.mean(y[env_good == 0 & dirty_p == 0], n[env_good == 0 & dirty_p == 0]),
  n_tot     = sum(n),
  EP        = first(WB_EP_Depth)
), by = .(country_code, year)]
gap[, gap_green := y_green - y_neutral]
gap[, gap_dirty := y_dirty - y_neutral]

## coorte = primo anno con EP > 0; never-treated = 10000 (convenzione sunab)
entry <- gap[EP > 0, .(entry_year = min(year)), by = country_code]
gap[entry, on = "country_code", entry_year := i.entry_year]
gap[is.na(entry_year), entry_year := 10000L]
cat("Destinazioni:", uniqueN(gap$country_code), "| trattate:", nrow(entry), "\n")

rows <- list()
for (g in c("gap_green", "gap_dirty")) {
  m_sa <- feols(as.formula(sprintf("%s ~ sunab(entry_year, year) | country_code + year", g)),
                data = gap, weights = ~n_tot, cluster = ~country_code)
  agg <- summary(m_sa, agg = "ATT")                       # ATT medio post
  cat(sprintf("%s — ATT Sun-Abraham: %+.4f (p=%.3f)\n", g,
              coef(agg)[["ATT"]], pvalue(agg)[["ATT"]]))
  cf <- data.table(outcome = g, term = names(coef(m_sa)), coef = coef(m_sa),
                   se = se(m_sa), pval = pvalue(m_sa))
  rows[[g]] <- rbind(cf, data.table(outcome = g, term = "ATT_aggregato",
                                    coef = coef(agg)[["ATT"]], se = se(agg)[["ATT"]],
                                    pval = pvalue(agg)[["ATT"]]))
}
out <- rbindlist(rows)
fwrite(out, here("New/Output/TripleDiff/Tables/sunab_gap.csv"))

## grafico: coefficienti sunab per anno relativo
cf <- out[grepl("year::", term)]
cf[, t := as.integer(gsub(".*year::(-?\\d+).*", "\\1", term))]
cf[, quale := fifelse(outcome == "gap_green", "green", "dirty")]
p <- ggplot(cf[t >= -6 & t <= 5], aes(t, coef, colour = quale)) +
  geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = -0.5, colour = "grey60") +
  geom_pointrange(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                  position = position_dodge(width = 0.4)) +
  scale_colour_manual(values = c(green = "forestgreen", dirty = "firebrick")) +
  labs(x = "Anni dall'entrata in vigore", y = "Gap di composizione vs neutri",
       title = "Event study Sun-Abraham sul gap di composizione (dest-anno)",
       colour = NULL) + theme_minimal()
ggsave(here("New/Output/TripleDiff/Diagnostics/eventstudy_sunab.png"), p, width = 9, height = 5)
cat("[OK] sunab_gap.csv + eventstudy_sunab.png\n")
