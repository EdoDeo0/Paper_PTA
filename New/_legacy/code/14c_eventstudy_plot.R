########################################################################
###### Fase R3 — Event study: versione grafica migliorata            ###
########################################################################

## Author: Edoardo Vitella
##
## Riprende le stime GIA' fatte da 14 (eventstudy_collapsed.csv) e rifà solo
## il grafico, applicando i suggerimenti della review 2026-07-06:
##   - faccette separate green / dirty (non sovrapposti)
##   - periodo di riferimento t=-1 esplicito (punto vuoto a zero)
##   - bande di confidenza 95% e 90% come ombre (non barre dominanti)
##   - linea verticale tratteggiata a t=-1 (ancorata al riferimento)
##   - bin accumulati etichettati "≤-6" e "≥+5"
## Nessuna ri-stima: solo presentazione.
##
## Output: New/Output/TripleDiff/Diagnostics/eventstudy_collapsed_v2.png

library(here); library(data.table); library(ggplot2)

cf <- fread(here("New/Output/TripleDiff/Diagnostics/eventstudy_collapsed.csv"))

## punto di riferimento esplicito (t = -1, effetto 0 per definizione)
rif <- data.table(term = "rif", b = 0, se = 0, t = -1L,
                  quale = c("green", "dirty"), riferimento = TRUE)
cf[, riferimento := FALSE]
cf <- rbind(cf, rif)

cf[, quale := factor(quale, levels = c("green", "dirty"),
                     labels = c("Prodotti green (vs neutri)", "Prodotti dirty (vs neutri)"))]

p <- ggplot(cf, aes(t, b)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey40") +
  geom_vline(xintercept = -1, linetype = 3, colour = "grey55") +
  ## bande: 95% chiara, 90% piu' scura (se una banda 90% esclude lo zero si vede)
  geom_ribbon(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se, fill = quale),
              alpha = 0.15, data = cf[riferimento == FALSE]) +
  geom_ribbon(aes(ymin = b - 1.645 * se, ymax = b + 1.645 * se, fill = quale),
              alpha = 0.25, data = cf[riferimento == FALSE]) +
  geom_line(aes(colour = quale), linewidth = 0.4, data = cf[riferimento == FALSE]) +
  geom_point(aes(colour = quale), size = 2.2, data = cf[riferimento == FALSE]) +
  geom_point(shape = 21, size = 2.6, fill = "white", colour = "grey30",
             data = cf[riferimento == TRUE]) +
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

ggsave(here("New/Output/TripleDiff/Diagnostics/eventstudy_collapsed_v2.png"),
       p, width = 11, height = 4.8, dpi = 200)
cat("[OK] eventstudy_collapsed_v2.png\n")
