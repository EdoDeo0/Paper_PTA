# ============================================================================
# Figures for paper_v3 — English labels, Stata-sourced estimates and SEs.
#
# Fig 1  eventstudy_collapsed_v3.png  <- Tables_Stata/eventstudy_twfe_stata.csv
# Fig 2  eventstudy_sunab_v3.png      <- Tables_Stata/sunab_stata.csv
#        (95% bands from eventstudyinteract SEs — the ones the paper quotes;
#         the v2 figure mistakenly used the fixest::sunab SEs)
#
# Run from repo root:
#   & 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' New\Paper\paper_v3\figures\make_figures_v3.R
# ============================================================================

library(ggplot2)

DIR_TS  <- "New/Output/TripleDiff/Tables_Stata"
DIR_OUT <- "New/Paper/paper_v3/figures"

col_green <- "#1a7d32"
col_dirty <- "#b22222"

# ---- Fig 1: TWFE event study (collapsed panel) ------------------------------

es <- read.csv(file.path(DIR_TS, "eventstudy_twfe_stata.csv"))
es$t <- as.numeric(es$t)
es$quale <- factor(es$quale, levels = c("green", "dirty"),
                   labels = c("Green products (vs. neutral)",
                              "Dirty products (vs. neutral)"))
es$lo95 <- es$coef - 1.96 * es$se
es$hi95 <- es$coef + 1.96 * es$se
es$lo90 <- es$coef - qnorm(0.95) * es$se
es$hi90 <- es$coef + qnorm(0.95) * es$se
es$xlab <- factor(es$t, levels = sort(unique(es$t)),
                  labels = ifelse(sort(unique(es$t)) == -6, "\u2264\u22126",
                           ifelse(sort(unique(es$t)) ==  5, "\u2265+5",
                                  sort(unique(es$t)))))
es$xnum <- as.numeric(es$xlab)
es$ref  <- es$source == "reference"

p1 <- ggplot(es, aes(x = xnum, y = coef)) +
  facet_wrap(~quale) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95, fill = quale), alpha = 0.15) +
  geom_ribbon(aes(ymin = lo90, ymax = hi90, fill = quale), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_vline(xintercept = unique(es$xnum[es$t == -1]), linetype = "dotted",
             colour = "grey45") +
  geom_line(aes(colour = quale)) +
  geom_point(aes(colour = quale), size = 2.2) +
  geom_point(data = es[es$ref, ], shape = 21, fill = "white", size = 2.6) +
  scale_x_continuous(breaks = sort(unique(es$xnum)), labels = levels(es$xlab)) +
  scale_colour_manual(values = c(col_green, col_dirty), guide = "none") +
  scale_fill_manual(values = c(col_green, col_dirty), guide = "none") +
  labs(x = "Years since PTA entry into force (t = \u22121 is the reference period)",
       y = "Differential effect on ln(mean export value)",
       title = "Event study: export composition around PTA entry into force",
       subtitle = "Collapsed HS6\u00d7destination\u00d7year panel; FE pd+dt+pt; destination-clustered SEs. Shading: 90% (dark) and 95% (light) CIs.",
       caption = "Endpoint bins accumulate (\u2264\u22126, \u2265+5). Never-treated destinations in the control group.") +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_OUT, "eventstudy_collapsed_v3.png"), p1,
       width = 12.5, height = 5.4, dpi = 200, bg = "white")

# ---- Fig 2: Sun-Abraham IW event study (destination-level gap) --------------

sa <- read.csv(file.path(DIR_TS, "sunab_stata.csv"))
sa <- sa[sa$term != "ATT_aggregato", ]
sa$rel <- ifelse(grepl("^g_m", sa$term),
                 -as.numeric(sub("g_m", "", sa$term)),
                  as.numeric(sub("g_p", "", sa$term)))
sa$margin <- factor(sa$spec, levels = c("gap_green", "gap_dirty"),
                    labels = c("green", "dirty"))
sa <- sa[sa$rel >= -10 & sa$rel <= 8, ]           # window discussed in the text
sa$lo <- sa$coef - 1.96 * sa$se
sa$hi <- sa$coef + 1.96 * sa$se

p2 <- ggplot(sa, aes(x = rel, y = coef, colour = margin)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, colour = "grey55") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0,
                position = position_dodge(width = 0.55), linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.55), size = 2.4) +
  annotate("point", x = -1, y = 0, shape = 21, fill = "white",
           colour = "grey30", size = 2.6) +
  scale_x_continuous(breaks = seq(-10, 8, 2)) +
  scale_colour_manual(values = c(green = col_green, dirty = col_dirty),
                      breaks = c("green", "dirty")) +
  labs(x = "Years since PTA entry into force (t = \u22121 is the reference period)",
       y = "Composition gap vs. neutral products",
       colour = NULL,
       title = "Sun\u2013Abraham interaction-weighted event study",
       subtitle = "Destination\u00d7year composition gap; 95% CIs from eventstudyinteract SEs, which include cohort-share estimation uncertainty.") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold"))

ggsave(file.path(DIR_OUT, "eventstudy_sunab_v3.png"), p2,
       width = 12.5, height = 6.2, dpi = 200, bg = "white")

cat("Figures written to", DIR_OUT, "\n")
