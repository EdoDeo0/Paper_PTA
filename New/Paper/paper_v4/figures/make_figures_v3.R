# ============================================================================
# Figures for paper_v3 — b/w-friendly, no in-plot titles, no grids,
# legends outside, uniform styling.
#
# Run from repo root:
#   Rscript New/Paper/paper_v3/figures/make_figures_v3.R
# ============================================================================

library(ggplot2)
library(patchwork)

DIR_TS  <- "New/Output/TripleDiff/Tables_Stata"
DIR_PAP <- "New/Paper/paper_v3"
DIR_OUT <- "New/Paper/paper_v3/figures"

# -- Common theme (no grid, no title, clean axes, Times-like font) -----------

theme_paper <- theme_bw(base_size = 13, base_family = "serif") +
  theme(
    panel.grid       = element_blank(),
    plot.title        = element_blank(),
    plot.subtitle     = element_blank(),
    plot.caption      = element_text(hjust = 0, size = 9, colour = "grey40"),
    strip.text        = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key        = element_rect(fill = "white", colour = NA)
  )

# b/w-friendly palette: black + grey60
col1 <- "black"
col2 <- "grey50"

# ---- Fig 1: EP timeline (two-panel: WB | TREND, separate axes) -------------

tl <- read.csv(file.path(DIR_PAP, "timeline_ep_data.csv"))
tl <- tl[!is.na(tl$n_treated) & tl$n_treated > 0, ]

p1A <- ggplot(tl, aes(x = year)) +
  geom_col(aes(y = n_treated), fill = "grey80", colour = "grey60", width = 0.7) +
  geom_line(aes(y = mean_WB * (20/8), linetype = "Mean WB EP Depth"),
            colour = "black", linewidth = 0.9) +
  geom_point(aes(y = mean_WB * (20/8), shape = "Mean WB EP Depth"),
             colour = "black", size = 2) +
  scale_y_continuous(
    name = "Number of treated destinations",
    sec.axis = sec_axis(~ . * (8/20), name = "Mean WB EP Depth (among treated)")
  ) +
  scale_x_continuous(breaks = tl$year) +
  scale_linetype_manual(values = c("Mean WB EP Depth" = "solid"), name = NULL) +
  scale_shape_manual(values = c("Mean WB EP Depth" = 16), name = NULL) +
  labs(x = NULL, subtitle = "(a) WB index") +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(face = "plain", size = 12),
        legend.position = "bottom")

p1B <- ggplot(tl, aes(x = year)) +
  geom_col(aes(y = n_treated), fill = "grey80", colour = "grey60", width = 0.7) +
  geom_line(aes(y = mean_TREND, linetype = "Mean TREND EP Count"),
            colour = "black", linewidth = 0.9) +
  geom_point(aes(y = mean_TREND, shape = "Mean TREND EP Count"),
             colour = "black", size = 2) +
  scale_y_continuous(
    name = NULL,
    sec.axis = sec_axis(~ ., name = "Mean TREND EP Count (among treated)")
  ) +
  scale_x_continuous(breaks = tl$year) +
  scale_linetype_manual(values = c("Mean TREND EP Count" = "dashed"), name = NULL) +
  scale_shape_manual(values = c("Mean TREND EP Count" = 17), name = NULL) +
  labs(x = NULL, subtitle = "(b) TREND index") +
  theme_paper +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(face = "plain", size = 12),
        legend.position = "bottom")

p1 <- p1A + p1B

ggsave(file.path(DIR_OUT, "fig_ep_timeline.pdf"), p1,
       width = 12, height = 5.5, device = cairo_pdf)

# ---- Fig 2 (map): geographic coverage --------------------------------------

if (requireNamespace("rnaturalearth", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)) {

  library(sf)
  library(rnaturalearth)

  world <- ne_countries(scale = "medium", returnclass = "sf")

  # Entry years from Table 1
  treated <- data.frame(
    iso_a3 = c("BGD","IND","LKA",                          # Bangkok 2002
               "BRN","KHM","IDN","LAO","MYS","MMR","PHL",  # ASEAN 2005
               "SGP","THA","TLS","VNM",                     # ASEAN 2005
               "CHL",                                        # 2006
               "PAK",                                        # 2007
               "NZL",                                        # 2008
               "PER",                                        # 2010
               "CRI",                                        # 2011
               "ISL","CHE",                                  # 2014
               "AUS","KOR"),                                 # 2015
    entry = c(rep(2002,3), rep(2005,11), 2006, 2007, 2008, 2010, 2011,
              rep(2014,2), rep(2015,2))
  )

  world <- merge(world, treated, by = "iso_a3", all.x = TRUE)
  world$entry_f <- factor(ifelse(is.na(world$entry), "Untreated", as.character(world$entry)))
  lvls <- c(sort(unique(as.character(treated$entry))), "Untreated")
  world$entry_f <- factor(world$entry_f, levels = lvls)

  n_treated_years <- length(unique(treated$entry))
  grey_vals <- c(grey.colors(n_treated_years, start = 0.1, end = 0.7), "grey92")

  p_map <- ggplot(world) +
    geom_sf(aes(fill = entry_f), colour = "white", linewidth = 0.15) +
    scale_fill_manual(values = grey_vals, name = "Entry year", na.value = "grey92") +
    coord_sf(ylim = c(-55, 80)) +
    theme_paper +
    theme(
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm")
    ) +
    guides(fill = guide_legend(nrow = 2))

  ggsave(file.path(DIR_OUT, "fig_map_treated.pdf"), p_map,
         width = 10, height = 5.5, device = cairo_pdf)
} else {
  message("Skipping map: install rnaturalearth + sf to regenerate fig_map_treated.pdf")
}

# ---- Fig 3: Composition shares by treatment status --------------------------

cs <- read.csv(file.path(DIR_PAP, "green_dirty_shares_by_year.csv"))
cs <- cs[!is.na(cs$year) & cs$year >= 2000, ]
cs$group <- ifelse(cs$treated == 1, "Treated", "Untreated")

# Reshape to long
cs_long <- rbind(
  data.frame(year = cs$year, group = cs$group, product = "Green",
             share = cs$green_share_val),
  data.frame(year = cs$year, group = cs$group, product = "Dirty",
             share = cs$dirty_share_val)
)
cs_long$product <- factor(cs_long$product, levels = c("Green", "Dirty"))

p3 <- ggplot(cs_long, aes(x = year, y = share, linetype = group, shape = product)) +
  geom_line(aes(group = interaction(group, product)), colour = "black", linewidth = 0.7) +
  geom_point(colour = "black", size = 2.2, fill = "white") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "Share of export value") +
  scale_x_continuous(breaks = seq(2000, 2015, 3)) +
  scale_linetype_manual(values = c("Treated" = "solid", "Untreated" = "dashed"),
                        name = "Destination") +
  scale_shape_manual(values = c("Green" = 16, "Dirty" = 17),
                     name = "Product type") +
  labs(x = NULL) +
  theme_paper +
  theme(legend.position = "bottom",
        legend.box = "horizontal") +
  guides(linetype = guide_legend(order = 1), shape = guide_legend(order = 2))

ggsave(file.path(DIR_OUT, "fig_composition_shares.pdf"), p3,
       width = 9, height = 5.5, device = cairo_pdf)

# ---- Fig 4: TWFE event study (collapsed panel) -----------------------------

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
                  labels = ifelse(sort(unique(es$t)) == -6, "≤−6",
                           ifelse(sort(unique(es$t)) ==  5, "≥+5",
                                  sort(unique(es$t)))))
es$xnum <- as.numeric(es$xlab)
es$ref  <- es$source == "reference"

# b/w: green panel in dark grey, dirty panel in medium grey
panel_fills <- c("grey30", "grey60")

p4 <- ggplot(es, aes(x = xnum, y = coef)) +
  facet_wrap(~quale) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95, fill = quale), alpha = 0.15) +
  geom_ribbon(aes(ymin = lo90, ymax = hi90, fill = quale), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30") +
  geom_vline(xintercept = unique(es$xnum[es$t == -1]), linetype = "dotted",
             colour = "grey45") +
  geom_line(aes(colour = quale), linewidth = 0.7) +
  geom_point(aes(colour = quale), size = 2.2) +
  geom_point(data = es[es$ref, ], shape = 21, fill = "white", size = 2.6,
             colour = "black") +
  scale_x_continuous(breaks = sort(unique(es$xnum)), labels = levels(es$xlab)) +
  scale_colour_manual(values = panel_fills, guide = "none") +
  scale_fill_manual(values = panel_fills, guide = "none") +
  labs(x = "Years since PTA entry into force (t = −1 is the reference period)",
       y = "Differential effect on ln(mean export value)",
       caption = "Endpoint bins accumulate (≤−6, ≥+5). Never-treated destinations in the control group.\nShading: 90% (dark) and 95% (light) CIs.") +
  theme_paper

ggsave(file.path(DIR_OUT, "eventstudy_collapsed_v3.png"), p4,
       width = 12.5, height = 5.4, dpi = 200, bg = "white")

# ---- Fig 5: Sun-Abraham IW event study (appendix) --------------------------

sa <- read.csv(file.path(DIR_TS, "sunab_stata.csv"))
sa <- sa[sa$term != "ATT_aggregato", ]
sa$rel <- ifelse(grepl("^g_m", sa$term),
                 -as.numeric(sub("g_m", "", sa$term)),
                  as.numeric(sub("g_p", "", sa$term)))
sa$margin <- factor(sa$spec, levels = c("gap_green", "gap_dirty"),
                    labels = c("Green", "Dirty"))
sa <- sa[sa$rel >= -10 & sa$rel <= 8, ]
sa$lo <- sa$coef - 1.96 * sa$se
sa$hi <- sa$coef + 1.96 * sa$se

p5 <- ggplot(sa, aes(x = rel, y = coef, colour = margin, shape = margin)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, colour = "grey55") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0,
                position = position_dodge(width = 0.55), linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.55), size = 2.4) +
  annotate("point", x = -1, y = 0, shape = 21, fill = "white",
           colour = "grey30", size = 2.6) +
  scale_x_continuous(breaks = seq(-10, 8, 2)) +
  scale_colour_manual(values = c(Green = col1, Dirty = col2)) +
  scale_shape_manual(values = c(Green = 16, Dirty = 17)) +
  labs(x = "Years since PTA entry into force (t = −1 is the reference period)",
       y = "Composition gap vs. neutral products",
       colour = NULL, shape = NULL,
       caption = "95% CIs from eventstudyinteract SEs (include cohort-share estimation uncertainty).") +
  theme_paper +
  theme(legend.position = c(0.92, 0.15))

ggsave(file.path(DIR_OUT, "eventstudy_sunab_v3.png"), p5,
       width = 12.5, height = 6.2, dpi = 200, bg = "white")

cat("All figures written to", DIR_OUT, "\n")
