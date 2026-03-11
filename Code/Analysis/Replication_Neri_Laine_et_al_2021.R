###########################################################################
###### Replication: Neri-Laine, Janda & Stephan (2021)             ######
###### "The Impact of Regional Trade Agreements on Georgia's        ######
######  Exporters: A Firm-Level Analysis"                           ######
###########################################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## ── PAPER SUMMARY ────────────────────────────────────────────────────────
## Neri-Laine et al. (2021) study the impact of the EU–Georgia Deep and
## Comprehensive Free Trade Area (DCFTA, in force 2016) on Georgian exporters
## using firm-level administrative data. Their identification strategy is a
## difference-in-differences design exploiting the sharp, scheduled timing of
## the DCFTA entry into force.
##
## Key findings:
##   (i)  DCFTA raised export participation (extensive margin) and value
##        (intensive margin) among Georgian firms already active pre-DCFTA.
##   (ii) Effects were concentrated among firms already exporting to the EU
##        and in sectors where the DCFTA provided the largest tariff cuts.
##  (iii) Parallel-trends tests using the pre-reform period confirm the
##        validity of the DiD design.
##
## ── REPLICATION STRATEGY ─────────────────────────────────────────────────
## Important caveat: the original paper uses Georgian firm-level microdata
## that are not publicly available. We replicate the methodology using our
## Chinese product-destination-year panel:
##
##   "Firm"           → product × firm × destination cell (fpd unit)
##   "PTA treatment"  → first year a PTA becomes active for destination d
##                       (G = min year with WB_EP_Depth > 0; else 0)
##   "Extensive margin" → product-destination NEW ENTRY:
##                        1 if (hs6, country_code) pair appears in year t
##                        but had no exports in year t-1 (entry event)
##   "Intensive margin" → conditional on positive flows: ln_export
##
## Estimation methods:
##   (1) Two-part OLS/LPM: logit-based LPM for entry + OLS for intensity
##   (2) Callaway-Sant'Anna (2021) staggered DiD with cohort-weighted ATT
##   (3) Event study around PTA entry (∓5 years)
##   (4) Heterogeneity: env_good products vs non-env_good
##
## Key outputs produced:
##   Table 1  – Summary statistics on treatment cohorts (G distribution)
##   Table 2  – Intensive margin (OLS): PTA depth → ln_export
##   Table 3  – Extensive margin (LPM): PTA → new product entry
##   Table 4  – Two-way FE DiD (binary PTA treatment)
##   Figure 1 – Callaway-Sant'Anna event study: WB index
##   Figure 2 – Callaway-Sant'Anna event study: TREND index
##   Figure 3 – Event study by product type (env vs non-env goods)
##
## This script uses the shared function library in pta_functions.R.


# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(dplyr)
library(ggplot2)
library(did)       # Callaway-Sant'Anna (2021) estimator
library(here)
library(lubridate)

source(here("Code/Analysis/pta_functions.R"))

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("Output/Analysis/Replication_NeriLaine_2021")
dirs      <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# DATA PREPARATION
# ─────────────────────────────────────────────────────────────────────
cat("Loading data for pre-processing...\n")

vars_needed <- c(
    "ln_export", "ln_export_qua", "ln_export_value",
    "export", "exp_qua", "uv_exp",
    "WB_EP_Depth", "WB_EP_Depth_Binary",
    "TREND_EP_Count", "TREND_EP_Count_Binary",
    "env_good", "tariffs", "ln_hhi_baci",
    "fpd", "year", "pdt", "hs6", "country_code"
)
dt <- as.data.table(read_fst(data_file, columns = vars_needed))

# ── Treatment cohort indicator G ───────────────────────────────────
## G = first year PTA is active for destination d (WB_EP_Depth > 0).
## Never-treated destinations get G = 0 (Callaway-Sant'Anna convention).
dt[, G_WB := {
    first_yr <- min(year[WB_EP_Depth > 0], na.rm = TRUE)
    ifelse(is.finite(first_yr), first_yr, 0L)
}, by = country_code]

dt[, G_TREND := {
    first_yr <- min(year[TREND_EP_Count > 0], na.rm = TRUE)
    ifelse(is.finite(first_yr), first_yr, 0L)
}, by = country_code]

# Binary PTA treatment dummies (1 after PTA entry, 0 before)
dt[, PTA_WB    := as.integer(WB_EP_Depth > 0)]
dt[, PTA_TREND := as.integer(TREND_EP_Count > 0)]

# ── Extensive margin: new product-destination entry ─────────────────
## For each (hs6, country_code) pair we define:
##   new_entry = 1 if year t is the FIRST year with positive exports
##               for that product-destination combination.
## This proxies the extensive margin without requiring explicit zeros.
dt[order(hs6, country_code, year),
   new_entry := as.integer(year == min(year)),
   by = .(hs6, country_code)]

# ── Numeric IDs for Callaway-Sant'Anna ──────────────────────────────
## CS requires unit ID and group ID to be numeric integers.
dt[, pd_id  := .GRP, by = .(hs6, country_code)]   # product-destination unit
dt[, pdt_id := .GRP, by = pdt]                       # product-dest-time cluster

# Save augmented dataset
augmented_file <- file.path(out_dir, "data_nerilaine_augmented.fst")
write_fst(dt, augmented_file, compress = 50)

# ── Summary: treatment cohort distribution ──────────────────────────
cat("\n=== Treatment Cohort Distribution (WB) ===\n")
print(dt[, .N, by = G_WB][order(G_WB)])

cat("\n=== Treatment Cohort Distribution (TREND) ===\n")
print(dt[, .N, by = G_TREND][order(G_TREND)])

rm(dt); gc()


# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────

cm_int_wb <- c(
    "WB_EP_Depth"    = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_int_trend <- c(
    "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_ext <- c(
    "PTA_WB"         = "\\textit{PTA Active\\textsubscript{dt} (WB)}",
    "PTA_TREND"      = "\\textit{PTA Active\\textsubscript{dt} (TREND)}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_twfe <- c(
    "PTA_WB"         = "\\textit{PTA}_{dt} \\textit{(WB)}",
    "PTA_TREND"      = "\\textit{PTA}_{dt} \\textit{(TREND)}",
    "PTA_WB:env_good"    = "\\textit{PTA}_{dt} \\textit{(WB)} $\\times$ \\textit{EnvGood}_{p}",
    "PTA_TREND:env_good" = "\\textit{PTA}_{dt} \\textit{(TREND)} $\\times$ \\textit{EnvGood}_{p}",
    "tariffs"            = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"        = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# BLOCK 1: Intensive Margin — OLS HDFE (Table 2)
# ─────────────────────────────────────────────────────────────────────
## Neri-Laine et al. show that conditional on exporting, PTAs increase
## the value of exports. We replicate this using our standard OLS-HDFE
## framework (equivalent to the paper's TWFE specification).
start <- now()

f_int_wb <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats_int_wb <- run_block(f_int_wb, "NL_B1_Intensive_WB", "ols",
                          augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_int_wb, cm_int_wb,
           "NL_Table2_Intensive_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))

f_int_trend <- c(
    "ln_export       ~ TREND_EP_Count | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats_int_trend <- run_block(f_int_trend, "NL_B1_Intensive_TREND", "ols",
                             augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_int_trend, cm_int_trend,
           "NL_Table2b_Intensive_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 2: Extensive Margin — Linear Probability Model (Table 3)
# ─────────────────────────────────────────────────────────────────────
## In Neri-Laine et al., extensive margin = probability of starting to
## export to a new destination. Our proxy: new_entry = 1 if this is the
## first year the product-destination pair records positive flows.
## We use a Linear Probability Model (LPM) via OLS-HDFE for tractability
## with high-dimensional FEs (logit is impractical with fpd FE).
## NOTE: new_entry ∈ {0,1} but feols handles this correctly as an LPM.

f_ext_wb <- c(
    "new_entry ~ PTA_WB   | hs6 + country_code + year",
    "new_entry ~ PTA_TREND | hs6 + country_code + year",
    "new_entry ~ PTA_WB  + tariffs + ln_hhi_baci | hs6 + country_code + year",
    "new_entry ~ PTA_TREND + tariffs + ln_hhi_baci | hs6 + country_code + year"
)
## Note: we use hs6 + country_code + year instead of fpd + year here
## because fpd is at the (firm × product × destination) level and a
## first-entry event is better identified at the product-destination level.
stats_ext <- run_block(f_ext_wb, "NL_B2_Extensive_LPM", "ols",
                       augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_ext, cm_ext,
           "NL_Table3_Extensive_LPM.tex", dirs$tables,
           digits = 5,
           dep_vars = c("WB (no ctrl)", "TREND (no ctrl)",
                        "WB (ctrl)", "TREND (ctrl)"),
           dep_subscript   = "pdt",
           group_headers   = c("New Entry Indicator"),
           group_cols      = c(4),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 3: TWFE DiD with Binary Treatment (Table 4)
# ─────────────────────────────────────────────────────────────────────
## Replicating the core DiD table of Neri-Laine et al.: two-way FE
## regression of outcomes on a binary PTA-active indicator. We also
## test heterogeneous effects for environmental goods in the same table.

f_twfe <- c(
    "ln_export ~ PTA_WB + tariffs + ln_hhi_baci | fpd + year",
    "ln_export ~ PTA_TREND + tariffs + ln_hhi_baci | fpd + year",
    "ln_export ~ PTA_WB * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export ~ PTA_TREND * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_twfe <- run_block(f_twfe, "NL_B3_TWFE_DiD", "ols",
                        augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_twfe, cm_twfe,
           "NL_Table4_TWFE_DiD.tex", dirs$tables,
           digits = 5,
           dep_vars = c("WB", "TREND", "WB × EnvGood", "TREND × EnvGood"),
           dep_subscript   = "fpdt",
           group_headers   = c("ln Exports"),
           group_cols      = c(4),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 4: Callaway-Sant'Anna Event Study — WB Index (Figure 1)
# ─────────────────────────────────────────────────────────────────────
## Neri-Laine et al. present event-study plots as their main visual evidence.
## We replicate this using the Callaway-Sant'Anna (2021) estimator which
## is robust to treatment effect heterogeneity across cohorts.
##
## IMPORTANT: CS requires a numeric unit ID. We use pd_id (product-destination)
## as the unit instead of fpd (firm-product-destination) to keep the problem
## computationally tractable and to align with the paper's destination logic.
## For a large dataset, consider subsetting by env_good or a random sample.

cat("\n=== Callaway-Sant'Anna (WB cohort) ===\n")

dt_cs <- as.data.table(read_fst(augmented_file, columns = c(
    "ln_export", "G_WB", "pd_id", "year", "env_good", "tariffs"
)))

# Aggregate to product-destination-year to reduce memory
dt_pd <- dt_cs[, .(
    ln_export = mean(ln_export, na.rm = TRUE),
    G_WB      = first(G_WB),
    env_good  = as.integer(mean(env_good, na.rm = TRUE) > 0.5)
), by = .(pd_id, year)]

att_wb <- att_gt(
    yname         = "ln_export",
    tname         = "year",
    idname        = "pd_id",
    gname         = "G_WB",
    data          = dt_pd,
    control_group = "nevertreated",
    est_method    = "reg",
    print_details = FALSE
)
saveRDS(att_wb, file.path(dirs$models, "CS_att_gt_WB.rds"))

es_wb <- aggte(att_wb, type = "dynamic", min_e = -4, max_e = 5)
cat("\n=== Event Study Summary (WB) ===\n")
print(summary(es_wb))

p_wb <- ggdid(es_wb) +
    labs(
        title    = "Event Study: Environmental PTA Depth and Chinese Exports",
        subtitle = "Callaway & Sant'Anna (2021) | Control: never-treated destinations | WB index",
        x        = "Years relative to PTA entry",
        y        = "ATT (ln Exports)"
    ) +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

ggsave(file.path(dirs$tables, "NL_Figure1_CS_EventStudy_WB.pdf"),
       plot = p_wb, width = 8, height = 5)
ggsave(file.path(dirs$tables, "NL_Figure1_CS_EventStudy_WB.png"),
       plot = p_wb, width = 8, height = 5, dpi = 300)

rm(dt_pd); gc()


# ─────────────────────────────────────────────────────────────────────
# BLOCK 5: Callaway-Sant'Anna Event Study — TREND Index (Figure 2)
# ─────────────────────────────────────────────────────────────────────

cat("\n=== Callaway-Sant'Anna (TREND cohort) ===\n")

dt_cs_trend <- as.data.table(read_fst(augmented_file, columns = c(
    "ln_export", "G_TREND", "pd_id", "year", "env_good"
)))

dt_pd_trend <- dt_cs_trend[, .(
    ln_export = mean(ln_export, na.rm = TRUE),
    G_TREND   = first(G_TREND)
), by = .(pd_id, year)]

att_trend <- att_gt(
    yname         = "ln_export",
    tname         = "year",
    idname        = "pd_id",
    gname         = "G_TREND",
    data          = dt_pd_trend,
    control_group = "nevertreated",
    est_method    = "reg",
    print_details = FALSE
)
saveRDS(att_trend, file.path(dirs$models, "CS_att_gt_TREND.rds"))

es_trend <- aggte(att_trend, type = "dynamic", min_e = -4, max_e = 5)

p_trend <- ggdid(es_trend) +
    labs(
        title    = "Event Study: Environmental PTA Depth and Chinese Exports",
        subtitle = "Callaway & Sant'Anna (2021) | Control: never-treated destinations | TREND index",
        x        = "Years relative to PTA entry",
        y        = "ATT (ln Exports)"
    ) +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

ggsave(file.path(dirs$tables, "NL_Figure2_CS_EventStudy_TREND.pdf"),
       plot = p_trend, width = 8, height = 5)
ggsave(file.path(dirs$tables, "NL_Figure2_CS_EventStudy_TREND.png"),
       plot = p_trend, width = 8, height = 5, dpi = 300)

rm(dt_cs_trend, dt_pd_trend); gc()


# ─────────────────────────────────────────────────────────────────────
# BLOCK 6: Event Study by Product Type (Figure 3)
# ─────────────────────────────────────────────────────────────────────
## Neri-Laine et al. split their sample by sector to show heterogeneous
## effects. We split by env_good to test whether PTA effects are larger
## for environmentally sensitive products.

for (eg_val in c(0L, 1L)) {
    label_str <- if (eg_val == 1L) "EnvGoods" else "NonEnvGoods"
    cat(sprintf("\n=== CS Event Study: %s ===\n", label_str))

    dt_sub <- dt_cs[env_good == eg_val, .(
        ln_export = mean(ln_export, na.rm = TRUE),
        G_WB      = first(G_WB)
    ), by = .(pd_id, year)]

    if (nrow(dt_sub) < 100) {
        cat("  Skipping: insufficient observations.\n")
        next
    }

    att_sub <- tryCatch(att_gt(
        yname         = "ln_export",
        tname         = "year",
        idname        = "pd_id",
        gname         = "G_WB",
        data          = dt_sub,
        control_group = "nevertreated",
        est_method    = "reg",
        print_details = FALSE
    ), error = function(e) {
        cat("  CS estimation failed:", conditionMessage(e), "\n")
        NULL
    })

    if (is.null(att_sub)) next

    saveRDS(att_sub, file.path(dirs$models, sprintf("CS_att_gt_WB_%s.rds", label_str)))
    es_sub <- aggte(att_sub, type = "dynamic", min_e = -4, max_e = 5)

    subtitle_str <- if (eg_val == 1L)
        "Environmental goods (env_good = 1)"
    else
        "Non-environmental goods (env_good = 0)"

    p_sub <- ggdid(es_sub) +
        labs(
            title    = "Event Study by Product Type",
            subtitle = paste0("Callaway & Sant'Anna (2021) | ", subtitle_str, " | WB index"),
            x        = "Years relative to PTA entry",
            y        = "ATT (ln Exports)"
        ) +
        theme_bw(base_size = 12) +
        theme(plot.title = element_text(face = "bold"))

    ggsave(file.path(dirs$tables,
                     sprintf("NL_Figure3_CS_EventStudy_WB_%s.pdf", label_str)),
           plot = p_sub, width = 8, height = 5)
}

rm(dt_cs); gc()


# ─────────────────────────────────────────────────────────────────────
# BLOCK 7: Parallel-Trends Validation (Table 5)
# ─────────────────────────────────────────────────────────────────────
## Neri-Laine et al. validate parallel trends with pre-treatment placebo
## tests. The Callaway-Sant'Anna procedure automatically provides pre-trend
## estimates (negative-k coefficients). We additionally run simple OLS
## "pre-trend" regressions using leads of the PTA treatment.
## Here we construct a "years-to-treatment" variable and test whether
## pre-treatment years have a significantly non-zero coefficient.

cat("\nLoading data for pre-trend test...\n")
dt_pt <- as.data.table(read_fst(augmented_file, columns = c(
    "ln_export", "G_WB", "year", "fpd", "pdt",
    "tariffs", "ln_hhi_baci"
)))

# Relative time to treatment (0 in entry year; negative = pre-treatment)
dt_pt[, rel_time := ifelse(G_WB > 0, year - G_WB, NA_integer_)]

# Create indicator dummies for periods -3 to +3 (omit -1 as reference)
for (k in c(-4:-2, 0:4)) {
    set(dt_pt, j = paste0("rel_", if (k < 0) paste0("m", abs(k)) else paste0("p", k)),
        value = as.integer(!is.na(dt_pt$rel_time) & dt_pt$rel_time == k))
}

# Save pre-trend dataset
pt_file <- file.path(out_dir, "data_pretrend.fst")
write_fst(dt_pt, pt_file, compress = 50)
rm(dt_pt); gc()

# Event-study OLS using relative-time dummies
f_pt <- paste(
    "ln_export ~",
    "rel_m4 + rel_m3 + rel_m2 +",           # pre-treatment: k = -4,-3,-2
    "rel_p0 + rel_p1 + rel_p2 + rel_p3 +",  # post-treatment: k = 0,+1,+2,+3
    "tariffs + ln_hhi_baci | fpd + year"
)

stats_pt <- run_block(
    formulas    = f_pt,
    block_name  = "NL_B7_PreTrend",
    estimator   = "ols",
    data_file   = pt_file,
    models_dir  = dirs$models,
    vcov        = ~pdt
)

# Extract and plot the event-study coefficients
pt_coefs <- stats_pt[[1]]$coefs
pt_se    <- stats_pt[[1]]$se
pt_label <- names(pt_coefs)

# Keep only rel_* terms
keep_mask <- grepl("^rel_", pt_label)
pt_coefs  <- pt_coefs[keep_mask]
pt_se     <- pt_se[keep_mask]
pt_k      <- ifelse(grepl("^rel_m", names(pt_coefs)),
                    -as.integer(sub("rel_m", "", names(pt_coefs))),
                     as.integer(sub("rel_p", "", names(pt_coefs))))

# Sort by relative time; insert reference period k = -1 (coef = 0, se = NA)
sorted_idx  <- order(pt_k)
sorted_k    <- pt_k[sorted_idx]
sorted_coef <- pt_coefs[sorted_idx]
sorted_se   <- pt_se[sorted_idx]

pre_mask  <- sorted_k < -1
post_mask <- sorted_k >= 0

event_df <- data.frame(
    k    = c(sorted_k[pre_mask], -1L, sorted_k[post_mask]),
    coef = c(sorted_coef[pre_mask], 0, sorted_coef[post_mask]),
    se   = c(sorted_se[pre_mask], NA_real_, sorted_se[post_mask])
)
event_df$lower <- event_df$coef - 1.96 * event_df$se
event_df$upper <- event_df$coef + 1.96 * event_df$se

p_pt <- ggplot(event_df, aes(x = k, y = coef)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_vline(xintercept =-0.5, linetype = "dotted", colour = "red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.25) +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    geom_point(colour = "steelblue", size = 2.5) +
    labs(
        title    = "Parallel-Trends Validation: OLS Event Study",
        subtitle = "Relative-time dummies | Reference: k = -1 | WB PTA cohort",
        x        = "Years relative to PTA entry",
        y        = "OLS coefficient (ln Exports)"
    ) +
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))

ggsave(file.path(dirs$tables, "NL_Figure4_PreTrend_OLS.pdf"),
       plot = p_pt, width = 8, height = 5)
ggsave(file.path(dirs$tables, "NL_Figure4_PreTrend_OLS.png"),
       plot = p_pt, width = 8, height = 5, dpi = 300)

cat("Pre-trend plot saved.\n")


# ─────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────
cat("\n=== REPLICATION: Neri-Laine et al. (2021) – COMPLETATO! ===\n")
cat("Output directory:", out_dir, "\n")
cat("Tabelle in:      ", dirs$tables, "\n")
cat("Modelli in:      ", dirs$models, "\n\n")
cat("Output generated:\n")
cat("  Table 2a  NL_Table2_Intensive_WB.tex         [CORE: intensive margin, WB]\n")
cat("  Table 2b  NL_Table2b_Intensive_TREND.tex     [Robustness: TREND]\n")
cat("  Table 3   NL_Table3_Extensive_LPM.tex        [Extensive margin: new entry LPM]\n")
cat("  Table 4   NL_Table4_TWFE_DiD.tex             [TWFE DiD with heterogeneity]\n")
cat("  Figure 1  NL_Figure1_CS_EventStudy_WB.pdf    [Callaway-Sant'Anna – WB]\n")
cat("  Figure 2  NL_Figure2_CS_EventStudy_TREND.pdf [Callaway-Sant'Anna – TREND]\n")
cat("  Figure 3  NL_Figure3_CS_EventStudy_WB_*.pdf  [By product type]\n")
cat("  Figure 4  NL_Figure4_PreTrend_OLS.pdf        [Parallel trends validation]\n\n")
cat("Tempo totale:", round(as.numeric(now() - start, units = "mins"), 1), "minuti\n")
