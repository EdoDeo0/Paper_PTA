###########################################################################
###### Replication: Baccini, Pinto & Ortiz-Mena (2017)             ######
###### "The Distributional Consequences of Preferential Trade      ######
######  Liberalization: Firm-Level Evidence"                        ######
###### World Politics 69(2): 373-395                               ######
###########################################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## ── PAPER SUMMARY ────────────────────────────────────────────────────────
## Baccini et al. (2017) analyse Chilean manufacturing firm-level data to
## show that PTAs have HETEROGENEOUS ("distributional") effects: the gains
## from preferential liberalisation accrue disproportionately to exporters
## in heavily-protected sectors and to capital-intensive firms. Their core
## empirical strategy interacts the PTA treatment variable with measures of
## pre-agreement protection and firm characteristics.
##
## ── REPLICATION STRATEGY ─────────────────────────────────────────────────
## We transpose the methodology to our Chinese product-destination-year data:
##   "Firm"            → product × firm × destination cell (fpd)
##   "Pre-PTA protection" → mean MFN tariff quintile of the (hs6 × dest) pair
##   "Distributional outcome" → heterogeneous responses in export value,
##                              quantity, and unit value across tariff tiers
##
## Key tables produced:
##   Table 1  – Baseline OLS (confirms main effect replication starting point)
##   Table 2  – Distributional effects: EPDepth × tariff quintile (WB index)
##   Table 3  – Distributional effects: TREND Depth × tariff quintile
##   Table 4  – Baseline PPML (confirming results with non-log estimator)
##   Table 5  – Heterogeneous effects: EPDepth × EnvGood (WB + TREND)
##
## ── IMPORTANT NOTE ───────────────────────────────────────────────────────
## Original paper uses firm-level Chilean ENIA data; here we adapt the
## distributional-heterogeneity logic to the product/destination dimension
## of Chinese customs data disaggregated by environmental PTA provisions.
##
## This script uses the shared function library in pta_functions.R.


# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

source(here("Code/Analysis/pta_functions.R"))

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("Output/Analysis/Replication_Baccini_2017")
dirs      <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# PRE-PROCESSING: TARIFF QUINTILE VARIABLE
# ─────────────────────────────────────────────────────────────────────
## Baccini et al. stratify firms/products by the level of protection they
## received BEFORE the PTA. We proxy pre-PTA protection with each
## product-destination pair's mean MFN tariff across the sample, then bin
## into quintiles. Quintile 1 = already-open products (low protection);
## Quintile 5 = most-protected products (high potential gains from a PTA).

augmented_file <- file.path(out_dir, "data_baccini_augmented.fst")

if (!file.exists(augmented_file)) {
    cat("Building augmented dataset with tariff quintiles...\n")

    dt <- as.data.table(read_fst(data_file, columns = c(
        "ln_export", "ln_export_qua", "ln_export_value",
        "export", "exp_qua", "uv_exp",
        "WB_EP_Depth", "TREND_EP_Count", "env_good",
        "tariffs", "ln_hhi_baci",
        "fpd", "year", "pdt", "hs6", "country_code"
    )))

    # Compute (hs6 × country_code) mean tariff as the structural protection proxy
    dt[, mean_tariff_pc := mean(tariffs, na.rm = TRUE), by = .(hs6, country_code)]

    # Quintile bins (NA-safe; ties go to the lower bin)
    qbreaks <- quantile(dt$mean_tariff_pc, probs = seq(0, 1, 0.2), na.rm = TRUE)
    dt[, tariff_quintile := as.integer(cut(
        mean_tariff_pc,
        breaks = qbreaks,
        labels = FALSE,
        include.lowest = TRUE
    ))]

    # Interaction dummies Q2–Q5 (Q1 = reference group)
    for (q in 2:5) {
        set(dt, j = paste0("tariff_q", q), value = as.integer(dt$tariff_quintile == q))
    }

    write_fst(dt, augmented_file, compress = 50)
    rm(dt)
    gc()
    cat("Augmented dataset saved to:", augmented_file, "\n")
} else {
    cat("Augmented dataset already exists. Skipping pre-processing.\n")
}


# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────

cm_baseline_wb <- c(
    "WB_EP_Depth"  = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"      = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"  = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_baseline_trend <- c(
    "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

# Distributional coefficient maps: main effect + 4 interaction tiers (Q2–Q5)
cm_dist_wb <- c(
    "WB_EP_Depth"              = "\\textit{EPDepth\\textsubscript{dt}} (Q1 = base)",
    "WB_EP_Depth:tariff_q2"    = "\\textit{EPDepth\\textsubscript{dt} $\\times$ Q2}",
    "WB_EP_Depth:tariff_q3"    = "\\textit{EPDepth\\textsubscript{dt} $\\times$ Q3}",
    "WB_EP_Depth:tariff_q4"    = "\\textit{EPDepth\\textsubscript{dt} $\\times$ Q4}",
    "WB_EP_Depth:tariff_q5"    = "\\textit{EPDepth\\textsubscript{dt} $\\times$ Q5 (High)}",
    "tariffs"                  = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"              = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_dist_trend <- c(
    "TREND_EP_Count"              = "\\textit{TREND Depth\\textsubscript{dt}} (Q1 = base)",
    "TREND_EP_Count:tariff_q2"    = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ Q2}",
    "TREND_EP_Count:tariff_q3"    = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ Q3}",
    "TREND_EP_Count:tariff_q4"    = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ Q4}",
    "TREND_EP_Count:tariff_q5"    = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ Q5 (High)}",
    "tariffs"                     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                 = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_env_wb <- c(
    "WB_EP_Depth"              = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good"     = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                  = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"              = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_env_trend <- c(
    "TREND_EP_Count"              = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good"     = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                 = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# BLOCK 1: OLS Baseline (Table 1)
# ─────────────────────────────────────────────────────────────────────
## Replication starting point: same specification as in the paper but
## confirming it applies to our Chinese customs data x PTA depth measures.
start <- now()

f_base_wb <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats_base_wb <- run_block(f_base_wb, "Baccini_B1_Baseline_WB", "ols",
                           augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_base_wb, cm_baseline_wb,
           "Baccini_Table1_OLS_Baseline_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 2: Distributional Effects – WB index (Table 2)
# ─────────────────────────────────────────────────────────────────────
## Core table replicating the "distributional" logic of Baccini et al.:
## the interaction EPDepth × tariff_quintile captures whether products that
## were more heavily protected (higher quintile) benefit disproportionately
## more from deep environmental provisions in PTAs.
## Hypothesis (H1): β on higher quintile interactions > β on Q1 baseline.

f_dist_wb <- c(
    paste("ln_export ~",
          "WB_EP_Depth + WB_EP_Depth:tariff_q2 + WB_EP_Depth:tariff_q3 +",
          "WB_EP_Depth:tariff_q4 + WB_EP_Depth:tariff_q5 |",
          "fpd + year"),
    paste("ln_export_qua ~",
          "WB_EP_Depth + WB_EP_Depth:tariff_q2 + WB_EP_Depth:tariff_q3 +",
          "WB_EP_Depth:tariff_q4 + WB_EP_Depth:tariff_q5 |",
          "fpd + year"),
    paste("ln_export_value ~",
          "WB_EP_Depth + WB_EP_Depth:tariff_q2 + WB_EP_Depth:tariff_q3 +",
          "WB_EP_Depth:tariff_q4 + WB_EP_Depth:tariff_q5 |",
          "fpd + year"),
    paste("ln_export ~",
          "WB_EP_Depth + WB_EP_Depth:tariff_q2 + WB_EP_Depth:tariff_q3 +",
          "WB_EP_Depth:tariff_q4 + WB_EP_Depth:tariff_q5 +",
          "tariffs + ln_hhi_baci | fpd + year"),
    paste("ln_export_qua ~",
          "WB_EP_Depth + WB_EP_Depth:tariff_q2 + WB_EP_Depth:tariff_q3 +",
          "WB_EP_Depth:tariff_q4 + WB_EP_Depth:tariff_q5 +",
          "tariffs + ln_hhi_baci | fpd + year"),
    paste("ln_export_value ~",
          "WB_EP_Depth + WB_EP_Depth:tariff_q2 + WB_EP_Depth:tariff_q3 +",
          "WB_EP_Depth:tariff_q4 + WB_EP_Depth:tariff_q5 +",
          "tariffs + ln_hhi_baci | fpd + year")
)
stats_dist_wb <- run_block(f_dist_wb, "Baccini_B2_Distribution_WB", "ols",
                           augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_dist_wb, cm_dist_wb,
           "Baccini_Table2_Distributional_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 3: Distributional Effects – TREND index (Table 3)
# ─────────────────────────────────────────────────────────────────────
## Robustness check using the TREND EP Count measure of PTA depth.

f_dist_trend <- c(
    paste("ln_export ~",
          "TREND_EP_Count + TREND_EP_Count:tariff_q2 + TREND_EP_Count:tariff_q3 +",
          "TREND_EP_Count:tariff_q4 + TREND_EP_Count:tariff_q5 |",
          "fpd + year"),
    paste("ln_export_qua ~",
          "TREND_EP_Count + TREND_EP_Count:tariff_q2 + TREND_EP_Count:tariff_q3 +",
          "TREND_EP_Count:tariff_q4 + TREND_EP_Count:tariff_q5 |",
          "fpd + year"),
    paste("ln_export_value ~",
          "TREND_EP_Count + TREND_EP_Count:tariff_q2 + TREND_EP_Count:tariff_q3 +",
          "TREND_EP_Count:tariff_q4 + TREND_EP_Count:tariff_q5 |",
          "fpd + year"),
    paste("ln_export ~",
          "TREND_EP_Count + TREND_EP_Count:tariff_q2 + TREND_EP_Count:tariff_q3 +",
          "TREND_EP_Count:tariff_q4 + TREND_EP_Count:tariff_q5 +",
          "tariffs + ln_hhi_baci | fpd + year"),
    paste("ln_export_qua ~",
          "TREND_EP_Count + TREND_EP_Count:tariff_q2 + TREND_EP_Count:tariff_q3 +",
          "TREND_EP_Count:tariff_q4 + TREND_EP_Count:tariff_q5 +",
          "tariffs + ln_hhi_baci | fpd + year"),
    paste("ln_export_value ~",
          "TREND_EP_Count + TREND_EP_Count:tariff_q2 + TREND_EP_Count:tariff_q3 +",
          "TREND_EP_Count:tariff_q4 + TREND_EP_Count:tariff_q5 +",
          "tariffs + ln_hhi_baci | fpd + year")
)
stats_dist_trend <- run_block(f_dist_trend, "Baccini_B3_Distribution_TREND", "ols",
                              augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_dist_trend, cm_dist_trend,
           "Baccini_Table3_Distributional_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 4: PPML Baseline (Table 4)
# ─────────────────────────────────────────────────────────────────────
## Baccini et al. check robustness with alternative estimators. Here we
## run PPML on positive export levels, which handles the log-of-zero issue
## and is the standard gravity-equation estimator (Santos Silva & Tenreyro).

f_ppml_wb <- c(
    "export  ~ WB_EP_Depth | fpd + year",
    "exp_qua ~ WB_EP_Depth | fpd + year",
    "uv_exp  ~ WB_EP_Depth | fpd + year",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats_ppml_wb <- run_block(f_ppml_wb, "Baccini_B4_PPML_WB", "ppml",
                           augmented_file, dirs$models, vcov = ~pdt)
make_table(stats_ppml_wb, cm_baseline_wb,
           "Baccini_Table4_PPML_Baseline_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 5: Heterogeneity by Product Type (Table 5)
# ─────────────────────────────────────────────────────────────────────
## Akin to the firm-type heterogeneity in Baccini et al., we test whether
## environmental goods respond differently from non-environmental goods to
## deeper environmental provisions — the most direct distributional test.

f_env_wb <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_env_wb <- run_block(f_env_wb, "Baccini_B5a_EnvGood_WB", "ols",
                          augmented_file, dirs$models, vcov = ~pdt)

f_env_trend <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats_env_trend <- run_block(f_env_trend, "Baccini_B5b_EnvGood_TREND", "ols",
                             augmented_file, dirs$models, vcov = ~pdt)

# Panel A: WB index (models 1–6), Panel B: TREND index (models 7–12)
# Write separate sub-tables so each fits on a page
make_table(stats_env_wb, cm_env_wb,
           "Baccini_Table5a_EnvGood_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))

make_table(stats_env_trend, cm_env_trend,
           "Baccini_Table5b_EnvGood_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "UnitValue",
                        "Exports", "Quantity", "UnitValue"),
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────
cat("\n=== REPLICATION: Baccini et al. (2017) – COMPLETATO! ===\n")
cat("Output directory:", out_dir, "\n")
cat("Tabelle in:      ", dirs$tables, "\n")
cat("Modelli in:      ", dirs$models, "\n\n")
cat("Tables generated:\n")
cat("  Table 1  Baccini_Table1_OLS_Baseline_WB.tex\n")
cat("  Table 2  Baccini_Table2_Distributional_WB.tex      [CORE: tariff quintile interactions]\n")
cat("  Table 3  Baccini_Table3_Distributional_TREND.tex   [Robustness: TREND measure]\n")
cat("  Table 4  Baccini_Table4_PPML_Baseline_WB.tex       [Robustness: PPML]\n")
cat("  Table 5a Baccini_Table5a_EnvGood_WB.tex            [Heterogeneity: env goods]\n")
cat("  Table 5b Baccini_Table5b_EnvGood_TREND.tex\n\n")
cat("Tempo totale:", round(as.numeric(now() - start, units = "mins"), 1), "minuti\n")
