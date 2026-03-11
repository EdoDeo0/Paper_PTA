###########################################################################
###### Replication: Lee, Park & Shin                                ######
###### "Trade Facilitation Provisions in Preferential Trade         ######
######  Agreements"                                                  ######
###########################################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## ── PAPER SUMMARY ────────────────────────────────────────────────────────
## Lee, Park & Shin study how trade facilitation (TF) provisions embedded in
## preferential trade agreements reduce trade costs and stimulate bilateral
## trade flows. Their core approach is a PPML gravity model with a PTA-depth
## index that emphasises procedural/market-access provisions (customs
## procedures, transparency, electronic commerce, TBT/SPS facilitation) rather
## than the "behind-the-border" regulatory harmonisation captured by other
## deep-integration indices.
##
## Key findings:
##   (i)  TF-type PTA provisions significantly increase trade volumes.
##   (ii) The effect is heterogeneous: goods with high trade costs (bulky,
##        perishable, or with complex compliance requirements) benefit more.
##  (iii) Legal enforceability (hard/binding provisions) amplifies the effect.
##
## ── REPLICATION STRATEGY ─────────────────────────────────────────────────
## Our dataset does not contain a pure "trade facilitation" sub-index;
## the WB and TREND databases cover ENVIRONMENTAL provisions. However, several
## sub-dimensions serve as close proxies for market-access facilitation:
##
##   Proxy for TF provisions:
##     TREND_GreenMarketAccess   – PTA clauses on green market access
##                                 (tariff reduction, quota elimination,
##                                  eco-label recognition, TBT/SPS provisions)
##     WB_GreenLiberalization    – WB clauses on green goods/services trade
##
##   Proxy for rule-of-law / enforceability:
##     TREND_Hard                – binding/enforceable TREND provisions
##     WB_EnforcementDSM         – WB enforcement & dispute-settlement clauses
##
##   Regulatory-procedure proxies:
##     TREND_RegulatorySpace     – provisions preserving signatories' right
##                                  to regulate (transparency/predictability)
##     WB_RegulatorySpaceExceptions – analogous WB measure
##
##   Summary/aggregate depth:
##     WB_EP_Depth / TREND_EP_Count  – total depth (as in existing scripts)
##
## We use PPML as the primary estimator (consistent with the TF literature),
## with OLS as robustness. Heterogeneity by product type is captured through
## the env_good dummy (environmental goods face more TBT/certification hurdles
## and should benefit most from TF-type provisions).
##
## Key tables produced:
##   Table 1  – PPML baseline: aggregate EP depth (WB + TREND)
##   Table 2  – PPML: green market access sub-index (WB + TREND)
##   Table 3  – PPML: side-by-side comparison of all sub-dimensions
##   Table 4  – Hard vs Soft provisions (enforceability test)
##   Table 5  – OLS robustness (same specs as Table 2)
##   Table 6  – Heterogeneity: interaction with env_good (which goods gain most?)
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
out_dir   <- here("Output/Analysis/Replication_Lee_TF")
dirs      <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────

cm_aggregate <- c(
    "WB_EP_Depth"    = "\\textit{EPDepth (WB)\\textsubscript{dt}}",
    "TREND_EP_Count" = "\\textit{EP Count (TREND)\\textsubscript{dt}}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_tf_proxy <- c(
    "TREND_GreenMarketAccess"  = "\\textit{Green Market Access (TREND)\\textsubscript{dt}}",
    "WB_GreenLiberalization"   = "\\textit{Green Liberalisation (WB)\\textsubscript{dt}}",
    "tariffs"                  = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"              = "\\textit{ln HHI\\textsubscript{pdt}}"
)

# All sub-dimensions in one table (each in a separate column triplet)
cm_all_sub <- c(
    "TREND_GreenMarketAccess"        = "\\textit{GreenAccess\\textsubscript{dt}}",
    "WB_GreenLiberalization"         = "\\textit{GreenLib\\textsubscript{dt}}",
    "TREND_Hard"                     = "\\textit{Hard Prov.\\textsubscript{dt}}",
    "TREND_Soft"                     = "\\textit{Soft Prov.\\textsubscript{dt}}",
    "WB_EnforcementDSM"              = "\\textit{Enf./DSM\\textsubscript{dt}}",
    "TREND_RegulatorySpace"          = "\\textit{Reg. Space\\textsubscript{dt}}",
    "WB_RegulatorySpaceExceptions"   = "\\textit{Reg. Space (WB)\\textsubscript{dt}}",
    "tariffs"                        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_hard_soft <- c(
    "TREND_Hard"            = "\\textit{Hard (Binding) Provisions\\textsubscript{dt}}",
    "TREND_Soft"            = "\\textit{Soft (Aspirational) Provisions\\textsubscript{dt}}",
    "WB_EnforcementDSM"     = "\\textit{Enforcement / DSM (WB)\\textsubscript{dt}}",
    "tariffs"               = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"           = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_het_trend <- c(
    "TREND_GreenMarketAccess"               = "\\textit{Green Market Access\\textsubscript{dt}}",
    "TREND_GreenMarketAccess:env_good"       = "\\textit{GMA\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                               = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                           = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_het_wb <- c(
    "WB_GreenLiberalization"               = "\\textit{Green Liberalisation\\textsubscript{dt}}",
    "WB_GreenLiberalization:env_good"       = "\\textit{GreenLib\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                          = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# BLOCK 1: PPML Baseline — Aggregate Depth (Table 1)
# ─────────────────────────────────────────────────────────────────────
## Starting point: confirm that deeper EP provisions in PTAs increase trade
## values, quantities, and unit values using PPML (preferred in TF literature).
start <- now()

f1_agg <- c(
    "export  ~ WB_EP_Depth | fpd + year",
    "exp_qua ~ WB_EP_Depth | fpd + year",
    "uv_exp  ~ WB_EP_Depth | fpd + year",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "export  ~ TREND_EP_Count | fpd + year",
    "exp_qua ~ TREND_EP_Count | fpd + year",
    "uv_exp  ~ TREND_EP_Count | fpd + year",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats1_agg <- run_block(f1_agg, "LeeTF_B1_PPML_Aggregate", "ppml",
                        data_file, dirs$models, vcov = ~pdt)

# Panel A (WB, cols 1-6) and Panel B (TREND, cols 7-12) as separate tables
make_table(stats1_agg[1:6], cm_aggregate[c("WB_EP_Depth", "tariffs", "ln_hhi_baci")],
           "LeeTF_Table1a_PPML_Baseline_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))

make_table(stats1_agg[7:12], cm_aggregate[c("TREND_EP_Count", "tariffs", "ln_hhi_baci")],
           "LeeTF_Table1b_PPML_Baseline_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 2: PPML – TF Proxies (Green Market Access, Table 2)
# ─────────────────────────────────────────────────────────────────────
## These sub-indices most directly proxy for trade-facilitation provisions:
## TREND_GreenMarketAccess covers provisions on tariff removal, eco-label
## recognition, customs cooperation, and TBT/SPS mutual recognition for
## green goods — precisely the mechanisms Lee et al. focus on.

f2_tf <- c(
    "export  ~ TREND_GreenMarketAccess | fpd + year",
    "exp_qua ~ TREND_GreenMarketAccess | fpd + year",
    "uv_exp  ~ TREND_GreenMarketAccess | fpd + year",
    "export  ~ TREND_GreenMarketAccess + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_GreenMarketAccess + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_GreenMarketAccess + tariffs + ln_hhi_baci | fpd + year",
    "export  ~ WB_GreenLiberalization | fpd + year",
    "exp_qua ~ WB_GreenLiberalization | fpd + year",
    "uv_exp  ~ WB_GreenLiberalization | fpd + year",
    "export  ~ WB_GreenLiberalization + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_GreenLiberalization + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_GreenLiberalization + tariffs + ln_hhi_baci | fpd + year"
)
stats2_tf <- run_block(f2_tf, "LeeTF_B2_PPML_TF_Proxies", "ppml",
                       data_file, dirs$models, vcov = ~pdt)

make_table(stats2_tf[1:6],
           cm_tf_proxy[c("TREND_GreenMarketAccess", "tariffs", "ln_hhi_baci")],
           "LeeTF_Table2a_PPML_GreenMarketAccess_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))

make_table(stats2_tf[7:12],
           cm_tf_proxy[c("WB_GreenLiberalization", "tariffs", "ln_hhi_baci")],
           "LeeTF_Table2b_PPML_GreenLiberalization_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 3: Side-by-Side Sub-Dimension Comparison (Table 3)
# ─────────────────────────────────────────────────────────────────────
## Lee et al. compare TF provisions against other types of PTA content.
## We implement this by estimating each outcome (export value) with each
## sub-index separately, showing all estimates in one wide table.

sub_dims <- c(
    "TREND_GreenMarketAccess",
    "WB_GreenLiberalization",
    "TREND_Hard",
    "TREND_Soft",
    "WB_EnforcementDSM",
    "TREND_RegulatorySpace",
    "WB_RegulatorySpaceExceptions"
)

col_labels <- c(
    "TREND GMA", "WB GreenLib", "Hard", "Soft",
    "WB Enf.", "TREND RegSp.", "WB RegSp."
)

f3_all_sub <- unlist(lapply(sub_dims, function(idx)
    sprintf("export ~ %s + tariffs + ln_hhi_baci | fpd + year", idx)
))
stats3_all_sub <- run_block(f3_all_sub, "LeeTF_B3_AllSubDims_PPML", "ppml",
                            data_file, dirs$models, vcov = ~pdt)

make_table(stats3_all_sub, cm_all_sub,
           "LeeTF_Table3_PPML_AllSubDimensions.tex", dirs$tables,
           digits = 5,
           dep_vars = col_labels,
           dep_subscript   = "fpdt",
           group_headers   = c("Export Value – all sub-indices"),
           group_cols      = c(length(sub_dims)),
           show_stats      = c("nobs", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 4: Hard vs Soft Provisions (Table 4)
# ─────────────────────────────────────────────────────────────────────
## Lee et al. find that legally binding (hard) TF provisions are more
## effective. We test this directly using TREND_Hard vs TREND_Soft and the
## WB enforcement measure. All three are included simultaneously so we
## can compare magnitudes in the same regression.

f4_hard_soft <- c(
    "export  ~ TREND_Hard + TREND_Soft + WB_EnforcementDSM | fpd + year",
    "exp_qua ~ TREND_Hard + TREND_Soft + WB_EnforcementDSM | fpd + year",
    "uv_exp  ~ TREND_Hard + TREND_Soft + WB_EnforcementDSM | fpd + year",
    "export  ~ TREND_Hard + TREND_Soft + WB_EnforcementDSM + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_Hard + TREND_Soft + WB_EnforcementDSM + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_Hard + TREND_Soft + WB_EnforcementDSM + tariffs + ln_hhi_baci | fpd + year"
)
stats4_hard_soft <- run_block(f4_hard_soft, "LeeTF_B4_HardSoft_PPML", "ppml",
                              data_file, dirs$models, vcov = ~pdt)
make_table(stats4_hard_soft, cm_hard_soft,
           "LeeTF_Table4_PPML_HardSoft.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 5: OLS Robustness (Table 5)
# ─────────────────────────────────────────────────────────────────────
## Lee et al. present OLS/HDFE estimates for comparison. We replicate
## the TF-proxy specifications from Block 2 using OLS.

f5_ols_tf <- c(
    "ln_export       ~ TREND_GreenMarketAccess | fpd + year",
    "ln_export_qua   ~ TREND_GreenMarketAccess | fpd + year",
    "ln_export_value ~ TREND_GreenMarketAccess | fpd + year",
    "ln_export       ~ TREND_GreenMarketAccess + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_GreenMarketAccess + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_GreenMarketAccess + tariffs + ln_hhi_baci | fpd + year",
    "ln_export       ~ WB_GreenLiberalization | fpd + year",
    "ln_export_qua   ~ WB_GreenLiberalization | fpd + year",
    "ln_export_value ~ WB_GreenLiberalization | fpd + year",
    "ln_export       ~ WB_GreenLiberalization + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_GreenLiberalization + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_GreenLiberalization + tariffs + ln_hhi_baci | fpd + year"
)
stats5_ols_tf <- run_block(f5_ols_tf, "LeeTF_B5_OLS_TF_Proxies", "ols",
                           data_file, dirs$models, vcov = ~pdt)

make_table(stats5_ols_tf[1:6],
           cm_tf_proxy[c("TREND_GreenMarketAccess", "tariffs", "ln_hhi_baci")],
           "LeeTF_Table5a_OLS_GreenMarketAccess_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value",
                        "\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))

make_table(stats5_ols_tf[7:12],
           cm_tf_proxy[c("WB_GreenLiberalization", "tariffs", "ln_hhi_baci")],
           "LeeTF_Table5b_OLS_GreenLiberalization_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value",
                        "\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 6: Heterogeneity by Product Type (Table 6)
# ─────────────────────────────────────────────────────────────────────
## Lee et al. find that TF provisions benefit more complex / compliance-
## intensive products most. In our data, env_good = 1 products bear
## exactly such burdens (certification, ecolabelling, TBT/SPS procedures).
## We test whether environmental goods gain more from green market-access
## provisions than ordinary goods.

f6_het_trend <- c(
    "export  ~ TREND_GreenMarketAccess * env_good | fpd + year",
    "exp_qua ~ TREND_GreenMarketAccess * env_good | fpd + year",
    "uv_exp  ~ TREND_GreenMarketAccess * env_good | fpd + year",
    "export  ~ TREND_GreenMarketAccess * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_GreenMarketAccess * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_GreenMarketAccess * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats6_het_trend <- run_block(f6_het_trend, "LeeTF_B6a_Het_TREND_PPML", "ppml",
                              data_file, dirs$models, vcov = ~pdt)

f6_het_wb <- c(
    "export  ~ WB_GreenLiberalization * env_good | fpd + year",
    "exp_qua ~ WB_GreenLiberalization * env_good | fpd + year",
    "uv_exp  ~ WB_GreenLiberalization * env_good | fpd + year",
    "export  ~ WB_GreenLiberalization * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_GreenLiberalization * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_GreenLiberalization * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats6_het_wb <- run_block(f6_het_wb, "LeeTF_B6b_Het_WB_PPML", "ppml",
                           data_file, dirs$models, vcov = ~pdt)

make_table(stats6_het_trend, cm_het_trend,
           "LeeTF_Table6a_Het_GreenMarketAccess_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))

make_table(stats6_het_wb, cm_het_wb,
           "LeeTF_Table6b_Het_GreenLiberalization_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────
cat("\n=== REPLICATION: Lee, Park & Shin – COMPLETATO! ===\n")
cat("Output directory:", out_dir, "\n")
cat("Tabelle in:      ", dirs$tables, "\n")
cat("Modelli in:      ", dirs$models, "\n\n")
cat("Tables generated:\n")
cat("  Table 1a LeeTF_Table1a_PPML_Baseline_WB.tex\n")
cat("  Table 1b LeeTF_Table1b_PPML_Baseline_TREND.tex\n")
cat("  Table 2a LeeTF_Table2a_PPML_GreenMarketAccess_TREND.tex  [CORE: TF proxy – TREND]\n")
cat("  Table 2b LeeTF_Table2b_PPML_GreenLiberalization_WB.tex   [CORE: TF proxy – WB]\n")
cat("  Table 3  LeeTF_Table3_PPML_AllSubDimensions.tex          [Sub-dimension comparison]\n")
cat("  Table 4  LeeTF_Table4_PPML_HardSoft.tex                  [Hard vs Soft provisions]\n")
cat("  Table 5a LeeTF_Table5a_OLS_GreenMarketAccess_TREND.tex   [OLS robustness]\n")
cat("  Table 5b LeeTF_Table5b_OLS_GreenLiberalization_WB.tex\n")
cat("  Table 6a LeeTF_Table6a_Het_GreenMarketAccess_TREND.tex   [Heterogeneity: env goods]\n")
cat("  Table 6b LeeTF_Table6b_Het_GreenLiberalization_WB.tex\n\n")
cat("Tempo totale:", round(as.numeric(now() - start, units = "mins"), 1), "minuti\n")
