###########################################################################
###### Replication: Crowley, Meng & Song (2021)                    ######
###### "The Value of Deep Trade Agreements in the Presence of       ######
######  Pricing-to-Market"                                          ######
###########################################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## ── PAPER SUMMARY ────────────────────────────────────────────────────────
## Crowley, Meng & Song (2021) develop and test the hypothesis that "deep"
## trade agreements — those with behind-the-border regulatory provisions —
## do not merely expand trade volumes but also enable exporters to engage in
## destination-specific PRICING-TO-MARKET (PTM): they charge higher (quality-
## adjusted) prices to destinations where deep agreements remove standards and
## regulatory barriers. The central empirical decomposition is:
##
##   ln(Export Value) = ln(Quantity) + ln(Unit Value)
##
## If deep PTAs raise unit values in addition to quantities the β on
## ln(UV) > 0, consistent with quality upgrading / PTM.
##
## The paper also tests WHICH dimensions of PTA depth drive the UV effect:
## hard (binding/enforceable) provisions reduce compliance uncertainty and
## may specialise in facilitating quality-differentiated goods, whereas soft
## (aspirational) provisions primarily expand market access volumes.
##
## ── REPLICATION STRATEGY ─────────────────────────────────────────────────
## We implement the value decomposition using our Chinese customs data and
## the WB + TREND environmental-provision sub-indices:
##
##   (1) Main decomposition: EPDepth → value, quantity, unit value (OLS + PPML)
##   (2) More-demanding FE structures to identify the PTM channel
##   (3) Sub-index analysis: hard vs soft vis-à-vis enforcement provisions
##   (4) Interaction with env_good: green goods may show stronger PTM
##   (5) WB sub-index breakdown: which dimension of depth drives the UV effect?
##
## Key tables produced:
##   Table 1  – OLS value decomposition (WB index, baseline + controls)
##   Table 2  – OLS value decomposition (TREND index)
##   Table 3  – PPML value decomposition (confirming with count model)
##   Table 4  – WB sub-index decomposition (PTA dimension analysis)
##   Table 5  – TREND sub-index decomposition (Hard vs Soft vs sub-dimensions)
##   Table 6  – Interaction with env_good (PTM stronger for green goods?)
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
out_dir   <- here("Output/Analysis/Replication_Crowley_2021")
dirs      <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────

cm_wb <- c(
    "WB_EP_Depth"  = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"      = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"  = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_trend <- c(
    "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

# WB sub-index coefficient map
# (sub-indices available in the merged dataset)
cm_wb_sub <- c(
    "WB_StandardsNonRegression"      = "\\textit{Standards \\& Non-Regression\\textsubscript{dt}}",
    "WB_EnforcementDSM"              = "\\textit{Enforcement / DSM\\textsubscript{dt}}",
    "WB_RegulatorySpaceExceptions"   = "\\textit{Regulatory Space\\textsubscript{dt}}",
    "WB_GreenLiberalization"         = "\\textit{Green Liberalisation\\textsubscript{dt}}",
    "WB_Assistance"                  = "\\textit{Capacity Assistance\\textsubscript{dt}}",
    "tariffs"                        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

# TREND sub-index coefficient map
cm_trend_sub <- c(
    "TREND_Hard"                     = "\\textit{Hard (Binding) Provisions\\textsubscript{dt}}",
    "TREND_Soft"                     = "\\textit{Soft (Aspirational) Provisions\\textsubscript{dt}}",
    "TREND_EnforcementDSM"           = "\\textit{Enforcement / DSM\\textsubscript{dt}}",
    "TREND_RegulatorySpace"          = "\\textit{Regulatory Space\\textsubscript{dt}}",
    "TREND_GreenMarketAccess"        = "\\textit{Green Market Access\\textsubscript{dt}}",
    "TREND_ClimateEnergy"            = "\\textit{Climate \\& Energy\\textsubscript{dt}}",
    "tariffs"                        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

# Interaction (PTM test on environmental goods)
cm_wb_ptm <- c(
    "WB_EP_Depth"              = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good"     = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                  = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"              = "\\textit{ln HHI\\textsubscript{pdt}}"
)

cm_trend_ptm <- c(
    "TREND_EP_Count"               = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good"      = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                      = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                  = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# BLOCK 1: OLS Value Decomposition — WB Index (Table 1)
# ─────────────────────────────────────────────────────────────────────
## Core test of the Crowley et al. mechanism: if β(UV) > 0 the data support
## quality upgrading / PTM. If β(UV) ≈ 0 then the PTA effect is purely
## a volume (quantity) expansion with no pricing adjustment.
start <- now()

f1_wb <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1_wb <- run_block(f1_wb, "Crowley_B1_OLS_WB", "ols",
                       data_file, dirs$models, vcov = ~pdt)
make_table(stats1_wb, cm_wb,
           "Crowley_Table1_OLS_Decomposition_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value",
                        "\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 2: OLS Value Decomposition — TREND Index (Table 2)
# ─────────────────────────────────────────────────────────────────────

f2_trend <- c(
    "ln_export       ~ TREND_EP_Count | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats2_trend <- run_block(f2_trend, "Crowley_B2_OLS_TREND", "ols",
                          data_file, dirs$models, vcov = ~pdt)
make_table(stats2_trend, cm_trend,
           "Crowley_Table2_OLS_Decomposition_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value",
                        "\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 3: PPML Value Decomposition (Table 3)
# ─────────────────────────────────────────────────────────────────────
## Santos Silva & Tenreyro (2006) show that OLS on log-transformed trade
## is biased in the presence of heteroskedasticity. Crowley et al. verify
## their findings with PPML. We replicate both estimators.

f3_ppml <- c(
    "export  ~ WB_EP_Depth | fpd + year",
    "exp_qua ~ WB_EP_Depth | fpd + year",
    "uv_exp  ~ WB_EP_Depth | fpd + year",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats3_ppml <- run_block(f3_ppml, "Crowley_B3_PPML_WB", "ppml",
                         data_file, dirs$models, vcov = ~pdt)
make_table(stats3_ppml, cm_wb,
           "Crowley_Table3_PPML_Decomposition_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("Exports", "Quantity", "Unit Value",
                        "Exports", "Quantity", "Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 4: WB Sub-Index Decomposition (Table 4)
# ─────────────────────────────────────────────────────────────────────
## Crowley et al. probe WHICH dimension of PTA depth drives quality upgrading
## vs. volume expansion. We decompose the WB EP index into its five sub-themes
## and run the three-outcome system for each sub-index.
## Each group of 3 columns = one sub-index × {value, quantity, UV}.
## NOTE: sub-indices must be present in the final fst; they are merged there
## from Data/Merged/Merged_TREND_WB_Indices_Only.dta.

# Outcome triplets for each WB sub-index (with controls)
wb_subindices <- c(
    "WB_StandardsNonRegression",
    "WB_EnforcementDSM",
    "WB_RegulatorySpaceExceptions",
    "WB_GreenLiberalization",
    "WB_Assistance"
)

wb_sub_labels <- c(
    "Standards",
    "Enforcement",
    "Reg. Space",
    "Green Lib.",
    "Assistance"
)

f4_wb_sub <- unlist(lapply(wb_subindices, function(idx) c(
    sprintf("ln_export       ~ %s + tariffs + ln_hhi_baci | fpd + year", idx),
    sprintf("ln_export_qua   ~ %s + tariffs + ln_hhi_baci | fpd + year", idx),
    sprintf("ln_export_value ~ %s + tariffs + ln_hhi_baci | fpd + year", idx)
)))

stats4_wb_sub <- run_block(f4_wb_sub, "Crowley_B4_WB_SubIndex", "ols",
                           data_file, dirs$models, vcov = ~pdt)

make_table(stats4_wb_sub, cm_wb_sub,
           "Crowley_Table4_WB_SubIndex.tex", dirs$tables,
           digits = 5,
           dep_vars = rep(c("ln Value", "ln Qty", "ln UV"), length(wb_subindices)),
           dep_subscript   = "fpdt",
           group_headers   = wb_sub_labels,
           group_cols      = rep(3, length(wb_subindices)),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 5: TREND Sub-Index Decomposition (Table 5)
# ─────────────────────────────────────────────────────────────────────
## The TREND database distinguishes legally BINDING (Hard) provisions from
## aspirational (Soft) ones. The pricing-to-market channel (Crowley et al.)
## predicts that Hard provisions drive quality upgrading more strongly because
## they reduce regulatory uncertainty and enable long-term quality investment.

trend_subindices <- c(
    "TREND_Hard",
    "TREND_Soft",
    "TREND_EnforcementDSM",
    "TREND_RegulatorySpace",
    "TREND_GreenMarketAccess",
    "TREND_ClimateEnergy"
)

trend_sub_labels <- c(
    "Hard",
    "Soft",
    "Enforcement",
    "Reg. Space",
    "Green Access",
    "Climate"
)

f5_trend_sub <- unlist(lapply(trend_subindices, function(idx) c(
    sprintf("ln_export       ~ %s + tariffs + ln_hhi_baci | fpd + year", idx),
    sprintf("ln_export_qua   ~ %s + tariffs + ln_hhi_baci | fpd + year", idx),
    sprintf("ln_export_value ~ %s + tariffs + ln_hhi_baci | fpd + year", idx)
)))

stats5_trend_sub <- run_block(f5_trend_sub, "Crowley_B5_TREND_SubIndex", "ols",
                              data_file, dirs$models, vcov = ~pdt)

make_table(stats5_trend_sub, cm_trend_sub,
           "Crowley_Table5_TREND_SubIndex.tex", dirs$tables,
           digits = 5,
           dep_vars = rep(c("ln Value", "ln Qty", "ln UV"), length(trend_subindices)),
           dep_subscript   = "fpdt",
           group_headers   = trend_sub_labels,
           group_cols      = rep(3, length(trend_subindices)),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# BLOCK 6: PTM Heterogeneity — Environmental Goods (Table 6)
# ─────────────────────────────────────────────────────────────────────
## Environmental goods may face stricter technical standards (ecolabels,
## efficiency certifications) that deep PTA provisions directly facilitate.
## If so, the unit-value response should be LARGER for env_good = 1,
## consistent with stronger PTM behaviour for green products.

f6_ptm_wb <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats6_wb <- run_block(f6_ptm_wb, "Crowley_B6a_PTM_EnvGood_WB", "ols",
                       data_file, dirs$models, vcov = ~pdt)

f6_ptm_trend <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats6_trend <- run_block(f6_ptm_trend, "Crowley_B6b_PTM_EnvGood_TREND", "ols",
                          data_file, dirs$models, vcov = ~pdt)

make_table(stats6_wb, cm_wb_ptm,
           "Crowley_Table6a_PTM_EnvGood_WB.tex", dirs$tables,
           digits = 5,
           dep_vars = c("\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value",
                        "\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))

make_table(stats6_trend, cm_trend_ptm,
           "Crowley_Table6b_PTM_EnvGood_TREND.tex", dirs$tables,
           digits = 5,
           dep_vars = c("\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value",
                        "\\textit{ln} Exports", "\\textit{ln} Quantity", "\\textit{ln} Unit Value"),
           dep_subscript   = "fpdt",
           group_headers   = c("Baseline", "With controls"),
           group_cols      = c(3, 3),
           show_stats      = c("nobs", "r2", "n_clust"))


# ─────────────────────────────────────────────────────────────────────
# SUMMARY
# ─────────────────────────────────────────────────────────────────────
cat("\n=== REPLICATION: Crowley, Meng & Song (2021) – COMPLETATO! ===\n")
cat("Output directory:", out_dir, "\n")
cat("Tabelle in:      ", dirs$tables, "\n")
cat("Modelli in:      ", dirs$models, "\n\n")
cat("Tables generated:\n")
cat("  Table 1  Crowley_Table1_OLS_Decomposition_WB.tex     [CORE: value decomposition, WB]\n")
cat("  Table 2  Crowley_Table2_OLS_Decomposition_TREND.tex  [Robustness: TREND measure]\n")
cat("  Table 3  Crowley_Table3_PPML_Decomposition_WB.tex    [Robustness: PPML]\n")
cat("  Table 4  Crowley_Table4_WB_SubIndex.tex              [Which WB dimension drives UV?]\n")
cat("  Table 5  Crowley_Table5_TREND_SubIndex.tex           [Hard vs Soft provisions]\n")
cat("  Table 6a Crowley_Table6a_PTM_EnvGood_WB.tex          [PTM on green goods]\n")
cat("  Table 6b Crowley_Table6b_PTM_EnvGood_TREND.tex\n\n")
cat("Tempo totale:", round(as.numeric(now() - start, units = "mins"), 1), "minuti\n")
