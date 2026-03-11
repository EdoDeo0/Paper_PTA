#############################
###### PPML Estimation ######
#############################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## PPML Estimation without zeros fill-in (only positive export flows)
## using fepois from the fixest package.
##
## This script uses the shared function library in pta_functions.R.
## All estimation and table-building logic lives there.

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

# Set the number of threads for fst (adjust based on your CPU)
# threads_fst(8) # 8 threads is a good default for modern CPUs (especially for laptops), but adjust as needed


source(here("Code/Analysis/pta_functions.R"))

# Set your own data file path (dataset not tracked in the repo – file too large)
data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst") ## On Windows
out_dir <- here("Output/Analysis/PPML")
dirs <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))


# ─────────────────────────────────────────────────────────────────────
# COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────

cm_wb <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend_int <- c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# RUNNING MODELS AND GENERATING TABLES
# ─────────────────────────────────────────────────────────────────────
start <- now()

# BLOCK 1: WB No Interaction
f1 <- c(
    "export  ~ WB_EP_Depth | fpd + year",
    "exp_qua ~ WB_EP_Depth | fpd + year",
    "uv_exp  ~ WB_EP_Depth | fpd + year",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction", "ppml", data_file, dirs$models, vcov = ~pdt)
make_table(stats1, cm_wb, "PPML_WB_No_Interaction.tex", dirs$tables, digits = 5)

# BLOCK 2: WB Interaction
f2 <- c(
    "export  ~ WB_EP_Depth * env_good | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good | fpd + year",
    "uv_exp  ~ WB_EP_Depth * env_good | fpd + year",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "WB Interaction", "ppml", data_file, dirs$models, vcov = ~pdt)
make_table(stats2, cm_wb_int, "PPML_WB_Interaction.tex", dirs$tables, digits = 5)

# BLOCK 3: TREND No Interaction
f3 <- c(
    "export  ~ TREND_EP_Count | fpd + year",
    "exp_qua ~ TREND_EP_Count | fpd + year",
    "uv_exp  ~ TREND_EP_Count | fpd + year",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "TREND No Interaction", "ppml", data_file, dirs$models, vcov = ~pdt)
make_table(stats3, cm_trend, "PPML_TREND_No_Interaction.tex", dirs$tables, digits = 5)

# BLOCK 4: TREND Interaction
f4 <- c(
    "export  ~ TREND_EP_Count * env_good | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good | fpd + year",
    "uv_exp  ~ TREND_EP_Count * env_good | fpd + year",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "TREND Interaction", "ppml", data_file, dirs$models, vcov = ~pdt)
make_table(stats4, cm_trend_int, "PPML_TREND_Interaction.tex", dirs$tables, digits = 5)

cat("\n=== COMPLETATO! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_*.rds\n")
cat("Tempo totale:", now() - start, "secondi\n")
