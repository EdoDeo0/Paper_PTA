#################################
###### PPML - CEM Estimation ####
#################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## PPML Estimation without zeros fill-in (only positive export flows)
## using fepois from the fixest package.
##
## This script uses the same model specifications as PPML.R, 
## but on the CEM dataset produced by CEM.R.

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

# Dataset produced by Matching_v5.R
data_file <- here("Data/Final Dataset/data_cem_matched.fst")
out_dir <- here("Output/Analysis/CEM/PPML")
dirs <- setup_output_dirs(out_dir)

stopifnot("Dataset CEM non trovato!" = file.exists(data_file))


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
# FIRM-PRODUCT-DESTINATION (fdp) + TIME (year) FIXED EFFECTS
# Cluster standard errors at the product-destination (pdt) level
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())

## BLOCK 1: WB No Interaction
cat("\n=== CEM WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
    "export  ~ WB_EP_Depth | fpd + year",
    "exp_qua ~ WB_EP_Depth | fpd + year",
    "uv_exp  ~ WB_EP_Depth | fpd + year",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "CEM WB No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1, cm_wb, "CEM_PPML_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()


## BLOCK 2: WB Interaction
cat("\n=== CEM WB Interaction (fpd + year FE) ===\n")
f2 <- c(
    "export  ~ WB_EP_Depth * env_good | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good | fpd + year",
    "uv_exp  ~ WB_EP_Depth * env_good | fpd + year",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "CEM WB Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2, cm_wb_int, "CEM_PPML_WB_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()


## BLOCK 3: TREND No Interaction
cat("\n=== CEM TREND No Interaction (fpd + year FE) ===\n")
f3 <- c(
    "export  ~ TREND_EP_Count | fpd + year",
    "exp_qua ~ TREND_EP_Count | fpd + year",
    "uv_exp  ~ TREND_EP_Count | fpd + year",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "CEM TREND No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3, cm_trend, "CEM_PPML_TREND_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()


## BLOCK 4: TREND Interaction
cat("\n=== CEM TREND Interaction (fpd + year FE) ===\n")
f4 <- c(
    "export  ~ TREND_EP_Count * env_good | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good | fpd + year",
    "uv_exp  ~ TREND_EP_Count * env_good | fpd + year",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "CEM TREND Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4, cm_trend_int, "CEM_PPML_TREND_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== DONE fpd + year FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_*.rds\n")
cat("Time for fpd + year FE:", now() - start, "seconds\n")





# ────────────────────────────────────────────────────────────────────
# Cluster standard errors at the destination-time level (dt)
# ────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())

## BLOCK 1: WB No Interaction
cat("\n=== CEM WB No Interaction (fpt + pd FE) ===\n")
f1 <- c(
    "export  ~ WB_EP_Depth | fpt + pd",
    "exp_qua ~ WB_EP_Depth | fpt + pd",
    "uv_exp  ~ WB_EP_Depth | fpt + pd",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pd"
)
stats1 <- run_block(f1, "CEM WB No Interaction (fpt + pd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats1, cm_wb, "CEM_PPML_WB_No_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()


## BLOCK 2: WB Interaction
cat("\n=== CEM WB Interaction (fpt + pd FE) ===\n")
f2 <- c(
    "export  ~ WB_EP_Depth * env_good | fpt + pd",
    "exp_qua ~ WB_EP_Depth * env_good | fpt + pd",
    "uv_exp  ~ WB_EP_Depth * env_good | fpt + pd",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pd"
)
stats2 <- run_block(f2, "CEM WB Interaction (fpt + pd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats2, cm_wb_int, "CEM_PPML_WB_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()


## BLOCK 3: TREND No Interaction
cat("\n=== CEM TREND No Interaction (fpt + pd FE) ===\n")
f3 <- c(
    "export  ~ TREND_EP_Count | fpt + pd",
    "exp_qua ~ TREND_EP_Count | fpt + pd",
    "uv_exp  ~ TREND_EP_Count | fpt + pd",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pd"
)
stats3 <- run_block(f3, "CEM TREND No Interaction (fpt + pd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats3, cm_trend, "CEM_PPML_TREND_No_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()


## BLOCK 4: TREND Interaction
cat("\n=== CEM TREND Interaction (fpt + pd FE) ===\n")
f4 <- c(
    "export  ~ TREND_EP_Count * env_good | fpt + pd",
    "exp_qua ~ TREND_EP_Count * env_good | fpt + pd",
    "uv_exp  ~ TREND_EP_Count * env_good | fpt + pd",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pd"
)
stats4 <- run_block(f4, "CEM TREND Interaction (fpt + pd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats4, cm_trend_int, "CEM_PPML_TREND_Interaction_fpt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== DONE fpt + pd FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_fpt.rds\n")
cat("Time for fpt + pd:", now() - start, "seconds\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-TIME FIXED EFFECTS (fpt) + FIRM-PRODUCT-DESTINATION (fpd) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())

## BLOCK 1: WB No Interaction
cat("\n=== CEM WB No Interaction (fpt + fpd FE) ===\n")
f1 <- c(
    "export  ~ WB_EP_Depth | fpt + fpd",
    "exp_qua ~ WB_EP_Depth | fpt + fpd",
    "uv_exp  ~ WB_EP_Depth | fpt + fpd",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + fpd"
)
stats1 <- run_block(f1, "CEM WB No Interaction (fpt + fpd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats1, cm_wb, "CEM_PPML_WB_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()


## BLOCK 2: WB Interaction
cat("\n=== CEM WB Interaction (fpt + fpd FE) ===\n")
f2 <- c(
    "export  ~ WB_EP_Depth * env_good | fpt + fpd",
    "exp_qua ~ WB_EP_Depth * env_good | fpt + fpd",
    "uv_exp  ~ WB_EP_Depth * env_good | fpt + fpd",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats2 <- run_block(f2, "CEM WB Interaction (fpt + fpd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats2, cm_wb_int, "CEM_PPML_WB_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()


## BLOCK 3: TREND No Interaction
cat("\n=== CEM TREND No Interaction (fpt + fpd FE) ===\n")
f3 <- c(
    "export  ~ TREND_EP_Count | fpt + fpd",
    "exp_qua ~ TREND_EP_Count | fpt + fpd",
    "uv_exp  ~ TREND_EP_Count | fpt + fpd",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + fpd"
)
stats3 <- run_block(f3, "CEM TREND No Interaction (fpt + fpd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats3, cm_trend, "CEM_PPML_TREND_No_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()


## BLOCK 4: TREND Interaction
cat("\n=== CEM TREND Interaction (fpt + fpd FE) ===\n")
f4 <- c(
    "export  ~ TREND_EP_Count * env_good | fpt + fpd",
    "exp_qua ~ TREND_EP_Count * env_good | fpt + fpd",
    "uv_exp  ~ TREND_EP_Count * env_good | fpt + fpd",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + fpd"
)
stats4 <- run_block(f4, "CEM TREND Interaction (fpt + fpd FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats4, cm_trend_int, "CEM_PPML_TREND_Interaction_fpt_fpd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== DONE fpt + fpd FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_fpt.rds\n")
cat("Time for fpt + fpd:", now() - start, "seconds\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-DESTINATION (fdp) + PRODUCT-TIME (pt) FIXED EFFECTS
# Cluster standard errors at the destination-time level (dt)
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust") # Put here the statistics you want in the tables (must be in the list of available stats in make_table())
setFixest_nthreads(1) # To avoid windows crash

## BLOCK 1: WB No Interaction
cat("\n=== CEM WB No Interaction (fpd + pt FE) ===\n")
f1 <- c(
    "export  ~ WB_EP_Depth | fpd + pt",
    "exp_qua ~ WB_EP_Depth | fpd + pt",
    "uv_exp  ~ WB_EP_Depth | fpd + pt",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + pt"
)
stats1 <- run_block(f1, "CEM WB No Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats1, cm_wb, "CEM_PPML_WB_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()


## BLOCK 2: WB Interaction
cat("\n=== CEM WB Interaction (fpd + pt FE) ===\n")
f2 <- c(
    "export  ~ WB_EP_Depth * env_good | fpd + pt",
    "exp_qua ~ WB_EP_Depth * env_good | fpd + pt",
    "uv_exp  ~ WB_EP_Depth * env_good | fpd + pt",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats2 <- run_block(f2, "CEM WB Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats2, cm_wb_int, "CEM_PPML_WB_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()


## BLOCK 3: TREND No Interaction
cat("\n=== CEM TREND No Interaction (fpd + pt FE) ===\n")
f3 <- c(
    "export  ~ TREND_EP_Count | fpd + pt",
    "exp_qua ~ TREND_EP_Count | fpd + pt",
    "uv_exp  ~ TREND_EP_Count | fpd + pt",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + pt"
)
stats3 <- run_block(f3, "CEM TREND No Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats3, cm_trend, "CEM_PPML_TREND_No_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()


## BLOCK 4: TREND Interaction
cat("\n=== CEM TREND Interaction (fpd + pt FE) ===\n")
f4 <- c(
    "export  ~ TREND_EP_Count * env_good | fpd + pt",
    "exp_qua ~ TREND_EP_Count * env_good | fpd + pt",
    "uv_exp  ~ TREND_EP_Count * env_good | fpd + pt",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + pt"
)
stats4 <- run_block(f4, "CEM TREND Interaction (fpd + pt FE)", "ppml", data_file, dirs$models, vcov = ~dt, requested_stats = show_stats_ppml)
make_table(stats4, cm_trend_int, "CEM_PPML_TREND_Interaction_fpd_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== DONE fpd + pt FE! ===\n")
cat("Tables in:", dirs$tables, "\n")
cat("Models in:", dirs$models, "\n")
cat("- 4 tables .tex\n- 24 PPML_*_fpt.rds\n")
cat("Time for fpd + pt:", now() - start, "seconds\n")
