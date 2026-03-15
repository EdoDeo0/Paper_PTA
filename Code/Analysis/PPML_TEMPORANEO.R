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
# FIRM-PRODUCT-DESTINATION (fdp) + TIME (year) FIXED EFFECTS
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction
cat("\n=== WB No Interaction (fpd + year FE) ===\n")
f1 <- c(
    "export  ~ WB_EP_Depth | fpd + year",
    "exp_qua ~ WB_EP_Depth | fpd + year",
    "uv_exp  ~ WB_EP_Depth | fpd + year",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1, cm_wb, "PPML_WB_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1)
gc()

# BLOCK 2: WB Interaction
cat("\n=== WB Interaction (fpd + year FE) ===\n")
f2 <- c(
    "export  ~ WB_EP_Depth * env_good | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good | fpd + year",
    "uv_exp  ~ WB_EP_Depth * env_good | fpd + year",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "WB Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2, cm_wb_int, "PPML_WB_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2)
gc()

# BLOCK 3: TREND No Interaction
cat("\n=== TREND No Interaction (fpd + year FE) ===\n")
f3 <- c(
    "export  ~ TREND_EP_Count | fpd + year",
    "exp_qua ~ TREND_EP_Count | fpd + year",
    "uv_exp  ~ TREND_EP_Count | fpd + year",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "TREND No Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3, cm_trend, "PPML_TREND_No_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3)
gc()

# BLOCK 4: TREND Interaction
cat("\n=== TREND Interaction (fpd + year FE) ===\n")
f4 <- c(
    "export  ~ TREND_EP_Count * env_good | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good | fpd + year",
    "uv_exp  ~ TREND_EP_Count * env_good | fpd + year",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "TREND Interaction (fpd + year FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4, cm_trend_int, "PPML_TREND_Interaction_fpd_year.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4)
gc()

cat("\n=== COMPLETATO! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_*.rds\n")
cat("Tempo totale:", now() - start, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# FIRM-PRODUCT-TIME FIXED EFFECTS (fpt)
# ─────────────────────────────────────────────────────────────────────
start_fpt <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction - firm-product-time FE
cat("\n=== WB No Interaction (firm-product-time FE) ===\n")
f1_fpt <- c(
    "export  ~ WB_EP_Depth | fpt",
    "exp_qua ~ WB_EP_Depth | fpt",
    "uv_exp  ~ WB_EP_Depth | fpt",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt"
)
stats1_fpt <- run_block(f1_fpt, "WB No Interaction (firm-product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1_fpt, cm_wb, "PPML_WB_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_fpt)
gc()


# BLOCK 2: WB Interaction - firm-product-time FE
cat("\n=== WB Interaction (firm-product-time FE) ===\n")
f2_fpt <- c(
    "export  ~ WB_EP_Depth * env_good | fpt",
    "exp_qua ~ WB_EP_Depth * env_good | fpt",
    "uv_exp  ~ WB_EP_Depth * env_good | fpt",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt"
)
stats2_fpt <- run_block(f2_fpt, "WB Interaction (firm-product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2_fpt, cm_wb_int, "PPML_WB_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_fpt)
gc()


# BLOCK 3: TREND No Interaction - firm-product-time FE
cat("\n=== TREND No Interaction (firm-product-time FE) ===\n")
f3_fpt <- c(
    "export  ~ TREND_EP_Count | fpt",
    "exp_qua ~ TREND_EP_Count | fpt",
    "uv_exp  ~ TREND_EP_Count | fpt",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt"
)
stats3_fpt <- run_block(f3_fpt, "TREND No Interaction (firm-product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3_fpt, cm_trend, "PPML_TREND_No_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_fpt)
gc()

# BLOCK 4: TREND Interaction - firm-product-time FE
cat("\n=== TREND Interaction (firm-product-time FE) ===\n")
f4_fpt <- c(
    "export  ~ TREND_EP_Count * env_good | fpt",
    "exp_qua ~ TREND_EP_Count * env_good | fpt",
    "uv_exp  ~ TREND_EP_Count * env_good | fpt",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt"
)
stats4_fpt <- run_block(f4_fpt, "TREND Interaction (firm-product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4_fpt, cm_trend_int, "PPML_TREND_Interaction_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_fpt)
gc()


cat("\n=== COMPLETATO fpt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_fpt.rds\n")
cat("Tempo totale fpt:", now() - start_fpt, "secondi\n")



# ─────────────────────────────────────────────────────────────────────
# PRODUCT-TIME FIXED EFFECTS (pt)
# ─────────────────────────────────────────────────────────────────────
start_pt <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

# BLOCK 1: WB No Interaction - product-time FE
cat("\n=== WB No Interaction (product-time FE) ===\n")
f1_pt <- c(
    "export  ~ WB_EP_Depth | pt",
    "exp_qua ~ WB_EP_Depth | pt",
    "uv_exp  ~ WB_EP_Depth | pt",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | pt"
)
stats1_pt <- run_block(f1_pt, "WB No Interaction (product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1_pt, cm_wb, "PPML_WB_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_pt)
gc()


# BLOCK 2: WB Interaction - product-time FE
cat("\n=== WB Interaction (product-time FE) ===\n")
f2_pt <- c(
    "export  ~ WB_EP_Depth * env_good | pt",
    "exp_qua ~ WB_EP_Depth * env_good | pt",
    "uv_exp  ~ WB_EP_Depth * env_good | pt",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pt"
)
stats2_pt <- run_block(f2_pt, "WB Interaction (product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2_pt, cm_wb_int, "PPML_WB_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_pt)
gc()

# BLOCK 3: TREND No Interaction - product-time FE
cat("\n=== TREND No Interaction (product-time FE) ===\n")
f3_pt <- c(
    "export  ~ TREND_EP_Count | pt",
    "exp_qua ~ TREND_EP_Count | pt",
    "uv_exp  ~ TREND_EP_Count | pt",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | pt"
)
stats3_pt <- run_block(f3_pt, "TREND No Interaction (product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3_pt, cm_trend, "PPML_TREND_No_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_pt)
gc()


# BLOCK 4: TREND Interaction - product-time FE
cat("\n=== TREND Interaction (product-time FE) ===\n")
f4_pt <- c(
    "export  ~ TREND_EP_Count * env_good | pt",
    "exp_qua ~ TREND_EP_Count * env_good | pt",
    "uv_exp  ~ TREND_EP_Count * env_good | pt",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pt"
)
stats4_pt <- run_block(f4_pt, "TREND Interaction (product-time FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4_pt, cm_trend_int, "PPML_TREND_Interaction_pt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_pt)
gc()


cat("\n=== COMPLETATO pt! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_pt.rds\n")
cat("Tempo totale pt:", now() - start_pt, "secondi\n")



# # ─────────────────────────────────────────────────────────────────────
# # FIRM-PRODUCT-DESTINATION (fpd) + FIRM-PRODUCT-TIME (fpt) FIXED EFFECTS
# # ─────────────────────────────────────────────────────────────────────
# start_fpd_fpt <- now()
# show_stats_ppml <- c("nobs", "r2", "n_clust")

# # BLOCK 1: WB No Interaction - fpd + fpt FE
# cat("\n=== WB No Interaction (fpd + fpt FE) ===\n")
# f1_fpd_fpt <- c(
#     "export  ~ WB_EP_Depth | fpd + fpt",
#     "exp_qua ~ WB_EP_Depth | fpd + fpt",
#     "uv_exp  ~ WB_EP_Depth | fpd + fpt",
#     "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt",
#     "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt",
#     "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + fpt"
# )
# stats1_fpd_fpt <- run_block(f1_fpd_fpt, "WB No Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats1_fpd_fpt, cm_wb, "PPML_WB_No_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats1_fpd_fpt)
# gc()

# # BLOCK 2: WB Interaction - fpd + fpt FE
# cat("\n=== WB Interaction (fpd + fpt FE) ===\n")
# f2_fpd_fpt <- c(
#     "export  ~ WB_EP_Depth * env_good | fpd + fpt",
#     "exp_qua ~ WB_EP_Depth * env_good | fpd + fpt",
#     "uv_exp  ~ WB_EP_Depth * env_good | fpd + fpt",
#     "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt",
#     "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt",
#     "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + fpt"
# )
# stats2_fpd_fpt <- run_block(f2_fpd_fpt, "WB Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats2_fpd_fpt, cm_wb_int, "PPML_WB_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats2_fpd_fpt)
# gc()

# # BLOCK 3: TREND No Interaction - fpd + fpt FE
# cat("\n=== TREND No Interaction (fpd + fpt FE) ===\n")
# f3_fpd_fpt <- c(
#     "export  ~ TREND_EP_Count | fpd + fpt",
#     "exp_qua ~ TREND_EP_Count | fpd + fpt",
#     "uv_exp  ~ TREND_EP_Count | fpd + fpt",
#     "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt",
#     "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt",
#     "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + fpt"
# )
# stats3_fpd_fpt <- run_block(f3_fpd_fpt, "TREND No Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats3_fpd_fpt, cm_trend, "PPML_TREND_No_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats3_fpd_fpt)
# gc()

# # BLOCK 4: TREND Interaction - fpd + fpt FE
# cat("\n=== TREND Interaction (fpd + fpt FE) ===\n")
# f4_fpd_fpt <- c(
#     "export  ~ TREND_EP_Count * env_good | fpd + fpt",
#     "exp_qua ~ TREND_EP_Count * env_good | fpd + fpt",
#     "uv_exp  ~ TREND_EP_Count * env_good | fpd + fpt",
#     "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt",
#     "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt",
#     "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + fpt"
# )
# stats4_fpd_fpt <- run_block(f4_fpd_fpt, "TREND Interaction (fpd + fpt FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats4_fpd_fpt, cm_trend_int, "PPML_TREND_Interaction_fpd_fpt.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats4_fpd_fpt)
# gc()


# cat("\n=== COMPLETATO fpd + fpt! ===\n")
# cat("Tabelle in:", dirs$tables, "\n")
# cat("Modelli in:", dirs$models, "\n")
# cat("- 4 tabelle .tex\n- 24 PPML_*_fpd_fpt.rds\n")
# cat("Tempo totale fpd + fpt:", now() - start_fpd_fpt, "secondi\n")


# # ─────────────────────────────────────────────────────────────────────
# # FIRM-PRODUCT-TIME (fpt) + PRODUCT-TIME (pt) + PRODUCT-DESTINATION (pd) [Crowley et al 2021]
# # ─────────────────────────────────────────────────────────────────────
# start_fpt_pt_pd <- now()
# show_stats_ppml <- c("nobs", "r2", "n_clust")

# cat("\n=== Crowley et al 2021: fpt + pt + pd FE ===\n")
# f1_fpt_pt_pd <- c(
#     "export  ~ WB_EP_Depth | fpt + pt + pd",
#     "exp_qua ~ WB_EP_Depth | fpt + pt + pd",
#     "uv_exp  ~ WB_EP_Depth | fpt + pt + pd",
#     "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpt + pt + pd"
# )
# stats1_fpt_pt_pd <- run_block(f1_fpt_pt_pd, "Crowley et al 2021: WB No Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats1_fpt_pt_pd, cm_wb, "PPML_WB_No_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats1_fpt_pt_pd)
# gc()


# f2_fpt_pt_pd <- c(
#     "export  ~ WB_EP_Depth * env_good | fpt + pt + pd",
#     "exp_qua ~ WB_EP_Depth * env_good | fpt + pt + pd",
#     "uv_exp  ~ WB_EP_Depth * env_good | fpt + pt + pd",
#     "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpt + pt + pd"
# )
# stats2_fpt_pt_pd <- run_block(f2_fpt_pt_pd, "Crowley et al 2021: WB Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats2_fpt_pt_pd, cm_wb_int, "PPML_WB_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats2_fpt_pt_pd)
# gc()


# f3_fpt_pt_pd <- c(
#     "export  ~ TREND_EP_Count | fpt + pt + pd",
#     "exp_qua ~ TREND_EP_Count | fpt + pt + pd",
#     "uv_exp  ~ TREND_EP_Count | fpt + pt + pd",
#     "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpt + pt + pd"
# )
# stats3_fpt_pt_pd <- run_block(f3_fpt_pt_pd, "Crowley et al 2021: TREND No Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats3_fpt_pt_pd, cm_trend, "PPML_TREND_No_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats3_fpt_pt_pd)
# gc()


# f4_fpt_pt_pd <- c(
#     "export  ~ TREND_EP_Count * env_good | fpt + pt + pd",
#     "exp_qua ~ TREND_EP_Count * env_good | fpt + pt + pd",
#     "uv_exp  ~ TREND_EP_Count * env_good | fpt + pt + pd",
#     "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd",
#     "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpt + pt + pd"
# )
# stats4_fpt_pt_pd <- run_block(f4_fpt_pt_pd, "Crowley et al 2021: TREND Interaction (fpt + pt + pd FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
# make_table(stats4_fpt_pt_pd, cm_trend_int, "PPML_TREND_Interaction_fpt_pt_pd.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
# rm(stats4_fpt_pt_pd)
# gc()

# cat("\n=== COMPLETATO fpt + pt + pd! ===\n")
# cat("Tabelle in:", dirs$tables, "\n")
# cat("Modelli in:", dirs$models, "\n")
# cat("- 4 tabelle .tex\n- 24 PPML_*_fpt_pt_pd.rds\n")
# cat("Tempo totale fpt + pt + pd:", now() - start_fpt_pt_pd, "secondi\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-TIME (ft) + TIME (year) + DESTINATION (country_code) [Neri-Leinè et al 2023]
# ─────────────────────────────────────────────────────────────────────
start_ft_year_dest <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

cat("\n=== Neri-Leinè et al 2023: ft + year + destination FE ===\n")
f1_ft_year_dest <- c(
    "export  ~ WB_EP_Depth | ft + year + destination",
    "exp_qua ~ WB_EP_Depth | ft + year + destination",
    "uv_exp  ~ WB_EP_Depth | ft + year + destination",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + destination",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + destination",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + year + destination"
)
stats1_ft_year_dest <- run_block(f1_ft_year_dest, "Neri-Leinè et al 2023: WB No Interaction (ft + year + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1_ft_year_dest, cm_wb, "PPML_WB_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_ft_year_dest)
gc()


f2_ft_year_dest <- c(
    "export  ~ WB_EP_Depth * env_good | ft + year + destination",
    "exp_qua ~ WB_EP_Depth * env_good | ft + year + destination",
    "uv_exp  ~ WB_EP_Depth * env_good | ft + year + destination",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + destination",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + destination",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + year + destination"
)
stats2_ft_year_dest <- run_block(f2_ft_year_dest, "Neri-Leinè et al 2023: WB Interaction (ft + year + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2_ft_year_dest, cm_wb_int, "PPML_WB_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_ft_year_dest)
gc()


f3_ft_year_dest <- c(
    "export  ~ TREND_EP_Count | ft + year + destination",
    "exp_qua ~ TREND_EP_Count | ft + year + destination",
    "uv_exp  ~ TREND_EP_Count | ft + year + destination",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + destination",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + destination",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + year + destination"
)
stats3_ft_year_dest <- run_block(f3_ft_year_dest, "Neri-Leinè et al 2023: TREND No Interaction (ft + year + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3_ft_year_dest, cm_trend, "PPML_TREND_No_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_ft_year_dest)
gc()


f4_ft_year_dest <- c(
    "export  ~ TREND_EP_Count * env_good | ft + year + destination",
    "exp_qua ~ TREND_EP_Count * env_good | ft + year + destination",
    "uv_exp  ~ TREND_EP_Count * env_good | ft + year + destination",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + destination",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + destination",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + year + destination"
)
stats4_ft_year_dest <- run_block(f4_ft_year_dest, "Neri-Leinè et al 2023: TREND Interaction (ft + year + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4_ft_year_dest, cm_trend_int, "PPML_TREND_Interaction_ft_year_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_ft_year_dest)
gc()

cat("\n=== COMPLETATO ft + year + destination! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_ft_year_dest.rds\n")
cat("Tempo totale ft + year + destination:", now() - start_ft_year_dest, "secondi\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-TIME (ft) + PRODUCT (hs6) [Neri-Leinè et al 2021]
# ─────────────────────────────────────────────────────────────────────
start_ft_product <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

cat("\n=== Neri-Leinè et al 2021: ft + product FE ===\n")
f1_ft_product <- c(
    "export  ~ WB_EP_Depth | ft + product",
    "exp_qua ~ WB_EP_Depth | ft + product",
    "uv_exp  ~ WB_EP_Depth | ft + product",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + product",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + product",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + product"
)
stats1_ft_product <- run_block(f1_ft_product, "Neri-Leinè et al 2021: WB No Interaction (ft + product FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1_ft_product, cm_wb, "PPML_WB_No_Interaction_ft_product.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_ft_product)
gc()

f2_ft_product <- c(
    "export  ~ WB_EP_Depth * env_good | ft + product",
    "exp_qua ~ WB_EP_Depth * env_good | ft + product",
    "uv_exp  ~ WB_EP_Depth * env_good | ft + product",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + product",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + product",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + product"
)
stats2_ft_product <- run_block(f2_ft_product, "Neri-Leinè et al 2021: WB Interaction (ft + product FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2_ft_product, cm_wb_int, "PPML_WB_Interaction_ft_product.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_ft_product)
gc()


f3_ft_product <- c(
    "export  ~ TREND_EP_Count | ft + product",
    "exp_qua ~ TREND_EP_Count | ft + product",
    "uv_exp  ~ TREND_EP_Count | ft + product",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + product",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + product",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + product"
)
stats3_ft_product <- run_block(f3_ft_product, "Neri-Leinè et al 2021: TREND No Interaction (ft + product FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3_ft_product, cm_trend, "PPML_TREND_No_Interaction_ft_product.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_ft_product)
gc()

f4_ft_product <- c(
    "export  ~ TREND_EP_Count * env_good | ft + product",
    "exp_qua ~ TREND_EP_Count * env_good | ft + product",
    "uv_exp  ~ TREND_EP_Count * env_good | ft + product",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + product",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + product",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + product"
)
stats4_ft_product <- run_block(f4_ft_product, "Neri-Leinè et al 2021: TREND Interaction (ft + product FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4_ft_product, cm_trend_int, "PPML_TREND_Interaction_ft_product.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_ft_product)
gc()

cat("\n=== COMPLETATO ft + product! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_ft_product.rds\n")
cat("Tempo totale ft + product:", now() - start_ft_product, "secondi\n")


# ─────────────────────────────────────────────────────────────────────
# FIRM-TIME (ft) + DESTINATION (country_code) [Lee et al 2021]
# ─────────────────────────────────────────────────────────────────────
start_ft_dest <- now()
show_stats_ppml <- c("nobs", "r2", "n_clust")

cat("\n=== Lee et al 2021: ft + destination FE ===\n")
f1_ft_dest <- c(
    "export  ~ WB_EP_Depth | ft + destination",
    "exp_qua ~ WB_EP_Depth | ft + destination",
    "uv_exp  ~ WB_EP_Depth | ft + destination",
    "export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + destination",
    "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + destination",
    "uv_exp  ~ WB_EP_Depth + tariffs + ln_hhi_baci | ft + destination"
)
stats1_ft_dest <- run_block(f1_ft_dest, "Lee et al 2021: WB No Interaction (ft + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1_ft_dest, cm_wb, "PPML_WB_No_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats1_ft_dest)
gc()


f2_ft_dest <- c(
    "export  ~ WB_EP_Depth * env_good | ft + destination",
    "exp_qua ~ WB_EP_Depth * env_good | ft + destination",
    "uv_exp  ~ WB_EP_Depth * env_good | ft + destination",
    "export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + destination",
    "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + destination",
    "uv_exp  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | ft + destination"
)
stats2_ft_dest <- run_block(f2_ft_dest, "Lee et al 2021: WB Interaction (ft + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2_ft_dest, cm_wb_int, "PPML_WB_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats2_ft_dest)
gc()


f3_ft_dest <- c(
    "export  ~ TREND_EP_Count | ft + destination",
    "exp_qua ~ TREND_EP_Count | ft + destination",
    "uv_exp  ~ TREND_EP_Count | ft + destination",
    "export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + destination",
    "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + destination",
    "uv_exp  ~ TREND_EP_Count + tariffs + ln_hhi_baci | ft + destination"
)
stats3_ft_dest <- run_block(f3_ft_dest, "Lee et al 2021: TREND No Interaction (ft + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats3_ft_dest, cm_trend, "PPML_TREND_No_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats3_ft_dest)
gc()


f4_ft_dest <- c(
    "export  ~ TREND_EP_Count * env_good | ft + destination",
    "exp_qua ~ TREND_EP_Count * env_good | ft + destination",
    "uv_exp  ~ TREND_EP_Count * env_good | ft + destination",
    "export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + destination",
    "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + destination",
    "uv_exp  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | ft + destination"
)
stats4_ft_dest <- run_block(f4_ft_dest, "Lee et al 2021: TREND Interaction (ft + destination FE)", "ppml", data_file, dirs$models, vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats4_ft_dest, cm_trend_int, "PPML_TREND_Interaction_ft_dest.tex", dirs$tables, digits = 5, show_stats = show_stats_ppml)
rm(stats4_ft_dest)
gc()


cat("\n=== COMPLETATO ft + destination! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_*_ft_dest.rds\n")
cat("Tempo totale ft + destination:", now() - start_ft_dest, "secondi\n")
