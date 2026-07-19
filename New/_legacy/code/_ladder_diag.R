library(here); library(fst); library(fixest); library(data.table)
source(here("Code/Analysis/pta_functions.R"))
OUT_DIR <- here("New/Output/OLS")
dirs <- setup_output_dirs(OUT_DIR)

load_rds <- function(block, i) {
  p <- file.path(dirs$models, sprintf("OLS_%s_%d.rds", block, i))
  if (!file.exists(p)) { message("Not found: ", p); return(NULL) }
  readRDS(p)
}

fe_specs <- data.frame(
  label       = c("fpd+t", "fpt+pd", "fpt+fpd", "fpd+pt"),
  wb_block    = c("WB_NI_fpd_year", "WB_NI_fpt_pd", "WB_NI_fpt_fpd", "WB_NI_fpd_pt"),
  trend_block = c("TREND_NI_fpd_year","TREND_NI_fpt_pd","TREND_NI_fpt_fpd","TREND_NI_fpd_pt"),
  stringsAsFactors = FALSE
)

for (j in seq_len(nrow(fe_specs))) {
  wb_b <- load_rds(fe_specs$wb_block[j], 1)
  wb_c <- load_rds(fe_specs$wb_block[j], 4)
  tr_b <- load_rds(fe_specs$trend_block[j], 1)
  tr_c <- load_rds(fe_specs$trend_block[j], 4)
  if (!is.null(wb_b)) {
    cat(sprintf("%-10s | WB_base: %8.6f (p=%.4f)  WB_ctrl: %8.6f (p=%.4f)  TREND_base: %8.6f (p=%.4f)  TREND_ctrl: %8.6f (p=%.4f)\n",
      fe_specs$label[j],
      wb_b$coefs["WB_EP_Depth"],  wb_b$pval["WB_EP_Depth"],
      wb_c$coefs["WB_EP_Depth"],  wb_c$pval["WB_EP_Depth"],
      tr_b$coefs["TREND_EP_Count"], tr_b$pval["TREND_EP_Count"],
      tr_c$coefs["TREND_EP_Count"], tr_c$pval["TREND_EP_Count"]))
  }
}
