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
  label       = c("\\textit{fpd} + \\textit{t}",
                  "\\textit{fpt} + \\textit{pd}",
                  "\\textit{fpt} + \\textit{fpd}",
                  "\\textit{fpd} + \\textit{pt}"),
  wb_block    = c("WB_NI_fpd_year", "WB_NI_fpt_pd", "WB_NI_fpt_fpd", "WB_NI_fpd_pt"),
  trend_block = c("TREND_NI_fpd_year","TREND_NI_fpt_pd","TREND_NI_fpt_fpd","TREND_NI_fpd_pt"),
  stringsAsFactors = FALSE
)

ladder_rows <- lapply(seq_len(nrow(fe_specs)), function(j) {
  wb_b <- load_rds(fe_specs$wb_block[j], 1)
  wb_c <- load_rds(fe_specs$wb_block[j], 4)
  tr_b <- load_rds(fe_specs$trend_block[j], 1)
  tr_c <- load_rds(fe_specs$trend_block[j], 4)
  if (is.null(wb_b) || is.null(wb_c) || is.null(tr_b) || is.null(tr_c)) {
    cat(sprintf("WARNING: modelli mancanti per %s\n", fe_specs$label[j]))
    return(NULL)
  }
  list(label = fe_specs$label[j],
       wb_coef_b = wb_b$coefs["WB_EP_Depth"],   wb_se_b = wb_b$se["WB_EP_Depth"],   wb_p_b = wb_b$pval["WB_EP_Depth"],
       wb_coef_c = wb_c$coefs["WB_EP_Depth"],   wb_se_c = wb_c$se["WB_EP_Depth"],   wb_p_c = wb_c$pval["WB_EP_Depth"],
       tr_coef_b = tr_b$coefs["TREND_EP_Count"], tr_se_b = tr_b$se["TREND_EP_Count"], tr_p_b = tr_b$pval["TREND_EP_Count"],
       tr_coef_c = tr_c$coefs["TREND_EP_Count"], tr_se_c = tr_c$se["TREND_EP_Count"], tr_p_c = tr_c$pval["TREND_EP_Count"])
})
ladder_rows <- Filter(Negate(is.null), ladder_rows)
cat(sprintf("Righe ladder: %d\n", length(ladder_rows)))

ladder_tex <- c("{", "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
  "\\begin{tabular}{lcccc}", "\\toprule",
  " & \\multicolumn{2}{c}{\\textit{WB EP Depth}} & \\multicolumn{2}{c}{\\textit{TREND EP Count}} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Fixed Effects & (1) Baseline & (2) Controls & (3) Baseline & (4) Controls \\\\", "\\midrule")

for (r in ladder_rows) {
  ladder_tex <- c(ladder_tex,
    paste0(r$label,
           " & ", fmt_coef(r$wb_coef_b, r$wb_p_b, 5),
           " & ", fmt_coef(r$wb_coef_c, r$wb_p_c, 5),
           " & ", fmt_coef(r$tr_coef_b, r$tr_p_b, 5),
           " & ", fmt_coef(r$tr_coef_c, r$tr_p_c, 5), " \\\\"),
    paste0(" & ", fmt_se(r$wb_se_b, 5), " & ", fmt_se(r$wb_se_c, 5),
           " & ", fmt_se(r$tr_se_b, 5), " & ", fmt_se(r$tr_se_c, 5), " \\\\"),
    "\\addlinespace")
}
ladder_tex <- c(head(ladder_tex, -1), "\\midrule",
  "\\multicolumn{5}{l}{\\footnotesize \\textit{Notes}: SEs clustered at destination (\\texttt{country\\_code}). N varies across specs.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)} \\\\",
  "\\bottomrule", "\\end{tabular}", "}")

out_path <- file.path(dirs$tables, "OLS_Ladder_FE.tex")
writeLines(ladder_tex, out_path)
cat("[OK] OLS_Ladder_FE.tex\n")
