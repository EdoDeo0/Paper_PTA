#############################
###### PPML Estimation ######
#############################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## PPML Estimation without zeros fill-in (only positive export flows) using fepois from fixest package


# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────

# Clean environment
rm(list = ls())

# Load libraries
library(fst)
library(fixest)
library(data.table)
library(here)

# Set the number of threads for fst (adjust based on your CPU)
threads_fst(8)

# Set your own data file (not in the repo, file too big)
data_file <- "/Users/edoardovitella/Desktop/PPML Estimation/final_dataset_pta_env_indices_compressed.fst"
out_dir <- here("Output/Analysis/PPML")
tables_output_dir <- file.path(out_dir, "Tables")
models_output_dir <- file.path(out_dir, "Models_Output")


# ─────────────────────────────────────────────────────────────────────
# FUNCTIONS
# ─────────────────────────────────────────────────────────────────────

# Estimate a PPML model and extract statistics for table generation
ppml_estimate <- function(formula_str, cluster_var = ~pdt, save_path = NULL) {
    # Extract variables from the formula (eg. "y ~ x | fe1 + fe2")
    parts <- strsplit(formula_str, "\\|")[[1]]
    main_vars <- all.vars(as.formula(trimws(parts[1])))
    fe_vars <- if (length(parts) > 1) trimws(strsplit(parts[2], "\\+")[[1]]) else character()
    cluster_name <- all.vars(cluster_var)[1]  # eg. ~pdt -> "pdt"
    all_vars <- unique(c(main_vars, fe_vars, cluster_name))
    
    # Load only the necessary columns
    data <- as.data.table(read_fst(data_file, columns = all_vars))
    
    # Estimate the model
    model <- fepois(as.formula(formula_str), data = data, cluster = cluster_var, lean = TRUE)
    
    # Save single model if requested
    if (!is.null(save_path)) {
        saveRDS(model, save_path)
    }
    
    # Extract statistics for table generation
    stats <- list(
        coefs = coef(model),
        se = se(model),
        pval = pvalue(model),
        nobs = model$nobs,
        n_clust = tryCatch(fitstat(model, "g")[[1]], error = function(e) NA),
        fe_vars = fe_vars,
        cluster_label = cluster_name
    )
    
    # Clean memory
    rm(data, model); gc()
    return(stats)
}

# Estimate a whole block of models
run_block <- function(formulas, block_name, cluster_var = ~pdt) {
    cat("\n===", block_name, "===\n")
    stats_list <- lapply(seq_along(formulas), function(i) {
        cat(sprintf("  [%d/%d] %s\n", i, length(formulas), formulas[i]))
        # Salva ogni modello come RDS separato in Models_Output
        save_path <- file.path(models_output_dir, sprintf("PPML_%s_%d.rds", gsub(" ", "_", block_name), i))
        ppml_estimate(formulas[i], cluster_var, save_path)
    })
    return(stats_list)
}

# Generate LaTeX table from a list of model statistics
# Dependent variables, group headers, and column grouping can be customized via parameters
make_table <- function(stats_list, coefmap, filename, 
                       cluster_label = NULL,
                       fe_labels = NULL,  # NULL = auto-genera da formula
                       dep_vars = c("Exports", "Quantity", "UnitValue", "Exports", "Quantity", "UnitValue"),
                       group_headers = c("Baseline", "With controls"),
                       group_cols = c(3, 3)) {
    
    n <- length(stats_list)
    # Se cluster_label non specificato, usa quello salvato nelle stats
    if (is.null(cluster_label)) cluster_label <- stats_list[[1]]$cluster_label
    
    # Se fe_labels non specificato, genera automaticamente da fe_vars
    if (is.null(fe_labels)) {
        fe_vars <- stats_list[[1]]$fe_vars
        # Converti nomi FE in notazione LaTeX: "fpd" -> "$\theta_{fpd}$", "year" -> "$\theta_t$"
        fe_labels <- sapply(fe_vars, function(fe) {
            # Abbreviazioni comuni
            fe_short <- switch(fe,
                "year" = "t",
                "product" = "p",
                "destination" = "d",
                "origin" = "o",
                fe  # default: usa nome originale
            )
            paste0("$\\theta_{", fe_short, "}$")
        })
        names(fe_labels) <- fe_labels
        fe_labels[] <- "Yes"  # tutti i FE presenti -> Yes
    }
    
    # Formatta coefficienti con stelle
    fmt_coef <- function(val, pval) {
        if (is.na(val)) return("")
        stars <- if (!is.na(pval) && pval < 0.01) "\\sym{***}" else 
                 if (!is.na(pval) && pval < 0.05) "\\sym{**}" else 
                 if (!is.na(pval) && pval < 0.10) "\\sym{*}" else ""
        paste0(formatC(val, digits = 5, format = "f"), stars)
    }
    fmt_se <- function(val) if (is.na(val)) "" else paste0("(", formatC(val, digits = 5, format = "f"), ")")
    fmt_n <- function(x) if (is.na(x)) "." else format(x, big.mark = ",", scientific = FALSE)
    
    # Corpo tabella
    body <- character()
    for (var in names(coefmap)) {
        coef_row <- paste0(coefmap[var], " & ", paste(sapply(stats_list, function(s) 
            if (var %in% names(s$coefs)) fmt_coef(s$coefs[var], s$pval[var]) else ""), collapse = " & "), "\\\\")
        se_row <- paste0("  & ", paste(sapply(stats_list, function(s) 
            if (var %in% names(s$se)) fmt_se(s$se[var]) else ""), collapse = " & "), "\\\\")
        body <- c(body, coef_row, se_row, "\\addlinespace")
    }
    body <- c(head(body, -1), "\\midrule")  # Rimuovi ultimo \addlinespace
    
    # Statistiche
    body <- c(body,
        paste0("Observations & ", paste(sapply(stats_list, function(s) fmt_n(s$nobs)), collapse = " & "), "\\\\"),
        paste0("Clusters (", cluster_label, ") & ", paste(sapply(stats_list, function(s) fmt_n(s$n_clust)), collapse = " & "), "\\\\")
    )
    # Righe FE (auto o manuali)
    for (fe_name in names(fe_labels)) {
        body <- c(body, paste0(fe_name, " & ", paste(rep(fe_labels[fe_name], n), collapse = " & "), "\\\\"))
    }
    
    # Header (parametrizzato)
    # Calcola cmidrule dinamicamente
    col_starts <- c(2, cumsum(group_cols[-length(group_cols)]) + 2)
    col_ends <- cumsum(group_cols) + 1
    cmidrules <- paste0("\\cmidrule(lr){", col_starts, "-", col_ends, "}", collapse = "")
    group_row <- paste0(paste0("&\\multicolumn{", group_cols, "}{c}{", group_headers, "}", collapse = ""), "\\\\", cmidrules)
    dep_row <- paste0(paste0("&\\textit{", dep_vars, "\\textsubscript{fpdt}}", collapse = ""), "\\\\")
    
    header <- c(
        "{",
        "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
        paste0("\\begin{tabular}{l*{", n, "}{c}}"),
        "\\toprule",
        group_row,
        paste0(paste0("&(", 1:n, ")", collapse = ""), "\\\\"),
        dep_row,
        "\\midrule"
    )
    
    # Footer
    footer <- c(
        "\\bottomrule",
        paste0("\\multicolumn{", n+1, "}{l}{\\footnotesize \\textit{Notes}: Standard errors clustered at the (", cluster_label, ") level are reported in parentheses. \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"),
        "\\end{tabular}",
        "}"
    )
    
    writeLines(c(header, body, footer), file.path(tables_output_dir, filename))
    cat("[OK]", filename, "\n")
}

# ─────────────────────────────────────────────────────────────────────
# COEF MAPS
# ─────────────────────────────────────────────────────────────────────

cm_wb <- c(
    "WB_EP_Depth" = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
    "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
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

# Check if data file exists before running models
stopifnot("File dati non trovato!" = file.exists(data_file))

# BLOCK 1: WB No Interaction
f1 <- c("export ~ WB_EP_Depth | fpd + year",
        "exp_qua ~ WB_EP_Depth | fpd + year",
        "uv_exp ~ WB_EP_Depth | fpd + year",
        "export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
        "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
        "uv_exp ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year")
stats1 <- run_block(f1, "WB No Interaction", cluster_var = ~pdt)
make_table(stats1, cm_wb, "PPML_WB_No_Interaction.tex")

# BLOCK 2: WB Interaction
f2 <- c("export ~ WB_EP_Depth * env_good | fpd + year",
        "exp_qua ~ WB_EP_Depth * env_good | fpd + year",
        "uv_exp ~ WB_EP_Depth * env_good | fpd + year",
        "export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
        "exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
        "uv_exp ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year")
stats2 <- run_block(f2, "WB Interaction", cluster_var = ~pdt)
make_table(stats2, cm_wb_int, "PPML_WB_Interaction.tex")

# BLOCK 3: TREND No Interaction
f3 <- c("export ~ TREND_EP_Count | fpd + year",
        "exp_qua ~ TREND_EP_Count | fpd + year",
        "uv_exp ~ TREND_EP_Count | fpd + year",
        "export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
        "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
        "uv_exp ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year")
stats3 <- run_block(f3, "TREND No Interaction", cluster_var = ~pdt)
make_table(stats3, cm_trend, "PPML_TREND_No_Interaction.tex")

# BLOCK 4: TREND Interaction
f4 <- c("export ~ TREND_EP_Count * env_good | fpd + year",
        "exp_qua ~ TREND_EP_Count * env_good | fpd + year",
        "uv_exp ~ TREND_EP_Count * env_good | fpd + year",
        "export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
        "exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
        "uv_exp ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year")
stats4 <- run_block(f4, "TREND Interaction", cluster_var = ~pdt)
make_table(stats4, cm_trend_int, "PPML_TREND_Interaction.tex")

cat("\n=== COMPLETATO! ===\n")
cat("Tabelle in:", tables_output_dir, "\n")
cat("Modelli in:", models_output_dir, "\n")
cat("- 4 tabelle .tex\n")
cat("- 24 model_*.rds (modelli singoli per summary())\n")
