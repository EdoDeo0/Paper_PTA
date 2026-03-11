#####################################
###### OLS / REGHDFE Estimation #####
#####################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
## OLS with HDFE (feols) - equivalent to Stata reghdfe

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

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir <- here("Output/Analysis/OLS")
tables_dir <- file.path(out_dir, "Tables")
models_dir <- file.path(out_dir, "Models_Output")

# Check if file exists before proceeding
stopifnot("File dati non trovato!" = file.exists(data_file))



# ─────────────────────────────────────────────────────────────────────
# FUNCTIONS
# ─────────────────────────────────────────────────────────────────────

# Produces OLS esitmates with HDFE and clustering, and returns a list of statistics for table construction
ols_estimate <- function(formula_str, cluster_var = ~pdt, save_path = NULL) {
    parts <- strsplit(formula_str, "\\|")[[1]]
    main_vars <- all.vars(as.formula(trimws(parts[1])))
    fe_vars <- if (length(parts) > 1) trimws(strsplit(parts[2], "\\+")[[1]]) else character()
    cluster_name <- all.vars(cluster_var)[1]
    all_vars <- unique(c(main_vars, fe_vars, cluster_name))

    data <- as.data.table(read_fst(data_file, columns = all_vars))
    model <- feols(as.formula(formula_str),
        data = data,
        cluster = cluster_var, lean = TRUE
    )

    if (!is.null(save_path)) saveRDS(model, save_path)

    stats <- list(
        coefs         = coef(model),
        se            = se(model),
        pval          = pvalue(model),
        nobs          = model$nobs,
        n_clust       = tryCatch(fitstat(model, "g")[[1]], error = function(e) NA),
        fe_vars       = fe_vars,
        cluster_label = cluster_name
    )

    rm(data, model)
    gc()
    return(stats)
}

# Runs a block of formulas, prints progress, and saves each model output as .rds for later use in tables
run_block <- function(formulas, block_name, cluster_var = ~pdt) {
    cat("\n===", block_name, "===\n")
    lapply(seq_along(formulas), function(i) {
        cat(sprintf(" [%d/%d] %s\n", i, length(formulas), formulas[i]))
        save_path <- file.path(
            models_dir,
            sprintf("OLS_%s_%d.rds", gsub(" ", "_", block_name), i)
        )
        ols_estimate(formulas[i], cluster_var, save_path)
    })
}


# Constructs a LaTeX table from a list of stats objects, using a mapping of variable names to LaTeX labels, and saves the .tex file
make_table <- function(stats_list, coefmap, filename,
                       cluster_label = NULL,
                       fe_labels = NULL,
                       dep_vars = c(
                           "Exports", "Quantity", "UnitValue",
                           "Exports", "Quantity", "UnitValue"
                       ),
                       group_headers = c("Baseline", "With controls"),
                       group_cols = c(3, 3)) {
    n <- length(stats_list)
    if (is.null(cluster_label)) cluster_label <- stats_list[[1]]$cluster_label

    if (is.null(fe_labels)) {
        fe_vars <- stats_list[[1]]$fe_vars
        fe_labels <- sapply(fe_vars, function(fe) {
            fe_short <- switch(fe,
                "year" = "t",
                "product" = "p",
                "destination" = "d",
                "origin" = "o",
                fe
            )
            paste0("$\\theta_{", fe_short, "}$")
        })
        names(fe_labels) <- fe_labels
        fe_labels[] <- "Yes"
    }

    fmt_coef <- function(val, pval) {
        if (is.na(val)) {
            return("")
        }
        stars <- if (!is.na(pval) && pval < 0.01) "\\sym{***}" else if (!is.na(pval) && pval < 0.05) "\\sym{**}" else if (!is.na(pval) && pval < 0.10) "\\sym{*}" else ""
        paste0(formatC(val, digits = 5, format = "f"), stars)
    }
    fmt_se <- function(val) {
        if (is.na(val)) {
            ""
        } else {
            paste0("(", formatC(val, digits = 5, format = "f"), ")")
        }
    }
    fmt_n <- function(x) {
        if (is.na(x)) {
            "."
        } else {
            format(x, big.mark = ",", scientific = FALSE)
        }
    }

    body <- character()
    for (var in names(coefmap)) {
        coef_row <- paste0(
            coefmap[var], " & ",
            paste(
                sapply(stats_list, function(s) {
                    if (var %in% names(s$coefs)) fmt_coef(s$coefs[var], s$pval[var]) else ""
                }),
                collapse = " & "
            ), "\\\\"
        )
        se_row <- paste0(
            " & ",
            paste(
                sapply(stats_list, function(s) {
                    if (var %in% names(s$se)) fmt_se(s$se[var]) else ""
                }),
                collapse = " & "
            ), "\\\\"
        )
        body <- c(body, coef_row, se_row, "\\addlinespace")
    }
    body <- c(head(body, -1), "\\midrule")

    body <- c(
        body,
        paste0(
            "Observations & ",
            paste(sapply(stats_list, function(s) fmt_n(s$nobs)), collapse = " & "), "\\\\"
        ),
        paste0(
            "Clusters (", cluster_label, ") & ",
            paste(sapply(stats_list, function(s) fmt_n(s$n_clust)), collapse = " & "), "\\\\"
        )
    )
    for (fe_name in names(fe_labels)) {
        body <- c(body, paste0(
            fe_name, " & ",
            paste(rep(fe_labels[fe_name], n), collapse = " & "), "\\\\"
        ))
    }

    col_starts <- c(2, cumsum(group_cols[-length(group_cols)]) + 2)
    col_ends <- cumsum(group_cols) + 1
    cmidrules <- paste0("\\cmidrule(lr){", col_starts, "-", col_ends, "}", collapse = "")
    group_row <- paste0(
        paste0("&\\multicolumn{", group_cols, "}{c}{", group_headers, "}", collapse = ""),
        "\\\\", cmidrules
    )
    dep_row <- paste0(
        paste0("&\\textit{", dep_vars, "\\textsubscript{fpdt}}", collapse = ""), "\\\\"
    )

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
    footer <- c(
        "\\bottomrule",
        paste0(
            "\\multicolumn{", n + 1, "}{l}{\\footnotesize \\textit{Notes}: ",
            "Standard errors clustered at the (", cluster_label,
            ") level are reported in parentheses. ",
            "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"
        ),
        "\\end{tabular}",
        "}"
    )

    writeLines(c(header, body, footer), file.path(tables_dir, filename))
    cat("[OK]", filename, "\n")
}


# ─────────────────────────────────────────────────────────────────────
# COEF MAPS
# ─────────────────────────────────────────────────────────────────────

cm_wb <- c(
    "WB_EP_Depth"           = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"               = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"           = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
    "WB_EP_Depth"           = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good1" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"               = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"           = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
    "TREND_EP_Count"              = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"                     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                 = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend_int <- c(
    "TREND_EP_Count"              = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good1"    = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"                 = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# RUN ALL BLOCKS
# ─────────────────────────────────────────────────────────────────────
start <- now()

# BLOCK 1: WB No Interaction
f1 <- c(
    "ln_export       ~ WB_EP_Depth | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth | fpd + year",
    "ln_export_value ~ WB_EP_Depth | fpd + year",
    "ln_export       ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction")
make_table(stats1, cm_wb, "OLS_WB_No_Interaction.tex")

# BLOCK 2: WB Interaction
f2 <- c(
    "ln_export       ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good | fpd + year",
    "ln_export       ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "WB Interaction")
make_table(stats2, cm_wb_int, "OLS_WB_Interaction.tex")

# BLOCK 3: TREND No Interaction
f3 <- c(
    "ln_export       ~ TREND_EP_Count | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count | fpd + year",
    "ln_export_value ~ TREND_EP_Count | fpd + year",
    "ln_export       ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats3 <- run_block(f3, "TREND No Interaction")
make_table(stats3, cm_trend, "OLS_TREND_No_Interaction.tex")

# BLOCK 4: TREND Interaction
f4 <- c(
    "ln_export       ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good | fpd + year",
    "ln_export       ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_qua   ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year",
    "ln_export_value ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year"
)
stats4 <- run_block(f4, "TREND Interaction")
make_table(stats4, cm_trend_int, "OLS_TREND_Interaction.tex")

cat("\n=== COMPLETATO! ===\n")
cat("Tabelle in:", tables_dir, "\n")
cat("Modelli in:", models_dir, "\n")
cat("- 4 tabelle .tex\n")
cat("- 24 OLS_*_*.rds\n")
cat("Tempo totale:", now() - start, "secondi\n")
