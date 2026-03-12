########################################################
######       PTA Paper – Shared Function Library  ######
########################################################

## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## Shared utility functions for model estimation and LaTeX table generation.
## Source this file at the top of each analysis script:
##
##   source(here("Code/Analysis/pta_functions.R"))
##
## Dependencies (must be loaded in the calling script):
##   fst, fixest, data.table

# ─────────────────────────────────────────────────────────────────────
# OUTPUT DIRECTORY SETUP
# ─────────────────────────────────────────────────────────────────────

#' Create output sub-directories (Tables + Models_Output) under a base directory.
#' Returns a named list with elements $tables and $models.
#'
#' @param base_dir  Base output directory (e.g. here("Output/Analysis/OLS"))
#' @return Named list: list(tables = ..., models = ...)
setup_output_dirs <- function(base_dir) {
    dirs <- list(
        tables = file.path(base_dir, "Tables"),
        models = file.path(base_dir, "Models_Output")
    )
    invisible(lapply(dirs, function(d) {
        if (!dir.exists(d)) dir.create(d, recursive = TRUE)
    }))
    dirs
}


# ─────────────────────────────────────────────────────────────────────
# DATA UTILITIES
# ─────────────────────────────────────────────────────────────────────

#' Parse a fixest-style formula string and return the vector of variable names
#' needed to load from disk (LHS + RHS main vars + FE vars + cluster var).
#'
#' @param formula_str  Formula as string, e.g. "y ~ x1 + x2 | fe1 + fe2"
#' @param vcov         Variance-covariance specification: a one-sided formula for
#'                     clustering (e.g. ~pdt), or a string for sandwich SEs
#'                     (e.g. "HC1", "HC2", "HC3", "hetero", "iid"). Default: "HC1".
#' @return Character vector of unique variable names
parse_formula_vars <- function(formula_str, vcov = "HC1") {
    parts <- strsplit(formula_str, "\\|")[[1]]
    main_vars <- all.vars(as.formula(trimws(parts[1])))
    fe_vars <- if (length(parts) > 1) {
        trimws(strsplit(trimws(parts[2]), "\\+")[[1]])
    } else {
        character()
    }
    # Only add cluster column when vcov is a formula (e.g. ~pdt)
    vcov_vars <- if (inherits(vcov, "formula")) all.vars(vcov) else character()
    unique(c(main_vars, fe_vars, vcov_vars))
}

#' Load only the columns needed for a given formula from an FST file.
#'
#' @param data_file   Path to the .fst file
#' @param formula_str Formula string (fixest style)
#' @param vcov        Variance-covariance specification (see parse_formula_vars). Default: "HC1".
#' @return data.table with only the required columns
load_formula_data <- function(data_file, formula_str, vcov = "HC1") {
    cols <- parse_formula_vars(formula_str, vcov)
    as.data.table(read_fst(data_file, columns = cols))
}


# ─────────────────────────────────────────────────────────────────────
# MODEL ESTIMATION
# ─────────────────────────────────────────────────────────────────────

#' Estimate a single model (OLS via feols, or PPML via fepois) and return
#' a compact list of statistics ready for table generation.
#'
#' @param formula_str  Formula string (fixest style), e.g. "y ~ x | fe1 + fe2"
#' @param estimator    "ols" (feols) or "ppml" (fepois)
#' @param data_file    Path to the .fst data file
#' @param vcov         Variance-covariance specification: a one-sided formula for
#'                     clustering (e.g. ~pdt), or a string for sandwich SEs
#'                     (e.g. "HC1", "HC2", "HC3", "hetero", "iid"). Default: "HC1".
#' @param lean         Passed to fixest::feols/fepois. If TRUE, large internal
#'                     objects are dropped to save memory, but many post-estimation
#'                     methods and fit statistics may become unavailable or unstable.
#'                     Default: TRUE.
#' @param save_path    If not NULL, saves output as .rds at this path.
#' @param save_mode    What to save in the .rds: "stats" (smallest), "bundle"
#'                     (list(model=..., stats=...)), or "model". Default: "stats".
#'                     Use "bundle" only when you need summary() from saved files.
#' @param requested_stats Character vector of stats to compute. Available keys:
#'                     "nobs", "n_clust", "r2", "ar2", "wr2", "f_stat".
#'                     Default: c("nobs", "n_clust", "r2").
#' @param n_clust_method Kept for backward compatibility; not used in the current
#'                     implementation. n_clust is computed via
#'                     fixest::fitstat(model, "g"), which returns the exact count
#'                     of clusters used by feols/fepois (equivalent to Stata's
#'                     e(N_clust1)).
#' @param extra_fitstats If TRUE, also attempts to compute additional fit statistics
#'                     (ar2, wr2, f_stat). Kept for backward compatibility.
#'                     Default: FALSE.
#' @return Named list: coefs, se, pval, nobs, n_clust, r2, ar2, wr2, f_stat,
#'         fe_vars, vcov_label
estimate_model <- function(formula_str,
                           estimator = c("ols", "ppml"),
                           data_file,
                           vcov = "HC1",
                           lean = TRUE,
                           save_path = NULL,
                           save_mode = c("stats", "bundle", "model"),
                           requested_stats = c("nobs", "n_clust", "r2"),
                           n_clust_method = "data",
                           n_clust_override = NULL,
                           preloaded_data = NULL,
                           extra_fitstats = FALSE) {
    estimator <- match.arg(estimator)
    save_mode <- match.arg(save_mode)

    model_vars <- parse_formula_vars(formula_str, vcov)
    if (is.null(preloaded_data)) {
        data <- load_formula_data(data_file, formula_str, vcov)
    } else {
        missing_vars <- setdiff(model_vars, names(preloaded_data))
        if (length(missing_vars) > 0) {
            stop("preloaded_data is missing variable(s): ", paste(missing_vars, collapse = ", "))
        }
        data <- preloaded_data[, ..model_vars]
    }

    model <- switch(estimator,
        ols = {
            if (inherits(vcov, "formula")) {
                feols(as.formula(formula_str), data = data, cluster = vcov, lean = lean)
            } else {
                feols(as.formula(formula_str), data = data, vcov = vcov, lean = lean)
            }
        },
        ppml = {
            if (inherits(vcov, "formula")) {
                fepois(as.formula(formula_str), data = data, cluster = vcov, lean = lean)
            } else {
                fepois(as.formula(formula_str), data = data, vcov = vcov, lean = lean)
            }
        }
    )

    parts <- strsplit(formula_str, "\\|")[[1]]
    fe_vars <- if (length(parts) > 1) {
        trimws(strsplit(trimws(parts[2]), "\\+")[[1]])
    } else {
        character()
    }

    # Label identifying the SE type: cluster variable name or vcov string
    vcov_label <- if (inherits(vcov, "formula")) all.vars(vcov)[1] else vcov

    stats <- list(
        coefs      = coef(model),
        se         = se(model),
        pval       = pvalue(model),
        nobs       = model$nobs,
        fe_vars    = fe_vars,
        vcov_label = vcov_label
    )

    valid_stats <- c("nobs", "n_clust", "r2", "ar2", "wr2", "f_stat")
    requested_stats <- unique(requested_stats)
    if (isTRUE(extra_fitstats)) {
        requested_stats <- unique(c(requested_stats, "ar2", "wr2", "f_stat"))
    }
    unknown_stats <- setdiff(requested_stats, valid_stats)
    if (length(unknown_stats) > 0) {
        warning("Unknown requested_stats key(s): ", paste(unknown_stats, collapse = ", "), " - ignored.")
    }
    requested_stats <- intersect(requested_stats, valid_stats)

    # Initialize optional stats as NA; compute only those explicitly requested.
    stats$n_clust <- NA_real_
    stats$r2 <- NA_real_
    stats$ar2 <- NA_real_
    stats$wr2 <- NA_real_
    stats$f_stat <- NA_real_

    if ("n_clust" %in% requested_stats) {
        stats$n_clust <- tryCatch(fitstat(model, "g")[[1]], error = function(e) NA_real_)
    }
    if ("r2" %in% requested_stats) {
        stats$r2 <- tryCatch(
            if (!is.null(model$sq.cor)) unname(model$sq.cor) else fitstat(model, "r2")[[1]],
            error = function(e) NA_real_
        )
    }
    if ("ar2" %in% requested_stats) {
        stats$ar2 <- tryCatch(fitstat(model, "ar2")[[1]], error = function(e) NA_real_)
    }
    if ("wr2" %in% requested_stats) {
        stats$wr2 <- tryCatch(fitstat(model, "wr2")[[1]], error = function(e) NA_real_)
    }
    if ("f_stat" %in% requested_stats) {
        stats$f_stat <- tryCatch(fitstat(model, "f.stat")[[1]], error = function(e) NA_real_)
    }

    if (!is.null(save_path)) {
        obj_to_save <- switch(save_mode,
            stats  = stats,
            bundle = list(model = model, stats = stats),
            model  = model
        )
        saveRDS(obj_to_save, save_path)
    }

    rm(data, model)
    # gc()
    return(stats)
}


#' Run a named block of formulas, print progress, and save each model/stats bundle as .rds.
#'
#' @param formulas    Character vector of formula strings
#' @param block_name  Label for the block (used in filenames and console output)
#' @param estimator   "ols" or "ppml"
#' @param data_file   Path to the .fst data file
#' @param models_dir  Directory where individual .rds bundles are saved
#' @param vcov        Variance-covariance specification (see estimate_model). Default: "HC1".
#' @param lean        Passed through to estimate_model(). Default: TRUE.
#' @param save_mode   Passed through to estimate_model(). Default: "stats".
#' @param requested_stats Passed through to estimate_model().
#'                      Default: c("nobs", "n_clust", "r2").
#' @param n_clust_method Passed through to estimate_model() (kept for
#'                      backward compatibility; currently unused).
#' @param preload_block_data If TRUE, load once all columns needed by formulas
#'                      in the block and reuse in-memory data for each model.
#'                      This reduces repeated allocations and can improve
#'                      stability on very large runs. Default: FALSE.
#' @param extra_fitstats Passed through to estimate_model(). Default: FALSE.
#' @param prefix      Filename prefix for .rds files (default: "OLS" or "PPML")
#' @return List of stats objects, one per formula
run_block <- function(formulas,
                      block_name,
                      estimator = c("ols", "ppml"),
                      data_file,
                      models_dir,
                      vcov = "HC1",
                      lean = TRUE,
                      save_mode = "stats",
                      requested_stats = c("nobs", "n_clust", "r2"),
                      n_clust_method = "data",
                      preload_block_data = FALSE,
                      extra_fitstats = FALSE,
                      prefix = NULL) {
    estimator <- match.arg(estimator)
    if (is.null(prefix)) prefix <- toupper(estimator)

    block_data <- NULL
    if (isTRUE(preload_block_data)) {
        block_vars <- unique(unlist(lapply(formulas, parse_formula_vars, vcov = vcov), use.names = FALSE))
        block_data <- as.data.table(read_fst(data_file, columns = block_vars))
    }

    cat("\n===", block_name, "===\n")
    out <- lapply(seq_along(formulas), function(i) {
        cat(sprintf("  [%d/%d] %s\n", i, length(formulas), formulas[[i]]))
        save_path <- file.path(
            models_dir,
            sprintf("%s_%s_%d.rds", prefix, gsub(" ", "_", block_name), i)
        )
        estimate_model(
            formulas[[i]], estimator, data_file, vcov, lean,
            save_path, save_mode, requested_stats, n_clust_method,
            n_clust_override = NULL,
            preloaded_data = block_data,
            extra_fitstats = extra_fitstats
        )
    })

    if (!is.null(block_data)) {
        rm(block_data)
    }
    out
}


# ─────────────────────────────────────────────────────────────────────
# LATEX TABLE HELPERS
# ─────────────────────────────────────────────────────────────────────

#' Convert an FE variable name to a LaTeX fixed-effect label.
#' Common shorthands: year -> t, product -> p, destination -> d, origin -> o.
#' Unknown names are used as-is inside the subscript.
#'
#' @param fe_name  Single FE variable name (character)
#' @return LaTeX string, e.g. "$\\theta_{t}$"
fe_to_latex <- function(fe_name) {
    fe_short <- switch(fe_name,
        "year"        = "t",
        "product"     = "p",
        "destination" = "d",
        "origin"      = "o",
        fe_name
    )
    paste0("$\\theta_{", fe_short, "}$")
}

#' Format a coefficient value with significance stars (LaTeX \sym notation).
#'
#' @param val    Numeric coefficient
#' @param pval   Corresponding p-value
#' @param digits Number of decimal places (default 3)
#' @return Character string, e.g. "0.123\\sym{**}"
fmt_coef <- function(val, pval, digits = 3) {
    if (is.na(val)) {
        return("")
    }
    stars <- if (!is.na(pval) && pval < 0.01) "\\sym{***}" else if (!is.na(pval) && pval < 0.05) "\\sym{**}" else if (!is.na(pval) && pval < 0.10) "\\sym{*}" else ""
    paste0(formatC(val, digits = digits, format = "f"), stars)
}

#' Format a standard error in parentheses.
#'
#' @param val    Numeric standard error
#' @param digits Number of decimal places (default 3)
#' @return Character string, e.g. "(0.023)"
fmt_se <- function(val, digits = 3) {
    if (is.na(val)) "" else paste0("(", formatC(val, digits = digits, format = "f"), ")")
}

#' Format an integer count with thousands separator.
#'
#' @param x  Numeric or integer
#' @return Character string, e.g. "1,234,567" or "." if NA
fmt_n <- function(x) {
    if (is.na(x)) "." else format(x, big.mark = ",", scientific = FALSE)
}


# ─────────────────────────────────────────────────────────────────────
# LATEX TABLE BUILDER
# ─────────────────────────────────────────────────────────────────────

#' Build and write a LaTeX regression table from a list of model-stats objects.
#'
#' @param stats_list    List of stats objects returned by estimate_model() / run_block()
#' @param coefmap       Named character vector: coefficient name -> LaTeX label
#' @param filename      Output filename (e.g. "OLS_WB_No_Interaction.tex")
#' @param tables_dir    Directory where the .tex file is saved
#' @param vcov_label    Label shown in SE-type rows (e.g. cluster variable name or "HC1").
#'                      NULL = taken automatically from the first stats object.
#' @param fe_labels     Named character vector: LaTeX FE label -> "Yes"/"No".
#'                      NULL = auto-generated from fe_vars in the first stats object.
#' @param dep_vars      Character vector of dependent-variable labels for column headers
#' @param dep_subscript LaTeX subscript appended to each dep_var label (default "fpdt")
#' @param group_headers Character vector of group header labels
#' @param group_cols    Integer vector: number of columns per group
#' @param show_stats    Character vector of statistics to include below coefficients.
#'                      Available keys: "nobs", "n_clust", "r2", "ar2", "wr2", "f_stat".
#'                      Default: c("nobs", "n_clust").
#' @param digits        Number of decimal places for coefficients, standard errors,
#'                      and numeric statistics such as R^2 (default 3).
make_table <- function(stats_list,
                       coefmap,
                       filename,
                       tables_dir,
                       vcov_label = NULL,
                       fe_labels = NULL,
                       dep_vars = c(
                           "Exports", "Quantity", "UnitValue",
                           "Exports", "Quantity", "UnitValue"
                       ),
                       dep_subscript = "fpdt",
                       group_headers = c("Baseline", "With controls"),
                       group_cols = c(3, 3),
                       show_stats = c("nobs", "n_clust"),
                       digits = 3) {
    n <- length(stats_list)

    if (is.null(vcov_label)) vcov_label <- stats_list[[1]]$vcov_label

    if (is.null(fe_labels)) {
        fe_vars <- stats_list[[1]]$fe_vars
        fe_labels <- setNames(
            rep("Yes", length(fe_vars)),
            sapply(fe_vars, fe_to_latex)
        )
    }

    # Label map for bottom statistics
    stat_label_map <- list(
        nobs    = "Observations",
        n_clust = paste0("Clusters (", vcov_label, ")"),
        r2      = "$R^{2}$",
        ar2     = "Adj.\\ $R^{2}$",
        wr2     = "Within $R^{2}$",
        f_stat  = "F-statistic"
    )

    # ── Coefficient rows ──────────────────────────────────────────────
    body <- character()
    for (var in names(coefmap)) {
        coef_row <- paste0(
            coefmap[var], " & ",
            paste(sapply(stats_list, function(s) {
                if (var %in% names(s$coefs)) fmt_coef(s$coefs[var], s$pval[var], digits) else ""
            }), collapse = " & "),
            "\\\\"
        )
        se_row <- paste0(
            " & ",
            paste(sapply(stats_list, function(s) {
                if (var %in% names(s$se)) fmt_se(s$se[var], digits) else ""
            }), collapse = " & "),
            "\\\\"
        )
        body <- c(body, coef_row, se_row, "\\addlinespace")
    }
    body <- c(head(body, -1), "\\midrule")

    # ── Summary statistics rows (driven by show_stats) ─────────────────
    for (stat_key in show_stats) {
        label <- stat_label_map[[stat_key]]
        if (is.null(label)) {
            warning("Unknown stat key '", stat_key, "' - skipped.")
            next
        }
        if (all(sapply(stats_list, function(s) {
            val <- s[[stat_key]]
            is.null(val) || all(is.na(val))
        }))) {
            warning(
                "Stat '", stat_key,
                "' is not available in stats_list. ",
                "Pass it in run_block(..., requested_stats = ...) to compute it."
            )
        }
        vals <- sapply(stats_list, function(s) {
            val <- s[[stat_key]]
            if (is.null(val) || is.na(val)) {
                return(".")
            }
            if (stat_key %in% c("nobs", "n_clust")) {
                fmt_n(val)
            } else {
                formatC(val, digits = digits, format = "f")
            }
        })
        body <- c(body, paste0(label, " & ", paste(vals, collapse = " & "), "\\\\"))
    }

    # ── Fixed-effect rows ─────────────────────────────────────────────
    for (fe_name in names(fe_labels)) {
        body <- c(body, paste0(
            fe_name, " & ",
            paste(rep(fe_labels[fe_name], n), collapse = " & "),
            "\\\\"
        ))
    }

    # ── Header ────────────────────────────────────────────────────────
    col_starts <- c(2, cumsum(group_cols[-length(group_cols)]) + 2)
    col_ends <- cumsum(group_cols) + 1
    cmidrules <- paste0("\\cmidrule(lr){", col_starts, "-", col_ends, "}", collapse = "")
    group_row <- paste0(
        paste0("&\\multicolumn{", group_cols, "}{c}{", group_headers, "}", collapse = ""),
        "\\\\", cmidrules
    )
    dep_row <- paste0(
        paste0(
            "&\\textit{", dep_vars,
            "\\textsubscript{", dep_subscript, "}}",
            collapse = ""
        ),
        "\\\\"
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

    # ── Footer (note adapts to clustered vs robust SEs) ───────────────
    se_note <- if (grepl("^HC|^hetero|^iid", vcov_label, ignore.case = TRUE)) {
        paste0("Heteroskedasticity-robust standard errors (", vcov_label, ")")
    } else {
        paste0("Standard errors clustered at the (", vcov_label, ") level")
    }
    footer <- c(
        "\\bottomrule",
        paste0(
            "\\multicolumn{", n + 1, "}{l}{\\footnotesize \\textit{Notes}: ",
            se_note, " are reported in parentheses. ",
            "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"
        ),
        "\\end{tabular}",
        "}"
    )

    if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
    writeLines(c(header, body, footer), file.path(tables_dir, filename))
    cat("[OK]", filename, "\n")
}
