## Pseudo Poisson Maximum Likelihood — v2
## Approccio a blocchi: stima 6 modelli -> esporta tabella -> libera RAM
## No lean = TRUE, così modelsummary funziona correttamente
## Output .tex Stata-style compatibile con \input{} in Overleaf

# ═══════════════════════════════════════════════
# SETUP
# ═══════════════════════════════════════════════

rm(list = ls())

library(fst)
library(fixest)
library(dplyr)
library(modelsummary)
library(data.table)
library(kableExtra)

# Caricamento selettivo per risparmiare RAM (solo variabili usate)
setwd("C:\\Users\\edodr\\Desktop\\PPML Estimation")  # On Windows
setwd("/Users/edoardovitella/Desktop/PPML Estimation")  # On Mac

vars_needed <- c(
    "export", "exp_qua", "uv_exp", "WB_EP_Depth", "TREND_EP_Count",
    "env_good", "tariffs", "ln_hhi_baci", "fpd", "year", "pdt"
)
# Caricamento dataset // NOT in this folder, file too big !!
data <- read_fst("final_dataset_pta_env_indices_compressed.fst", columns = vars_needed)

# Convert in data.table for faster processing
data <- as.data.table(data)

# ═══════════════════════════════════════════════
# DEFINIZIONE FUNZIONE EXPORT + COEFMAP
# ═══════════════════════════════════════════════
#
# Produce file .tex Stata-style richiamabili in Overleaf con:
#   \begin{table}[htbp]
#     \centering
#     \caption{...}
#     \resizebox{\textwidth}{!}{\input{Tabelle/nome_file}}
#   \end{table}

out_dir <- "C:/Work/Paper_PTA/Output/Analysis"

options(modelsummary_format_numeric_latex = "plain")
options(modelsummary_factory_latex = "kableExtra")
stars_note <- c("*" = 0.10, "**" = 0.05, "***" = 0.01)

format_nobs <- function(x) {
    if (is.numeric(x)) {
        return(format(x, big.mark = ",", scientific = FALSE))
    }
    return(x)
}

gofmap <- list(
    list(raw = "nobs", clean = "Observations", fmt = format_nobs)
)

# ----- Coef maps -----
coefmap_wb_noint <- c(
    "WB_EP_Depth" = "\\textit{EPDepth\\textsubscript{dt}}",
    "tariffs"     = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci" = "\\textit{ln HHI\\textsubscript{pdt}}"
)

coefmap_wb_int <- c(
    "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
    "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)

coefmap_trend_noint <- c(
    "TREND_EP_Count" = "\\textit{TREND Depth\\textsubscript{dt}}",
    "tariffs"        = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"    = "\\textit{ln HHI\\textsubscript{pdt}}"
)

coefmap_trend_int <- c(
    "TREND_EP_Count"          = "\\textit{TREND Depth\\textsubscript{dt}}",
    "TREND_EP_Count:env_good" = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
    "tariffs"                 = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
    "ln_hhi_baci"             = "\\textit{ln HHI\\textsubscript{pdt}}"
)


# ─────────────────────────────────────────────────────────────────────
# export_stata_style()
#
# Usa modelsummary per generare LaTeX, poi riformatta l'output per
# produrre un .tex identico a esttab di Stata:
#   { \def\sym ... \begin{tabular} ... \end{tabular} }
#
# Senza \begin{table}, senza \begingroup — pronto per \input{}
# ─────────────────────────────────────────────────────────────────────


export_stata_style <- function(models, filename, coefmap) {
    n_models <- length(models)
    n_cols <- n_models + 1
    model_names <- paste0("(", seq_len(n_models), ")")
    names(models) <- model_names

    # ── Numero cluster per modello usando fitstat ("g") ──
    n_clust_vec <- sapply(models, function(m) {
        g <- tryCatch(fitstat(m, "g")[[1]], error = function(e) NA)
        if (is.na(g)) {
            return(".")
        }
        return(g)
    })

    n_clust_cells <- sapply(n_clust_vec, function(x) {
        if (is.na(x) || is.null(x) || x == ".") "." else format(x, big.mark = ",", scientific = FALSE)
    })

    # ── Extra rows (clusters + FE) ──
    extra_rows <- data.frame(
        term = c("Clusters (pdt)", "$\\theta_{fpd}$", "$\\theta_t$"),
        stringsAsFactors = FALSE
    )
    for (j in seq_len(n_models)) {
        extra_rows[[model_names[j]]] <- c(n_clust_cells[j], "Yes", "Yes")
    }

    # ── Genera LaTeX grezzo via modelsummary ──
    tmp <- tempfile(fileext = ".tex")
    modelsummary(
        models,
        stars    = stars_note,
        output   = tmp,
        coef_map = coefmap,
        gof_map  = gofmap,
        gof_omit = "FE|RMSE|AIC|BIC|R2|IC|Log|Adj|Within|Pseudo|Std",
        add_rows = extra_rows,
        escape   = FALSE,
        fmt      = 5
    )
    raw <- readLines(tmp)
    unlink(tmp)

    # ── Estrai il corpo (tra primo \midrule e \bottomrule) ──
    midrule_pos <- grep("\\midrule", raw, fixed = TRUE)
    bottomrule_pos <- grep("\\bottomrule", raw, fixed = TRUE)

    if (length(midrule_pos) == 0 || length(bottomrule_pos) == 0) {
        # Fallback: prova con \hline
        hline_pos <- grep("\\hline", raw, fixed = TRUE)
        if (length(hline_pos) >= 2) {
            midrule_pos <- hline_pos[1]
            bottomrule_pos <- hline_pos[length(hline_pos)]
            body <- raw[(midrule_pos + 1):(bottomrule_pos - 1)]
        } else {
            cat("DEBUG — modelsummary raw output:\n")
            cat(raw, sep = "\n")
            stop("Nessun delimitatore (\\midrule / \\bottomrule / \\hline) trovato nell'output di modelsummary")
        }
    } else {
        body <- raw[(midrule_pos[1] + 1):(bottomrule_pos[1] - 1)]
    }

    # Rimuovi righe FE auto di fixest (checkmark), righe vuote
    body <- body[!grepl("fixed effects|checkmark", body, ignore.case = TRUE)]
    body <- body[trimws(body) != "\\\\"]
    body <- body[nchar(trimws(body)) > 0]

    # Converti stelle: $^{***}$ -> \sym{***}
    body <- gsub("\\$\\^\\{(\\*{1,3})\\}\\$", "\\\\sym{\\1}", body)

    # ── Inserisci \addlinespace e \midrule ──
    processed <- character()
    for (i in seq_along(body)) {
        line <- body[i]

        # \midrule prima di "Observations"
        if (grepl("^\\s*Observations", line)) {
            processed <- c(processed, "\\midrule", line)
            next
        }

        processed <- c(processed, line)

        # Dopo righe SE (\s+&...), aggiungi \addlinespace
        # tranne prima di Observations / Clusters / theta
        is_se_row <- grepl("^\\s+&", line)
        if (is_se_row) {
            next_real <- ""
            if (i < length(body)) {
                for (j in (i + 1):length(body)) {
                    if (nchar(trimws(body[j])) > 0) {
                        next_real <- body[j]
                        break
                    }
                }
            }
            if (!grepl("Observations|Clusters|\\$\\\\theta", next_real)) {
                processed <- c(processed, "\\addlinespace")
            }
        }
    }

    # ── Assembla output finale Stata-style ──
    output <- c(
        "{",
        "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}",
        paste0("\\begin{tabular}{l*{", n_models, "}{c}}"),
        "\\toprule",
        # Spanning header
        paste0(
            "                &\\multicolumn{3}{c}{Baseline}",
            "                            &\\multicolumn{3}{c}{With controls}",
            "                       \\\\\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}"
        ),
        # Numeri colonna (1)...(6)
        paste0(
            "                ",
            paste0("&\\multicolumn{1}{c}{(", seq_len(n_models), ")}", collapse = ""),
            "\\\\"
        ),
        # Etichette variabili dipendenti
        paste0(
            "                ",
            "&\\multicolumn{1}{c}{\\textit{Exports\\textsubscript{fpdt}}}",
            "&\\multicolumn{1}{c}{\\textit{Quantity\\textsubscript{fpdt}}}",
            "&\\multicolumn{1}{c}{\\textit{UnitValue\\textsubscript{fpdt}}}",
            "&\\multicolumn{1}{c}{\\textit{Exports\\textsubscript{fpdt}}}",
            "&\\multicolumn{1}{c}{\\textit{Quantity\\textsubscript{fpdt}}}",
            "&\\multicolumn{1}{c}{\\textit{UnitValue\\textsubscript{fpdt}}}\\\\"
        ),
        "\\midrule",
        # Corpo (coefficienti + GOF + extra rows)
        processed,
        "\\bottomrule",
        # Note
        paste0(
            "\\multicolumn{", n_cols,
            "}{l}{\\footnotesize \\textit{Notes}: ",
            "Standard errors clustered at the (pdt) level are reported in parentheses. ",
            "\\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\"
        ),
        "\\end{tabular}",
        "}"
    )

    writeLines(output, file.path(out_dir, filename))
    cat("Salvata:", filename, "\n")
}


# ═══════════════════════════════════════════════
# REGRESSIONI + EXPORT A BLOCCHI
# Strategia: stima 6 modelli -> esporta tabella -> rm() + gc()
# ═══════════════════════════════════════════════

### ── BLOCCO 1: WB No Interaction (m1-m6) ──
cat("\n=== BLOCCO 1: WB No Interaction ===\n")
models <- list()
models[[1]] <- fepois(export ~ WB_EP_Depth | fpd + year, data = data, cluster = ~pdt)
models[[2]] <- fepois(exp_qua ~ WB_EP_Depth | fpd + year, data = data, cluster = ~pdt)
models[[3]] <- fepois(uv_exp ~ WB_EP_Depth | fpd + year, data = data, cluster = ~pdt)
models[[4]] <- fepois(export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[5]] <- fepois(exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[6]] <- fepois(uv_exp ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)

export_stata_style(models, "PPML_WB_NoInteraction.tex", coefmap_wb_noint)
rm(models)
gc()


### ── BLOCCO 2: WB With Interaction (m7-m12) ──
cat("\n=== BLOCCO 2: WB Interaction ===\n")
models <- list()
models[[1]] <- fepois(export ~ WB_EP_Depth * env_good | fpd + year, data = data, cluster = ~pdt)
models[[2]] <- fepois(exp_qua ~ WB_EP_Depth * env_good | fpd + year, data = data, cluster = ~pdt)
models[[3]] <- fepois(uv_exp ~ WB_EP_Depth * env_good | fpd + year, data = data, cluster = ~pdt)
models[[4]] <- fepois(export ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[5]] <- fepois(exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[6]] <- fepois(uv_exp ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)

export_stata_style(models, "PPML_WB_Interaction.tex", coefmap_wb_int)
rm(models)
gc()


### ── BLOCCO 3: TREND No Interaction (m1T-m6T) ──
cat("\n=== BLOCCO 3: TREND No Interaction ===\n")
models <- list()
models[[1]] <- fepois(export ~ TREND_EP_Count | fpd + year, data = data, cluster = ~pdt)
models[[2]] <- fepois(exp_qua ~ TREND_EP_Count | fpd + year, data = data, cluster = ~pdt)
models[[3]] <- fepois(uv_exp ~ TREND_EP_Count | fpd + year, data = data, cluster = ~pdt)
models[[4]] <- fepois(export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[5]] <- fepois(exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[6]] <- fepois(uv_exp ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)

export_stata_style(models, "PPML_TREND_NoInteraction.tex", coefmap_trend_noint)
rm(models)
gc()


### ── BLOCCO 4: TREND With Interaction (m7T-m12T) ──
cat("\n=== BLOCCO 4: TREND Interaction ===\n")
models <- list()
models[[1]] <- fepois(export ~ TREND_EP_Count * env_good | fpd + year, data = data, cluster = ~pdt)
models[[2]] <- fepois(exp_qua ~ TREND_EP_Count * env_good | fpd + year, data = data, cluster = ~pdt)
models[[3]] <- fepois(uv_exp ~ TREND_EP_Count * env_good | fpd + year, data = data, cluster = ~pdt)
models[[4]] <- fepois(export ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[5]] <- fepois(exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)
models[[6]] <- fepois(uv_exp ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | fpd + year, data = data, cluster = ~pdt)

export_stata_style(models, "PPML_TREND_Interaction.tex", coefmap_trend_int)
rm(models)
gc()


cat("\n=== Tutte le 4 tabelle esportate in", out_dir, "===\n")
