#####################################################################
## PPML Firm-Level con Zero Fill-In — Solo Green Goods
## Versione CORRETTA: imputation covariate per lookup dimensionale
#####################################################################
##
## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## ─── LOGICA FILL-IN ───────────────────────────────────────────────
##
##  Il panel originale ha 4 dimensioni: f (impresa), p (prodotto),
##  d (destinazione), t (anno). Il fill-in NON collassa su f.
##
##  Per ogni tripla (f, p, d) con almeno un anno di export positivo,
##  creiamo una riga per OGNI anno del campione, imputando export = 0
##  dove mancante (sampling zeros).
##
##  Le covariate nelle righe create vengono recuperate tramite
##  lookup sulle loro dimensioni di variazione effettiva:
##
##    Covariata        | Dim. variazione | Lookup key
##    ─────────────────|─────────────────|────────────────
##    WB_EP_Depth      | (d, t)          | (d, year)
##    TREND_EP_Count   | (d, t)          | (d, year)
##    tariffs          | (p, d, t)       | (p, d, year)
##    ln_hhi_baci      | (p, d, t)       | (p, d, year)
##    env_good         | (p)             | assegna 1L direttamente
##
##  NON si calcola nessuna media: il valore di ogni covariata è
##  unico nelle sue dimensioni di variazione, quindi basta prendere
##  il primo valore non-NA tramite unique() + merge.
##
#####################################################################

rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

source(here("Code/Analysis/pta_functions.R"))

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("Output/Analysis/PPML_ZeroFill_GreenGoods")
dirs      <- setup_output_dirs(out_dir)

stopifnot("File dati non trovato!" = file.exists(data_file))

# ─────────────────────────────────────────────────────────────────────
# 1. CARICAMENTO DATI
# ─────────────────────────────────────────────────────────────────────
dt <- read_fst(data_file, as.data.table = TRUE)
setkey(dt, f, p, d, year)

# ─────────────────────────────────────────────────────────────────────
# 2. COSTRUZIONE TAVOLE LOOKUP PER COVARIATE
#    IMPORTANTE: costruite sul dataset COMPLETO (non solo green goods)
#    per garantire la massima copertura dei valori disponibili.
#    Le lookup tables estraggono i valori unici per dimensione:
#    non esistono ambiguità, perché ogni covariata è costante
#    nelle sue dimensioni di variazione per definizione.
# ─────────────────────────────────────────────────────────────────────

# 2a. Covariate (d, t): WB_EP_Depth, TREND_EP_Count
#     Prendiamo i valori univoci per (d, year). Usiamo unique() che
#     garantisce l'assenza di duplicati — nessuna media necessaria.
lkp_dt <- unique(
  dt[!is.na(WB_EP_Depth) | !is.na(TREND_EP_Count),
     .(d, year, WB_EP_Depth, TREND_EP_Count)]
)
# Verifica: non devono esserci duplicati su (d, year)
stopifnot(nrow(lkp_dt) == nrow(unique(lkp_dt[, .(d, year)])))
setkey(lkp_dt, d, year)

# 2b. Covariate (p, d, t): tariffs, ln_hhi_baci
lkp_pdt <- unique(
  dt[!is.na(tariffs) | !is.na(ln_hhi_baci),
     .(p, d, year, tariffs, ln_hhi_baci)]
)
# Verifica: non devono esserci duplicati su (p, d, year)
stopifnot(nrow(lkp_pdt) == nrow(unique(lkp_pdt[, .(p, d, year)])))
setkey(lkp_pdt, p, d, year)

# ─────────────────────────────────────────────────────────────────────
# 3. FILTRO SUI GREEN GOODS
#    Solo DOPO aver costruito le lookup tables, così queste coprono
#    anche (p, d, t) con export = 0 in tutte le imprese green.
# ─────────────────────────────────────────────────────────────────────
dt_green <- dt[env_good == 1L]

# ─────────────────────────────────────────────────────────────────────
# 4. ZERO FILL-IN (SAMPLING ZEROS)
#    Per ogni tripla (f, p, d) con almeno un anno di export
#    positivo (o anche solo presente), creiamo righe per
#    TUTTI gli anni del campione globale.
# ─────────────────────────────────────────────────────────────────────
all_years <- sort(unique(dt$year))

# Tripli (f, p, d) attivi tra i green goods
fpd_active <- unique(dt_green[, .(f, p, d)])

# Cross-join: ogni tripla (f, p, d) × tutti gli anni
dt_filled <- fpd_active[, .(year = all_years), by = .(f, p, d)]
setkey(dt_filled, f, p, d, year)

# ─────────────────────────────────────────────────────────────────────
# 5. MERGE DEGLI OUTCOME (export, exp_qua, uv_exp)
#    Le righe create dal fill-in ricevono NA → convertiamo in 0
# ─────────────────────────────────────────────────────────────────────
outcome_vars <- intersect(c("export", "exp_qua", "uv_exp"), names(dt_green))
dt_outcomes  <- dt_green[, c("f", "p", "d", "year", outcome_vars), with = FALSE]
setkey(dt_outcomes, f, p, d, year)

dt_filled <- merge(dt_filled, dt_outcomes, by = c("f", "p", "d", "year"), all.x = TRUE)

for (v in outcome_vars) {
  dt_filled[is.na(get(v)), (v) := 0]
}

# ─────────────────────────────────────────────────────────────────────
# 6. IMPUTATION CORRETTA DELLE COVARIATE
#    REGOLA FONDAMENTALE:
#    → Se la covariata varia su (d, t): lookup su (d, year)
#    → Se la covariata varia su (p, d, t): lookup su (p, d, year)
#    → Se la covariata varia su (p): assegnazione diretta
#
#    NON si calcola mai una media tra imprese (f) o prodotti (p)
#    su dimensioni che non fanno parte della variazione della covariata.
# ─────────────────────────────────────────────────────────────────────

# 6a. Covariate (d, t) → lookup su (d, year)
#     Le colonne WB_EP_Depth e TREND_EP_Count potrebbero già essere
#     presenti nel dt_filled se erano nelle dt_outcomes.
#     Le rimuoviamo prima del merge per evitare duplicati.
cols_dt <- c("WB_EP_Depth", "TREND_EP_Count")
cols_dt_present <- intersect(cols_dt, names(dt_filled))
if (length(cols_dt_present) > 0) {
  dt_filled[, (cols_dt_present) := NULL]
}
setkey(dt_filled, d, year)
dt_filled <- merge(dt_filled, lkp_dt, by = c("d", "year"), all.x = TRUE)

# 6b. Covariate (p, d, t) → lookup su (p, d, year)
cols_pdt <- c("tariffs", "ln_hhi_baci")
cols_pdt_present <- intersect(cols_pdt, names(dt_filled))
if (length(cols_pdt_present) > 0) {
  dt_filled[, (cols_pdt_present) := NULL]
}
setkey(dt_filled, p, d, year)
dt_filled <- merge(dt_filled, lkp_pdt, by = c("p", "d", "year"), all.x = TRUE)

# 6c. env_good → (p): per costruzione tutte le righe sono green goods
dt_filled[, env_good := 1L]

# Ripristina chiave principale
setkey(dt_filled, f, p, d, year)

# ─────────────────────────────────────────────────────────────────────
# 7. DIAGNOSTICA POST FILL-IN
# ─────────────────────────────────────────────────────────────────────
cat("\n─── DIAGNOSTICA FILL-IN ───\n")
cat("Righe totali nel panel espanso:      ", format(nrow(dt_filled), big.mark = ","), "\n")
cat("Di cui export == 0 (sampling zeros): ",
    format(sum(dt_filled$export == 0), big.mark = ","), "\n")
cat("Di cui export > 0:                   ",
    format(sum(dt_filled$export > 0),  big.mark = ","), "\n")
cat("\nNA residui nelle covariate principali:\n")
for (v in c("WB_EP_Depth", "TREND_EP_Count", "tariffs", "ln_hhi_baci")) {
  if (v %in% names(dt_filled)) {
    n_na <- sum(is.na(dt_filled[[v]]))
    cat(sprintf("  %-20s: %s NA (%.1f%%)\n", v, format(n_na, big.mark = ","),
                100 * n_na / nrow(dt_filled)))
  }
}

# ─────────────────────────────────────────────────────────────────────
# 8. COSTRUZIONE IDENTIFICATORI FIXED EFFECTS
# ─────────────────────────────────────────────────────────────────────
dt_filled[, fpd  := .GRP, by = .(f, p, d)]
dt_filled[, pdt  := .GRP, by = .(p, d, year)]
dt_filled[, fpdt := .GRP, by = .(f, p, d, year)]

# ─────────────────────────────────────────────────────────────────────
# 9. SALVATAGGIO DATASET CON FILL-IN
# ─────────────────────────────────────────────────────────────────────
filled_path <- here("Data/Final Dataset/green_goods_zero_filled.fst")
write_fst(dt_filled, filled_path)
cat("\nDataset salvato in:", filled_path, "\n")

# ─────────────────────────────────────────────────────────────────────
# 10. COEFFICIENT MAPS
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

# ─────────────────────────────────────────────────────────────────────
# 11. PPML ESTIMATION
# ─────────────────────────────────────────────────────────────────────
start <- now()
show_stats_ppml <- c("nobs", "n_clust")

# BLOCK 1: WB No Interaction
f1 <- c(
  "export ~ WB_EP_Depth | fpd + year",
  "exp_qua ~ WB_EP_Depth | fpd + year",
  "uv_exp ~ WB_EP_Depth | fpd + year",
  "export ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp ~ WB_EP_Depth + tariffs + ln_hhi_baci | fpd + year"
)
stats1 <- run_block(f1, "WB No Interaction – Green ZeroFill", "ppml",
                    filled_path, dirs$models,
                    vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats1, cm_wb, "PPML_ZF_Green_WB_No_Interaction.tex",
           dirs$tables, digits = 5, show_stats = show_stats_ppml)

# BLOCK 2: TREND No Interaction
f2 <- c(
  "export ~ TREND_EP_Count | fpd + year",
  "exp_qua ~ TREND_EP_Count | fpd + year",
  "uv_exp ~ TREND_EP_Count | fpd + year",
  "export ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
  "exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year",
  "uv_exp ~ TREND_EP_Count + tariffs + ln_hhi_baci | fpd + year"
)
stats2 <- run_block(f2, "TREND No Interaction – Green ZeroFill", "ppml",
                    filled_path, dirs$models,
                    vcov = ~pdt, requested_stats = show_stats_ppml)
make_table(stats2, cm_trend, "PPML_ZF_Green_TREND_No_Interaction.tex",
           dirs$tables, digits = 5, show_stats = show_stats_ppml)

cat("\n=== COMPLETATO! ===\n")
cat("Tabelle in:", dirs$tables, "\n")
cat("Tempo totale:", now() - start, "secondi\n")
