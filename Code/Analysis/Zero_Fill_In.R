##########################################################################
######   PPML Aggregato con Zero Fill-In (livello prodotto-dest-anno)  ###
##########################################################################
##
## Author: Edoardo Vitella
## PhD student at University of Trento and Free University of Bozen
##
## ── OBIETTIVO ──────────────────────────────────────────────────────────
## Il PPML firm-level usa solo flussi positivi, introducendo un bias di
## selezione: si stima l'effetto delle EPs condizionando sull'evento
## "la firma esportava già", che è esso stesso funzione del trattamento.
##
## Questo script:
##   1. Aggrega i flussi al livello prodotto-destinazione-anno (pdt)
##   2. Esegue il zero fill-in per le celle (hs6 x country_code x year)
##      con almeno una osservazione positiva nel periodo 2000-2015
##      (sampling zeros, non structural zeros)
##   3. Stima il PPML sul dataset aggregato con la specifica:
##      E[X_pdt] = exp(β·EPDepth_dt + θ_pd + θ_t)
##
## ── DIFFERENZE RISPETTO AL PPML FIRM-LEVEL ─────────────────────────────
##   - Unità di analisi: pdt invece di fpdt
##   - FE: pd + year invece di fpd + year
##   - Clustering: ~pd (product-destination) invece di ~pdt
##   - Y = export aggregato (include zero per sampling zeros)
##
## ── OUTPUT ──────────────────────────────────────────────────────────────
## Data:
##   ppml_agg_pdt_zerofill.fst       → dataset aggregato con zeri
## Tabelle:
##   PPML_Agg_WB_No_Interaction.tex
##   PPML_Agg_WB_Interaction.tex
##   PPML_Agg_TREND_No_Interaction.tex
##   PPML_Agg_TREND_Interaction.tex

# ─────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────
rm(list = ls())

library(fst)
library(fixest)
library(data.table)
library(here)
library(lubridate)

source(here("Code/Analysis/pta_functions.R"))

data_dir  <- here("Data/Final Dataset")
data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("Output/Analysis/PPML_Aggregated")
dirs      <- setup_output_dirs(out_dir)

stopifnot("Data file not found!" = file.exists(data_file))

agg_file  <- file.path(data_dir, "ppml_agg_pdt_zerofill.fst")

# ─────────────────────────────────────────────────────────────────────
# PARTE 1 — CARICAMENTO E AGGREGAZIONE A LIVELLO PDT
# ─────────────────────────────────────────────────────────────────────
cat("=== PARTE 1: Caricamento e aggregazione a livello pdt ===\n")
start_total <- now()

vars_needed <- c(
  "export", "exp_qua",
  "WB_EP_Depth", "TREND_EP_Count",
  "env_good", "tariffs", "ln_hhi_baci",
  "hs6", "country_code", "year"
)

cat("Caricamento dataset...\n")
dt <- as.data.table(read_fst(data_file, columns = vars_needed))

cat(sprintf("Osservazioni firm-level caricate: %s\n",
            format(nrow(dt), big.mark = ",")))
cat(sprintf("Prodotti unici (hs6): %d\n",    dt[, uniqueN(hs6)])
cat(sprintf("Destinazioni uniche: %d\n",     dt[, uniqueN(country_code)])
cat(sprintf("Anni: %d – %d\n",               dt[, min(year)], dt[, max(year)]))

# ── Aggregazione: somma export e quantità per cella pdt ───────────────
# Le variabili pdt-level (tariffs, HHI, EPDepth, env_good) sono identiche
# per tutte le firme della stessa cella → prendiamo il primo valore.
cat("Aggregazione a livello pdt...\n")

dt_agg <- dt[, .(
  agg_export  = sum(export,  na.rm = TRUE),   # flusso aggregato (Y per PPML)
  agg_exp_qua = sum(exp_qua, na.rm = TRUE),   # quantità aggregata
  # Variabili pdt-level: identiche within-cella, prendiamo prima occorrenza
  WB_EP_Depth   = first(WB_EP_Depth),
  TREND_EP_Count = first(TREND_EP_Count),
  env_good      = first(env_good),
  tariffs       = first(tariffs),
  ln_hhi_baci   = first(ln_hhi_baci)
), by = .(hs6, country_code, year)]

cat(sprintf("Celle pdt con flusso positivo: %s\n",
            format(nrow(dt_agg), big.mark = ",")))

# ── Lookup tabelle per tariffe e HHI (usati per riempire le celle zero) ─
# Per ogni (hs6, country_code, year), la tariffa MFN è la stessa per tutte
# le firme → il lookup è già embedded in dt_agg.
# Per le celle zero (fill-in), tentiamo di imputare la tariffa media
# dello stesso (hs6, country_code) nel tempo o dello stesso (hs6, year)
# quando quella specifica cella non ha mai avuto flussi positivi.

dt_tariff_lookup <- dt_agg[!is.na(tariffs),
                             .(tariff_mean = mean(tariffs, na.rm = TRUE)),
                             by = .(hs6, country_code)]

dt_hhi_lookup    <- dt_agg[!is.na(ln_hhi_baci),
                             .(hhi_mean = mean(ln_hhi_baci, na.rm = TRUE)),
                             by = .(hs6, country_code)]

# ── env_good è product-level: lookup hs6 → env_good ─────────────────
dt_envgood_lookup <- dt_agg[, .(env_good = first(env_good)), by = hs6]

rm(dt); gc()

# ─────────────────────────────────────────────────────────────────────
# PARTE 2 — ZERO FILL-IN
# ─────────────────────────────────────────────────────────────────────
## Principio: includiamo solo coppie (hs6, country_code) per cui esiste
## ALMENO UNA osservazione positiva nell'intero periodo 2000–2015.
## Queste sono "sampling zeros" (nessuna firma ha esportato in quel dato
## anno, ma la relazione commerciale esiste).
## Non includiamo (hs6, country_code) mai osservati: quelli sono
## "structural zeros" che il PPML non dovrebbe trattare come informazione.

cat("=== PARTE 2: Zero fill-in ===\n")

# Coppie (hs6, country_code) con almeno un flusso positivo nel periodo
active_pairs <- unique(dt_agg[agg_export > 0, .(hs6, country_code)])
years_all    <- dt_agg[, sort(unique(year))]

cat(sprintf("Coppie (hs6, country_code) attive: %s\n",
            format(nrow(active_pairs), big.mark = ",")))
cat(sprintf("Anni nel campione: %d (%d – %d)\n",
            length(years_all), min(years_all), max(years_all)))

# Griglia completa: tutte le combinazioni (hs6, country_code, year) attive
cat("Costruzione griglia completa pdt...\n")
dt_grid <- active_pairs[, CJ(
  hs6          = hs6,
  country_code = country_code,
  year         = years_all,
  unique       = TRUE
), by = .(hs6, country_code)][, .(hs6, country_code, year)]

# Rimuovi la colonna di raggruppamento aggiuntiva se presente
dt_grid <- unique(dt_grid[, .(hs6, country_code, year)])

cat(sprintf("Celle nella griglia completa: %s\n",
            format(nrow(dt_grid), big.mark = ",")))
cat(sprintf("Celle con flusso positivo: %s (%.1f%%)\n",
            format(nrow(dt_agg), big.mark = ","),
            100 * nrow(dt_agg) / nrow(dt_grid)))

# ── Merge: flussi aggregati sulla griglia → NA diventa 0 ─────────────
dt_full <- merge(dt_grid, dt_agg,
                 by = c("hs6", "country_code", "year"),
                 all.x = TRUE)

# Fill-in degli export e quantità: NA → 0
dt_full[is.na(agg_export),  agg_export  := 0]
dt_full[is.na(agg_exp_qua), agg_exp_qua := 0]

cat(sprintf("Osservazioni totali dopo fill-in: %s\n",
            format(nrow(dt_full), big.mark = ",")))
cat(sprintf("  di cui zero: %s (%.1f%%)\n",
            format(nrow(dt_full[agg_export == 0]), big.mark = ","),
            100 * mean(dt_full$agg_export == 0)))

# ─────────────────────────────────────────────────────────────────────
# PARTE 3 — MERGE COVARIATE PER LE CELLE ZERO
# ─────────────────────────────────────────────────────────────────────
cat("=== PARTE 3: Merge covariate ===\n")

## 3a. env_good (product-level)
dt_full <- merge(dt_full, dt_envgood_lookup, by = "hs6", all.x = TRUE,
                 suffixes = c("", "_lookup"))
dt_full[is.na(env_good) & !is.na(env_good_lookup),
        env_good := env_good_lookup]
dt_full[, env_good_lookup := NULL]

## 3b. Tariffe: usa il valore diretto dove disponibile; imputa la media
##    della coppia (hs6, country_code) per le celle zero senza tariffa
dt_full <- merge(dt_full, dt_tariff_lookup,
                 by = c("hs6", "country_code"), all.x = TRUE)
dt_full[is.na(tariffs) & !is.na(tariff_mean), tariffs := tariff_mean]
dt_full[, tariff_mean := NULL]

## 3c. HHI: stessa logica delle tariffe
dt_full <- merge(dt_full, dt_hhi_lookup,
                 by = c("hs6", "country_code"), all.x = TRUE)
dt_full[is.na(ln_hhi_baci) & !is.na(hhi_mean), ln_hhi_baci := hhi_mean]
dt_full[, hhi_mean := NULL]

## 3d. EPDepth e TREND: variabili destination-year.
##     Per le celle zero, WB_EP_Depth e TREND_EP_Count sono NA perché non
##     venivano caricate per quelle celle. Dobbiamo reimputarle dal
##     dataset originale degli indici (livello country_code × year).
##     Costruiamo il lookup dt-level dagli stessi dati aggregati.

dt_ep_lookup <- dt_agg[, .(
  WB_EP_Depth_dt    = first(WB_EP_Depth),
  TREND_EP_Count_dt = first(TREND_EP_Count)
), by = .(country_code, year)]

dt_full <- merge(dt_full, dt_ep_lookup,
                 by = c("country_code", "year"), all.x = TRUE)

# Completa i NA: usa i valori dal lookup dt dove il campo diretto è mancante
dt_full[is.na(WB_EP_Depth) & !is.na(WB_EP_Depth_dt),
        WB_EP_Depth := WB_EP_Depth_dt]
dt_full[is.na(TREND_EP_Count) & !is.na(TREND_EP_Count_dt),
        TREND_EP_Count := TREND_EP_Count_dt]
dt_full[, c("WB_EP_Depth_dt", "TREND_EP_Count_dt") := NULL]

# Per destinazioni senza alcun PTA attivo, EPDepth = 0 (come nel dataset originale)
dt_full[is.na(WB_EP_Depth),    WB_EP_Depth    := 0]
dt_full[is.na(TREND_EP_Count), TREND_EP_Count := 0]

rm(dt_agg, dt_grid, dt_ep_lookup,
   dt_tariff_lookup, dt_hhi_lookup, dt_envgood_lookup)
gc()

# ─────────────────────────────────────────────────────────────────────
# PARTE 4 — COSTRUZIONE ID E VARIABILI FINALI
# ─────────────────────────────────────────────────────────────────────
cat("=== PARTE 4: Costruzione ID e variabili ===\n")

# pd: product-destination ID (usato come FE e per il clustering)
dt_full[, pd := .GRP, by = .(hs6, country_code)]

# pdt: product-destination-year ID (cluster alternativo se necessario)
dt_full[, pdt := .GRP, by = .(hs6, country_code, year)]

# unit value aggregato: solo per celle con quantità positiva
# uv_agg = NA per celle zero (PPML su UV stima solo sulle righe non-NA)
dt_full[, uv_agg := ifelse(agg_exp_qua > 0,
                            agg_export / agg_exp_qua,
                            NA_real_)]

# env_good come intero (coerente con il dataset originale)
dt_full[, env_good := as.integer(env_good)]

# Ordina per efficienza computazionale
setorder(dt_full, hs6, country_code, year)

cat(sprintf("Copertura EPDepth: %d osservazioni con WB_EP_Depth > 0 (%.1f%%)\n",
            dt_full[WB_EP_Depth > 0, .N],
            100 * mean(dt_full$WB_EP_Depth > 0)))
cat(sprintf("Copertura env_good: %d prodotti ambientali (%.1f%% delle celle)\n",
            dt_full[env_good == 1, .N],
            100 * mean(dt_full$env_good == 1, na.rm = TRUE)))
cat(sprintf("NA residui — tariffs: %d | ln_hhi_baci: %d\n",
            sum(is.na(dt_full$tariffs)),
            sum(is.na(dt_full$ln_hhi_baci))))

# ── Salvataggio ───────────────────────────────────────────────────────
cat("Salvataggio dataset aggregato...\n")
write_fst(dt_full, agg_file, compress = 50)
cat("Salvato in:", agg_file, "\n")
cat(sprintf("Tempo aggregazione: %.1f minuti\n",
            as.numeric(now() - start_total, units = "mins")))

rm(dt_full); gc()

# ─────────────────────────────────────────────────────────────────────
# PARTE 5 — COEFFICIENT MAPS
# ─────────────────────────────────────────────────────────────────────
## Nota sui subscript: pd invece di fpd per ricordare che siamo al
## livello aggregato (prodotto-destinazione, non firma-prodotto-destinazione)

cm_wb <- c(
  "WB_EP_Depth"  = "\\textit{EPDepth\\textsubscript{dt}}",
  "tariffs"      = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"  = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_wb_int <- c(
  "WB_EP_Depth"          = "\\textit{EPDepth\\textsubscript{dt}}",
  "WB_EP_Depth:env_good" = "\\textit{EPDepth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"              = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"          = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend <- c(
  "TREND_EP_Count"  = "\\textit{TREND Depth\\textsubscript{dt}}",
  "tariffs"         = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"     = "\\textit{ln HHI\\textsubscript{pdt}}"
)
cm_trend_int <- c(
  "TREND_EP_Count"             = "\\textit{TREND Depth\\textsubscript{dt}}",
  "TREND_EP_Count:env_good"    = "\\textit{TREND Depth\\textsubscript{dt} $\\times$ EnvGood\\textsubscript{p}}",
  "tariffs"                    = "\\textit{ln MFN Tariffs\\textsubscript{pdt}}",
  "ln_hhi_baci"                = "\\textit{ln HHI\\textsubscript{pdt}}"
)

show_stats_ppml <- c("nobs", "n_clust")

# ─────────────────────────────────────────────────────────────────────
# PARTE 6 — STIMA PPML
# ─────────────────────────────────────────────────────────────────────
## FE: pd (product-destination) + year
## Clustering: ~pd  (serial correlation within product-destination pair)
##
## Le specifiche sono identiche al PPML firm-level ma:
##   - Y è l'export aggregato (include zeri)
##   - FE pd invece di fpd
##   - Clustering ~pd invece di ~pdt
##
## NB: le specifiche con `+ tariffs + ln_hhi_baci` useranno solo le righe
##     con tariffe non-NA. Per massimizzare l'uso dei dati, le stime
##     "No Controls" sono le più affidabili con il fill-in.

cat("=== PARTE 6: Stima PPML aggregato ===\n")
start <- now()

## BLOCCO 1: WB No Interaction
f1 <- c(
  "agg_export  ~ WB_EP_Depth | pd + year",
  "agg_exp_qua ~ WB_EP_Depth | pd + year",
  "uv_agg      ~ WB_EP_Depth | pd + year",
  "agg_export  ~ WB_EP_Depth + tariffs + ln_hhi_baci | pd + year",
  "agg_exp_qua ~ WB_EP_Depth + tariffs + ln_hhi_baci | pd + year",
  "uv_agg      ~ WB_EP_Depth + tariffs + ln_hhi_baci | pd + year"
)
stats1 <- run_block(f1, "AggPPML_WB_NoInt", "ppml",
                    agg_file, dirs$models,
                    vcov = ~pd,
                    requested_stats = show_stats_ppml)
make_table(stats1, cm_wb, "PPML_Agg_WB_No_Interaction.tex", dirs$tables,
           digits = 5, show_stats = show_stats_ppml)

## BLOCCO 2: WB Interaction
f2 <- c(
  "agg_export  ~ WB_EP_Depth * env_good | pd + year",
  "agg_exp_qua ~ WB_EP_Depth * env_good | pd + year",
  "uv_agg      ~ WB_EP_Depth * env_good | pd + year",
  "agg_export  ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pd + year",
  "agg_exp_qua ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pd + year",
  "uv_agg      ~ WB_EP_Depth * env_good + tariffs + ln_hhi_baci | pd + year"
)
stats2 <- run_block(f2, "AggPPML_WB_Int", "ppml",
                    agg_file, dirs$models,
                    vcov = ~pd,
                    requested_stats = show_stats_ppml)
make_table(stats2, cm_wb_int, "PPML_Agg_WB_Interaction.tex", dirs$tables,
           digits = 5, show_stats = show_stats_ppml)

## BLOCCO 3: TREND No Interaction
f3 <- c(
  "agg_export  ~ TREND_EP_Count | pd + year",
  "agg_exp_qua ~ TREND_EP_Count | pd + year",
  "uv_agg      ~ TREND_EP_Count | pd + year",
  "agg_export  ~ TREND_EP_Count + tariffs + ln_hhi_baci | pd + year",
  "agg_exp_qua ~ TREND_EP_Count + tariffs + ln_hhi_baci | pd + year",
  "uv_agg      ~ TREND_EP_Count + tariffs + ln_hhi_baci | pd + year"
)
stats3 <- run_block(f3, "AggPPML_TREND_NoInt", "ppml",
                    agg_file, dirs$models,
                    vcov = ~pd,
                    requested_stats = show_stats_ppml)
make_table(stats3, cm_trend, "PPML_Agg_TREND_No_Interaction.tex", dirs$tables,
           digits = 5, show_stats = show_stats_ppml)

## BLOCCO 4: TREND Interaction
f4 <- c(
  "agg_export  ~ TREND_EP_Count * env_good | pd + year",
  "agg_exp_qua ~ TREND_EP_Count * env_good | pd + year",
  "uv_agg      ~ TREND_EP_Count * env_good | pd + year",
  "agg_export  ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pd + year",
  "agg_exp_qua ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pd + year",
  "uv_agg      ~ TREND_EP_Count * env_good + tariffs + ln_hhi_baci | pd + year"
)
stats4 <- run_block(f4, "AggPPML_TREND_Int", "ppml",
                    agg_file, dirs$models,
                    vcov = ~pd,
                    requested_stats = show_stats_ppml)
make_table(stats4, cm_trend_int, "PPML_Agg_TREND_Interaction.tex", dirs$tables,
           digits = 5, show_stats = show_stats_ppml)

cat(sprintf("\n=== COMPLETATO! ===\n"))
cat("Tabelle in:", dirs$tables, "\n")
cat("Modelli in:", dirs$models, "\n")
cat("Dataset aggregato:", agg_file, "\n")
cat("- 4 tabelle .tex\n- 24 PPML_Agg_*_*.rds\n")
cat(sprintf("Tempo totale: %.1f minuti\n",
            as.numeric(now() - start_total, units = "mins")))
