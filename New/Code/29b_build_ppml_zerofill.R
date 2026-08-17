########################################################################
###### 29b — Builder: ppml_agg_pdt_zerofill.fst (input PPML con zeri) ###
########################################################################
## Author: Edoardo Vitella
##
## Costruisce l'input del PPML sul margine estensivo:
##   Data/Final Dataset/ppml_agg_pdt_zerofill.fst
## consumato da 30_robustness_extensive_ppml.R.
##
## RICOSTRUITO (2026-08-17) dalla console history di RStudio: lo script
## originale non era mai stato salvato come file .R — girato a mano in una
## sessione interattiva del 2026-03-21. Il .fst attuale e' del 2026-07-21.
##
## >>> DA RICOSTRUIRE IN FUTURO (non urgente) <<<
## Il .fst sul disco NON e' stato rigenerato da questo script: e' l'output
## della vecchia sessione interattiva. La sua colonna `env_good` CONGELATA e'
## stantia (238 prodotti green, mappatura di luglio) rispetto ai green code
## attuali (green_codes_hs1996.csv del 2026-08-07 -> 246 prodotti). NON e' un
## bug dei risultati: 30_robustness_extensive_ppml.R NON legge env_good dal
## .fst, la RICALCOLA a runtime dai CSV correnti (idem dirty, TotalDepth), e i
## trattamenti nel .fst gia' riflettono il fix di luglio (WB_EP_Depth max=17).
## Rilanciare questo script prima della replica finale rende il .fst anche
## internamente coerente (env_good congelata = 246), eliminando il rischio che
## un futuro consumer legga la classificazione vecchia dal file.
##
## NATURA DELLA GRIGLIA: zero-fill CONDIZIONATO, non cross-join completo. Solo
## le coppie (hs6, country_code) con >=1 flusso positivo nel periodo, completate
## su tutti gli anni. Le combinazioni prodotto-destinazione mai scambiate non
## entrano. Il PPML misura quindi il margine estensivo temporale within-coppia,
## non la nascita di mercati-prodotto del tutto nuovi.
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (micro firm-level)
## Output: Data/Final Dataset/ppml_agg_pdt_zerofill.fst (~8,31M celle hs6 x dest x anno)

# ── SETUP ──────────────────────────────────────────────────────────────
rm(list = ls())
library(fst); library(data.table); library(here); library(lubridate)
threads_fst(1)

data_dir  <- here("Data/Final Dataset")
data_file <- file.path(data_dir, "final_dataset_pta_env_indices_compressed.fst")
agg_file  <- file.path(data_dir, "ppml_agg_pdt_zerofill.fst")
stopifnot("File dati micro non trovato!" = file.exists(data_file))

# ── PARTE 1 — CARICAMENTO E AGGREGAZIONE A LIVELLO PDT ──────────────────
cat("=== PARTE 1: Caricamento e aggregazione a livello pdt ===\n")
start_total <- now()
vars_needed <- c("export", "exp_qua",
                 "WB_EP_Depth", "TREND_EP_Count",
                 "env_good", "tariffs", "ln_hhi_baci",
                 "hs6", "country_code", "year")
cat("Caricamento dataset...\n")
dt <- as.data.table(read_fst(data_file, columns = vars_needed))
cat(sprintf("Osservazioni firm-level caricate: %s\n", format(nrow(dt), big.mark = ",")))
cat(sprintf("Prodotti unici (hs6): %d | Destinazioni: %d | Anni: %d-%d\n",
            dt[, uniqueN(hs6)], dt[, uniqueN(country_code)], dt[, min(year)], dt[, max(year)]))

# Aggregazione: somma export/quantita' per cella pdt. Le variabili pdt-level
# (tariffs, HHI, EPDepth, env_good) sono identiche within-cella -> prima occorrenza.
cat("Aggregazione a livello pdt...\n")
dt_agg <- dt[, .(
  agg_export     = sum(export,  na.rm = TRUE),   # flusso aggregato (Y per PPML)
  agg_exp_qua    = sum(exp_qua, na.rm = TRUE),   # quantita' aggregata
  WB_EP_Depth    = first(WB_EP_Depth),
  TREND_EP_Count = first(TREND_EP_Count),
  env_good       = first(env_good),
  tariffs        = first(tariffs),
  ln_hhi_baci    = first(ln_hhi_baci)
), by = .(hs6, country_code, year)]
cat(sprintf("Celle pdt con flusso positivo: %s\n", format(nrow(dt_agg), big.mark = ",")))

# Lookup per imputazione delle celle zero
dt_tariff_lookup  <- dt_agg[!is.na(tariffs),
  .(tariff_mean = mean(tariffs, na.rm = TRUE)), by = .(hs6, country_code)]
dt_hhi_lookup     <- dt_agg[!is.na(ln_hhi_baci),
  .(hhi_mean = mean(ln_hhi_baci, na.rm = TRUE)), by = .(hs6, country_code)]
dt_envgood_lookup <- dt_agg[, .(env_good = first(env_good)), by = hs6]
rm(dt); gc()

# ── PARTE 2 — ZERO FILL-IN (CONDIZIONATO) ──────────────────────────────
cat("=== PARTE 2: Zero fill-in ===\n")
active_pairs <- unique(dt_agg[agg_export > 0, .(hs6, country_code)])
years_all    <- dt_agg[, sort(unique(year))]
cat(sprintf("Coppie (hs6, country_code) attive: %s | Anni: %d (%d-%d)\n",
            format(nrow(active_pairs), big.mark = ","),
            length(years_all), min(years_all), max(years_all)))

# Griglia completa: solo le coppie attive x tutti gli anni
cat("Costruzione griglia completa pdt...\n")
dt_grid <- active_pairs[, CJ(hs6 = hs6, country_code = country_code,
                             year = years_all, unique = TRUE),
                        by = .(hs6, country_code)][, .(hs6, country_code, year)]
dt_grid <- unique(dt_grid[, .(hs6, country_code, year)])
cat(sprintf("Celle nella griglia completa: %s (positive: %.1f%%)\n",
            format(nrow(dt_grid), big.mark = ","), 100 * nrow(dt_agg) / nrow(dt_grid)))

# Merge: flussi aggregati sulla griglia -> NA diventa 0
dt_full <- merge(dt_grid, dt_agg, by = c("hs6", "country_code", "year"), all.x = TRUE)
dt_full[is.na(agg_export),  agg_export  := 0]
dt_full[is.na(agg_exp_qua), agg_exp_qua := 0]
cat(sprintf("Osservazioni totali dopo fill-in: %s (di cui zero: %.1f%%)\n",
            format(nrow(dt_full), big.mark = ","), 100 * mean(dt_full$agg_export == 0)))

# ── PARTE 3 — MERGE COVARIATE PER LE CELLE ZERO ────────────────────────
cat("=== PARTE 3: Merge covariate ===\n")
## 3a. env_good (product-level)
dt_full <- merge(dt_full, dt_envgood_lookup, by = "hs6", all.x = TRUE, suffixes = c("", "_lookup"))
dt_full[is.na(env_good) & !is.na(env_good_lookup), env_good := env_good_lookup]
dt_full[, env_good_lookup := NULL]
## 3b. Tariffe: valore diretto dove c'e', altrimenti media della coppia (hs6, country_code)
dt_full <- merge(dt_full, dt_tariff_lookup, by = c("hs6", "country_code"), all.x = TRUE)
dt_full[is.na(tariffs) & !is.na(tariff_mean), tariffs := tariff_mean]
dt_full[, tariff_mean := NULL]
## 3c. HHI: stessa logica delle tariffe
dt_full <- merge(dt_full, dt_hhi_lookup, by = c("hs6", "country_code"), all.x = TRUE)
dt_full[is.na(ln_hhi_baci) & !is.na(hhi_mean), ln_hhi_baci := hhi_mean]
dt_full[, hhi_mean := NULL]
## 3d. EPDepth / TREND: lookup (country_code, year); destinazioni senza PTA -> 0
dt_ep_lookup <- dt_agg[, .(WB_EP_Depth_dt = first(WB_EP_Depth),
                           TREND_EP_Count_dt = first(TREND_EP_Count)),
                       by = .(country_code, year)]
dt_full <- merge(dt_full, dt_ep_lookup, by = c("country_code", "year"), all.x = TRUE)
dt_full[is.na(WB_EP_Depth)    & !is.na(WB_EP_Depth_dt),    WB_EP_Depth    := WB_EP_Depth_dt]
dt_full[is.na(TREND_EP_Count) & !is.na(TREND_EP_Count_dt), TREND_EP_Count := TREND_EP_Count_dt]
dt_full[, c("WB_EP_Depth_dt", "TREND_EP_Count_dt") := NULL]
dt_full[is.na(WB_EP_Depth),    WB_EP_Depth    := 0]
dt_full[is.na(TREND_EP_Count), TREND_EP_Count := 0]
rm(dt_agg, dt_grid, dt_ep_lookup, dt_tariff_lookup, dt_hhi_lookup, dt_envgood_lookup); gc()

# ── PARTE 4 — COSTRUZIONE ID E VARIABILI FINALI ────────────────────────
cat("=== PARTE 4: Costruzione ID e variabili ===\n")
# Group ids per cluster e fixed effects
dt_full[, pd  := .GRP, by = .(hs6, country_code)]
dt_full[, pt  := .GRP, by = .(hs6, year)]
dt_full[, dt  := .GRP, by = .(country_code, year)]
dt_full[, pdt := .GRP, by = .(hs6, country_code, year)]
# unit value aggregato: solo per celle con quantita' positiva (NA sulle zero)
dt_full[, uv_agg := ifelse(agg_exp_qua > 0, agg_export / agg_exp_qua, NA_real_)]
dt_full[, env_good := as.integer(env_good)]
setorder(dt_full, hs6, country_code, year)
cat(sprintf("Copertura EPDepth: %d oss con WB_EP_Depth > 0 (%.1f%%)\n",
            dt_full[WB_EP_Depth > 0, .N], 100 * mean(dt_full$WB_EP_Depth > 0)))
cat(sprintf("NA residui - tariffs: %d | ln_hhi_baci: %d\n",
            sum(is.na(dt_full$tariffs)), sum(is.na(dt_full$ln_hhi_baci))))

# ── SALVATAGGIO ────────────────────────────────────────────────────────
cat("Salvataggio dataset aggregato...\n")
write_fst(dt_full, agg_file, compress = 50)
cat("Salvato in:", agg_file, "\n")
cat(sprintf("Tempo totale: %.1f minuti\n", as.numeric(now() - start_total, units = "mins")))
