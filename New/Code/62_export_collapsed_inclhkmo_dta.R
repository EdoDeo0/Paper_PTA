########################################################
###### 62 — Export panel collassato incl. HK/Macao -> .dta per Stata ###
########################################################
## Author: Edoardo Vitella
## Prerequisito: 10_collapsed_panel.R (panel_pdt_collapsed_inclHKMO.fst presente)
##
## PERCHE' ESISTE. `52_export_collapsed_dta.R` esporta il solo campione baseline
## (escl. HK/Macao). Le tabelle di `Tabelle_Stime.pdf` hanno pero' una matrice
## 2x2 di varianti: campione {escl, incl HK/Macao} x controllo di profondita'
## {TotalDepth, DESTA}. Questo script produce il secondo campione; il secondo
## controllo NON richiede un export separato, perche' entrambe le misure di
## profondita' (TotalDepth_nonEnv e DESTA_depth_index) sono colonne dello
## stesso file. Due .dta coprono quindi tutte e quattro le varianti.
##
## E' una copia fedele di 52: stessa logica, stesse fonti, stesso schema di
## colonne. L'UNICA differenza e' il panel di input e il nome dell'output.
## 52 non e' stato parametrizzato di proposito: il suo output e' un artefatto
## verificato e riscriverlo per fattorizzare del codice metterebbe a rischio
## un risultato gia' agli atti per zero beneficio.
##
## Output: New/Data/Collapsed/collapsed_omnibus_inclHKMO.dta
##
## Esecuzione (da PowerShell, root progetto):
##   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New/Code/62_export_collapsed_inclhkmo_dta.R

rm(list = ls())
library(here)
library(data.table)
library(fst)
library(haven)
threads_fst(1)

## --- Percorsi ----------------------------------------------------------------
CACHE_FST   <- here("New/Data/Collapsed/panel_pdt_collapsed_inclHKMO.fst")
GREEN_FILE  <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE  <- here("New/Data/Classifications/dirty_goods_hs6.csv")
CO2_FILE    <- here("New/Data/Classifications/co2_intensity_hs6.csv")
DEPTH_FILE  <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
DESTA_FILE  <- here("New/Data/TotalDepth/desta_depth_country_year.csv")
BYAREA_FILE <- here("New/Data/TotalDepth/wb_totaldepth_byarea_country_year.csv")
IDX_FILE    <- here("Data/Merged/Merged_TREND_WB_Indices_Only.csv")
HS4_FILE    <- here("New/Data/Subsamples/flag_prodHS4.csv")
DS_FILE     <- here("New/Data/Subsamples/flag_deepshallow.csv")
CEM_FILE    <- here("Output/CEM/matched_countries.csv")
OUT_DTA     <- here("New/Data/Collapsed/collapsed_omnibus_inclHKMO.dta")

stopifnot("panel inclHKMO non trovato: eseguire prima 10_collapsed_panel.R" = file.exists(CACHE_FST))

## --- Caricamento panel collassato -------------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
cat("Panel collassato (incl. HK/Macao):", format(nrow(cell), big.mark = ","), "celle\n")

## Guardia dataset stantio: max WB_EP_Depth deve essere 17 (post-fix luglio 2026)
mx <- max(cell$WB_EP_Depth, na.rm = TRUE)
if (mx != 17)
  stop(sprintf("WB_EP_Depth max=%d, atteso 17. Dataset stantio: rigenerare da 10.", mx))

## Guardia campione: questo panel DEVE contenere piu' celle del baseline
if (nrow(cell) <= 3773498)
  stop("Il panel incl. HK/Macao non e' piu' grande del baseline: input sbagliato.")

## --- Green (env_good, apec_egl) ----------------------------------------------
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
green_codes <- unique(green$hs6_final)
apec_codes  <- green[!is.na(apec_egl) & apec_egl == 1L, unique(hs6_final)]
cell[, hs6_str := sprintf("%06d", as.integer(hs6))]
cell[, env_good := as.integer(hs6_str %in% green_codes)]
cell[, apec_egl := as.integer(hs6_str %in% apec_codes)]
cell[, hs6_str  := NULL]

## --- Dirty -------------------------------------------------------------------
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = as.integer(dirty))]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]

## --- Intensita' CO2 (per la robustezza dirty continua) ----------------------
## Standardizzazione IDENTICA a 29_robustness_co2intensity.R: media e sd sui
## non mancanti, poi i mancanti prendono la media (quindi z = 0).
co2 <- fread(CO2_FILE)[, .(hs6 = as.integer(hs6_int), co2_total)]
cell[co2, on = "hs6", co2_total := i.co2_total]
mu  <- mean(cell$co2_total, na.rm = TRUE)
sdv <- sd(cell$co2_total, na.rm = TRUE)
cell[is.na(co2_total), co2_total := mu]
cell[, co2_z := (co2_total - mu) / sdv]
cat(sprintf("[co2] media=%.9f sd=%.9f\n", mu, sdv))

## --- TotalDepth_nonEnv -------------------------------------------------------
dep <- fread(DEPTH_FILE)[, .(country_code, year, TotalDepth_nonEnv)]
cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]

## --- DESTA_depth_index (NA permesso) ----------------------------------------
desta <- fread(DESTA_FILE)[, .(country_code, year, DESTA_depth_index)]
cell[desta, on = c("country_code", "year"), DESTA_depth_index := i.DESTA_depth_index]

## --- TotalDepth_targeted (14 aree ad alta corr) ------------------------------
LOW_CORR_AREAS <- c("Labor.Market.Regulations", "Visa.and.Asylum", "Subsidies")
byarea   <- fread(BYAREA_FILE)
area_cols <- setdiff(names(byarea), c("Country", "year", "country_code"))
keep_cols <- setdiff(area_cols, LOW_CORR_AREAS)
cat(sprintf("[targeted] aree incluse: %d/%d\n", length(keep_cols), length(area_cols)))
byarea[, TotalDepth_targeted := rowSums(.SD, na.rm = TRUE), .SDcols = keep_cols]
depth_tgt <- byarea[, .(country_code, year, TotalDepth_targeted)]
cell[depth_tgt, on = c("country_code", "year"), TotalDepth_targeted := i.TotalDepth_targeted]
cell[is.na(TotalDepth_targeted), TotalDepth_targeted := 0]

## --- 7 sub-indici EP ---------------------------------------------------------
SUBS <- c("WB_GreenLiberalization", "TREND_GreenMarketAccess",
          "WB_EnforcementDSM", "TREND_EnforcementDSM", "TREND_Hard",
          "TREND_Soft", "TREND_RegulatorySpace")
idx <- fread(IDX_FILE)[, c("country_code", "year", SUBS), with = FALSE]
cell[idx, on = c("country_code", "year"), (SUBS) := mget(paste0("i.", SUBS))]
for (s in SUBS) cell[is.na(get(s)), (s) := 0]

## --- Flag campioni -----------------------------------------------------------
hs4_flag <- fread(HS4_FILE)[, .(hs6, in_HS4match = as.integer(in_HS4match))]
cell[hs4_flag, on = "hs6", in_HS4match := i.in_HS4match]
cell[is.na(in_HS4match), in_HS4match := 0L]

ds_flag  <- fread(DS_FILE)[, .(country_code, group_deepshallow = group)]
cell[ds_flag, on = "country_code", group_deepshallow := i.group_deepshallow]
cell[is.na(group_deepshallow), group_deepshallow := "control"]
cell[, deepshallow := fcase(
  group_deepshallow == "deep",    1L,
  group_deepshallow == "shallow", 2L,
  default = 0L)]
cell[, group_deepshallow := NULL]

cem_cc <- fread(CEM_FILE)$country_code
cell[, cem_matched := as.integer(country_code %in% cem_cc)]

## --- ID FE (interi compatti, ricalcolati su QUESTO campione) ----------------
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

## --- Variabili derivate ------------------------------------------------------
cell[, EP_share := fifelse(TotalDepth_nonEnv > 0, WB_EP_Depth / TotalDepth_nonEnv, NA_real_)]
cell[, dose_bin := fcase(
  WB_EP_Depth == 0,                    0L,
  WB_EP_Depth >= 1 & WB_EP_Depth <= 5, 1L,
  WB_EP_Depth >= 6 & WB_EP_Depth <= 7, 2L,
  WB_EP_Depth >= 8,                    3L)]
cell[, trend := as.integer(year) - 2000L]

## --- Riepilogo e scrittura --------------------------------------------------
cat(sprintf("Celle totali: %s\n", format(nrow(cell), big.mark = ",")))
cat(sprintf("green: %.1f%% | dirty: %.1f%% | APEC-green: %.1f%%\n",
            100 * mean(cell$env_good), 100 * mean(cell$dirty_p), 100 * mean(cell$apec_egl)))
cat(sprintf("Trattati (WB_EP_Depth>0): %d country_code (atteso 25: 23 + HK + Macao)\n",
            uniqueN(cell[WB_EP_Depth > 0, country_code])))
cat("Colonne nel .dta:", paste(names(cell), collapse = ", "), "\n")

dir.create(dirname(OUT_DTA), recursive = TRUE, showWarnings = FALSE)
write_dta(cell, OUT_DTA)
cat("OK:", OUT_DTA, "\n")
