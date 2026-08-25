########################################################
###### 52 — Export panel collassato -> .dta per Stata  ###
########################################################
## Author: Edoardo Vitella
## Prerequisito: 10_collapsed_panel.R (panel_pdt_collapsed.fst gia' presente)
##
## Cosa fa: carica il panel collassato baseline (excl HK/Macao, totaldepth)
## e vi aggiunge TUTTE le variabili necessarie per le verifiche Stata S2-S4:
##   env_good, dirty_p, apec_egl                (classificazioni hs6-livello)
##   TotalDepth_nonEnv, DESTA_depth_index,       (depth controls, country-year)
##     TotalDepth_targeted
##   7 sub-indici EP                             (country-year)
##   in_HS4match, deepshallow, cem_matched       (flag campioni)
##   pd, dt, pt                                  (FE ID interi)
##   EP_share, dose_bin, trend                   (variabili derivate)
## Output: New/Data/Collapsed/collapsed_omnibus.dta
##
## Esecuzione (da PowerShell, root progetto):
##   Rscript New/Code/52_export_collapsed_dta.R

rm(list = ls())
library(here)
library(data.table)
library(fst)
library(haven)
threads_fst(1)

## --- Percorsi ----------------------------------------------------------------
CACHE_FST   <- here("New/Data/Collapsed/panel_pdt_collapsed.fst")
GREEN_FILE  <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE  <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DEPTH_FILE  <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
DESTA_FILE  <- here("New/Data/TotalDepth/desta_depth_country_year.csv")
BYAREA_FILE <- here("New/Data/TotalDepth/wb_totaldepth_byarea_country_year.csv")
IDX_FILE    <- here("Data/Merged/Merged_TREND_WB_Indices_Only.csv")
HS4_FILE    <- here("New/Data/Subsamples/flag_prodHS4.csv")
DS_FILE     <- here("New/Data/Subsamples/flag_deepshallow.csv")
CEM_FILE    <- here("Output/CEM/matched_countries.csv")
OUT_DTA     <- here("New/Data/Collapsed/collapsed_omnibus.dta")

stopifnot("panel_pdt_collapsed.fst non trovato: eseguire prima 10_collapsed_panel.R" = file.exists(CACHE_FST))

## --- Caricamento panel collassato -------------------------------------------
cell <- as.data.table(read_fst(CACHE_FST))
cat("Panel collassato:", format(nrow(cell), big.mark = ","), "celle\n")
cat("Colonne input:", paste(names(cell), collapse = ", "), "\n")

## Guardia dataset stantio: max WB_EP_Depth deve essere 17 (post-fix luglio 2026)
mx <- max(cell$WB_EP_Depth, na.rm = TRUE)
if (mx != 17)
  stop(sprintf("WB_EP_Depth max=%d, atteso 17. Dataset stantio: rigenerare da 10 sulla copia canonica.", mx))

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

## --- TotalDepth_nonEnv -------------------------------------------------------
dep <- fread(DEPTH_FILE)[, .(country_code, year, TotalDepth_nonEnv)]
cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]

## --- DESTA_depth_index (NA permesso: Timor Est e altri non in DESTA) --------
desta <- fread(DESTA_FILE)[, .(country_code, year, DESTA_depth_index)]
cell[desta, on = c("country_code", "year"), DESTA_depth_index := i.DESTA_depth_index]
# NA rimane: in Stata il .do droppa le osservazioni trattate con DESTA mancante

## --- TotalDepth_targeted (14 aree ad alta corr, escl. 3 aree a corr < 0.7) --
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
# prodHS4: hs6 con un codice verde nella stessa HS4
hs4_flag <- fread(HS4_FILE)[, .(hs6, in_HS4match = as.integer(in_HS4match))]
cell[hs4_flag, on = "hs6", in_HS4match := i.in_HS4match]
cell[is.na(in_HS4match), in_HS4match := 0L]

# deepshallow: paesi PTA (deep o shallow) vs controlli
ds_flag  <- fread(DS_FILE)[, .(country_code, group_deepshallow = group)]
cell[ds_flag, on = "country_code", group_deepshallow := i.group_deepshallow]
cell[is.na(group_deepshallow), group_deepshallow := "control"]
# converti in numerico: 1=deep, 2=shallow, 0=control (evita stringhe nel .dta)
cell[, deepshallow := fcase(
  group_deepshallow == "deep",    1L,
  group_deepshallow == "shallow", 2L,
  default = 0L)]
cell[, group_deepshallow := NULL]

# CEM: paesi nel campione CEM v1
cem_cc <- fread(CEM_FILE)$country_code
cell[, cem_matched := as.integer(country_code %in% cem_cc)]

## --- ID FE (interi compatti) ------------------------------------------------
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

## --- Variabili derivate per spec specifiche ----------------------------------
# EP_share: solo per paesi trattati (TotalDepth > 0)
cell[, EP_share := fifelse(TotalDepth_nonEnv > 0, WB_EP_Depth / TotalDepth_nonEnv, NA_real_)]

# Dose bins (come in 16b)
cell[, dose_bin := fcase(
  WB_EP_Depth == 0,                    0L,   # mai trattati
  WB_EP_Depth >= 1 & WB_EP_Depth <= 5, 1L,   # basso
  WB_EP_Depth >= 6 & WB_EP_Depth <= 7, 2L,   # medio
  WB_EP_Depth >= 8,                    3L)]  # alto

# Trend lineare di destinazione (anni dal 2000, per dest-trends)
cell[, trend := as.integer(year) - 2000L]

## --- Riepilogo e scrittura --------------------------------------------------
cat(sprintf("Celle totali: %s\n", format(nrow(cell), big.mark = ",")))
cat(sprintf("green: %.1f%% | dirty: %.1f%% | APEC-green: %.1f%%\n",
            100 * mean(cell$env_good), 100 * mean(cell$dirty_p), 100 * mean(cell$apec_egl)))
cat(sprintf("Trattati (WB_EP_Depth>0): %d country_code\n", uniqueN(cell[WB_EP_Depth > 0, country_code])))
cat(sprintf("deepshallow=1 (deep): %d cc | deepshallow=2 (shallow): %d cc\n",
            uniqueN(cell[deepshallow == 1, country_code]),
            uniqueN(cell[deepshallow == 2, country_code])))
cat(sprintf("cem_matched=1: %d cc\n", uniqueN(cell[cem_matched == 1, country_code])))
cat(sprintf("DESTA NA su trattati: %d celle\n", nrow(cell[WB_EP_Depth > 0 & is.na(DESTA_depth_index)])))
cat("Colonne nel .dta:", paste(names(cell), collapse = ", "), "\n")

dir.create(dirname(OUT_DTA), recursive = TRUE, showWarnings = FALSE)
write_dta(cell, OUT_DTA)
cat("OK:", OUT_DTA, "\n")
