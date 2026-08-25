########################################################
###### 64 — Export griglia zero-fill PPML: TUTTE le varianti -> .dta  ###
########################################################
## Author: Edoardo Vitella
## Prerequisito: ppml_agg_pdt_zerofill.fst (da 29b_build_ppml_zerofill.R)
##
## PERCHE' ESISTE. `55_export_ppml_dta.R` esporta la sola variante baseline:
## esclude HK/Macao e porta il solo controllo TotalDepth. Le tabelle di
## `Tabelle_Stime.pdf` hanno pero' la matrice 2x2 campione x profondita'.
##
## SCELTA DI DISEGNO. Non servono quattro file: la griglia zero-fill di partenza
## CONTIENE gia' Hong Kong e Macao (e' 55 a toglierli a valle) e gli ID delle FE
## sono calcolati sulla griglia piena. Basta quindi un solo .dta che porti
##   - un flag `hkmo` (1 = Hong Kong o Macao) per selezionare il campione
##   - entrambe le misure di profondita' come colonne
## e il do-file Stata sceglie la variante. Nessuna griglia va ricostruita.
##
## Output: New/Data/Collapsed/ppml_zerofill_all.dta
##
## Esecuzione:
##   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New/Code/64_export_ppml_variants_dta.R

rm(list = ls())
library(here)
library(data.table)
library(fst)
library(haven)
threads_fst(1)

PPML_FST   <- here("Data/Final Dataset/ppml_agg_pdt_zerofill.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
DESTA_FILE <- here("New/Data/TotalDepth/desta_depth_country_year.csv")
OUT_DTA    <- here("New/Data/Collapsed/ppml_zerofill_all.dta")

stopifnot("ppml_agg_pdt_zerofill.fst non trovato: eseguire prima 29b." = file.exists(PPML_FST))

d <- as.data.table(read_fst(PPML_FST, columns = c(
  "agg_export", "hs6", "country_code", "year",
  "WB_EP_Depth", "TREND_EP_Count", "pd", "dt", "pt")))
cat("Celle zerofill (griglia piena):", format(nrow(d), big.mark = ","), "\n")
cat("Quota zeri:", sprintf("%.1f%%", 100 * mean(d$agg_export == 0)), "\n")

## Flag HK/Macao invece dell'esclusione: la selezione la fa il do-file
d[, hkmo := as.integer(country_code %in% c(110L, 121L))]
cat(sprintf("celle HK/Macao: %s (%.2f%%)\n",
            format(sum(d$hkmo), big.mark = ","), 100 * mean(d$hkmo)))

## env_good ricalcolato da green_codes_hs1996.csv (come in 30 e in 55)
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
green_codes <- unique(green$hs6_final)
d[, hs6_str  := sprintf("%06d", as.integer(hs6))]
d[, env_good := as.integer(hs6_str %in% green_codes)]
d[, hs6_str  := NULL]
cat(sprintf("env_good: %.1f%% celle green\n", 100 * mean(d$env_good)))

## dirty_p
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = as.integer(dirty))]
d[dirty, on = "hs6", dirty_p := i.dirty_p]
d[is.na(dirty_p), dirty_p := 0L]
cat(sprintf("dirty_p: %.1f%% celle dirty\n", 100 * mean(d$dirty_p)))

## TotalDepth_nonEnv
dep <- fread(DEPTH_FILE)[, .(country_code, year, TotalDepth_nonEnv)]
d[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
d[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]

## DESTA_depth_index (NA lasciato: il do-file droppa i trattati non coperti)
desta <- fread(DESTA_FILE)[, .(country_code, year, DESTA_depth_index)]
d[desta, on = c("country_code", "year"), DESTA_depth_index := i.DESTA_depth_index]
cat(sprintf("DESTA NA su celle trattate: %s\n",
            format(nrow(d[WB_EP_Depth > 0 & is.na(DESTA_depth_index)]), big.mark = ",")))

## Guardia: WB_EP_Depth max deve essere 17
mx <- max(d$WB_EP_Depth, na.rm = TRUE)
if (mx != 17) stop(sprintf("WB_EP_Depth max=%d, atteso 17. Dataset stantio.", mx))

## Guardia: escludendo HK/Macao si devono ritrovare le celle del baseline (55).
## NB: 8.310.464 e' la griglia PIENA; il campione baseline (escl. HK/Macao) e'
## 8.179.904, ed e' da li' che la stima PPML scende a 7.895.543 dopo la rimozione
## iterativa dei singleton. La tabella descrittiva del paper riportava per errore
## la griglia piena in una colonna dichiarata "HK e Macao esclusi": corretto il
## 2026-08-25.
n_excl <- nrow(d[hkmo == 0])
cat(sprintf("celle escl. HK/Macao: %s (atteso 8,179,904 = il .dta di 55)\n",
            format(n_excl, big.mark = ",")))
if (n_excl != 8179904)
  warning(sprintf("celle escl. HK/Macao = %d, attese 8,179,904: verificare la griglia.", n_excl))

cat("Colonne nel .dta:", paste(names(d), collapse = ", "), "\n")
write_dta(d, OUT_DTA)
cat("OK:", OUT_DTA, "—", format(nrow(d), big.mark = ","), "celle\n")
