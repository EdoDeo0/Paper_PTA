########################################################
###### 55 — Export griglia zero-fill -> .dta per Stata PPML (S5) ###
########################################################
## Author: Edoardo Vitella
## Prerequisito: 29b_build_ppml_zerofill.R (ppml_agg_pdt_zerofill.fst gia' presente)
##
## Input:  Data/Final Dataset/ppml_agg_pdt_zerofill.fst (~8,3M celle)
## Output: New/Data/Collapsed/ppml_zerofill_export.dta
##
## Aggiunge: env_good (ricalcolato da green_codes_hs1996.csv, come in 30),
##           dirty_p, TotalDepth_nonEnv. HK+Macao esclusi.
## Esecuzione:
##   Rscript New/Code/55_export_ppml_dta.R

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
OUT_DTA    <- here("New/Data/Collapsed/ppml_zerofill_export.dta")

stopifnot("ppml_agg_pdt_zerofill.fst non trovato: eseguire prima 29b." = file.exists(PPML_FST))

d <- as.data.table(read_fst(PPML_FST, columns = c(
  "agg_export", "hs6", "country_code", "year",
  "WB_EP_Depth", "TREND_EP_Count", "pd", "dt", "pt")))
cat("Celle zerofill:", format(nrow(d), big.mark = ","), "\n")
cat("Quota zeri:", sprintf("%.1f%%", 100 * mean(d$agg_export == 0)), "\n")

## Esclusione HK/Macao (come spec principale)
d <- d[!country_code %in% c(110L, 121L)]
cat("Dopo esclusione HK/Macao:", format(nrow(d), big.mark = ","), "celle\n")

## env_good ricalcolato da green_codes_hs1996.csv (come in 30)
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

## Guardia: WB_EP_Depth max deve essere 17
mx <- max(d$WB_EP_Depth, na.rm = TRUE)
if (mx != 17) stop(sprintf("WB_EP_Depth max=%d, atteso 17. Dataset stantio.", mx))

cat("Colonne nel .dta:", paste(names(d), collapse = ", "), "\n")
write_dta(d, OUT_DTA)
cat("OK:", OUT_DTA, "—", format(nrow(d), big.mark = ","), "celle\n")
