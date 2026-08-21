## 48e — Esporta full panel trimmato come .dta per Stata boottest
## Prerequisito: tmp_trim_fullpanel.fst (scritto da 46b2_wcb_fullpanel_rerun.R)
## Output: New/Data/Collapsed/tmp_check_trim_fullpanel.dta

rm(list = ls()); gc()
library(here); library(data.table); library(fst); library(haven)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

stopifnot(SAMPLE == "excl", DEPTH == "totaldepth")

DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
IN_FST     <- here("New/Data/Collapsed/tmp_trim_fullpanel.fst")
OUT_DTA    <- here("New/Data/Collapsed/tmp_check_trim_fullpanel.dta")

stopifnot(file.exists(IN_FST))

green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
dirty     <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
dep       <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]

cat("Leggo FST...\n")
d <- as.data.table(read_fst(IN_FST))

## Interazioni (stesso schema di 48_trim_export_dta.R per il collassato)
cat("Costruisco interazioni...\n")
d[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% green_set)]
d[dirty, on = "hs6", dirty_p := i.dirty_p]
d[is.na(dirty_p), dirty_p := 0L]
d[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
d[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]

d[, wb_green := WB_EP_Depth  * env_good]
d[, wb_dirty := WB_EP_Depth  * dirty_p]
d[, tr_green := TREND_EP_Count * env_good]
d[, tr_dirty := TREND_EP_Count * dirty_p]
d[, td_green := get(DEPTH_VAR)  * env_good]
d[, td_dirty := get(DEPTH_VAR)  * dirty_p]

## Teniamo solo le colonne che servono a Stata
keep <- c("ln_export", "wb_green", "wb_dirty", "tr_green", "tr_dirty",
          "td_green", "td_dirty", "pd", "dt", "pt", "country_code")
d <- d[, ..keep]
setnames(d, "ln_export", "y")

cat(sprintf("N = %s | Scrivo .dta...\n", format(nrow(d), big.mark = ",")))
write_dta(d, OUT_DTA)
cat(sprintf("[OK] %s\n", basename(OUT_DTA)))
