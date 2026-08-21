########################################################
###### 48 — Export dati trimmati/decomp a .dta per verifica Stata ###
########################################################
## Author: Edoardo Vitella
##
## Cosa fa: replica ESATTAMENTE la data prep di 46 (Part A) e 47 (Part A)
## ed esporta i 3 dataset collassati in .dta per reghdfe Stata.
## ZERO stime R: nessun feols, nessun rischio corruzione.
##
## Output:
##   New/Data/Collapsed/tmp_check_trim.dta
##   New/Data/Collapsed/tmp_check_decomp_qua.dta
##   New/Data/Collapsed/tmp_check_decomp_uv.dta

rm(list = ls())
gc()
library(here)
library(data.table)
library(fst)
library(haven)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

stopifnot("config errata: SAMPLE deve essere excl"       = SAMPLE == "excl")
stopifnot("config errata: DEPTH deve essere totaldepth"  = DEPTH  == "totaldepth")

GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DATA_FST   <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
OUT_DIR    <- here("New/Data/Collapsed")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
dirty     <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
dep       <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]

## ============================================================
## 1. TRIM collassato (replicare 46 Part A righe 76-103)
## ============================================================
cat("\n=== 1. Trim collassato ===\n")
CACHE_FST <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
stopifnot("panel_pdt_collapsed.fst non trovato" = file.exists(CACHE_FST))
cell <- as.data.table(read_fst(CACHE_FST))
stopifnot("Dataset stantio: max(WB_EP_Depth) != 17" = max(cell$WB_EP_Depth, na.rm = TRUE) == 17)

cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% green_set)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
if (DEPTH_DROP_UNMEASURED) {
  n0   <- nrow(cell)
  cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
  cat(sprintf("[depth] %d celle escluse\n", n0 - nrow(cell)))
}
cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

n_pre <- nrow(cell)
q01   <- quantile(cell$y, 0.01)
q99   <- quantile(cell$y, 0.99)
cell  <- cell[y >= q01 & y <= q99]
cat(sprintf("Trim [%.4f, %.4f]: %d -> %d celle (-%d)\n",
            q01, q99, n_pre, nrow(cell), n_pre - nrow(cell)))

cell[, `:=`(
  wb_green = WB_EP_Depth      * env_good,
  wb_dirty = WB_EP_Depth      * dirty_p,
  tr_green = TREND_EP_Count   * env_good,
  tr_dirty = TREND_EP_Count   * dirty_p,
  td_green = get(DEPTH_VAR)   * env_good,
  td_dirty = get(DEPTH_VAR)   * dirty_p
)]

out <- cell[, .(y, n, wb_green, wb_dirty, tr_green, tr_dirty, td_green, td_dirty,
                pd, dt, pt, country_code)]
haven::write_dta(as.data.frame(out), file.path(OUT_DIR, "tmp_check_trim.dta"))
cat(sprintf("Scritto: tmp_check_trim.dta (%d righe)\n", nrow(out)))
rm(cell, out); gc()

## ============================================================
## 2. DECOMP collassato — ln_export_qua e ln_export_value
##    (replicare 47 Part A righe 80-103)
## ============================================================
for (oc in c("ln_export_qua", "ln_export_value")) {
  label <- if (oc == "ln_export_qua") "qua" else "uv"
  cat(sprintf("\n=== 2. Decomp collassato: %s ===\n", oc))

  d_raw <- as.data.table(read_fst(DATA_FST, columns = c(
    oc, "hs6", "country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
  if (HKMO_DROP) d_raw <- d_raw[!country_code %in% HKMO_CODES]
  d_raw <- d_raw[!is.na(get(oc))]
  stopifnot("Dataset stantio: max(WB_EP_Depth) != 17" = max(d_raw$WB_EP_Depth, na.rm = TRUE) == 17)

  cell <- d_raw[, .(y = mean(get(oc)), n = .N,
                    WB_EP_Depth = first(WB_EP_Depth), TREND_EP_Count = first(TREND_EP_Count)),
                by = .(hs6, country_code, year)]
  rm(d_raw); gc()

  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% green_set)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]
  cell[is.na(dirty_p), dirty_p := 0L]
  cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
  if (DEPTH_DROP_UNMEASURED) cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
  cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]

  cell[, `:=`(
    wb_green = WB_EP_Depth      * env_good,
    wb_dirty = WB_EP_Depth      * dirty_p,
    tr_green = TREND_EP_Count   * env_good,
    tr_dirty = TREND_EP_Count   * dirty_p,
    td_green = get(DEPTH_VAR)   * env_good,
    td_dirty = get(DEPTH_VAR)   * dirty_p
  )]

  out <- cell[, .(y, n, wb_green, wb_dirty, tr_green, tr_dirty, td_green, td_dirty,
                  pd, dt, pt, country_code)]
  fname <- sprintf("tmp_check_decomp_%s.dta", label)
  haven::write_dta(as.data.frame(out), file.path(OUT_DIR, fname))
  cat(sprintf("Scritto: %s (%d righe)\n", fname, nrow(out)))
  rm(cell, out); gc()
}

cat("\n=== FATTO. 3 file .dta pronti in New/Data/Collapsed/ ===\n")
cat("Passo successivo: aprire Stata e lanciare New/Code/stata/48_trim_check.do\n")
