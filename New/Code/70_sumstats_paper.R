################################################################################
## 70_sumstats_paper.R — Genera le summary statistics per il paper
##
## Ricalcola env_good e dirty_p dalle LISTE CANONICHE (le stesse usate dalle
## stime), NON dalle colonne stantie del .fst/.dta originale.
## Output: sumstats_fullpanel.csv, sumstats_fullpanel_exHKMO.csv,
##         sumstats_collapsed.csv  (in New/Paper/paper_v3/)
################################################################################

library(here)
library(fst)
library(data.table)

## --- Liste canoniche --------------------------------------------------------
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")

green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
green_codes <- unique(green$hs6_final)

dirty <- fread(DIRTY_FILE)
dirty_codes <- unique(as.character(dirty[dirty == 1L, hs6]))

## --- Full panel -------------------------------------------------------------
FST_PATH <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
if (!file.exists(FST_PATH)) stop("File .fst non trovato: ", FST_PATH)

cols <- c("ln_export", "ln_export_qua", "ln_export_value",
          "hs6", "country_code", "year",
          "WB_EP_Depth", "TREND_EP_Count",
          "tariffs", "ln_hhi_baci")

dt <- as.data.table(read_fst(FST_PATH, columns = cols))

## Ricalcola green e dirty dalla lista canonica (come fa 52_export_collapsed_dta.R)
dt[, hs6_str := sprintf("%06d", as.integer(hs6))]
dt[, env_good := as.integer(hs6_str %in% green_codes)]
dt[, dirty_p  := as.integer(hs6_str %in% dirty_codes)]
dt[, hs6_str  := NULL]

## Funzione per calcolare le sumstats
calc_ss <- function(d) {
  vars <- c("ln_export", "ln_export_qua", "ln_export_value",
            "env_good", "dirty_p",
            "WB_EP_Depth", "TREND_EP_Count",
            "tariffs", "ln_hhi_baci")
  rbindlist(lapply(vars, function(v) {
    if (is.null(d[[v]])) return(data.table(variable = v, N = 0L,
      mean = NA_real_, median = NA_real_, sd = NA_real_,
      min = NA_real_, max = NA_real_))
    x_ok <- d[[v]][!is.na(d[[v]])]
    if (length(x_ok) == 0L) return(data.table(variable = v, N = 0L,
      mean = NA_real_, median = NA_real_, sd = NA_real_,
      min = NA_real_, max = NA_real_))
    data.table(
      variable = v,
      N        = length(x_ok),
      mean     = mean(x_ok),
      median   = median(x_ok),
      sd       = sd(x_ok),
      min      = min(x_ok),
      max      = max(x_ok)
    )
  }))
}

ss_full      <- calc_ss(dt)
ss_full_exHK <- calc_ss(dt[!(country_code %in% c(110L, 121L))])

OUT_DIR <- here("New/Paper/paper_v3")
fwrite(ss_full,      file.path(OUT_DIR, "sumstats_fullpanel.csv"))
fwrite(ss_full_exHK, file.path(OUT_DIR, "sumstats_fullpanel_exHKMO.csv"))

## --- Pannello collassato ----------------------------------------------------
COLL_PATH <- here("New/Data/Collapsed/collapsed_omnibus.dta")
if (file.exists(COLL_PATH)) {
  library(haven)
  coll <- as.data.table(read_dta(COLL_PATH))
  coll[, hs6_str := sprintf("%06d", as.integer(hs6))]
  coll[, env_good := as.integer(hs6_str %in% green_codes)]
  coll[, dirty_p  := as.integer(hs6_str %in% dirty_codes)]
  coll[, hs6_str  := NULL]
  ss_coll <- calc_ss(coll)
  fwrite(ss_coll, file.path(OUT_DIR, "sumstats_collapsed.csv"))
  cat("[collapsed] scritto.\n")
} else {
  cat("[collapsed] file non trovato:", COLL_PATH, "— saltato.\n")
}

## --- Controllo vs 17.do -----------------------------------------------------
N_exHK <- nrow(dt[!(country_code %in% c(110L, 121L))])
pct_green <- ss_full_exHK[variable == "env_good", mean] * 100
pct_dirty <- ss_full_exHK[variable == "dirty_p",  mean] * 100

cat(sprintf(
  "\nCONTROLLO vs 17.do (atteso: righe 45781211, green 11.5%%, dirty 7.0%%)\n  righe: %d | green: %.1f%% | dirty: %.1f%%\n",
  N_exHK, pct_green, pct_dirty
))
