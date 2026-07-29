########################################################
###### 10 — Collinearita' EP vs TotalDepth              ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 24_r76_collinearity.R. Run: ~30s.
##
## Cosa fa: quanto sono correlate la profondita' ambientale (EP) e la
## profondita' non-ambientale (TotalDepth) a livello destinazione-anno? Se
## ~1, il controllo TotalDepth assorbirebbe quasi tutta la variazione e il
## "precise null" della triple-diff andrebbe qualificato. Calcoli sul
## pannello destinazione-anno (paesi trattati in-sample, HK+MO esclusi):
## correlazione grezza, correlazione within (demeaning per paese e anno -
## la variazione che il triple-diff usa davvero), VIF.
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv (da 04)
## Output: New/Output/Diagnostics/14_descriptives_collinearity.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(fst)
library(data.table)
library(here)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

## --- Parametri e percorsi --------------------------------------------------
DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
OUT_DIR <- here("New/Output/Diagnostics")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Caricamento dati ----------------------------------------------------
d <- as.data.table(read_fst(DATA_FILE,
                             columns = c("country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
u <- unique(d, by = c("country_code", "year"))
rm(d)
gc()

td <- fread(DEPTH_FILE)
setnames(td, tolower(names(td)))
u <- merge(u, td[, .(country_code, year, totaldepth_nonenv)],
           by = c("country_code", "year"), all.x = TRUE)
u[is.na(totaldepth_nonenv), totaldepth_nonenv := 0]

trat <- hkmo_filter(u[WB_EP_Depth > 0])
cat(sprintf("Country-year trattati in-sample (HK+MO %s): %d\n\n",
            if (HKMO_DROP) "esclusi" else "inclusi", nrow(trat)))

## --- Sezione 1: correlazione grezza sui trattati ---------------------------
r_raw_wb <- cor(trat$WB_EP_Depth, trat$totaldepth_nonenv)
r_raw_tr <- cor(trat$TREND_EP_Count, trat$totaldepth_nonenv)

## --- Sezione 2: correlazione within (demeaning paese+anno) -----------------
# demeaning alternato, 10 iterazioni: approssima la variazione residua sotto
# FE paese+anno, la stessa che il triple-diff usa interagita con green/dirty
w <- copy(trat)
for (i in 1:10) {
  w[, `:=`(ep_w = WB_EP_Depth - mean(WB_EP_Depth),
           tr_w = TREND_EP_Count - mean(TREND_EP_Count),
           td_w = totaldepth_nonenv - mean(totaldepth_nonenv)), by = country_code]
  w[, `:=`(ep_w = ep_w - mean(ep_w), tr_w = tr_w - mean(tr_w),
           td_w = td_w - mean(td_w)), by = year]
  w[, `:=`(WB_EP_Depth = ep_w, TREND_EP_Count = tr_w, totaldepth_nonenv = td_w)]
}
r_win_wb <- cor(w$ep_w, w$td_w)
r_win_tr <- cor(w$tr_w, w$td_w)

## --- Sezione 3: VIF dalla regressione EP ~ TD (grezza, sui trattati) -------
vif_wb <- 1 / (1 - summary(lm(WB_EP_Depth ~ totaldepth_nonenv, trat))$r.squared)
vif_tr <- 1 / (1 - summary(lm(TREND_EP_Count ~ totaldepth_nonenv, trat))$r.squared)

## --- Sezione 4: salvataggio report -------------------------------------------
out <- sprintf(
"# 10 - Collinearita' EP vs TotalDepth (destinazione-anno, %d trattati in-sample)

|                         | WB_EP_Depth | TREND_EP_Count |
|-------------------------|------------:|---------------:|
| corr grezza con TD      | %.3f        | %.3f           |
| corr within (FE c+t)    | %.3f        | %.3f           |
| VIF (da regressione su TD) | %.2f     | %.2f           |

Nota: la corr within approssima la variazione residua sotto FE paese+anno
(demeaning alternato, 10 iterazioni). Il triple-diff usa questa variazione
interagita con green/dirty.
", nrow(trat), r_raw_wb, r_raw_tr, r_win_wb, r_win_tr, vif_wb, vif_tr)

cat(out)
writeLines(out, out_path(file.path(OUT_DIR, "14_descriptives_collinearity.md")))
cat("[OK] 14_descriptives_collinearity.md\n")
