########################################################
###### 35 — Corr. EP vs TotalDepth vs DESTA (double-check §8.9) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.9. Confronta corr(EP, TotalDepth_nonEnv) — stessa fonte WB
## di EP, collinearita' in parte artefatto di database — con corr(EP,
## DESTA_depth_index), da un dataset indipendente (Dur-Baccini-Elsig 2014).
## Se la riduzione e' sostanziale, DESTA sostituisce TotalDepth come
## controllo di robustezza; se no, la collinearita' e' strutturale, non un
## artefatto di misura, e va documentato come tale.
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         New/Data/TotalDepth/desta_depth_country_year.csv (da 32)
## Output: New/Output/Diagnostics/32_desta_check.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(fst)
library(data.table)
library(here)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

DATA_FILE  <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
DESTA_FILE <- here("New/Data/TotalDepth/desta_depth_country_year.csv")
OUT_MD     <- here("New/Output/Diagnostics/32_desta_check.md")
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)
stopifnot("Eseguire prima 32_desta_depth.R" = file.exists(DESTA_FILE))

## --- Caricamento dati ----------------------------------------------------
d <- as.data.table(read_fst(DATA_FILE,
                             columns = c("country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
u <- unique(d, by = c("country_code", "year"))
rm(d); gc()

td <- fread(DEPTH_FILE)
setnames(td, tolower(names(td)))
u <- merge(u, td[, .(country_code, year, totaldepth_nonenv)],
           by = c("country_code", "year"), all.x = TRUE)
u[is.na(totaldepth_nonenv), totaldepth_nonenv := 0]

desta <- fread(DESTA_FILE)
u <- merge(u, desta[, .(country_code, year, DESTA_depth_index)],
           by = c("country_code", "year"), all.x = TRUE)

## Campione: trattati in-sample (WB_EP_Depth > 0), HK+MO esclusi come da spec
trat <- hkmo_filter(u[WB_EP_Depth > 0])
n_all <- nrow(trat)
trat_desta <- trat[!is.na(DESTA_depth_index)]
n_desta <- nrow(trat_desta)
cat(sprintf("Country-year trattati in-sample: %d totali, %d con copertura DESTA (%.1f%%)\n",
            n_all, n_desta, 100 * n_desta / n_all))
if (n_all > n_desta) {
  missing_cc <- unique(trat[!trat_desta, on = .(country_code, year)]$country_code)
  cat("Country_code senza copertura DESTA (verosimilmente East Timor, cc=144):",
      paste(missing_cc, collapse = ", "), "\n")
}

## --- Correlazioni grezze sui trattati con copertura DESTA -------------------
r_raw_wb_td    <- cor(trat_desta$WB_EP_Depth, trat_desta$totaldepth_nonenv)
r_raw_wb_desta <- cor(trat_desta$WB_EP_Depth, trat_desta$DESTA_depth_index)
r_raw_tr_td    <- cor(trat_desta$TREND_EP_Count, trat_desta$totaldepth_nonenv)
r_raw_tr_desta <- cor(trat_desta$TREND_EP_Count, trat_desta$DESTA_depth_index)
r_td_desta     <- cor(trat_desta$totaldepth_nonenv, trat_desta$DESTA_depth_index)

## --- Correlazione within (demeaning paese+anno, come in 14) -----------------
w <- copy(trat_desta)
for (i in 1:10) {
  w[, `:=`(ep_w = WB_EP_Depth - mean(WB_EP_Depth),
           tr_w = TREND_EP_Count - mean(TREND_EP_Count),
           td_w = totaldepth_nonenv - mean(totaldepth_nonenv),
           ds_w = DESTA_depth_index - mean(DESTA_depth_index)), by = country_code]
  w[, `:=`(ep_w = ep_w - mean(ep_w), tr_w = tr_w - mean(tr_w),
           td_w = td_w - mean(td_w), ds_w = ds_w - mean(ds_w)), by = year]
  w[, `:=`(WB_EP_Depth = ep_w, TREND_EP_Count = tr_w,
           totaldepth_nonenv = td_w, DESTA_depth_index = ds_w)]
}
r_win_wb_td    <- cor(w$ep_w, w$td_w)
r_win_wb_desta <- cor(w$ep_w, w$ds_w)
r_win_tr_td    <- cor(w$tr_w, w$td_w)
r_win_tr_desta <- cor(w$tr_w, w$ds_w)

## --- VIF confronto -----------------------------------------------------------
vif_wb_td    <- 1 / (1 - summary(lm(WB_EP_Depth ~ totaldepth_nonenv, trat_desta))$r.squared)
vif_wb_desta <- 1 / (1 - summary(lm(WB_EP_Depth ~ DESTA_depth_index, trat_desta))$r.squared)

reduction_raw  <- r_raw_wb_td - r_raw_wb_desta
reduction_win  <- r_win_wb_td - r_win_wb_desta
esito <- if (abs(reduction_win) < 0.05) "NEGATIVO" else "POSITIVO"

cat(sprintf("\ncorr grezza:  EP-TD=%.3f  EP-DESTA=%.3f  (riduzione %.3f)\n",
            r_raw_wb_td, r_raw_wb_desta, reduction_raw))
cat(sprintf("corr within:  EP-TD=%.3f  EP-DESTA=%.3f  (riduzione %.3f)\n",
            r_win_wb_td, r_win_wb_desta, reduction_win))
cat(sprintf("corr TD-DESTA: %.3f\n", r_td_desta))
cat(sprintf("VIF: EP~TD=%.2f  EP~DESTA=%.2f\n", vif_wb_td, vif_wb_desta))
cat("Esito:", esito, "\n")

## --- Report --------------------------------------------------------------
md <- c(
sprintf("# 8.9 — DESTA depth index come double-check del controllo TotalDepth (%d trattati con copertura DESTA su %d totali)", n_desta, n_all),
"",
"DESTA depth_index (Dur, Baccini & Elsig 2014) copre 7 aree tematiche (beni,",
"servizi, investimenti, standard, appalti, concorrenza, IPR) da un dataset",
"indipendente da quello WB che misura EP — le environmental provisions non",
"rientrano nel conteggio DESTA per costruzione.",
"",
"## Correlazione grezza (country-year trattati)",
"",
"| | WB_EP_Depth vs TotalDepth_nonEnv | WB_EP_Depth vs DESTA_depth_index |",
"|---|---:|---:|",
sprintf("| corr grezza | %.3f | %.3f |", r_raw_wb_td, r_raw_wb_desta),
sprintf("| corr within (FE paese+anno) | %.3f | %.3f |", r_win_wb_td, r_win_wb_desta),
sprintf("| VIF | %.2f | %.2f |", vif_wb_td, vif_wb_desta),
"",
"| | TREND_EP_Count vs TotalDepth_nonEnv | TREND_EP_Count vs DESTA_depth_index |",
"|---|---:|---:|",
sprintf("| corr grezza | %.3f | %.3f |", r_raw_tr_td, r_raw_tr_desta),
sprintf("| corr within (FE paese+anno) | %.3f | %.3f |", r_win_tr_td, r_win_tr_desta),
"",
sprintf("**Correlazione TotalDepth_nonEnv ~ DESTA_depth_index (quanto si somigliano le due misure di profondita' generale): %.3f**", r_td_desta),
"",
"## Esito",
"",
sprintf("Riduzione della correlazione within (WB_EP_Depth): %.3f (soglia di rilevanza: 0,05 in valore assoluto).",
        reduction_win),
"",
if (esito == "NEGATIVO")
  sprintf("**Esito NEGATIVO**: la riduzione (%.3f) e' sotto la soglia di rilevanza. Il DESTA non risolve la collinearita': corr(EP, DESTA) resta alta quanto corr(EP, TotalDepth) sotto FE paese+anno. Conclusione: la collinearita' EP-profondita' e' **strutturale** (gli accordi profondi coprono tipicamente anche le disposizioni ambientali), non un artefatto della codifica del database WB. Nessuna sostituzione di TotalDepth con DESTA nella spec principale.",
        reduction_win)
else
  sprintf("**Esito POSITIVO**: la riduzione (%.3f) supera la soglia di rilevanza. Una parte della collinearita' EP-TotalDepth era effettivamente artefatto di database (stesso codificatore, stesso questionario WB). Prossimo passo: sostituire TotalDepth_nonEnv con DESTA_depth_index in 16_main_tripledd_collapsed.R come robustezza e confrontare SE/coefficienti EP con la spec principale.",
        reduction_win),
"",
sprintf("Copertura: %d/%d (%.1f%%) dei country-year trattati in-sample hanno una codifica DESTA valida (East Timor escluso, non presente nel dataset DESTA dyads).",
        n_desta, n_all, 100 * n_desta / n_all)
)
writeLines(md, OUT_MD)
cat("[OK]", OUT_MD, "\n")
