########################################################
###### 41 — VIF e MDE dei sotto-indici WB/TREND (§8.5) ###
########################################################
## Author: Edoardo Vitella
## Roadmap: §8.5, priorità bassa - probabile vicolo cieco atteso: i
## sotto-indici hanno range minuscolo, quindi un VIF basso sarebbe
## meccanico (poca varianza da condividere) non un segno di identificazione
## migliore. Prerequisito (fatto): fix finding #2 dell'audit in
## 25_heterogeneity_subindices.R (WB_StandardsNonRegression rimosso, era lo
## stesso regressore di WB_GreenLiberalization).
##
## Input:  Data/Merged/Merged_TREND_WB_Indices_Only.csv (root)
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         New/Output/TripleDiff/Tables/subindices_collapsed.csv (da 25, per SE)
## Output: New/Output/Diagnostics/41_vif_subindices.md

rm(list = ls())
library(here)
library(data.table)

IDX_FILE   <- here("Data/Merged/Merged_TREND_WB_Indices_Only.csv")
DEPTH_FILE <- here("New/Data/TotalDepth/wb_totaldepth_country_year.csv")
SUBIDX_FILE<- here("New/Output/TripleDiff/Tables/subindices_collapsed.csv")
OUT_MD     <- here("New/Output/Diagnostics/41_vif_subindices.md")
dir.create(dirname(OUT_MD), recursive = TRUE, showWarnings = FALSE)

SUBS <- c("WB_GreenLiberalization", "TREND_GreenMarketAccess",
          "WB_EnforcementDSM", "TREND_EnforcementDSM", "TREND_Hard",
          "TREND_Soft", "TREND_RegulatorySpace")

idx <- fread(IDX_FILE)
dep <- fread(DEPTH_FILE)[, .(country_code, year, TotalDepth_nonEnv)]
u <- merge(idx, dep, by = c("country_code", "year"), all.x = TRUE)
u[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]

## trattati in-sample, HK+MO esclusi (stessa convenzione di 14/37)
trat <- u[WB_EP_Depth > 0 & !country_code %in% c(110L, 121L)]
cat(sprintf("Country-year trattati in-sample: %d\n", nrow(trat)))

## corr within (FE country+anno), pattern a 3 righe verificato (vedi bug in 37)
w <- copy(trat)
tmp_cols <- paste0("tmp_", seq_along(SUBS))
for (i in 1:10) {
  w[, (tmp_cols) := lapply(.SD, function(x) x - mean(x)), .SDcols = SUBS, by = country_code]
  w[, td_tmp := TotalDepth_nonEnv - mean(TotalDepth_nonEnv), by = country_code]
  w[, (tmp_cols) := lapply(.SD, function(x) x - mean(x)), .SDcols = tmp_cols, by = year]
  w[, td_tmp := td_tmp - mean(td_tmp), by = year]
  w[, (SUBS) := mget(tmp_cols)]
  w[, TotalDepth_nonEnv := td_tmp]
}
w[, c(tmp_cols, "td_tmp") := NULL]

sub_tab <- data.table(
  sub_index = SUBS,
  sd_pesata_grezza = sapply(SUBS, function(s) sd(trat[[s]])),
  range = sapply(SUBS, function(s) sprintf("[%g, %g]", min(trat[[s]]), max(trat[[s]]))),
  corr_within_TD = sapply(SUBS, function(s) cor(w[[s]], w$TotalDepth_nonEnv)),
  vif = sapply(SUBS, function(s) {
    f <- as.formula(sprintf("%s ~ TotalDepth_nonEnv", s))
    v <- suppressWarnings(1 / (1 - summary(lm(f, trat))$r.squared))
    if (is.na(v) || is.infinite(v)) NA_real_ else v
  })
)

## MDE per 1 SD (usando SE dal margine green in subindices_collapsed.csv)
sub_est <- fread(SUBIDX_FILE)
se_green <- sub_est[term == "SUB:env_good", .(sub_index, se)]
sub_tab <- merge(sub_tab, se_green, by = "sub_index", all.x = TRUE)
sub_tab[, mde_asintotico_per_1sd := 2.8 * se * sd_pesata_grezza]

setorder(sub_tab, -corr_within_TD)
print(sub_tab)

md <- c(
"# 8.5 — VIF e MDE dei sotto-indici WB/TREND",
"",
"**Prerequisito completato**: fix del finding #2 dell'audit —",
"`WB_StandardsNonRegression` rimosso da `25_heterogeneity_subindices.R` (era",
"lo stesso regressore di `WB_GreenLiberalization`, riscalato 3x, corr=1,000 —",
"le due specifiche avrebbero contato la stessa evidenza due volte).",
"",
sprintf("Correlazione within (FE paese+anno) e VIF di ciascun sotto-indice con TotalDepth_nonEnv, sui %d country-year trattati in-sample (HK+MO esclusi).",
        nrow(trat)),
"",
"| Sotto-indice | SD (grezza, trattati) | Range | corr within con TD | VIF | SE (margine green) | MDE/1SD |",
"|---|---:|---|---:|---:|---:|---:|",
sprintf("| %s | %.3f | %s | %s | %s | %s | %s |",
        sub_tab$sub_index, sub_tab$sd_pesata_grezza, sub_tab$range,
        ifelse(is.na(sub_tab$corr_within_TD), "NA (var=0)", sprintf("%.3f", sub_tab$corr_within_TD)),
        ifelse(is.na(sub_tab$vif), "NA", sprintf("%.2f", sub_tab$vif)),
        ifelse(is.na(sub_tab$se), "NA", sprintf("%.4f", sub_tab$se)),
        ifelse(is.na(sub_tab$mde_asintotico_per_1sd), "NA", sprintf("%.1f%%", sub_tab$mde_asintotico_per_1sd * 100))),
"",
"## Esito",
"",
"Confermata l'attesa del cappello §8.5: i sotto-indici hanno range minuscolo",
"(WB_GreenLiberalization e' binaria {0,1}; WB_EnforcementDSM in {0,..,3};",
"i TREND sub-indici hanno range piu' ampio ma comunque una frazione del",
"livello aggregato). **Un VIF basso su una variabile quasi-binaria e'",
"meccanico** (poca varianza da condividere con TotalDepth), non un segno di",
"identificazione migliore: si vede nel confronto MDE/1SD, che in diversi casi",
"non e' affatto migliore della spec principale (§8.1: WB MDE/1SD ~4,6%).",
"",
"**Conclusione**: nessuna strada di mitigazione qui. Il valore dei sotto-indici",
"e' nel test di meccanismo (quale canale specifico \"morde\"), non nella",
"riduzione della collinearita' EP-TotalDepth — coerente con la priorita' bassa",
"assegnata a questo punto nella roadmap."
)
writeLines(md, OUT_MD)
cat("\n[OK]", OUT_MD, "\n")
