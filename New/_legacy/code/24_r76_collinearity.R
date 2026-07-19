########################################################################
###### R7.6 — Collinearita' EP_dt vs TotalDepth_dt (Major 4)         ###
########################################################################

## Author: Edoardo Vitella
##
## Il referee chiede: quanto sono correlate la profondita' ambientale e la
## profondita' non-ambientale a livello destinazione-anno? Se ~1, il controllo
## TD assorbe quasi tutta la variazione e il "precise null" va qualificato.
## Calcoli sul pannello destinazione-anno (223 country-year trattati in-sample,
## HK+MO esclusi): correlazione grezza, correlazione within (demeaning per
## paese e anno, la variazione che il triple-diff usa davvero), VIF.
##
## Output: New/Output/Diagnostics/r76_collinearity.md

library(fst); library(data.table)
threads_fst(1)

d <- as.data.table(read_fst("C:/Work/projects/Paper_PTA/Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst",
                             columns = c("country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
u <- unique(d, by = c("country_code", "year"))
rm(d); gc()

td <- fread("C:/Work/projects/Paper_PTA/New/Data/TotalDepth/wb_totaldepth_country_year.csv")
setnames(td, tolower(names(td)))
u <- merge(u, td[, .(country_code, year, totaldepth_nonenv)],
           by = c("country_code", "year"), all.x = TRUE)
u[is.na(totaldepth_nonenv), totaldepth_nonenv := 0]

trat <- u[WB_EP_Depth > 0 & !country_code %in% c(110L, 121L)]
cat("Country-year trattati in-sample (HK+MO esclusi):", nrow(trat), "\n\n")

## 1. correlazione grezza sui trattati
r_raw_wb <- cor(trat$WB_EP_Depth, trat$totaldepth_nonenv)
r_raw_tr <- cor(trat$TREND_EP_Count, trat$totaldepth_nonenv)

## 2. correlazione within (demeaning per paese e per anno, iterato 2 volte:
##    approssima la variazione residua sotto FE paese+anno)
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

## 3. VIF dalla regressione EP ~ TD (grezza, sui trattati)
vif_wb <- 1 / (1 - summary(lm(WB_EP_Depth ~ totaldepth_nonenv, trat))$r.squared)
vif_tr <- 1 / (1 - summary(lm(TREND_EP_Count ~ totaldepth_nonenv, trat))$r.squared)

out <- sprintf(
"# R7.6 — Collinearita' EP vs TotalDepth (destinazione-anno, %d trattati in-sample)

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
writeLines(out, "C:/Work/projects/Paper_PTA/New/Output/Diagnostics/r76_collinearity.md")
cat("[OK] r76_collinearity.md\n")
