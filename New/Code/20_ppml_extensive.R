########################################################################
###### Fase R4 — PPML aggregato CON ZERI (margine estensivo)         ###
########################################################################

## Author: Edoardo Vitella
##
## L'OLS su ln(export) vede solo i flussi positivi (margine intensivo).
## L'eventuale "green trade creation" — nuove combinazioni prodotto-mercato
## che nascono grazie all'accordo — vive nel margine estensivo: serve PPML
## su un panel con gli ZERI (Santos Silva & Tenreyro 2006).
## Usa il file gia' pronto Data/Final Dataset/ppml_agg_pdt_zerofill.fst
## (8,3M celle hs6 x dest x anno, zero-filled, FE precalcolate pd/dt/pt).
## Igiene consueta: env_good ricalcolato HS1996, dirty da 05, TotalDepth da 06;
## HK+MO esclusi. Un sottoprocesso callr per modello (allocatore fragile).
##
## Output: New/Output/TripleDiff/Tables/ppml_extensive.csv

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here); library(data.table)

stima_ppml <- function(tr) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(4)
  base <- "C:/Work/projects/Paper_PTA"
  d <- as.data.table(read_fst(file.path(base, "Data/Final Dataset/ppml_agg_pdt_zerofill.fst"),
    columns = c("agg_export", "hs6", "country_code", "year",
                "WB_EP_Depth", "TREND_EP_Count", "pd", "dt", "pt")))
  d <- d[!country_code %in% c(110, 121)]
  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  d[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[
    , .(hs6 = as.integer(hs6), dirty_p = dirty)]
  d[dirty, on = "hs6", dirty_p := i.dirty_p]; d[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  d[, country_code := as.integer(country_code)]
  d[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  d[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cat(sprintf("[%s] celle: %s | quota zeri: %.1f%%\n", tr,
              format(nrow(d), big.mark = ","), 100 * mean(d$agg_export == 0)))
  f <- sprintf("agg_export ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt", tr, tr)
  m <- fepois(as.formula(f), data = d, cluster = ~country_code, lean = TRUE)
  list(coefs = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs)
}

rows <- list()
cache_dir <- here("New/Output/TripleDiff/Models_Output")
for (tr in c("WB_EP_Depth", "TREND_EP_Count")) {
  tr_name <- if (tr == "WB_EP_Depth") "WB" else "TREND"
  cat("=== PPML", tr_name, "===\n")
  rds <- file.path(cache_dir, sprintf("PPML_ext_%s.rds", tr_name))
  if (file.exists(rds)) { r <- readRDS(rds) } else {
    r <- tryCatch(callr::r(stima_ppml, args = list(tr = tr)),
                  error = function(e) { cat("[FALLITO]", tr_name, "\n"); NULL })
    if (!is.null(r)) saveRDS(r, rds)
  }
  if (!is.null(r)) {
    print(round(cbind(coef = r$coefs, pval = r$pval), 5))
    rows[[tr_name]] <- data.table(treat = tr_name, term = names(r$coefs),
                                  coef = r$coefs, se = r$se, pval = r$pval, nobs = r$nobs)
  }
}
if (length(rows)) {
  fwrite(rbindlist(rows), here("New/Output/TripleDiff/Tables/ppml_extensive.csv"))
  cat("[OK] ppml_extensive.csv\n")
}
