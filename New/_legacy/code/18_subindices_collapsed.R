########################################################################
###### Fase R4 — Eterogeneità per SOTTO-INDICE EP (panel collassato) ###
########################################################################

## Author: Edoardo Vitella
##
## L'indice aggregato somma clausole CON meccanismo commerciale (green market
## access, standard) e clausole SENZA (cooperazione, regulatory space): il null
## aggregato può nascondere un canale specifico che morde (REPORT §2.2/§3.2).
## Qui la triple-diff del collassato viene ristimata sostituendo all'indice
## aggregato un sotto-indice alla volta:
##   - meccanismo GREEN:  WB_GreenLiberalization, TREND_GreenMarketAccess
##   - meccanismo DIRTY:  WB_StandardsNonRegression
##   - moderatori:        WB_EnforcementDSM, TREND_EnforcementDSM, TREND_Hard
##   - PLACEBO (nessun meccanismo: se "mordono" e' selezione):
##                        TREND_Soft, TREND_RegulatorySpace
## Fonte sotto-indici: Data/Merged/Merged_TREND_WB_Indices_Only.csv (dest-anno).
## Pattern: un sottoprocesso callr per modello (allocatore R fragile).
##
## Output: New/Output/TripleDiff/Tables/subindices_collapsed.csv

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here); library(data.table)

SUBS <- c("WB_GreenLiberalization", "TREND_GreenMarketAccess",
          "WB_StandardsNonRegression",
          "WB_EnforcementDSM", "TREND_EnforcementDSM", "TREND_Hard",
          "TREND_Soft", "TREND_RegulatorySpace")

stima_sub <- function(sub_var) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(4)
  base <- "C:/Work/projects/Paper_PTA"
  cell <- as.data.table(read_fst(file.path(base, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[
    , .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  idx <- fread(file.path(base, "Data/Merged/Merged_TREND_WB_Indices_Only.csv"))
  idx <- idx[, c("country_code", "year", sub_var), with = FALSE]
  setnames(idx, sub_var, "SUB")
  cell[idx, on = c("country_code", "year"), SUB := i.SUB]
  cell[is.na(SUB), SUB := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  m <- feols(y ~ SUB:env_good + SUB:dirty_p +
               TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | pd + dt + pt,
             data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
  list(coefs = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs)
}

rows <- list()
cache_dir <- here("New/Output/TripleDiff/Models_Output")
for (s in SUBS) {
  cat("===", s, "===\n")
  rds <- file.path(cache_dir, sprintf("SUBIDX_%s.rds", s))
  if (file.exists(rds)) {
    r <- readRDS(rds)
  } else {
    r <- tryCatch(callr::r(stima_sub, args = list(sub_var = s)),
                  error = function(e) { cat("[FALLITO]", s, "\n"); NULL })
    if (!is.null(r)) saveRDS(r, rds)
  }
  if (!is.null(r)) {
    for (term in c("SUB:env_good", "SUB:dirty_p")) {
      cat(sprintf("  %s %-12s: %+.5f (p=%.4f)\n", s,
                  sub("SUB:", "x ", term), r$coefs[[term]], r$pval[[term]]))
    }
    rows[[s]] <- data.table(sub_index = s, term = names(r$coefs), coef = r$coefs,
                            se = r$se, pval = r$pval, nobs = r$nobs)
  }
}
out <- rbindlist(rows)
fwrite(out, here("New/Output/TripleDiff/Tables/subindices_collapsed.csv"))
cat("\n[OK] subindices_collapsed.csv\n")
