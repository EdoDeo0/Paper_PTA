########################################################################
###### Fase R3 — Retry full-panel triple-diff, sessione DIRETTA      ###
########################################################################

## Author: Edoardo Vitella
##
## PERCHÉ: 07_triple_diff.R (sezione A via callr) muore con "recursive gc
## invocation" sul demeaning di fpd+fdt+pt a 45,8M righe (2026-07-06).
## Precedente rilevante (session-log 2026-06-11): lo stesso identico crash
## su fpd+pt via callr fu risolto eseguendo lo script IN SESSIONE DIRETTA
## (01c_fpd_pt.R, senza sottoprocesso). Questo script ritenta così:
##   - nessun callr: tutto nella sessione principale di Rscript
##   - UN SOLO modello (WB x ln_export x base): test di fattibilità
##   - mem.clean = TRUE, gc() espliciti, colonne minime (9)
## Se funziona: estendere agli altri modelli. Se muore anche così: il full
## panel richiede una macchina più capiente; fanno fede 13 (sub-campioni)
## e 14 (panel collassato).
##
## Cache: stesso formato/percorso di 07 sezione A (TD_WB_ln_export_base.rds),
## così il risultato si integra con la tabella di stabilità di 13.

library(fst); library(fixest); library(data.table); library(here)
threads_fst(1); setFixest_nthreads(4)   # 4 thread: meno workspace di demeaning in RAM

out_rds <- here("New/Output/TripleDiff/Models_Output/TD_WB_ln_export_base.rds")
dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)
if (file.exists(out_rds)) { cat("[SKIP] già in cache:", out_rds, "\n"); quit(save = "no") }

cat("Loading 9 columns...\n")
d <- as.data.table(read_fst(
  here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  columns = c("ln_export", "WB_EP_Depth", "hs6", "country_code", "year",
              "fpd", "fdt", "pt", "TREND_EP_Count")))
d <- d[!country_code %in% c(110L, 121L)]
gc()

green <- fread(here("New/Data/Concordance/Env_Codes_HS1996.csv"),
               colClasses = list(character = "hs6_final"))
d[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(here("New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
d[dirty, on = "hs6", dirty_p := i.dirty_p]; d[is.na(dirty_p), dirty_p := 0L]
dep <- fread(here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[, .(country_code, year, TotalDepth_nonEnv)]
d[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
d[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
d[, hs6 := NULL]
gc()
cat("Rows:", format(nrow(d), big.mark = ","), "\n")

cat("Stima diretta (WB x ln_export x base)...\n")
m <- feols(ln_export ~ WB_EP_Depth:env_good + WB_EP_Depth:dirty_p +
             TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | fpd + fdt + pt,
           data = d, cluster = ~country_code, lean = TRUE, mem.clean = TRUE)
st <- list(formula = "ln_export ~ WB:green + WB:dirty + TD:green + TD:dirty | fpd+fdt+pt",
           coefs = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs,
           n_clust = tryCatch(fitstat(m, "g")[[1]], error = function(e) NA))
saveRDS(st, out_rds)
print(st$coefs); print(st$pval)
cat("[OK] salvato:", out_rds, "\n")
