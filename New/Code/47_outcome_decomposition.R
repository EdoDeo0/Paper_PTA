########################################################
###### 47 — Decomposizione: quantita' e valore unitario ###
########################################################
## Author: Edoardo Vitella
## Run: ~15-20 min (ogni stima in processo R separato via system2).
##
## Cosa fa: la triple-diff su ln_export cattura il margine intensivo totale.
## Questo script lo decompone in quantita' (ln_export_qua) e valore unitario
## (ln_export_value = ln(export/exp_qua)), per verificare se l'effetto EP
## agisce via prezzo, quantita', o entrambi.
##
## Strategia anti-crash: ogni singola stima feols gira in un processo R
## indipendente (Rscript su uno script worker temporaneo). L'orchestratore
## prepara i dati, li salva su .fst, e lancia il worker.
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/tripledd_decomp_collapsed.csv
##         New/Output/TripleDiff/Tables/tripledd_decomp_fullpanel.csv
##         New/Output/TripleDiff/Tables/wcb_decomp_collapsed.csv
##         New/Output/TripleDiff/Tables/wcb_decomp_fullpanel.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
gc()
library(here)
library(data.table)
library(fst)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

DATA_FST   <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
OUT_DIR    <- here("New/Output/TripleDiff/Tables")
WORK_DIR   <- here("New/Data/Collapsed")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)
stopifnot("Dataset principale non trovato" = file.exists(DATA_FST))

green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
dep   <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]

RSCRIPT  <- file.path(R.home("bin"), "Rscript")
OUTCOMES <- c("ln_export_qua", "ln_export_value")

run_worker <- function(worker_code, label, max_tries = 5, timeout = 420) {
  tf <- tempfile(fileext = ".R")
  writeLines(worker_code, tf)
  on.exit(unlink(tf))
  for (tent in 1:max_tries) {
    cat(sprintf("  [%s] tentativo %d ... ", label, tent))
    res <- processx::run(RSCRIPT, args = tf, timeout = timeout,
                         stdout = "", stderr = "", error_on_status = FALSE)
    if (isTRUE(res$timeout)) {
      cat(sprintf("timeout (%ds) — kill e retry\n", timeout))
    } else if (res$status == 0) {
      cat("OK\n"); return(invisible(TRUE))
    } else {
      cat(sprintf("crash (exit %d)\n", res$status))
    }
    Sys.sleep(3)
  }
  stop(sprintf("%s fallito dopo %d tentativi", label, max_tries))
}

## ========================================================================
## PARTE A: PANEL COLLASSATO — un outcome alla volta
## ========================================================================
cat("\n===== PARTE A: panel collassato (qua + uv) =====\n")

part_files_c <- c()
wcb_files_c  <- c()

for (oc in OUTCOMES) {
  cat(sprintf("\n--- Collassato: %s ---\n", oc))

  d_raw <- as.data.table(read_fst(DATA_FST, columns = c(
    oc, "hs6", "country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
  if (HKMO_DROP) d_raw <- d_raw[!country_code %in% HKMO_CODES]
  d_raw <- d_raw[!is.na(get(oc))]
  stopifnot("Dataset stantio: max(WB_EP_Depth) != 17" = max(d_raw$WB_EP_Depth, na.rm = TRUE) == 17)
  cell <- d_raw[, .(y = mean(get(oc)), n = .N,
                    WB_EP_Depth = first(WB_EP_Depth), TREND_EP_Count = first(TREND_EP_Count)),
                by = .(hs6, country_code, year)]
  rm(d_raw); gc()
  cat("Celle:", format(nrow(cell), big.mark = ","), "\n")

  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% green_set)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]
  cell[is.na(dirty_p), dirty_p := 0L]
  cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
  if (DEPTH_DROP_UNMEASURED) cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
  cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]

  tmp_fst <- file.path(WORK_DIR, sprintf("tmp_decomp_%s.fst", oc))
  write_fst(cell, tmp_fst)
  rm(cell); gc()

  ## stime asintotiche
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    part_csv <- out_path(file.path(OUT_DIR, sprintf("tmp_decomp_c_%s_%s.csv", oc, tolower(tr_name))))
    part_files_c <- c(part_files_c, part_csv)

    worker <- sprintf('
library(fst); library(data.table); library(fixest)
threads_fst(1); setFixest_nthreads(4)
cell <- as.data.table(read_fst("%s"))
f <- "y ~ %s:env_good + %s:dirty_p + %s:env_good + %s:dirty_p | pd + dt + pt"
m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
out <- data.table(outcome = "%s", treat = "%s", var = names(coef(m)), coef = coef(m),
                  se = se(m), pval = pvalue(m), nobs = m$nobs,
                  nclust = uniqueN(cell$country_code))
fwrite(out, "%s")
cat(sprintf("[%s/%s] ep_green %%+.6f | ep_dirty %%+.6f\\n",
            coef(m)[grep("env_good", names(coef(m)))[1]],
            coef(m)[grep("dirty_p", names(coef(m)))[1]]))
',    gsub("\\\\", "/", tmp_fst), tr, tr, DEPTH_VAR, DEPTH_VAR,
      oc, tr_name, gsub("\\\\", "/", part_csv), tr_name, oc)

    run_worker(worker, paste("collapsed", oc, tr_name))
  }

  ## WCB (guardia FW in-worker — vedi commento in 46/A2)
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    wcb_csv <- out_path(file.path(OUT_DIR, sprintf("tmp_wcb_decomp_c_%s_%s.csv", oc, tolower(tr_name))))
    wcb_files_c <- c(wcb_files_c, wcb_csv)

    worker <- sprintf('
library(fst); library(data.table); library(fixest); library(fwildclusterboot)
threads_fst(1); setFixest_nthreads(4)
set.seed(42); dqrng::dqset.seed(42)
cell <- as.data.table(read_fst("%s"))
cell[, `:=`(ep_green = %s * env_good, ep_dirty = %s * dirty_p,
            td_green = %s * env_good, td_dirty = %s * dirty_p)]
m_ref <- feols(y ~ ep_green + ep_dirty + td_green + td_dirty | pd + dt + pt,
               data = cell, weights = ~n, cluster = ~country_code)
ref_green <- coef(m_ref)[["ep_green"]]; ref_dirty <- coef(m_ref)[["ep_dirty"]]
keep_obs <- obs(m_ref); rm(m_ref); gc()
cell_s <- cell[keep_obs]; rm(cell, keep_obs); gc()
X <- fixest::demean(cell_s[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                    f = cell_s[, .(pd, dt, pt)], weights = cell_s$n)
df <- as.data.frame(X); df$n_w <- cell_s$n; df$country_code <- cell_s$country_code
rm(X, cell_s); gc()
m_lm <- lm(y ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)
stopifnot(
  "FW identity FAILED (ep_green)" = abs(coef(m_lm)[["ep_green"]] - ref_green) < 1e-6,
  "FW identity FAILED (ep_dirty)" = abs(coef(m_lm)[["ep_dirty"]] - ref_dirty) < 1e-6
)
res <- list()
for (param in c("ep_green", "ep_dirty")) {
  bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
  cat(sprintf("  WCB %s/%s/%%s: p=%%+.4f\\n", param, bt$p_val))
  res[[param]] <- data.table(outcome = "%s", treat = "%s", term = param,
    coef = coef(m_lm)[[param]], p_wcb = bt$p_val,
    conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L,
    nobs = nrow(df), nclust = uniqueN(df$country_code), fe = "pd+dt+pt")
}
fwrite(rbindlist(res), "%s")
',    gsub("\\\\", "/", tmp_fst), tr, tr, DEPTH_VAR, DEPTH_VAR,
      oc, tr_name, oc, tr_name, gsub("\\\\", "/", wcb_csv))

    run_worker(worker, paste("WCB collapsed", oc, tr_name))
  }

  unlink(tmp_fst)
}

fwrite(rbindlist(lapply(part_files_c, fread)),
       out_path(file.path(OUT_DIR, "tripledd_decomp_collapsed.csv")))
unlink(part_files_c)
cat("[OK] tripledd_decomp_collapsed.csv\n")

fwrite(rbindlist(lapply(wcb_files_c, fread)),
       out_path(file.path(OUT_DIR, "wcb_decomp_collapsed.csv")))
unlink(wcb_files_c)
cat("[OK] wcb_decomp_collapsed.csv\n")

## ========================================================================
## PARTE B: FULL PANEL (pd+dt+pt, non pesato) — un outcome alla volta
## ========================================================================
cat("\n===== PARTE B: full panel decomp (pd+dt+pt) =====\n")

part_files_fp <- c()
wcb_files_fp  <- c()

for (oc in OUTCOMES) {
  cat(sprintf("\n--- Full panel: %s ---\n", oc))

  d <- as.data.table(read_fst(DATA_FST, columns = c(
    oc, "hs6", "country_code", "year",
    "WB_EP_Depth", "TREND_EP_Count", "env_good")))
  if (HKMO_DROP) d <- d[!country_code %in% HKMO_CODES]
  d <- d[!is.na(get(oc))]
  stopifnot("Dataset stantio: max(WB_EP_Depth) != 17" = max(d$WB_EP_Depth, na.rm = TRUE) == 17)
  cat(sprintf("  Obs: %s\n", format(nrow(d), big.mark = ",")))

  d[dirty, on = "hs6", dirty_p := i.dirty_p]
  d[is.na(dirty_p), dirty_p := 0L]
  d[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
  if (DEPTH_DROP_UNMEASURED) d <- d[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
  d[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
  d[, pd := .GRP, by = .(hs6, country_code)]
  d[, dt := .GRP, by = .(country_code, year)]
  d[, pt := .GRP, by = .(hs6, year)]

  tmp_fst <- file.path(WORK_DIR, sprintf("tmp_decomp_fp_%s.fst", oc))
  write_fst(d, tmp_fst)
  rm(d); gc()

  ## stime asintotiche
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    part_csv <- out_path(file.path(OUT_DIR, sprintf("tmp_decomp_fp_%s_%s.csv", oc, tolower(tr_name))))
    part_files_fp <- c(part_files_fp, part_csv)

    worker <- sprintf('
library(fst); library(data.table); library(fixest)
threads_fst(1); setFixest_nthreads(4)
d <- as.data.table(read_fst("%s"))
f <- "%s ~ %s:env_good + %s:dirty_p + %s:env_good + %s:dirty_p | pd + dt + pt"
m <- feols(as.formula(f), data = d, cluster = ~country_code, lean = TRUE)
out <- data.table(outcome = "%s", treat = "%s", var = names(coef(m)), coef = coef(m),
                  se = se(m), pval = pvalue(m), nobs = m$nobs,
                  nclust = uniqueN(d$country_code))
fwrite(out, "%s")
cat(sprintf("[%s/%s] ep_green %%+.6f | ep_dirty %%+.6f\\n",
            coef(m)[grep("env_good", names(coef(m)))[1]],
            coef(m)[grep("dirty_p", names(coef(m)))[1]]))
',    gsub("\\\\", "/", tmp_fst), oc, tr, tr, DEPTH_VAR, DEPTH_VAR,
      oc, tr_name, gsub("\\\\", "/", part_csv), tr_name, oc)

    run_worker(worker, paste("full panel", oc, tr_name))
  }

  ## WCB (guardia FW in-worker + obs() — vedi commento in 46/A2)
  for (tr_name in c("WB", "TREND")) {
    tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    wcb_csv <- out_path(file.path(OUT_DIR, sprintf("tmp_wcb_decomp_fp_%s_%s.csv", oc, tolower(tr_name))))
    wcb_files_fp <- c(wcb_files_fp, wcb_csv)

    worker <- sprintf('
library(fst); library(data.table); library(fixest); library(fwildclusterboot)
threads_fst(1); setFixest_nthreads(4)
set.seed(42); dqrng::dqset.seed(42)
d <- as.data.table(read_fst("%s"))
d[, `:=`(ep_green = %s * env_good, ep_dirty = %s * dirty_p,
         td_green = %s * env_good, td_dirty = %s * dirty_p)]
m_ref <- feols(%s ~ ep_green + ep_dirty + td_green + td_dirty | pd + dt + pt,
               data = d, cluster = ~country_code)
ref_green <- coef(m_ref)[["ep_green"]]; ref_dirty <- coef(m_ref)[["ep_dirty"]]
keep_obs <- obs(m_ref); rm(m_ref); gc()
d_s <- d[keep_obs]; rm(d, keep_obs); gc()
X <- fixest::demean(d_s[, .(%s, ep_green, ep_dirty, td_green, td_dirty)],
                    f = d_s[, .(pd, dt, pt)])
df <- as.data.frame(X); df$country_code <- d_s$country_code
rm(X, d_s); gc()
m_lm <- lm(%s ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df)
stopifnot(
  "FW identity FAILED (ep_green)" = abs(coef(m_lm)[["ep_green"]] - ref_green) < 1e-6,
  "FW identity FAILED (ep_dirty)" = abs(coef(m_lm)[["ep_dirty"]] - ref_dirty) < 1e-6
)
res <- list()
for (param in c("ep_green", "ep_dirty")) {
  bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
  cat(sprintf("  WCB fp %s/%s/%%s: p=%%+.4f\\n", param, bt$p_val))
  res[[param]] <- data.table(outcome = "%s", treat = "%s", term = param,
    coef = coef(m_lm)[[param]], p_wcb = bt$p_val,
    conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L,
    nobs = nrow(df), nclust = uniqueN(df$country_code), fe = "pd+dt+pt")
}
fwrite(rbindlist(res), "%s")
',    gsub("\\\\", "/", tmp_fst), tr, tr, DEPTH_VAR, DEPTH_VAR,
      oc, oc, oc,
      oc, tr_name, oc, tr_name, gsub("\\\\", "/", wcb_csv))

    run_worker(worker, paste("WCB full panel", oc, tr_name))
  }

  unlink(tmp_fst)
}

fwrite(rbindlist(lapply(part_files_fp, fread)),
       out_path(file.path(OUT_DIR, "tripledd_decomp_fullpanel.csv")))
unlink(part_files_fp)
cat("[OK] tripledd_decomp_fullpanel.csv\n")

fwrite(rbindlist(lapply(wcb_files_fp, fread)),
       out_path(file.path(OUT_DIR, "wcb_decomp_fullpanel.csv")))
unlink(wcb_files_fp)
cat("[OK] wcb_decomp_fullpanel.csv\n")

cat("\n=== DONE 47 (outcome decomposition) ===\n")
