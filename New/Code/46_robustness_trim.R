########################################################
###### 46 — Robustezza: trimming p1/p99 sull'outcome ###
########################################################
## Author: Edoardo Vitella
## Run: ~10-15 min (ogni stima in processo R separato via system()).
##
## Cosa fa: stima la triple-diff su outcome trimmato al percentile 1-99,
## sia sul panel collassato (pd+dt+pt, pesato) che sul full panel
## (pd+dt+pt, non pesato).
##
## Strategia anti-crash: ogni singola stima feols gira in un processo R
## indipendente (Rscript su uno script worker temporaneo). L'orchestratore
## prepara i dati, li salva su .fst, e lancia il worker. Questo evita sia
## la pressione cumulativa dell'in-process, sia l'overhead di serializzazione
## di callr.
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
## Output: New/Output/TripleDiff/Tables/tripledd_trimmed_collapsed.csv
##         New/Output/TripleDiff/Tables/tripledd_trimmed_fullpanel.csv
##         New/Output/TripleDiff/Tables/wcb_trimmed_collapsed.csv
##         New/Output/TripleDiff/Tables/wcb_trimmed_fullpanel.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
gc()
library(here)
library(data.table)
library(fst)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
OUT_DIR    <- here("New/Output/TripleDiff/Tables")
WORK_DIR   <- here("New/Data/Collapsed")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(WORK_DIR, recursive = TRUE, showWarnings = FALSE)

green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
dep   <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]

RSCRIPT <- file.path(R.home("bin"), "Rscript")

## Helper: scrive un worker R temporaneo e lo lancia
run_worker <- function(worker_code, label, max_tries = 5) {
  tf <- tempfile(fileext = ".R")
  writeLines(worker_code, tf)
  on.exit(unlink(tf))
  for (tent in 1:max_tries) {
    cat(sprintf("  [%s] tentativo %d ... ", label, tent))
    rc <- system2(RSCRIPT, args = shQuote(tf), stdout = "", stderr = "")
    if (rc == 0) { cat("OK\n"); return(invisible(TRUE)) }
    cat(sprintf("crash (exit %d)\n", rc))
    Sys.sleep(2)
  }
  stop(sprintf("%s fallito dopo %d tentativi", label, max_tries))
}

## ========================================================================
## PARTE A: PANEL COLLASSATO (trimmato) — preparazione dati
## ========================================================================
cat("\n===== PARTE A: panel collassato trimmato =====\n")

CACHE_FST <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
stopifnot("panel_pdt_collapsed.fst non trovato" = file.exists(CACHE_FST))
cell <- as.data.table(read_fst(CACHE_FST))

cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% green_set)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]
cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
if (DEPTH_DROP_UNMEASURED) {
  n0 <- nrow(cell)
  cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
  cat(sprintf("[depth] %s: %d celle escluse\n", DEPTH_VAR, n0 - nrow(cell)))
}
cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
cell[, pd := .GRP, by = .(hs6, country_code)]
cell[, dt := .GRP, by = .(country_code, year)]
cell[, pt := .GRP, by = .(hs6, year)]

n_pre <- nrow(cell)
q01 <- quantile(cell$y, 0.01)
q99 <- quantile(cell$y, 0.99)
cell <- cell[y >= q01 & y <= q99]
cat(sprintf("Trimming collassato: [%.4f, %.4f] | %d -> %d celle (-%d, %.2f%%)\n",
            q01, q99, n_pre, nrow(cell), n_pre - nrow(cell),
            100 * (n_pre - nrow(cell)) / n_pre))

TRIM_COLLAPSED_FST <- file.path(WORK_DIR, "tmp_trim_collapsed.fst")
write_fst(cell, TRIM_COLLAPSED_FST)
rm(cell); gc()

## --- A1: stime asintotiche — un worker per indice ---
for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  out_csv <- out_path(file.path(OUT_DIR, sprintf("tripledd_trimmed_collapsed_%s.csv", tolower(tr_name))))

  worker <- sprintf('
library(fst); library(data.table); library(fixest)
threads_fst(1); setFixest_nthreads(4)
cell <- as.data.table(read_fst("%s"))
f <- "y ~ %s:env_good + %s:dirty_p + %s:env_good + %s:dirty_p | pd + dt + pt"
m <- feols(as.formula(f), data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
out <- data.table(treat = "%s", var = names(coef(m)), coef = coef(m),
                  se = se(m), pval = pvalue(m), nobs = m$nobs,
                  nclust = uniqueN(cell$country_code))
fwrite(out, "%s")
cat(sprintf("[%s] ep_green %%+.6f (p=%%.4f) | ep_dirty %%+.6f (p=%%.4f)\\n",
            coef(m)[grep("env_good", names(coef(m)))[1]],
            pvalue(m)[grep("env_good", names(coef(m)))[1]],
            coef(m)[grep("dirty_p", names(coef(m)))[1]],
            pvalue(m)[grep("dirty_p", names(coef(m)))[1]]))
',  gsub("\\\\", "/", TRIM_COLLAPSED_FST),
    tr, tr, DEPTH_VAR, DEPTH_VAR, tr_name,
    gsub("\\\\", "/", out_csv), tr_name)

  run_worker(worker, paste("collapsed", tr_name))
}

res_wb  <- fread(out_path(file.path(OUT_DIR, "tripledd_trimmed_collapsed_wb.csv")))
res_tr  <- fread(out_path(file.path(OUT_DIR, "tripledd_trimmed_collapsed_trend.csv")))
fwrite(rbind(res_wb, res_tr), out_path(file.path(OUT_DIR, "tripledd_trimmed_collapsed.csv")))
unlink(c(out_path(file.path(OUT_DIR, "tripledd_trimmed_collapsed_wb.csv")),
         out_path(file.path(OUT_DIR, "tripledd_trimmed_collapsed_trend.csv"))))
cat("[OK] tripledd_trimmed_collapsed.csv\n")

## --- A2: WCB collassato trimmato — un worker per indice ---
cat("\n--- WCB collapsed trimmed ---\n")
for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  out_csv <- out_path(file.path(OUT_DIR, sprintf("wcb_trimmed_collapsed_%s.csv", tolower(tr_name))))

  worker <- sprintf('
library(fst); library(data.table); library(fixest); library(fwildclusterboot)
threads_fst(1); setFixest_nthreads(4)
set.seed(42); dqrng::dqset.seed(42)
cell <- as.data.table(read_fst("%s"))
cell[, `:=`(ep_green = %s * env_good, ep_dirty = %s * dirty_p,
            td_green = %s * env_good, td_dirty = %s * dirty_p)]
X <- fixest::demean(cell[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                    f = cell[, .(pd, dt, pt)], weights = cell$n)
df <- as.data.frame(X); df$n_w <- cell$n; df$country_code <- cell$country_code
rm(X, cell); gc()
m_lm <- lm(y ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)
cat(sprintf("[%s] coef: ep_green %%+.6f | ep_dirty %%+.6f\\n",
            coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))
res <- list()
for (param in c("ep_green", "ep_dirty")) {
  cat("  boottest:", param, "... ")
  bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
  cat(sprintf("p_wcb = %%.4f\\n", bt$p_val))
  res[[param]] <- data.table(treat = "%s", term = param, coef = coef(m_lm)[[param]],
    p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L,
    nobs = nrow(df), nclust = uniqueN(df$country_code), fe = "pd+dt+pt", trim = "p01_p99")
}
fwrite(rbindlist(res), "%s")
',  gsub("\\\\", "/", TRIM_COLLAPSED_FST),
    tr, tr, DEPTH_VAR, DEPTH_VAR, tr_name, tr_name,
    gsub("\\\\", "/", out_csv))

  run_worker(worker, paste("WCB collapsed", tr_name))
}

wcb_wb <- fread(out_path(file.path(OUT_DIR, "wcb_trimmed_collapsed_wb.csv")))
wcb_tr <- fread(out_path(file.path(OUT_DIR, "wcb_trimmed_collapsed_trend.csv")))
fwrite(rbind(wcb_wb, wcb_tr), out_path(file.path(OUT_DIR, "wcb_trimmed_collapsed.csv")))
unlink(c(out_path(file.path(OUT_DIR, "wcb_trimmed_collapsed_wb.csv")),
         out_path(file.path(OUT_DIR, "wcb_trimmed_collapsed_trend.csv"))))
cat("[OK] wcb_trimmed_collapsed.csv\n")

unlink(TRIM_COLLAPSED_FST)

## ========================================================================
## PARTE B: FULL PANEL (trimmato, pd+dt+pt) — preparazione dati
## ========================================================================
cat("\n===== PARTE B: full panel trimmato (pd+dt+pt) =====\n")

DATA_FST <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
d <- as.data.table(read_fst(DATA_FST, columns = c(
  "ln_export", "hs6", "country_code", "year",
  "WB_EP_Depth", "TREND_EP_Count", "env_good")))
if (HKMO_DROP) d <- d[!country_code %in% HKMO_CODES]
d <- d[!is.na(ln_export)]

d[dirty, on = "hs6", dirty_p := i.dirty_p]
d[is.na(dirty_p), dirty_p := 0L]
d[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
if (DEPTH_DROP_UNMEASURED) d <- d[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
d[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]

n_pre <- nrow(d)
q01 <- quantile(d$ln_export, 0.01)
q99 <- quantile(d$ln_export, 0.99)
d <- d[ln_export >= q01 & ln_export <= q99]
cat(sprintf("Trimming full panel: [%.4f, %.4f] | %s -> %s obs (-%s, %.2f%%)\n",
            q01, q99, format(n_pre, big.mark = ","), format(nrow(d), big.mark = ","),
            format(n_pre - nrow(d), big.mark = ","),
            100 * (n_pre - nrow(d)) / n_pre))

d[, pd := .GRP, by = .(hs6, country_code)]
d[, dt := .GRP, by = .(country_code, year)]
d[, pt := .GRP, by = .(hs6, year)]

TRIM_FULL_FST <- file.path(WORK_DIR, "tmp_trim_fullpanel.fst")
write_fst(d, TRIM_FULL_FST)
rm(d); gc()

## --- B1: stime asintotiche full panel ---
for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  out_csv <- out_path(file.path(OUT_DIR, sprintf("tripledd_trimmed_fullpanel_%s.csv", tolower(tr_name))))

  worker <- sprintf('
library(fst); library(data.table); library(fixest)
threads_fst(1); setFixest_nthreads(4)
d <- as.data.table(read_fst("%s"))
f <- "ln_export ~ %s:env_good + %s:dirty_p + %s:env_good + %s:dirty_p | pd + dt + pt"
m <- feols(as.formula(f), data = d, cluster = ~country_code, lean = TRUE)
out <- data.table(treat = "%s", var = names(coef(m)), coef = coef(m),
                  se = se(m), pval = pvalue(m), nobs = m$nobs,
                  nclust = uniqueN(d$country_code))
fwrite(out, "%s")
cat(sprintf("[%s] ep_green %%+.6f (p=%%.4f) | ep_dirty %%+.6f (p=%%.4f)\\n",
            coef(m)[grep("env_good", names(coef(m)))[1]],
            pvalue(m)[grep("env_good", names(coef(m)))[1]],
            coef(m)[grep("dirty_p", names(coef(m)))[1]],
            pvalue(m)[grep("dirty_p", names(coef(m)))[1]]))
',  gsub("\\\\", "/", TRIM_FULL_FST),
    tr, tr, DEPTH_VAR, DEPTH_VAR, tr_name,
    gsub("\\\\", "/", out_csv), tr_name)

  run_worker(worker, paste("full panel", tr_name))
}

res_wb <- fread(out_path(file.path(OUT_DIR, "tripledd_trimmed_fullpanel_wb.csv")))
res_tr <- fread(out_path(file.path(OUT_DIR, "tripledd_trimmed_fullpanel_trend.csv")))
fwrite(rbind(res_wb, res_tr), out_path(file.path(OUT_DIR, "tripledd_trimmed_fullpanel.csv")))
unlink(c(out_path(file.path(OUT_DIR, "tripledd_trimmed_fullpanel_wb.csv")),
         out_path(file.path(OUT_DIR, "tripledd_trimmed_fullpanel_trend.csv"))))
cat("[OK] tripledd_trimmed_fullpanel.csv\n")

## --- B2: WCB full panel trimmato ---
cat("\n--- WCB full panel trimmed ---\n")
for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  out_csv <- out_path(file.path(OUT_DIR, sprintf("wcb_trimmed_fullpanel_%s.csv", tolower(tr_name))))

  worker <- sprintf('
library(fst); library(data.table); library(fixest); library(fwildclusterboot)
threads_fst(1); setFixest_nthreads(4)
set.seed(42); dqrng::dqset.seed(42)
d <- as.data.table(read_fst("%s"))
d[, `:=`(ep_green = %s * env_good, ep_dirty = %s * dirty_p,
         td_green = %s * env_good, td_dirty = %s * dirty_p)]
X <- fixest::demean(d[, .(ln_export, ep_green, ep_dirty, td_green, td_dirty)],
                    f = d[, .(pd, dt, pt)])
df <- as.data.frame(X); df$country_code <- d$country_code
rm(X, d); gc()
m_lm <- lm(ln_export ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df)
cat(sprintf("[%s] coef: ep_green %%+.6f | ep_dirty %%+.6f\\n",
            coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))
res <- list()
for (param in c("ep_green", "ep_dirty")) {
  cat("  boottest:", param, "... ")
  bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
  cat(sprintf("p_wcb = %%.4f\\n", bt$p_val))
  res[[param]] <- data.table(treat = "%s", term = param, coef = coef(m_lm)[[param]],
    p_wcb = bt$p_val, conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L,
    nobs = nrow(df), nclust = uniqueN(df$country_code), fe = "pd+dt+pt", trim = "p01_p99")
}
fwrite(rbindlist(res), "%s")
',  gsub("\\\\", "/", TRIM_FULL_FST),
    tr, tr, DEPTH_VAR, DEPTH_VAR, tr_name, tr_name,
    gsub("\\\\", "/", out_csv))

  run_worker(worker, paste("WCB full panel", tr_name))
}

wcb_wb <- fread(out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_wb.csv")))
wcb_tr <- fread(out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_trend.csv")))
fwrite(rbind(wcb_wb, wcb_tr), out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel.csv")))
unlink(c(out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_wb.csv")),
         out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_trend.csv"))))
cat("[OK] wcb_trimmed_fullpanel.csv\n")

unlink(TRIM_FULL_FST)

cat("\n=== DONE 46 (trimming robustness) ===\n")
