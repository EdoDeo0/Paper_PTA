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

## Guardia: non sovrascrivere CSV verificati cross-software (regola M8).
## Per rigenerarli davvero: FORCE_OVERWRITE_VERIFIED <- TRUE (e poi ri-arbitrare con Stata).
FORCE_OVERWRITE_VERIFIED <- FALSE
.protected <- c("tripledd_trimmed_collapsed.csv", "tripledd_decomp_collapsed.csv",
                "wcb_trimmed_collapsed.csv", "wcb_decomp_collapsed.csv",
                "wcb_trimmed_fullpanel.csv")
for (.f in file.path(OUT_DIR, .protected)) {
  if (file.exists(.f) && !FORCE_OVERWRITE_VERIFIED) {
    .src <- tryCatch(names(fread(.f)), error = function(e) character())
    if ("source" %in% .src)
      stop(sprintf("%s ha colonna 'source' (verificato Stata). Questo script lo sovrascriverebbe con output R non verificato. Usare 49/50/48e, o FORCE_OVERWRITE_VERIFIED=TRUE.", basename(.f)))
  }
}

green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
dep   <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]

RSCRIPT <- file.path(R.home("bin"), "Rscript")

## Helper: scrive un worker R temporaneo e lo lancia (con timeout via processx)
run_worker <- function(worker_code, label, max_tries = 8, timeout = 420) {
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
## PARTE A: PANEL COLLASSATO (trimmato) — preparazione dati
## ========================================================================
cat("\n===== PARTE A: panel collassato trimmato =====\n")

CACHE_FST <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
stopifnot("panel_pdt_collapsed.fst non trovato" = file.exists(CACHE_FST))
cell <- as.data.table(read_fst(CACHE_FST))
stopifnot("Dataset stantio: max(WB_EP_Depth) != 17" = max(cell$WB_EP_Depth, na.rm = TRUE) == 17)

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
## Guardia (layer 1): FW identity (demean+lm vs feols, <1e-6).
## Guardia (layer 2, solo TREND): cross-check coef vs A1 nell'orchestratore.
## Worker usa: fixest caricato DOPO data construction + singleton filter manuale
## (no obs() → lean=TRUE), nthreads=1 (meno pressione GC durante feols).
cat("\n--- WCB collapsed trimmed ---\n")

## Legge la referenza A1 TREND collapsed (usata nel cross-check layer 2)
.a1_collapsed <- fread(out_path(file.path(OUT_DIR, "tripledd_trimmed_collapsed.csv")))
.a1_trend_dirty_ref <- .a1_collapsed[treat == "TREND" & grepl("dirty_p", var), coef][1]
cat(sprintf("[A1 ref] TREND ep_dirty = %+.6f\n", .a1_trend_dirty_ref))

for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  out_csv <- out_path(file.path(OUT_DIR, sprintf("wcb_trimmed_collapsed_%s.csv", tolower(tr_name))))

  worker <- sprintf('
# Fase 1: dati + singleton filter — SENZA fixest (nessun finalizer C durante costruzione)
library(fst); library(data.table)
threads_fst(1)
cell <- as.data.table(read_fst("%s"))
cell[, `:=`(ep_green = %s * env_good, ep_dirty = %s * dirty_p,
            td_green = %s * env_good, td_dirty = %s * dirty_p)]
cell <- cell[, .(y, ep_green, ep_dirty, td_green, td_dirty, pd, dt, pt, n, country_code)]
# Singleton filter manuale (iterativo, puro data.table — no obs())
repeat {
  n_pd <- cell[, .N, by = pd]; n_dt <- cell[, .N, by = dt]; n_pt <- cell[, .N, by = pt]
  d_pd <- n_pd[N == 1L, pd]; d_dt <- n_dt[N == 1L, dt]; d_pt <- n_pt[N == 1L, pt]
  if (!length(d_pd) && !length(d_dt) && !length(d_pt)) break
  cell <- cell[!pd %%in%% d_pd & !dt %%in%% d_dt & !pt %%in%% d_pt]
}
cat(sprintf("  singleton filter: %%d obs rimaste\\n", nrow(cell)))
gc(full = TRUE)
# Fase 2: feols + FW (fixest caricato solo ora, nthreads=1 per ridurre pressione GC)
library(fixest); setFixest_nthreads(1)
m_ref <- feols(y ~ ep_green + ep_dirty + td_green + td_dirty | pd + dt + pt,
               data = cell, weights = ~n, cluster = ~country_code, lean = TRUE)
ref_green <- coef(m_ref)[["ep_green"]]; ref_dirty <- coef(m_ref)[["ep_dirty"]]
gc()
X <- fixest::demean(cell[, .(y, ep_green, ep_dirty, td_green, td_dirty)],
                    f = cell[, .(pd, dt, pt)], weights = cell$n)
df <- as.data.frame(X); df$n_w <- cell$n; df$country_code <- cell$country_code
rm(X, m_ref, cell); gc()
m_lm <- lm(y ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df, weights = n_w)
stopifnot(
  "FW identity FAILED (ep_green)" = abs(coef(m_lm)[["ep_green"]] - ref_green) < 1e-6,
  "FW identity FAILED (ep_dirty)" = abs(coef(m_lm)[["ep_dirty"]] - ref_dirty) < 1e-6
)
cat(sprintf("[%s] coef: ep_green %%+.6f | ep_dirty %%+.6f\\n",
            coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))
# Fase 3: boottest (fwildclusterboot caricato solo ora)
library(fwildclusterboot)
set.seed(42); dqrng::dqset.seed(42)
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
    tr, tr, DEPTH_VAR, DEPTH_VAR,
    tr_name, tr_name,
    gsub("\\\\", "/", out_csv))

  ## Layer-2 cross-check per TREND: retry se coef WCB ep_dirty diverge >0.003 da A1
  if (tr_name == "TREND") {
    for (attempt in seq_len(8L)) {
      cat(sprintf("  [WCB collapsed TREND] tentativo %d ... ", attempt))
      tf <- tempfile(fileext = ".R"); writeLines(worker, tf); on.exit(unlink(tf), add = TRUE)
      res_run <- processx::run(RSCRIPT, args = tf, timeout = 420L,
                               stdout = "", stderr = "", error_on_status = FALSE)
      if (isTRUE(res_run$timeout)) {
        cat("timeout (420s) — kill e retry\n"); Sys.sleep(3); next
      }
      if (res_run$status != 0) {
        cat(sprintf("crash (exit %d)\n", res_run$status)); Sys.sleep(3); next
      }
      if (!file.exists(out_csv)) {
        cat("no output — retry\n"); Sys.sleep(3); next
      }
      # Layer 2: confronta coef WCB vs A1
      wcb_check <- tryCatch(fread(out_csv), error = function(e) NULL)
      if (is.null(wcb_check) || nrow(wcb_check) == 0) {
        cat("CSV vuoto — retry\n"); unlink(out_csv); Sys.sleep(3); next
      }
      wcb_dirty <- wcb_check[term == "ep_dirty", coef]
      if (length(wcb_dirty) == 0 || is.na(wcb_dirty)) {
        cat("coef mancante — retry\n"); unlink(out_csv); Sys.sleep(3); next
      }
      if (!is.na(.a1_trend_dirty_ref) && abs(wcb_dirty - .a1_trend_dirty_ref) > 0.003) {
        cat(sprintf("coef corrotto (WCB=%.4f vs A1=%.4f) — retry\n",
                    wcb_dirty, .a1_trend_dirty_ref))
        unlink(out_csv); Sys.sleep(3); next
      }
      cat("OK\n"); break
    }
    if (!file.exists(out_csv)) stop("WCB collapsed TREND fallito dopo 8 tentativi")
  } else {
    run_worker(worker, paste("WCB collapsed", tr_name))
  }
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
stopifnot("Dataset stantio: max(WB_EP_Depth) != 17" = max(d$WB_EP_Depth, na.rm = TRUE) == 17)

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
## Guardia: FW identity su obs di feols (< 1e-6). Nessun A1 cross-check.
cat("\n--- WCB full panel trimmed ---\n")

for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  out_csv <- out_path(file.path(OUT_DIR, sprintf("wcb_trimmed_fullpanel_%s.csv", tolower(tr_name))))

  worker <- sprintf('
# Fase 1: feols + FW — SENZA fwildclusterboot (evita recursive gc da suoi finalizer)
library(fst); library(data.table); library(fixest)
threads_fst(1); setFixest_nthreads(4)
d <- as.data.table(read_fst("%s"))
d[, `:=`(ep_green = %s * env_good, ep_dirty = %s * dirty_p,
         td_green = %s * env_good, td_dirty = %s * dirty_p)]
d <- d[, .(ln_export, ep_green, ep_dirty, td_green, td_dirty, pd, dt, pt, country_code)]
gc(full = TRUE)
m_ref <- feols(ln_export ~ ep_green + ep_dirty + td_green + td_dirty | pd + dt + pt,
               data = d, cluster = ~country_code)
ref_green <- coef(m_ref)[["ep_green"]]; ref_dirty <- coef(m_ref)[["ep_dirty"]]
keep_obs <- obs(m_ref); rm(m_ref); gc()
d_s <- d[keep_obs]; rm(d, keep_obs); gc()
X <- fixest::demean(d_s[, .(ln_export, ep_green, ep_dirty, td_green, td_dirty)],
                    f = d_s[, .(pd, dt, pt)])
df <- as.data.frame(X); df$country_code <- d_s$country_code
rm(X, d_s); gc()
m_lm <- lm(ln_export ~ 0 + ep_green + ep_dirty + td_green + td_dirty, data = df)
stopifnot(
  "FW identity FAILED (ep_green)" = abs(coef(m_lm)[["ep_green"]] - ref_green) < 1e-6,
  "FW identity FAILED (ep_dirty)" = abs(coef(m_lm)[["ep_dirty"]] - ref_dirty) < 1e-6
)
cat(sprintf("[%s] coef: ep_green %%+.6f | ep_dirty %%+.6f\\n",
            coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))
# Fase 2: boottest — carichiamo fwildclusterboot solo ora
library(fwildclusterboot)
set.seed(42); dqrng::dqset.seed(42)
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
    tr, tr, DEPTH_VAR, DEPTH_VAR,
    tr_name, tr_name,
    gsub("\\\\", "/", out_csv))

  run_worker(worker, paste("WCB full panel", tr_name), timeout = 3600)
}

wcb_wb <- fread(out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_wb.csv")))
wcb_tr <- fread(out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_trend.csv")))
fwrite(rbind(wcb_wb, wcb_tr), out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel.csv")))
unlink(c(out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_wb.csv")),
         out_path(file.path(OUT_DIR, "wcb_trimmed_fullpanel_trend.csv"))))
cat("[OK] wcb_trimmed_fullpanel.csv\n")

unlink(TRIM_FULL_FST)

cat("\n=== DONE 46 (trimming robustness) ===\n")
