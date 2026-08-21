########################################################
###### 46b2 — Standalone rerun: WCB full panel trimmato
########################################################
## Estratto autonomo di 46_robustness_trim.R, Parte B + B2.
## Usa: prepara tmp_trim_fullpanel.fst (se mancante) e gira WCB B=9999.
## Output: wcb_trimmed_fullpanel_wb.csv, wcb_trimmed_fullpanel_trend.csv
##         (combinati poi in wcb_trimmed_fullpanel.csv)

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

green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
dep   <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]

RSCRIPT <- file.path(R.home("bin"), "Rscript")

run_worker <- function(worker_code, label, max_tries = 8, timeout = 3600) {
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
## PARTE B: FULL PANEL (trimmato, pd+dt+pt) — preparazione dati
## ========================================================================
TRIM_FULL_FST <- file.path(WORK_DIR, "tmp_trim_fullpanel.fst")

if (!file.exists(TRIM_FULL_FST)) {
  cat("\n===== PARTE B: costruzione tmp_trim_fullpanel.fst =====\n")
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

  write_fst(d, TRIM_FULL_FST)
  rm(d); gc()
  cat("[OK] tmp_trim_fullpanel.fst scritto\n")
} else {
  cat("[OK] tmp_trim_fullpanel.fst gia' presente, skip\n")
}

## --- B2: WCB full panel trimmato (B=9999) ---
cat("\n--- WCB full panel trimmed (B=9999) ---\n")

for (tr_name in c("WB", "TREND")) {
  tr <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  out_csv <- out_path(file.path(OUT_DIR, sprintf("wcb_trimmed_fullpanel_%s.csv", tolower(tr_name))))

  worker <- sprintf('
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

cat("\n=== DONE 46b2 ===\n")
