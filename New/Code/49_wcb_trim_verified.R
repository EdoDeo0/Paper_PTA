########################################################
###### 49 — WCB trimmato collassato con layer-2 vs Stata ###
########################################################
## Author: Edoardo Vitella
##
## Cosa fa: rigenera wcb_trimmed_collapsed.csv con:
##   - FW guard (layer 1): demean+lm vs feols < 1e-6
##   - Layer-2 cross-check: coef WCB vs riferimento STATA (< 1e-4)
##     per ENTRAMBI WB e TREND (non solo TREND come in 46)
##   - B = 9999, timeout = 420s, max_tries = 8
##
## Prerequisiti:
##   1. New/Data/Collapsed/tmp_trim_collapsed.fst  (da 46 o 48_trim_export_dta)
##   2. New/Output/TripleDiff/Tables/stata_check_46_47_collapsed.csv (da 48_trim_check.do)
##
## Output: New/Output/TripleDiff/Tables/wcb_trimmed_collapsed.csv

rm(list = ls())
gc()
library(here)
library(data.table)
library(fst)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

stopifnot(SAMPLE == "excl", DEPTH == "totaldepth")

TRIM_FST  <- here("New/Data/Collapsed/tmp_trim_collapsed.fst")
STATA_REF <- here("New/Output/TripleDiff/Tables/stata_check_46_47_collapsed.csv")
OUT_DIR   <- here("New/Output/TripleDiff/Tables")
RSCRIPT   <- file.path(R.home("bin"), "Rscript")

## Ricostruisce tmp_trim_collapsed.fst se non esiste (stessa data prep di 46 Part A)
if (!file.exists(TRIM_FST)) {
  cat("[setup] tmp_trim_collapsed.fst mancante — ricostruisco...\n")
  GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
  DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
  CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
  stopifnot(file.exists(CACHE_FST))
  green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
  dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  dep   <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]
  cell  <- as.data.table(read_fst(CACHE_FST))
  stopifnot(max(cell$WB_EP_Depth, na.rm = TRUE) == 17)
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% green_set)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]
  cell[is.na(dirty_p), dirty_p := 0L]
  cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
  if (DEPTH_DROP_UNMEASURED) cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
  cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  q01 <- quantile(cell$y, 0.01); q99 <- quantile(cell$y, 0.99)
  cell <- cell[y >= q01 & y <= q99]
  cat(sprintf("[setup] Trim [%.4f, %.4f]: %d celle\n", q01, q99, nrow(cell)))
  write_fst(cell, TRIM_FST)
  rm(cell, green_set, dirty, dep); gc()
  cat("[setup] tmp_trim_collapsed.fst scritto.\n")
}
stopifnot("stata_check mancante — lanciare prima 48_trim_check.do" =
            file.exists(STATA_REF))

## Carica riferimento Stata per entrambi gli indici
stata <- fread(STATA_REF)[dataset == "trim"]
stopifnot(nrow(stata) == 8)

get_stata_ref <- function(treat_label, term_pattern) {
  v <- stata[treat == treat_label & grepl(term_pattern, var), coef]
  stopifnot(length(v) == 1)
  v
}

cat(sprintf("[Stata ref] WB   ep_dirty = %+.8f\n",
            get_stata_ref("WB", "wb_dirty")))
cat(sprintf("[Stata ref] TREND ep_dirty = %+.8f\n",
            get_stata_ref("TREND", "tr_dirty")))

## Worker WCB (identico al blocco A2 di 46, con FW guard)
make_worker <- function(tr_var, tr_name, depth_var, fst_path, out_csv) {
  sprintf('
# Fase 1: dati + singleton filter (no fixest)
library(fst); library(data.table)
threads_fst(1)
cell <- as.data.table(read_fst("%s"))
cell[, `:=`(ep_green = %s * env_good, ep_dirty = %s * dirty_p,
            td_green = %s * env_good, td_dirty = %s * dirty_p)]
cell <- cell[, .(y, ep_green, ep_dirty, td_green, td_dirty, pd, dt, pt, n, country_code)]
repeat {
  n_pd <- cell[, .N, by = pd]; n_dt <- cell[, .N, by = dt]; n_pt <- cell[, .N, by = pt]
  d_pd <- n_pd[N == 1L, pd]; d_dt <- n_dt[N == 1L, dt]; d_pt <- n_pt[N == 1L, pt]
  if (!length(d_pd) && !length(d_dt) && !length(d_pt)) break
  cell <- cell[!pd %%in%% d_pd & !dt %%in%% d_dt & !pt %%in%% d_pt]
}
cat(sprintf("  singleton filter: %%d obs\\n", nrow(cell)))
gc(full = TRUE)

# Fase 2: feols + FW guard
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
cat(sprintf("[%s] FW OK: ep_green %%+.8f | ep_dirty %%+.8f\\n",
            coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))

# Fase 3: boottest
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
',
    gsub("\\\\", "/", fst_path),
    tr_var, tr_var, depth_var, depth_var,
    tr_name, tr_name,
    gsub("\\\\", "/", out_csv))
}

## Orchestratore con layer-2 vs Stata
run_wcb_verified <- function(tr_name) {
  tr_var    <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
  stata_var <- c(WB = "wb_dirty",    TREND = "tr_dirty")[[tr_name]]
  stata_ref_dirty <- get_stata_ref(if (tr_name == "WB") "WB" else "TREND",
                                   stata_var)
  out_csv <- file.path(OUT_DIR, sprintf("wcb_trimmed_collapsed_%s.csv", tolower(tr_name)))

  worker <- make_worker(tr_var, tr_name, DEPTH_VAR, TRIM_FST, out_csv)

  for (attempt in seq_len(8L)) {
    cat(sprintf("\n  [WCB collapsed %s] tentativo %d ...\n", tr_name, attempt))
    tf <- tempfile(fileext = ".R"); writeLines(worker, tf)

    res_run <- processx::run(RSCRIPT, args = tf, timeout = 420L,
                             stdout = "", stderr = "", error_on_status = FALSE)
    unlink(tf)

    if (isTRUE(res_run$timeout)) {
      cat("  timeout (420s) — retry\n"); Sys.sleep(3); next
    }
    if (res_run$status != 0) {
      cat(sprintf("  crash (exit %d)\n", res_run$status)); Sys.sleep(3); next
    }
    if (!file.exists(out_csv)) {
      cat("  no output — retry\n"); Sys.sleep(3); next
    }

    wcb_check <- tryCatch(fread(out_csv), error = function(e) NULL)
    if (is.null(wcb_check) || nrow(wcb_check) == 0) {
      cat("  CSV vuoto — retry\n"); unlink(out_csv); Sys.sleep(3); next
    }

    wcb_dirty <- wcb_check[term == "ep_dirty", coef]
    if (length(wcb_dirty) == 0 || is.na(wcb_dirty)) {
      cat("  coef mancante — retry\n"); unlink(out_csv); Sys.sleep(3); next
    }

    # Layer-2: confronta coef WCB vs STATA (< 1e-4, cross-software)
    if (abs(wcb_dirty - stata_ref_dirty) > 1e-4) {
      cat(sprintf("  [LAYER-2] coef corrotto: WCB=%.8f vs Stata=%.8f (delta=%.2e) — RETRY\n",
                  wcb_dirty, stata_ref_dirty, abs(wcb_dirty - stata_ref_dirty)))
      unlink(out_csv); Sys.sleep(3); next
    }

    cat(sprintf("  [LAYER-2] OK: WCB=%.8f vs Stata=%.8f (delta=%.2e)\n",
                wcb_dirty, stata_ref_dirty, abs(wcb_dirty - stata_ref_dirty)))
    cat(sprintf("  [WCB collapsed %s] tentativo %d ACCETTATO\n", tr_name, attempt))
    break
  }
  if (!file.exists(out_csv))
    stop(sprintf("WCB collapsed %s fallito dopo 8 tentativi", tr_name))
}

cat("\n===== WCB collassato trimmato =====\n")
run_wcb_verified("WB")
run_wcb_verified("TREND")

## Unifica
wcb_wb <- fread(file.path(OUT_DIR, "wcb_trimmed_collapsed_wb.csv"))
wcb_tr <- fread(file.path(OUT_DIR, "wcb_trimmed_collapsed_trend.csv"))
fwrite(rbind(wcb_wb, wcb_tr), file.path(OUT_DIR, "wcb_trimmed_collapsed.csv"))
unlink(c(file.path(OUT_DIR, "wcb_trimmed_collapsed_wb.csv"),
         file.path(OUT_DIR, "wcb_trimmed_collapsed_trend.csv")))

cat("\n=== FATTO: wcb_trimmed_collapsed.csv rigenerato con layer-2 vs Stata ===\n")
wcb <- fread(file.path(OUT_DIR, "wcb_trimmed_collapsed.csv"))
print(wcb[, .(treat, term, coef, p_wcb, B)])
