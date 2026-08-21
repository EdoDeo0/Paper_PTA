########################################################
###### 50 — WCB decomposizione collassata con layer-2 vs Stata ###
########################################################
## Author: Edoardo Vitella
##
## Cosa fa: rigenera wcb_decomp_collapsed.csv per ln_export_qua e
## ln_export_value (unit value), con:
##   - FW guard (layer 1): demean+lm vs feols < 1e-6
##   - Layer-2 cross-check: coef WCB vs STATA < 1e-4
##   - B = 9999, timeout = 420s, max_tries = 8
##
## Prerequisito: stata_check_46_47_collapsed.csv (da 48_trim_check.do)
## Output: New/Output/TripleDiff/Tables/wcb_decomp_collapsed.csv

rm(list = ls())
gc()
library(here)
library(data.table)
library(fst)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

stopifnot(SAMPLE == "excl", DEPTH == "totaldepth")

DATA_FST   <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
STATA_REF  <- here("New/Output/TripleDiff/Tables/stata_check_46_47_collapsed.csv")
OUT_DIR    <- here("New/Output/TripleDiff/Tables")
WORK_DIR   <- here("New/Data/Collapsed")
RSCRIPT    <- file.path(R.home("bin"), "Rscript")

stopifnot(file.exists(DATA_FST), file.exists(STATA_REF))

stata <- fread(STATA_REF)
green_set <- unique(fread(GREEN_FILE, colClasses = list(character = "hs6_final"))$hs6_final)
dirty     <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
dep       <- fread(DEPTH_FILE)[, .(country_code, year, dep_val__ = get(DEPTH_VAR))]

OUTCOMES <- c("ln_export_qua", "ln_export_value")
DS_LABEL <- c(ln_export_qua = "decomp_qua", ln_export_value = "decomp_uv")

wcb_parts <- list()

for (oc in OUTCOMES) {
  ds_label <- DS_LABEL[[oc]]
  cat(sprintf("\n===== Decomp collassato: %s =====\n", oc))

  ## Costruisce FST se non esiste
  tmp_fst <- file.path(WORK_DIR, sprintf("tmp_decomp_%s.fst", oc))
  if (!file.exists(tmp_fst)) {
    cat("[setup] FST mancante — ricostruisco...\n")
    d_raw <- as.data.table(read_fst(DATA_FST, columns = c(
      oc, "hs6", "country_code", "year", "WB_EP_Depth", "TREND_EP_Count")))
    if (HKMO_DROP) d_raw <- d_raw[!country_code %in% HKMO_CODES]
    d_raw <- d_raw[!is.na(get(oc))]
    stopifnot(max(d_raw$WB_EP_Depth, na.rm = TRUE) == 17)
    cell <- d_raw[, .(y = mean(get(oc)), n = .N,
                      WB_EP_Depth = first(WB_EP_Depth), TREND_EP_Count = first(TREND_EP_Count)),
                  by = .(hs6, country_code, year)]
    rm(d_raw); gc()
    cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% green_set)]
    cell[dirty, on = "hs6", dirty_p := i.dirty_p]
    cell[is.na(dirty_p), dirty_p := 0L]
    cell[dep, on = c("country_code", "year"), (DEPTH_VAR) := i.dep_val__]
    if (DEPTH_DROP_UNMEASURED) cell <- cell[!(is.na(get(DEPTH_VAR)) & WB_EP_Depth > 0)]
    cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]
    cell[, pd := .GRP, by = .(hs6, country_code)]
    cell[, dt := .GRP, by = .(country_code, year)]
    cell[, pt := .GRP, by = .(hs6, year)]
    write_fst(cell, tmp_fst)
    rm(cell); gc()
    cat(sprintf("[setup] %s scritto.\n", basename(tmp_fst)))
  }

  for (tr_name in c("WB", "TREND")) {
    tr_var    <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")[[tr_name]]
    stata_var <- c(WB = "wb_dirty",    TREND = "tr_dirty")[[tr_name]]
    stata_ref_dirty <- stata[dataset == ds_label & treat == tr_name & var == stata_var, coef]
    stopifnot(length(stata_ref_dirty) == 1)
    cat(sprintf("[Stata ref] %s %s ep_dirty = %+.8f\n", oc, tr_name, stata_ref_dirty))

    out_csv <- file.path(OUT_DIR, sprintf("wcb_decomp_c_%s_%s.csv", oc, tolower(tr_name)))

    worker <- sprintf('
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
  "FW FAILED (ep_green)" = abs(coef(m_lm)[["ep_green"]] - ref_green) < 1e-6,
  "FW FAILED (ep_dirty)" = abs(coef(m_lm)[["ep_dirty"]] - ref_dirty) < 1e-6
)
cat(sprintf("[%s] FW OK: ep_green %%+.8f | ep_dirty %%+.8f\\n",
            coef(m_lm)[["ep_green"]], coef(m_lm)[["ep_dirty"]]))
library(fwildclusterboot)
set.seed(42); dqrng::dqset.seed(42)
res <- list()
for (param in c("ep_green", "ep_dirty")) {
  cat("  boottest:", param, "... ")
  bt <- boottest(m_lm, param = param, clustid = "country_code", B = 9999)
  cat(sprintf("p_wcb = %%.4f\\n", bt$p_val))
  res[[param]] <- data.table(outcome = "%s", treat = "%s", term = param,
    coef = coef(m_lm)[[param]], p_wcb = bt$p_val,
    conf_low = bt$conf_int[1], conf_high = bt$conf_int[2], B = 9999L,
    nobs = nrow(df), nclust = uniqueN(df$country_code), fe = "pd+dt+pt")
}
fwrite(rbindlist(res), "%s")
',
      gsub("\\\\", "/", tmp_fst),
      tr_var, tr_var, DEPTH_VAR, DEPTH_VAR,
      tr_name, oc, tr_name,
      gsub("\\\\", "/", out_csv))

    for (attempt in seq_len(8L)) {
      cat(sprintf("  [WCB %s %s] tentativo %d ...\n", oc, tr_name, attempt))
      tf <- tempfile(fileext = ".R"); writeLines(worker, tf)
      res_run <- processx::run(RSCRIPT, args = tf, timeout = 420L,
                               stdout = "", stderr = "", error_on_status = FALSE)
      unlink(tf)
      if (isTRUE(res_run$timeout))  { cat("  timeout — retry\n"); Sys.sleep(3); next }
      if (res_run$status != 0)      { cat(sprintf("  crash (exit %d)\n", res_run$status)); Sys.sleep(3); next }
      if (!file.exists(out_csv))    { cat("  no output — retry\n"); Sys.sleep(3); next }
      wcb_check <- tryCatch(fread(out_csv), error = function(e) NULL)
      if (is.null(wcb_check) || nrow(wcb_check) == 0) {
        cat("  CSV vuoto — retry\n"); unlink(out_csv); Sys.sleep(3); next }
      wcb_dirty <- wcb_check[term == "ep_dirty", coef]
      if (length(wcb_dirty) == 0 || is.na(wcb_dirty)) {
        cat("  coef mancante — retry\n"); unlink(out_csv); Sys.sleep(3); next }
      if (abs(wcb_dirty - stata_ref_dirty) > 1e-4) {
        cat(sprintf("  [LAYER-2] corrotto: WCB=%.8f vs Stata=%.8f — RETRY\n",
                    wcb_dirty, stata_ref_dirty))
        unlink(out_csv); Sys.sleep(3); next
      }
      cat(sprintf("  [LAYER-2] OK: delta=%.2e\n", abs(wcb_dirty - stata_ref_dirty)))
      cat(sprintf("  [WCB %s %s] tentativo %d ACCETTATO\n", oc, tr_name, attempt))
      break
    }
    if (!file.exists(out_csv)) stop(sprintf("WCB %s %s fallito", oc, tr_name))
    wcb_parts[[length(wcb_parts) + 1]] <- fread(out_csv)
    unlink(out_csv)
  }
}

## Unifica e scrive
final <- rbindlist(wcb_parts)
fwrite(final, file.path(OUT_DIR, "wcb_decomp_collapsed.csv"))
cat("\n=== FATTO: wcb_decomp_collapsed.csv ===\n")
print(final[, .(outcome, treat, term, coef, p_wcb, B)])
