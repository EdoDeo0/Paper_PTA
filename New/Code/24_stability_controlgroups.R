########################################################
###### 20 — Stabilita' della triple-diff sui sotto-campioni ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 13_tripledd_stability.R. Run: ~10-20 min (dipende da quanti
##              modelli sono gia' cachati; se manca STAB_deepshallow_TREND
##              rifà quella stima su un sotto-campione da 5,3M righe con FE
##              fpd+fdt+pt, a rischio crash come da nota storica).
##
## Cosa fa: rilancia la triple-diff (stessa formula del full panel, 17/18
## Stata) sui sotto-campioni di controllo (11, 12) e produce la tabella di
## stabilita' del coefficiente d'interazione (stile Caselli et al., Table 5):
##   prodHS4    -> solo gli HS6 con in_HS4match=TRUE (non-verdi nella stessa HS4 di un verde)
##   deepshallow-> solo partner PTA (group deep/shallow): identificazione within-treated
##   cem_v1     -> solo i paesi in Output/CEM/matched_countries.csv (trattati+controlli)
## Solo outcome principale (ln_export) e specifica base (senza controlli):
## l'obiettivo e' la STABILITA' di EP:green e EP:dirty, non la tabella completa.
##
## NOTA: C-overlap ESCLUSO da questa run - tiene ~100% delle righe (vedi
## overlap_diagnostics.txt), quindi crasherebbe con lo stesso errore di
## allocatore del full panel. La riga "full" (panel intero) non e' mai stata
## prodotta da questo script - il full panel gira solo via Stata (13/14).
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         New/Data/Subsamples/flag_prodHS4.csv, flag_deepshallow.csv (da 07)
##         Output/CEM/matched_countries.csv (root, CEM v1)
## Output: New/Output/TripleDiff/Tables/tripledd_stability.csv

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(callr)
library(here)
library(data.table)
source(here("New/Code/_sample_config.R"))

## --- Parametri e percorsi --------------------------------------------------
DATA_FILE  <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
OUT_DIR    <- here("New/Output/TripleDiff")
NTHREADS   <- 6L
dir.create(file.path(OUT_DIR, "Models_Output"), recursive = TRUE, showWarnings = FALSE)

# filtro per gruppo: o su hs6 (keep_hs6) o su country_code (keep_cc).
# ordinati dal piu' piccolo: i primi risultati arrivano subito e i gruppi
# grandi rischiosi (RAM) vengono per ultimi.
groups <- list(
  prodHS4     = list(keep_hs6 = fread(here("New/Data/Subsamples/flag_prodHS4.csv"))[in_HS4match == TRUE, hs6]),
  deepshallow = list(keep_cc = fread(here("New/Data/Subsamples/flag_deepshallow.csv"))[group %in% c("deep", "shallow"), country_code]),
  cem_v1      = list(keep_cc = fread(here("Output/CEM/matched_countries.csv"))$country_code)
)

## --- Stima di un gruppo (self-contained, gira in sottoprocesso callr) ------
estimate_group <- function(data_file, green_file, dirty_file, depth_file, out_dir,
                           nthreads, group_name, keep_hs6, keep_cc,
                           hkmo_drop, suffix, depth_var, depth_drop_unmeasured) {
  library(fst)
  library(fixest)
  library(data.table)
  threads_fst(1)
  setFixest_nthreads(nthreads)

  cols <- c("ln_export", "WB_EP_Depth", "TREND_EP_Count", "hs6",
            "country_code", "year", "fpd", "fdt", "pt")
  d <- as.data.table(read_fst(data_file, columns = cols))
  # HK+MO: filtro inline, il sottoprocesso callr non eredita hkmo_filter()
  if (hkmo_drop) d <- d[!country_code %in% c(110L, 121L)]
  if (!is.null(keep_hs6)) d <- d[hs6 %in% keep_hs6]
  if (!is.null(keep_cc))  d <- d[country_code %in% keep_cc]

  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  d[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  d[dirty, on = "hs6", dirty_p := i.dirty_p]
  d[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, dep_val__ = as.numeric(get(depth_var)))]
  d[dep, on = c("country_code", "year"), (depth_var) := i.dep_val__]
  if (depth_drop_unmeasured) {
    n0 <- nrow(d)
    d <- d[!(is.na(get(depth_var)) & WB_EP_Depth > 0)]
    cat(sprintf("[depth] %s: %d righe trattate senza copertura escluse (%.3f%%)\n",
                depth_var, n0 - nrow(d), 100 * (n0 - nrow(d)) / n0))
  }
  d[is.na(get(depth_var)), (depth_var) := 0]
  cat(sprintf("[%s] righe: %s | green: %.1f%% | dirty: %.1f%%\n", group_name,
              format(nrow(d), big.mark = ","), 100 * mean(d$env_good), 100 * mean(d$dirty_p)))

  out <- list()
  treats <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")
  for (tr_name in names(treats)) {
    tr <- treats[[tr_name]]
    rds <- file.path(out_dir, "Models_Output",
                     sprintf("STAB_%s_%s%s.rds", group_name, tr_name, suffix))
    if (file.exists(rds)) { out[[tr_name]] <- readRDS(rds); next }
    f <- sprintf("ln_export ~ %s:env_good + %s:dirty_p + %s:env_good + %s:dirty_p | fpd + fdt + pt",
                 tr, tr, depth_var, depth_var)
    m <- feols(as.formula(f), data = d, cluster = ~country_code, lean = TRUE, mem.clean = TRUE)
    if (anyNA(coef(m))) stop(sprintf("feols non converge (%s, %s): coefficienti NA", group_name, tr_name))
    st <- list(group = group_name, treat = tr_name, coefs = coef(m), se = se(m),
               pval = pvalue(m), nobs = m$nobs)
    saveRDS(st, rds)
    out[[tr_name]] <- st
    rm(m)
    gc()
  }
  out
}

## --- Esecuzione: un sottoprocesso per gruppo -------------------------------
results <- list()
for (g in names(groups)) {
  cat("\n=== Gruppo:", g, "===\n")
  # tryCatch: se un gruppo crasha (es. RAM), gli altri girano comunque
  results[[g]] <- tryCatch(
    callr::r(estimate_group, args = list(
      data_file = DATA_FILE, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
      depth_file = DEPTH_FILE, out_dir = OUT_DIR, nthreads = NTHREADS,
      group_name = g,
      keep_hs6 = if (is.null(groups[[g]]$keep_hs6)) NULL else groups[[g]]$keep_hs6,
      keep_cc  = if (is.null(groups[[g]]$keep_cc))  NULL else groups[[g]]$keep_cc,
      hkmo_drop = HKMO_DROP, suffix = OUT_SUFFIX,
      depth_var = DEPTH_VAR, depth_drop_unmeasured = DEPTH_DROP_UNMEASURED
    ), show = TRUE),
    error = function(e) { cat("[FALLITO]", g, ":", conditionMessage(e), "\n"); NULL })
}
results <- Filter(Negate(is.null), results)

## --- Tabella di stabilita' ---------------------------------------------------
rows <- list()
for (g in names(results)) {
  for (tr_name in names(results[[g]])) {
    st <- results[[g]][[tr_name]]
    rows[[paste(g, tr_name)]] <- data.table(group = g, treat = tr_name,
      term = names(st$coefs), coef = st$coefs, se = st$se, pval = st$pval, nobs = st$nobs)
  }
}
stab <- rbindlist(rows)
fwrite(stab, out_path(file.path(OUT_DIR, "Tables", "tripledd_stability.csv")))
cat("\n[OK] tripledd_stability.csv - confrontare EP:env_good (e :dirty_p) tra i gruppi.\n")
