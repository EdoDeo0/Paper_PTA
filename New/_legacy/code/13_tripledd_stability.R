########################################################################
###### Fase R-control — Stabilità della triple-diff sui control group ##
########################################################################

## Author: Edoardo Vitella
##
## Checkpoint ROADMAP §7.4.5: rieseguire la triple-diff (§7.1, stessa formula
## di 07_triple_diff.R) sui sub-campioni di controllo e produrre la tabella di
## stabilità del coefficiente d'interazione (stile Caselli et al., Table 5).
##
## Gruppi (definiti dagli script 08-11 + CEM v1):
##   cem_v1     -> solo i paesi in Output/CEM/matched_countries.csv (trattati+controlli)
##   prodHS4    -> solo gli HS6 con in_HS4match=TRUE (non-verdi nella stessa HS4 di un verde)
##   overlap    -> solo gli HS6 con overlap_cem=TRUE (common support trattati/controlli CEM)
##   deepshallow-> solo partner PTA (group deep/shallow): identificazione within-treated
##
## Solo outcome principale (ln_export) e specifica base (senza controlli):
## l'obiettivo è la STABILITÀ di EP:green e EP:dirty, non la tabella completa.
## La riga "full" viene letta dalla cache di 07 (stessi modelli, non ristimati).
##
## NB CEM: filtro non pesato (i pesi CEM in matched_countries.csv non vengono
## applicati) — il sub-campione definisce il confronto, semplicità > finezza.
## Un solo job pesante alla volta: eseguire DOPO la fine di 07.

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here); library(data.table)

SHARED <- list(
  data_file  = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  green_file = here("New/Data/Concordance/Env_Codes_HS1996.csv"),
  dirty_file = here("New/Data/Dirty/dirty_goods_hs6.csv"),
  depth_file = here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"),
  out_dir    = here("New/Output/TripleDiff"),
  nthreads   = 6L
)

## filtro per gruppo: o su hs6 (keep_hs6) o su country_code (keep_cc).
## Ordinati dal più piccolo: i primi risultati arrivano subito e i gruppi
## grandi rischiosi (RAM) vengono per ultimi.
## NB: C-overlap ESCLUSO da questa run — tiene ~100% delle righe (vedi
## overlap_diagnostics.txt), quindi crasherebbe con lo stesso errore di
## allocatore del full panel (07, 2026-07-06). Da stimare su macchina più
## capiente insieme al full.
groups <- list(
  prodHS4 = list(keep_hs6 = fread(here("New/Data/Subsamples/flag_prodHS4.csv"))[in_HS4match == TRUE, hs6]),
  deepshallow = list(keep_cc = fread(here("New/Data/Subsamples/flag_deepshallow.csv"))[group %in% c("deep", "shallow"), country_code]),
  cem_v1 = list(keep_cc = fread(here("Output/CEM/matched_countries.csv"))$country_code)
)

# ── Stima di un gruppo (self-contained, gira in sottoprocesso callr) ──
estimate_group <- function(data_file, green_file, dirty_file, depth_file, out_dir,
                           nthreads, group_name, keep_hs6, keep_cc) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(nthreads)

  cols <- c("ln_export", "WB_EP_Depth", "TREND_EP_Count", "hs6",
            "country_code", "year", "fpd", "fdt", "pt")
  d <- as.data.table(read_fst(data_file, columns = cols))
  d <- d[!country_code %in% c(110L, 121L)]                    # HK+MO fuori (come 07)
  if (!is.null(keep_hs6)) d <- d[hs6 %in% keep_hs6]
  if (!is.null(keep_cc))  d <- d[country_code %in% keep_cc]

  ## env_good dalla lista green HS1996 e dirty_p dalla lista 05 (come 07)
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  d[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  d[dirty, on = "hs6", dirty_p := i.dirty_p]; d[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, TotalDepth_nonEnv)]
  d[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  d[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cat(sprintf("[%s] righe: %s | green: %.1f%% | dirty: %.1f%%\n", group_name,
              format(nrow(d), big.mark = ","), 100 * mean(d$env_good), 100 * mean(d$dirty_p)))

  out <- list()
  treats <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")
  for (tr_name in names(treats)) {
    tr <- treats[[tr_name]]
    rds <- file.path(out_dir, "Models_Output", sprintf("STAB_%s_%s.rds", group_name, tr_name))
    if (file.exists(rds)) { out[[tr_name]] <- readRDS(rds); next }
    f <- sprintf("ln_export ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p | fpd + fdt + pt", tr, tr)
    m <- feols(as.formula(f), data = d, cluster = ~country_code, lean = TRUE, mem.clean = TRUE)
    st <- list(group = group_name, treat = tr_name, coefs = coef(m), se = se(m),
               pval = pvalue(m), nobs = m$nobs)
    saveRDS(st, rds); out[[tr_name]] <- st
    rm(m); gc()
  }
  out
}

# ── Esecuzione: un sottoprocesso per gruppo ───────────────────────────
dir.create(file.path(SHARED$out_dir, "Models_Output"), recursive = TRUE, showWarnings = FALSE)
results <- list()
for (g in names(groups)) {
  cat("\n=== Gruppo:", g, "===\n")
  ## tryCatch: se un gruppo crasha (es. RAM), gli altri girano comunque
  results[[g]] <- tryCatch(
    callr::r(estimate_group, args = c(SHARED, list(
      group_name = g,
      keep_hs6 = if (is.null(groups[[g]]$keep_hs6)) NULL else groups[[g]]$keep_hs6,
      keep_cc  = if (is.null(groups[[g]]$keep_cc))  NULL else groups[[g]]$keep_cc)), show = TRUE),
    error = function(e) { cat("[FALLITO]", g, ":", conditionMessage(e), "\n"); NULL })
}
results <- Filter(Negate(is.null), results)

# ── Tabella di stabilità (aggiunge la riga full dalla cache di 07) ────
rows <- list()
for (tr_name in c("WB", "TREND")) {
  full_rds <- file.path(SHARED$out_dir, "Models_Output", sprintf("TD_%s_ln_export_base.rds", tr_name))
  if (file.exists(full_rds)) {
    st <- readRDS(full_rds)
    rows[[paste0("full_", tr_name)]] <- data.table(group = "full", treat = tr_name,
      term = names(st$coefs), coef = st$coefs, se = st$se, pval = st$pval, nobs = st$nobs)
  }
}
for (g in names(results)) for (tr_name in names(results[[g]])) {
  st <- results[[g]][[tr_name]]
  rows[[paste(g, tr_name)]] <- data.table(group = g, treat = tr_name,
    term = names(st$coefs), coef = st$coefs, se = st$se, pval = st$pval, nobs = st$nobs)
}
stab <- rbindlist(rows)
fwrite(stab, file.path(SHARED$out_dir, "Tables", "tripledd_stability.csv"))
cat("\n[OK] tripledd_stability.csv — confrontare EP:env_good (e :dirty_p) tra i gruppi.\n")
