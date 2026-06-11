########################################################
###### Fase R3 — Triple-diff sulla composizione     ####
########################################################

## Author: Edoardo Vitella
##
## SPECIFICA PRINCIPALE del ridisegno (ROADMAP §7.1):
##   ln_export ~ EP:green_p + EP:dirty_p + TotalDepth:green_p + TotalDepth:dirty_p
##             | fpd + fdt + pt,  cluster = ~country_code
## L'FE fdt assorbe TUTTO cio' che varia a impresa-dest-anno (incluso il PTA stesso):
## identificazione = entro impresa-destinazione-anno, tra prodotti green/dirty vs neutri.
##
## PREREQUISITI (eseguire prima):
##   - New/Code/05_dirty_goods.R  -> New/Data/Dirty/dirty_goods_hs6.csv
##   - New/Code/06_total_depth.R  -> New/Data/TotalDepth/wb_totaldepth_country_year.csv
##   - [opzionale] WITS (04) per tariffs_pref; in assenza usa `tariffs` (MFN) con caveat
##   - Esito audit R1 (02): se concordanza HS assente, NON eseguire su dati non concordati
##
## STATO: DRAFT — rivedere dopo l'audit R1. Un solo job pesante alla volta!
## Pattern callr identico a 01_inference_fix.R (sottoprocesso per sezione).

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here)

SHARED <- list(
  data_file  = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  dirty_file = here("New/Data/Dirty/dirty_goods_hs6.csv"),
  depth_file = here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"),
  out_dir    = here("New/Output/TripleDiff"),
  nthreads   = 12L,
  excl_hkmo  = TRUE   # C4: Hong Kong (110) e Macao (121) fuori dalla main spec
)

# ─────────────────────────────────────────────────────────────────────
# SEZIONE A — Stime principali (self-contained, gira in sottoprocesso)
# ─────────────────────────────────────────────────────────────────────
section_main <- function(data_file, dirty_file, depth_file, out_dir, nthreads, excl_hkmo) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(nthreads)
  dir.create(file.path(out_dir, "Models_Output"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "Tables"), recursive = TRUE, showWarnings = FALSE)

  outcomes <- c("ln_export", "ln_export_qua", "ln_export_value")
  treats   <- c(WB = "WB_EP_Depth", TREND = "TREND_EP_Count")

  ## colonne necessarie (una sola lettura del fst)
  cols <- unique(c(outcomes, unname(treats), "env_good", "hs6", "country_code", "year",
                   "fpd", "fdt", "pt", "tariffs", "ln_hhi_baci", "AD_pdt"))
  cat("Loading", length(cols), "columns...\n")
  d <- as.data.table(read_fst(data_file, columns = cols))
  if (excl_hkmo) { d <- d[!country_code %in% c(110L, 121L)]; cat("HK+MO esclusi\n") }

  ## merge classificazione dirty (hs6) e TotalDepth (country_code x year)
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  d[dirty, on = "hs6", dirty_p := i.dirty_p][is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, TotalDepth_nonEnv)]
  d[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  d[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cat(sprintf("Rows: %s | green: %.1f%% | dirty: %.1f%%\n", format(nrow(d), big.mark = ","),
              100 * mean(d$env_good == 1, na.rm = TRUE), 100 * mean(d$dirty_p == 1)))

  results <- list()
  for (tr_name in names(treats)) {
    tr <- treats[[tr_name]]
    for (y in outcomes) {
      for (spec in c("base", "controls")) {
        key <- paste(tr_name, y, spec, sep = "_")
        rds <- file.path(out_dir, "Models_Output", paste0("TD_", key, ".rds"))
        if (file.exists(rds)) { cat("[SKIP]", key, "\n"); results[[key]] <- readRDS(rds); next }
        ctrl <- if (spec == "controls") " + tariffs + ln_hhi_baci + AD_pdt" else ""
        ## NB: livelli di EP/TotalDepth assorbiti da fdt; env_good/dirty_p da fpd e pt
        f <- sprintf("%s ~ %s:env_good + %s:dirty_p + TotalDepth_nonEnv:env_good + TotalDepth_nonEnv:dirty_p%s | fpd + fdt + pt",
                     y, tr, tr, ctrl)
        cat("Estimating:", f, "\n")
        m <- feols(as.formula(f), data = d, cluster = ~country_code, lean = TRUE)
        st <- list(formula = f, coefs = coef(m), se = se(m), pval = pvalue(m),
                   nobs = m$nobs, n_clust = tryCatch(fitstat(m, "g")[[1]], error = function(e) NA))
        saveRDS(st, rds); results[[key]] <- st
        print(st$coefs); rm(m); gc()
      }
    }
  }

  ## tabella riassuntiva CSV (la versione LaTeX dopo la review dei risultati)
  summ <- rbindlist(lapply(names(results), function(k) {
    st <- results[[k]]
    data.table(model = k, term = names(st$coefs), coef = st$coefs,
               se = st$se, pval = st$pval, nobs = st$nobs, n_clust = st$n_clust)
  }))
  fwrite(summ, file.path(out_dir, "Tables", "tripledd_summary.csv"))
  cat("[OK] tripledd_summary.csv\n")
}

# ─────────────────────────────────────────────────────────────────────
# SEZIONE B — Event study differenziale (pre-trend del triple-diff)
# ─────────────────────────────────────────────────────────────────────
section_eventstudy <- function(data_file, depth_file, out_dir, nthreads, excl_hkmo) {
  library(fst); library(fixest); library(data.table); library(ggplot2)
  threads_fst(1); setFixest_nthreads(nthreads)
  dir.create(file.path(out_dir, "Diagnostics"), recursive = TRUE, showWarnings = FALSE)

  d <- as.data.table(read_fst(data_file, columns = c(
    "ln_export", "env_good", "country_code", "year", "fpd", "fdt", "pt", "WB_EP_Depth")))
  if (excl_hkmo) d <- d[!country_code %in% c(110L, 121L)]

  ## entry year per destinazione = primo anno con EP depth > 0
  entry <- d[WB_EP_Depth > 0, .(entry_year = min(year)), by = country_code]
  d[entry, on = "country_code", entry_year := i.entry_year]
  d[, rel_time := fifelse(is.na(entry_year), -1000L, year - entry_year)]  # -1000 = mai trattato
  d[, rel_time := pmax(pmin(rel_time, 5L), -6L)]                          # binning [-6, +5]
  d[rel_time == -1000L | is.na(entry_year), rel_time := -1L]              # never-treated nel ref

  rds <- file.path(out_dir, "Diagnostics", "eventstudy.rds")
  if (!file.exists(rds)) {
    cat("Event study (leads/lags x env_good)...\n")
    m <- feols(ln_export ~ i(rel_time, env_good, ref = -1) | fpd + fdt + pt,
               data = d, cluster = ~country_code, lean = TRUE)
    st <- list(coefs = coef(m), se = se(m), pval = pvalue(m), nobs = m$nobs)
    saveRDS(st, rds); rm(m); gc()
  } else st <- readRDS(rds)

  cf <- data.table(term = names(st$coefs), b = st$coefs, se = st$se)
  cf <- cf[grepl("rel_time", term)][, t := as.integer(gsub(".*rel_time::(-?\\d+).*", "\\1", term))]
  p <- ggplot(cf, aes(t, b)) +
    geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = -0.5, colour = "grey60") +
    geom_pointrange(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se)) +
    labs(x = "Anni dall'entrata in vigore del PTA", y = "Effetto differenziale green goods",
         title = "Event study: green vs altri prodotti attorno all'entrata PTA") + theme_minimal()
  ggsave(file.path(out_dir, "Diagnostics", "eventstudy_green.png"), p, width = 8, height = 5)
  cat("[OK] eventstudy_green.png — controllare assenza di pre-trend differenziali\n")
}

# ─────────────────────────────────────────────────────────────────────
# SEZIONE C — Permutation inference (su panel collassato d x t x green)
# Riassegna il vettore EP depth tra le destinazioni trattate (timing fisso):
# testa il CONTENUTO ambientale, non l'accordo. Collasso alla BDM (2004)
# per rendere fattibili 1000 permutazioni.
# ─────────────────────────────────────────────────────────────────────
section_permutation <- function(data_file, out_dir, nthreads, excl_hkmo, n_perm = 1000L) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(nthreads)
  dir.create(file.path(out_dir, "Diagnostics"), recursive = TRUE, showWarnings = FALSE)

  d <- as.data.table(read_fst(data_file, columns = c(
    "ln_export", "env_good", "country_code", "year", "WB_EP_Depth")))
  if (excl_hkmo) d <- d[!country_code %in% c(110L, 121L)]

  ## collasso: media ln_export per destinazione x anno x green
  cell <- d[!is.na(ln_export), .(y = mean(ln_export), n = .N,
                                 EP = first(WB_EP_Depth)), by = .(country_code, year, env_good)]
  rm(d); gc()
  cell[, dt_id := .GRP, by = .(country_code, year)]
  cell[, dg_id := .GRP, by = .(country_code, env_good)]
  cell[, tg_id := .GRP, by = .(year, env_good)]

  est <- function(dat) coef(feols(y ~ EP:env_good | dt_id + dg_id + tg_id,
                                  data = dat, weights = ~n, lean = TRUE))[["EP:env_good"]]
  b_obs <- est(cell)
  cat(sprintf("Coefficiente osservato (collassato): %.6f\n", b_obs))

  ## profilo EP per destinazione trattata (vettore depth nel tempo)
  treated <- unique(cell[EP > 0, country_code])
  prof <- unique(cell[country_code %in% treated, .(country_code, year, EP)])
  set.seed(42)
  b_perm <- replicate(n_perm, {
    remap <- setNames(sample(treated), treated)        # permuta i profili tra trattate
    pp <- copy(prof)[, country_code := remap[as.character(country_code)]]
    cc <- copy(cell)[, EP := NULL][pp, on = c("country_code", "year"), EP := i.EP][is.na(EP), EP := 0]
    tryCatch(est(cc), error = function(e) NA_real_)
  })
  pval <- mean(abs(b_perm) >= abs(b_obs), na.rm = TRUE)
  cat(sprintf("Permutation p-value (n=%d): %.4f\n", n_perm, pval))
  fwrite(data.table(b_obs = b_obs, p_perm = pval, n_perm = n_perm),
         file.path(out_dir, "Diagnostics", "permutation_summary.csv"))
  saveRDS(b_perm, file.path(out_dir, "Diagnostics", "permutation_draws.rds"))
  cat("[OK] permutation_summary.csv\n")
}

# ─────────────────────────────────────────────────────────────────────
# ESECUZIONE (un sottoprocesso alla volta)
# ─────────────────────────────────────────────────────────────────────
stopifnot(file.exists(SHARED$dirty_file), file.exists(SHARED$depth_file))

cat("\n=== SEZIONE A: stime principali ===\n")
callr::r(section_main, args = SHARED[c("data_file","dirty_file","depth_file","out_dir","nthreads","excl_hkmo")], show = TRUE)

cat("\n=== SEZIONE B: event study ===\n")
callr::r(section_eventstudy, args = SHARED[c("data_file","depth_file","out_dir","nthreads","excl_hkmo")], show = TRUE)

cat("\n=== SEZIONE C: permutation ===\n")
callr::r(section_permutation, args = SHARED[c("data_file","out_dir","nthreads","excl_hkmo")], show = TRUE)

cat("\n=== DONE Fase R3 (draft) ===\n")
cat("Output: New/Output/TripleDiff/{Tables,Models_Output,Diagnostics}\n")
cat("NB: wild bootstrap sull'interazione principale -> aggiungere dopo review risultati.\n")
