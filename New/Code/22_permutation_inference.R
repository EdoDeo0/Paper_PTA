########################################################
###### 18 — Permutation inference (coarse + spec esatta) ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: sezione 4 di 14_tripledd_collapsed.R (permutazione grezza
##              green) + 14b_permutation_dirty.R (permutazione grezza dirty)
##              + 29_r710_permutation_true.R (permutazione sulla spec
##              esatta). Run: sezione A ~2 min; sezione B con cache completa
##              (40 batch gia' presenti) ~1 min, da zero ~1h40m.
##
## Cosa fa: DUE permutazioni distinte e complementari, entrambe citate nel
## paper (sezione "The dirty margin: anatomy of a false positive"):
##
## A) PERMUTAZIONE GREZZA (aggregata dest x anno x green/dirty, ~7k celle,
##    solo WB): stesso schema di rimescolamento (profili EP scambiati tra
##    le destinazioni trattate, 1000 draw, seed 42) ma su un aggregato piu'
##    grossolano della spec vera. E' il numero "coarser" citato nel paper
##    (dirty: +0.004, p=0.50) - complementare alla spec esatta, non
##    ridondante: mostra che aggregare oltre un certo punto fa sparire
##    (anzi invertire segno) il segnale.
##
## B) PERMUTAZIONE SULLA SPEC ESATTA (3,68M celle, FE pd+dt+pt): stesso
##    schema di rimescolamento ma stimato sulla specifica vera (non un
##    aggregato). Frisch-Waugh incrementale per velocita': y e le
##    interazioni TD non cambiano con la permutazione -> demean UNA volta
##    per batch rispetto a pd+dt+pt; ad ogni permutazione si demeanano solo
##    ep_green/ep_dirty e si stima via qr.solve su 4 colonne. VERIFICA: la
##    permutazione identita' deve riprodurre i coefficienti di 12
##    (tripledd_collapsed.csv) o lo script si ferma. Batch da 50
##    permutazioni, un sottoprocesso callr per batch, cache .rds per batch
##    (resumabile - allocatore R instabile su questa macchina).
##
## Input:  New/Data/Collapsed/panel_pdt_collapsed.fst (da 06)
##         New/Data/Classifications/green_codes_hs1996.csv, dirty_goods_hs6.csv
##         New/Data/TotalDepth/wb_totaldepth_country_year.csv
##         New/Output/TripleDiff/Tables/tripledd_collapsed.csv (da 12, per la verifica)
## Output: New/Output/TripleDiff/Diagnostics/permutation_collapsed.csv (A, green)
##         New/Output/TripleDiff/Diagnostics/permutation_collapsed_dirty.csv (A, dirty)
##         New/Output/TripleDiff/Tables/r710_permutation_draws.csv (B)
##         New/Output/TripleDiff/Tables/r710_permutation_summary.csv (B)

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)
library(fst)
library(fixest)
library(callr)
source(here("New/Code/_sample_config.R"))
threads_fst(1)

CACHE_FST  <- out_path(here("New/Data/Collapsed/panel_pdt_collapsed.fst"))
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
DIRTY_FILE <- here("New/Data/Classifications/dirty_goods_hs6.csv")
DIAG_DIR   <- here("New/Output/TripleDiff/Diagnostics")
TAB_DIR    <- here("New/Output/TripleDiff/Tables")
dir.create(DIAG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

## ============================================================================
## SEZIONE A — permutazione grezza (aggregata dest x anno x green/dirty, WB)
## ============================================================================
cell <- as.data.table(read_fst(CACHE_FST))
green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
dirty <- fread(DIRTY_FILE)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
cell[dirty, on = "hs6", dirty_p := i.dirty_p]
cell[is.na(dirty_p), dirty_p := 0L]

run_coarse_permutation <- function(cell, group_var, term_label, out_file) {
  # collasso a dest x anno x (green o dirty)
  cg <- cell[, c(list(y = weighted.mean(y, n), n = sum(n), EP = first(WB_EP_Depth))),
             by = c("country_code", "year", group_var)]
  cg[, dt_id := .GRP, by = .(country_code, year)]
  cg[, dg_id := .GRP, by = c("country_code", group_var)]
  cg[, tg_id := .GRP, by = c("year", group_var)]

  f <- as.formula(sprintf("y ~ EP:%s | dt_id + dg_id + tg_id", group_var))
  est <- function(dat) coef(feols(f, data = dat, weights = ~n, lean = TRUE))[[sprintf("EP:%s", group_var)]]
  b_obs <- est(cg)
  treated <- unique(cg[EP > 0, country_code])
  prof <- unique(cg[country_code %in% treated, .(country_code, year, EP)])

  set.seed(42)
  b_perm <- replicate(1000L, {
    remap <- setNames(sample(treated), treated)
    pp <- copy(prof)[, country_code := remap[as.character(country_code)]]
    cc <- copy(cg)[, EP := NULL][pp, on = c("country_code", "year"), EP := i.EP][is.na(EP), EP := 0]
    tryCatch(est(cc), error = function(e) NA_real_)
  })
  pval <- mean(abs(b_perm) >= abs(b_obs), na.rm = TRUE)
  cat(sprintf("[coarse %s] coeff osservato %.6f | p-value %.4f (n=1000)\n", term_label, b_obs, pval))
  fwrite(data.table(b_obs = b_obs, p_perm = pval, n_perm = 1000L), out_file)
}

cat("=== Sezione A: permutazione grezza GREEN (WB) ===\n")
run_coarse_permutation(cell, "env_good", "GREEN", out_path(file.path(DIAG_DIR, "permutation_collapsed.csv")))

cat("\n=== Sezione A: permutazione grezza DIRTY (WB) ===\n")
run_coarse_permutation(cell, "dirty_p", "DIRTY", out_path(file.path(DIAG_DIR, "permutation_collapsed_dirty.csv")))

## ============================================================================
## SEZIONE B — permutazione sulla spec esatta (3,68M celle, WB e TREND)
## ============================================================================
## modalita' smoke test: prova veloce (1 batch da 3 permutazioni invece di
## 1000, cache separata) per controllare che lo script non abbia errori prima
## di lanciare il calcolo vero (~1h40m). Stessa logica di _sample_config.R:
## si edita la riga qui sotto e si salva, nessuna variabile d'ambiente.
##   FALSE -> run vero (1000 permutazioni)
##   TRUE  -> prova veloce (3 permutazioni)
TEST    <- FALSE
N_PERM  <- if (TEST) 3L else 1000L
BATCH   <- if (TEST) 3L else 50L
N_BATCH <- N_PERM %/% BATCH
BATCH_CACHE <- out_path(here(if (TEST) "New/Output/TripleDiff/Models/r710_smoke"
                             else      "New/Output/TripleDiff/Models/r710_batches"))
dir.create(BATCH_CACHE, recursive = TRUE, showWarnings = FALSE)

# un batch: carica, demeana i fissi, poi BATCH permutazioni incrementali
run_exact_batch <- function(data_file, green_file, dirty_file, depth_file, tripledd_file,
                            treat_var, batch_id, batch_size, depth_var, depth_drop_unmeasured) {
  library(fst)
  library(fixest)
  library(data.table)
  threads_fst(1)
  setFixest_nthreads(2)

  cell <- as.data.table(read_fst(data_file))
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(dirty_file)[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]
  cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(depth_file)[, .(country_code, year, dep_val__ = get(depth_var))]
  cell[dep, on = c("country_code", "year"), (depth_var) := i.dep_val__]
  if (depth_drop_unmeasured) {
    n0 <- nrow(cell)
    cell <- cell[!(is.na(get(depth_var)) & WB_EP_Depth > 0)]
    cat(sprintf("[depth] %s: %d celle trattate senza copertura escluse (%.3f%%)\n",
                depth_var, n0 - nrow(cell), 100 * (n0 - nrow(cell)) / n0))
  }
  cell[is.na(get(depth_var)), (depth_var) := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  cell[, `:=`(td_green = get(depth_var) * env_good,
              td_dirty = get(depth_var) * dirty_p)]

  fes <- cell[, .(pd, dt, pt)]
  # demean dei fissi (y, td) una volta sola
  Xf <- as.matrix(fixest::demean(cell[, .(y, td_green, td_dirty)], f = fes, weights = cell$n))
  y_dm <- Xf[, 1]; tdg_dm <- Xf[, 2]; tdb_dm <- Xf[, 3]
  rm(Xf); gc()
  sw <- sqrt(cell$n)  # per la WLS via QR (pesi = n)

  # profili EP dei trattati
  treated <- sort(unique(cell[get(treat_var) > 0, country_code]))
  prof <- unique(cell[country_code %in% treated, .(country_code, year, EP = get(treat_var))])

  stima_perm <- function(ep_vec) {
    eg <- ep_vec * cell$env_good; eb <- ep_vec * cell$dirty_p
    Xe <- as.matrix(fixest::demean(data.frame(eg = eg, eb = eb), f = fes, weights = cell$n))
    X <- cbind(Xe[, 1], Xe[, 2], tdg_dm, tdb_dm)
    cf <- qr.solve(X * sw, y_dm * sw)  # WLS: identica a lm pesato
    c(green = cf[1], dirty = cf[2])
  }

  # verifica identita' (solo batch 1): deve riprodurre 12
  if (batch_id == 1L) {
    b0 <- stima_perm(cell[[treat_var]])
    att <- fread(tripledd_file)
    tr_key <- if (treat_var == "WB_EP_Depth") "WB" else "TREND"
    ag <- att[treat == tr_key & grepl(":env_good$", term), coef]
    ab <- att[treat == tr_key & grepl(":dirty_p$", term), coef]
    cat(sprintf("[check %s] FW identita': green %+.6f (12: %+.6f) | dirty %+.6f (12: %+.6f)\n",
                tr_key, b0[1], ag, b0[2], ab))
    if (abs(b0[1] - ag) > 1e-5 || abs(b0[2] - ab) > 1e-5)
      stop("FW non riproduce i coefficienti di 12: fermo il job.")
  }

  set.seed(1000L + batch_id)
  res <- matrix(NA_real_, batch_size, 2)
  for (i in seq_len(batch_size)) {
    remap <- setNames(sample(treated), treated)
    pp <- copy(prof)[, country_code := remap[as.character(country_code)]]
    tmp <- copy(cell[, .(country_code, year)])
    tmp[pp, on = c("country_code", "year"), EP := i.EP]
    tmp[is.na(EP), EP := 0]
    res[i, ] <- tryCatch(stima_perm(tmp$EP), error = function(e) c(NA_real_, NA_real_))
    if (i %% 10L == 0L) cat(sprintf("  [%s batch %d] %d/%d\n", treat_var, batch_id, i, batch_size))
  }
  data.table(treat = treat_var, batch = batch_id, draw = seq_len(batch_size),
             b_green = res[, 1], b_dirty = res[, 2])
}

cat("\n=== Sezione B: permutazione sulla spec esatta ===\n")
for (tv in c("WB_EP_Depth", "TREND_EP_Count")) {
  for (b in seq_len(N_BATCH)) {
    rds <- file.path(BATCH_CACHE, sprintf("perm_%s_b%02d.rds", tv, b))
    if (file.exists(rds)) { cat("[cache]", basename(rds), "\n"); next }
    ok <- FALSE
    for (tent in 1:3) {
      cat(sprintf("== %s batch %d/%d (tentativo %d) - %s\n", tv, b, N_BATCH, tent,
                  format(Sys.time(), "%H:%M:%S")))
      r <- tryCatch(callr::r(run_exact_batch, args = list(
        data_file = CACHE_FST, green_file = GREEN_FILE, dirty_file = DIRTY_FILE,
        depth_file = DEPTH_FILE, tripledd_file = out_path(file.path(TAB_DIR, "tripledd_collapsed.csv")),
        treat_var = tv, batch_id = b, batch_size = BATCH,
        depth_var = DEPTH_VAR, depth_drop_unmeasured = DEPTH_DROP_UNMEASURED
      ), show = TRUE), error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
      if (!is.null(r)) { saveRDS(r, rds); ok <- TRUE; break }
    }
    if (!ok) cat("[BATCH FALLITO dopo 3 tentativi]", tv, b, "- proseguo\n")
  }
}

## aggregazione
files <- list.files(BATCH_CACHE, pattern = "^perm_.*\\.rds$", full.names = TRUE)
draws <- rbindlist(lapply(files, readRDS))
if (nrow(draws) == 0L) stop("Nessun batch riuscito: niente da aggregare.")
suff <- paste0(if (TEST) "_smoke" else "", SAMPLE_SUFFIX)
fwrite(draws, file.path(TAB_DIR, sprintf("r710_permutation_draws%s.csv", suff)))

obs <- fread(out_path(file.path(TAB_DIR, "tripledd_collapsed.csv")))
summ <- list()
for (tv in c("WB_EP_Depth", "TREND_EP_Count")) {
  tr_key <- if (tv == "WB_EP_Depth") "WB" else "TREND"
  bg <- obs[treat == tr_key & grepl(":env_good$", term), coef]
  bb <- obs[treat == tr_key & grepl(":dirty_p$", term), coef]
  dd <- draws[treat == tv]
  summ[[tv]] <- data.table(
    treat = tr_key, n_perm = nrow(dd),
    b_obs_green = bg, p_perm_green = mean(abs(dd$b_green) >= abs(bg), na.rm = TRUE),
    b_obs_dirty = bb, p_perm_dirty = mean(abs(dd$b_dirty) >= abs(bb), na.rm = TRUE))
}
out <- rbindlist(summ)
print(out)
fwrite(out, file.path(TAB_DIR, sprintf("r710_permutation_summary%s.csv", suff)))
cat(sprintf("[OK] r710_permutation_summary%s.csv - %s\n", suff, format(Sys.time())))
