########################################################################
###### R7.10 — Permutation inference sulla spec collassata VERA      ###
########################################################################

## Author: Edoardo Vitella
##
## Major 5: la permutation di 14 gira su un aggregato dest×anno×green
## (~7k celle), non sulla specifica vera (3,68M celle, FE pd+dt+pt).
## Qui: 1.000 riassegnazioni dei profili EP (depth+timing) tra le 23
## destinazioni trattate (stesso schema di 14, remap casuale), stimate
## sulla spec vera per WB e TREND.
##
## COME (velocita'): Frisch-Waugh incrementale. y e le interazioni TD non
## cambiano con la permutazione -> demean UNA volta per batch rispetto a
## pd+dt+pt; a ogni permutazione si demeanano solo ep_green/ep_dirty e si
## stima lm.wfit su 4 colonne. VERIFICA: la permutazione identita' deve
## riprodurre i coefficienti di 14 (tripledd_collapsed.csv) o ci si ferma.
##
## RESUMABILITA' (allocatore R instabile su questa macchina): batch da 50
## permutazioni, un sottoprocesso callr per batch, cache .rds per batch.
## Rilanciabile: i batch gia' fatti vengono saltati. Seed = 1000 + batch.
##
## Lancio notturno (detached, da PowerShell):
##   Start-Process "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" `
##     -ArgumentList '"C:\Work\projects\Paper_PTA\New\Code\29_r710_permutation_true.R"' `
##     -WindowStyle Hidden `
##     -RedirectStandardOutput "C:\Work\projects\Paper_PTA\New\Output\r710_run.log" `
##     -RedirectStandardError  "C:\Work\projects\Paper_PTA\New\Output\r710_run.err"
##
## Output: New/Output/TripleDiff/Tables/r710_permutation_draws.csv
##         New/Output/TripleDiff/Tables/r710_permutation_summary.csv

library(callr); library(here); library(data.table)

## modalita' smoke test: R710_TEST=1 -> 1 batch da 3, cache separata
TEST    <- identical(Sys.getenv("R710_TEST"), "1")
N_PERM  <- if (TEST) 3L else 1000L
BATCH   <- if (TEST) 3L else 50L
N_BATCH <- N_PERM %/% BATCH
CACHE   <- here(if (TEST) "New/Output/TripleDiff/Models/r710_smoke"
                else      "New/Output/TripleDiff/Models/r710_batches")
if (!dir.exists(CACHE)) dir.create(CACHE, recursive = TRUE)

## Un batch: carica, demeana i fissi, poi BATCH permutazioni incrementali.
run_batch <- function(treat_var, batch_id, batch_size) {
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(2)
  base <- "C:/Work/projects/Paper_PTA"
  cell <- as.data.table(read_fst(file.path(base, "New/Data/Collapsed/panel_pdt_collapsed.fst")))
  green <- fread(file.path(base, "New/Data/Concordance/Env_Codes_HS1996.csv"),
                 colClasses = list(character = "hs6_final"))
  cell[, env_good := as.integer(sprintf("%06d", as.integer(hs6)) %in% unique(green$hs6_final))]
  dirty <- fread(file.path(base, "New/Data/Dirty/dirty_goods_hs6.csv"))[, .(hs6 = as.integer(hs6), dirty_p = dirty)]
  cell[dirty, on = "hs6", dirty_p := i.dirty_p]; cell[is.na(dirty_p), dirty_p := 0L]
  dep <- fread(file.path(base, "New/Data/TotalDepth/wb_totaldepth_country_year.csv"))[
    , .(country_code, year, TotalDepth_nonEnv)]
  cell[dep, on = c("country_code", "year"), TotalDepth_nonEnv := i.TotalDepth_nonEnv]
  cell[is.na(TotalDepth_nonEnv), TotalDepth_nonEnv := 0]
  cell[, pd := .GRP, by = .(hs6, country_code)]
  cell[, dt := .GRP, by = .(country_code, year)]
  cell[, pt := .GRP, by = .(hs6, year)]
  cell[, `:=`(td_green = TotalDepth_nonEnv * env_good,
              td_dirty = TotalDepth_nonEnv * dirty_p)]

  fes <- cell[, .(pd, dt, pt)]
  ## demean dei fissi (y, td) una volta sola
  Xf <- as.matrix(fixest::demean(cell[, .(y, td_green, td_dirty)], f = fes, weights = cell$n))
  y_dm <- Xf[, 1]; tdg_dm <- Xf[, 2]; tdb_dm <- Xf[, 3]; rm(Xf); gc()
  sw <- sqrt(cell$n)                       # per la WLS via QR (pesi = n)

  ## profili EP dei trattati (schema di 14)
  treated <- sort(unique(cell[get(treat_var) > 0, country_code]))
  prof <- unique(cell[country_code %in% treated,
                      .(country_code, year, EP = get(treat_var))])

  stima_perm <- function(ep_vec) {
    eg <- ep_vec * cell$env_good; eb <- ep_vec * cell$dirty_p
    Xe <- as.matrix(fixest::demean(data.frame(eg = eg, eb = eb), f = fes, weights = cell$n))
    X <- cbind(Xe[, 1], Xe[, 2], tdg_dm, tdb_dm)
    cf <- qr.solve(X * sw, y_dm * sw)      # WLS: identica a lm pesato
    c(green = cf[1], dirty = cf[2])
  }

  ## verifica identita' (solo batch 1): deve riprodurre 14
  if (batch_id == 1L) {
    b0 <- stima_perm(cell[[treat_var]])
    att <- fread(file.path(base, "New/Output/TripleDiff/Tables/tripledd_collapsed.csv"))
    tr_key <- if (treat_var == "WB_EP_Depth") "WB" else "TREND"
    ag <- att[treat == tr_key & grepl(":env_good$", term), coef]
    ab <- att[treat == tr_key & grepl(":dirty_p$", term), coef]
    cat(sprintf("[check %s] FW identita': green %+.6f (14: %+.6f) | dirty %+.6f (14: %+.6f)\n",
                tr_key, b0[1], ag, b0[2], ab))
    if (abs(b0[1] - ag) > 1e-5 || abs(b0[2] - ab) > 1e-5)
      stop("FW non riproduce i coefficienti di 14: fermo il job.")
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

## ── Orchestratore: batch per WB e TREND, cache, retry ─────────────────
for (tv in c("WB_EP_Depth", "TREND_EP_Count")) {
  for (b in seq_len(N_BATCH)) {
    rds <- file.path(CACHE, sprintf("perm_%s_b%02d.rds", tv, b))
    if (file.exists(rds)) { cat("[cache]", basename(rds), "\n"); next }
    ok <- FALSE
    for (tent in 1:3) {
      cat(sprintf("== %s batch %d/%d (tentativo %d) — %s\n", tv, b, N_BATCH, tent,
                  format(Sys.time(), "%H:%M:%S")))
      r <- tryCatch(callr::r(run_batch,
                             args = list(treat_var = tv, batch_id = b, batch_size = BATCH),
                             show = TRUE),
                    error = function(e) { cat("[CRASH]", conditionMessage(e), "\n"); NULL })
      if (!is.null(r)) { saveRDS(r, rds); ok <- TRUE; break }
    }
    if (!ok) cat("[BATCH FALLITO dopo 3 tentativi]", tv, b, "— proseguo\n")
  }
}

## ── Aggregazione ──────────────────────────────────────────────────────
files <- list.files(CACHE, pattern = "^perm_.*\\.rds$", full.names = TRUE)
draws <- rbindlist(lapply(files, readRDS))
if (nrow(draws) == 0L) stop("Nessun batch riuscito: niente da aggregare.")
suff <- if (TEST) "_smoke" else ""
fwrite(draws, here(sprintf("New/Output/TripleDiff/Tables/r710_permutation_draws%s.csv", suff)))

obs <- fread(here("New/Output/TripleDiff/Tables/tripledd_collapsed.csv"))
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
fwrite(out, here(sprintf("New/Output/TripleDiff/Tables/r710_permutation_summary%s.csv", suff)))
cat(sprintf("[OK] r710_permutation_summary%s.csv — %s\n", suff, format(Sys.time())))
