########################################################
###### 66c — Fusione e verifica dei blocchi di permutazione ###
########################################################
## Author: Edoardo Vitella
##
## COSA FA. I blocchi prodotti da 66b_permutation_chunk.do coprono intervalli
## disgiunti di repliche della stessa variante. Questo script li rimette insieme
## e produce esattamente gli stessi file che avrebbe scritto 66 in un run unico:
##
##   permutation_draws_treatedonly{sfx}.csv        estrazioni 1..1000 riunite
##   permutation_collapsed_treatedonly{sfx}.csv    sommario (schema 56b)
##   r710_permutation_summary{sfx}.csv             sommario in schema R
##
## PRIMA DI SCRIVERE, TRE CONTROLLI. Nessun file viene prodotto se uno fallisce.
##
##   1. COPERTURA. Le repliche riunite devono essere esattamente 1..NREPS, senza
##      buchi e senza doppioni. Un blocco morto a meta' lascia un buco; due
##      blocchi con intervalli sovrapposti lasciano doppioni. Entrambi i casi
##      falserebbero il p-value (il denominatore non tornerebbe), quindi sono
##      errori bloccanti, non avvisi.
##
##   2. ACCORDO SU b_obs. Ogni blocco ricalcola per conto suo il coefficiente
##      osservato e lo scrive nel proprio file _bobs_. Sono la stessa stima sugli
##      stessi dati: devono coincidere a 1e-10. Se un blocco avesse letto un
##      dataset diverso, o fosse partito con parametri sbagliati, si tradirebbe
##      qui invece di contribuire silenziosamente al p-value.
##
##   3. IDENTITA' CON 66 (il controllo che conta). Il seed dipende solo dal numero
##      di replica, quindi spezzare la sequenza NON deve cambiare un solo numero.
##      Le prime repliche erano gia' state prodotte da 66 in un run continuo e
##      sono conservate in Diagnostics/permutation_collaudo66{sfx}.csv: qui si
##      verifica che i blocchi le riproducano IDENTICHE. E' la prova che la
##      parallelizzazione e' una riorganizzazione del calcolo e non una sua
##      approssimazione.
##
## USO (a blocchi terminati):
##   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New/Code/66c_merge_permutation_chunks.R
##
## Esce con stato 1 se una variante non supera i controlli.

rm(list = ls())
suppressWarnings(suppressMessages(library(here)))

DIR_TS <- here("New/Output/TripleDiff/Tables_Stata")
DIR_D  <- here("New/Output/TripleDiff/Diagnostics")
NREPS  <- 1000
SFX    <- c("_desta", "_inclHKMO", "_inclHKMO_desta")

problemi <- 0L

for (sfx in SFX) {

  cat("\n=========================================================\n")
  cat(" variante '", sfx, "'\n", sep = "")
  cat("=========================================================\n")

  ## -- raccolta blocchi --------------------------------------------------
  pat <- paste0("^permutation_draws_treatedonly", sfx, "_r[0-9]+_[0-9]+\\.csv$")
  ff  <- list.files(DIR_TS, pattern = pat, full.names = TRUE)
  if (!length(ff)) {
    cat("  nessun blocco trovato — salto\n"); next
  }
  d <- do.call(rbind, lapply(ff, utils::read.csv, stringsAsFactors = FALSE))
  cat(sprintf("  blocchi: %d | righe raccolte: %d\n", length(ff), nrow(d)))
  for (f in sort(ff)) {
    n <- nrow(utils::read.csv(f, stringsAsFactors = FALSE))
    cat(sprintf("    %-56s %4d\n", basename(f), n))
  }

  ## -- controllo 1: copertura -------------------------------------------
  mancanti <- setdiff(seq_len(NREPS), d$rep)
  doppie   <- d$rep[duplicated(d$rep)]
  if (length(mancanti) || length(doppie)) {
    cat(sprintf("  [BLOCCATO] repliche mancanti: %d | duplicate: %d\n",
                length(mancanti), length(doppie)))
    if (length(mancanti))
      cat("    mancanti (prime 20): ",
          paste(utils::head(mancanti, 20), collapse = ", "), "\n")
    if (length(doppie))
      cat("    duplicate (prime 20): ",
          paste(utils::head(unique(doppie), 20), collapse = ", "), "\n")
    problemi <- problemi + 1L
    next
  }
  d <- d[order(d$rep), ]
  cat("  [ok] copertura 1..", NREPS, " completa, nessun doppione\n", sep = "")

  ## -- controllo 2: accordo su b_obs fra i blocchi -----------------------
  patb <- paste0("^permutation_bobs_treatedonly", sfx, "_r[0-9]+_[0-9]+\\.csv$")
  fb   <- list.files(DIR_TS, pattern = patb, full.names = TRUE)
  if (!length(fb)) {
    cat("  [BLOCCATO] nessun file _bobs_: impossibile verificare i blocchi\n")
    problemi <- problemi + 1L; next
  }
  b <- do.call(rbind, lapply(fb, utils::read.csv, stringsAsFactors = FALSE))
  cols <- c("b_obs_wb_green", "b_obs_wb_dirty", "b_obs_tr_green", "b_obs_tr_dirty",
            "nobs", "nclust")
  sp <- max(vapply(cols, function(cc) diff(range(b[[cc]])), numeric(1)))
  if (sp > 1e-10) {
    cat(sprintf("  [BLOCCATO] i blocchi non concordano su b_obs (spread %.3e)\n", sp))
    print(b[, c("repfrom", "repto", cols)])
    problemi <- problemi + 1L; next
  }
  cat(sprintf("  [ok] %d blocchi concordano su b_obs (spread %.1e)\n", nrow(b), sp))

  b_obs <- c(WB_ep_green    = b$b_obs_wb_green[1], WB_ep_dirty    = b$b_obs_wb_dirty[1],
             TREND_ep_green = b$b_obs_tr_green[1], TREND_ep_dirty = b$b_obs_tr_dirty[1])
  nobs   <- b$nobs[1]
  nclust <- b$nclust[1]

  ## -- controllo 3: identita' con il run continuo di 66 ------------------
  fref <- file.path(DIR_D, paste0("permutation_collaudo66", sfx, ".csv"))
  if (!file.exists(fref)) {
    cat("  [BLOCCATO] manca il riferimento", basename(fref), "\n")
    problemi <- problemi + 1L; next
  }
  r <- utils::read.csv(fref, stringsAsFactors = FALSE)
  com <- intersect(r$rep, d$rep)
  vv  <- c("b_wb_green", "b_wb_dirty", "b_tr_green", "b_tr_dirty")
  dm  <- max(abs(as.matrix(r[match(com, r$rep), vv]) -
                 as.matrix(d[match(com, d$rep), vv])))
  if (!is.finite(dm) || dm > 0) {
    cat(sprintf("  [BLOCCATO] i blocchi NON riproducono 66 su %d repliche (|d|max=%.3e)\n",
                length(com), dm))
    cat("    Spezzare la sequenza doveva essere neutrale. Non lo e': non fondere.\n")
    problemi <- problemi + 1L; next
  }
  cat(sprintf("  [ok] identita' con 66 su %d repliche di collaudo (|d|max=%.1e)\n",
              length(com), dm))

  ## -- p-value (formule identiche a 66) ---------------------------------
  ## p_perm e' la versione con correzione (1+k)/(1+B), che e' quella citata dal
  ## paper; p_perm_naive e' k/B, riportata solo per trasparenza.
  colonna <- c(WB_ep_green = "b_wb_green", WB_ep_dirty = "b_wb_dirty",
               TREND_ep_green = "b_tr_green", TREND_ep_dirty = "b_tr_dirty")
  n_ext <- p_c <- p_n <- setNames(numeric(4), names(b_obs))
  for (k in names(b_obs)) {
    n_ext[k] <- sum(abs(d[[colonna[k]]]) >= abs(b_obs[k]))
    p_c[k]   <- (1 + n_ext[k]) / (1 + NREPS)
    p_n[k]   <- n_ext[k] / NREPS
    cat(sprintf("  %-15s b_obs=%12.9f  estremi=%4d  p=%.4f\n",
                k, b_obs[k], n_ext[k], p_c[k]))
  }

  ## -- scrittura ---------------------------------------------------------
  utils::write.csv(d, file.path(DIR_TS,
    paste0("permutation_draws_treatedonly", sfx, ".csv")), row.names = FALSE)

  righe <- do.call(rbind, lapply(names(b_obs), function(k) {
    p <- strsplit(k, "_", fixed = TRUE)[[1]]
    data.frame(treat = p[1], var = paste(p[2], p[3], sep = "_"),
               b_obs = b_obs[k], p_perm = p_c[k], p_perm_naive = p_n[k],
               nreps = NREPS, ndraws_extreme = n_ext[k],
               nobs = nobs, nclust = nclust, design = "treated_only",
               source = "reghdfe_permutation_stata_66b+66c",
               stringsAsFactors = FALSE)
  }))
  utils::write.csv(righe, file.path(DIR_TS,
    paste0("permutation_collapsed_treatedonly", sfx, ".csv")), row.names = FALSE)

  ## stesso sommario in schema R, per 44_make_tables_tex.R
  rr <- data.frame(
    treat = c("WB", "TREND"), n_perm = NREPS,
    n_used_green = NREPS, n_used_dirty = NREPS,
    b_obs_green = c(b_obs["WB_ep_green"],  b_obs["TREND_ep_green"]),
    p_perm_green = c(p_c["WB_ep_green"],   p_c["TREND_ep_green"]),
    b_obs_dirty = c(b_obs["WB_ep_dirty"],  b_obs["TREND_ep_dirty"]),
    p_perm_dirty = c(p_c["WB_ep_dirty"],   p_c["TREND_ep_dirty"]),
    stringsAsFactors = FALSE)
  utils::write.csv(rr, file.path(DIR_TS,
    paste0("r710_permutation_summary", sfx, ".csv")), row.names = FALSE)

  cat("  [scritto] draws + collapsed + summary\n")
}

cat("\n---------------------------------------------------------\n")
if (problemi == 0L) cat("Tutte le varianti fuse e verificate.\n") else
  cat(sprintf("VARIANTI NON FUSE: %d. Nessun file scritto per quelle.\n", problemi))
cat("---------------------------------------------------------\n")

quit(status = if (problemi == 0L) 0L else 1L)
