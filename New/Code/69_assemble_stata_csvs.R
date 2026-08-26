########################################################
###### 69 — Assemblaggio: output Stata sotto i nomi canonici (solo I/O) ###
########################################################
## Author: Edoardo Vitella
##
## PERCHE' ESISTE. Alcuni risultati esistono gia' in Stata, ma sotto un nome di
## file o uno schema di colonne diverso da quello che `44_make_tables_tex.R` si
## aspetta. Senza questo passaggio il generatore ripiegherebbe su R pur avendo
## il numero Stata a disposizione — e il rapporto di provenienza segnalerebbe
## un buco che in realta' non c'e'.
##
## NESSUNA STIMA QUI DENTRO: si legge un CSV Stata e se ne riscrive un altro con
## nomi di colonna diversi. Stesso spirito di 48c e 58c.
##
## CONVERSIONI
##   permutation_collapsed_treatedonly.csv  ->  r710_permutation_summary.csv
##       (da formato lungo, una riga per margine, a formato largo)
##   omnibus_collapsed_reghdfe.csv spec=apec ->  tripledd_collapsed_apecgreen.csv
##       (lista verde ristretta all'APEC: i coefficienti sono gia' nell'omnibus)
##
## Output: New/Output/TripleDiff/Tables_Stata/{r710_permutation_summary,
##         tripledd_collapsed_apecgreen}.csv
##
## Esecuzione:
##   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New/Code/69_assemble_stata_csvs.R

rm(list = ls())
suppressWarnings(suppressMessages(library(here)))

DIR_TS <- here("New/Output/TripleDiff/Tables_Stata")
DIR_T  <- here("New/Output/TripleDiff/Tables")

rd <- function(p) if (file.exists(p)) utils::read.csv(p, stringsAsFactors = FALSE) else NULL

## confronto con il gemello R: serve a non scrivere in silenzio numeri diversi
confronta <- function(nome, d_new, key_new, val_new) {
  p_r <- file.path(DIR_T, nome)
  d_r <- rd(p_r)
  if (is.null(d_r) || !"coef" %in% names(d_r)) {
    cat(sprintf("   [%s] nessun gemello R con cui confrontare\n", nome)); return(invisible())
  }
  kr <- if ("term" %in% names(d_r)) paste(d_r$treat, d_r$term) else as.character(seq_len(nrow(d_r)))
  com <- intersect(kr, key_new)
  if (!length(com)) { cat(sprintf("   [%s] chiavi non appaiate\n", nome)); return(invisible()) }
  dmax <- max(abs(d_r$coef[match(com, kr)] - val_new[match(com, key_new)]), na.rm = TRUE)
  cat(sprintf("   [%s] confronto con R: n=%d, scarto max = %.2e%s\n",
              nome, length(com), dmax, if (dmax > 1e-6) "   *** ATTENZIONE ***" else ""))
}

cat("=== 69: assemblaggio CSV Stata sotto i nomi canonici ===\n\n")

## ── 1. Permutazione baseline: da formato lungo a largo ──────────────────────
p <- rd(file.path(DIR_TS, "permutation_collapsed_treatedonly.csv"))
if (is.null(p)) {
  cat("[1] permutation_collapsed_treatedonly.csv assente: salto.\n")
} else {
  out <- do.call(rbind, lapply(c("WB", "TREND"), function(tr) {
    g <- p[p$treat == tr & p$var == "ep_green", ]
    b <- p[p$treat == tr & p$var == "ep_dirty", ]
    if (!nrow(g) || !nrow(b)) return(NULL)
    data.frame(treat = tr, n_perm = g$nreps[1],
               n_used_green = g$nreps[1], n_used_dirty = b$nreps[1],
               b_obs_green = g$b_obs[1], p_perm_green = g$p_perm[1],
               b_obs_dirty = b$b_obs[1], p_perm_dirty = b$p_perm[1],
               stringsAsFactors = FALSE)
  }))
  f <- file.path(DIR_TS, "r710_permutation_summary.csv")
  utils::write.csv(out, f, row.names = FALSE)
  cat(sprintf("[1] scritto %s (%d righe)\n", basename(f), nrow(out)))
  ## confronto mirato: i coefficienti osservati devono coincidere con R
  r <- rd(file.path(DIR_T, "r710_permutation_summary.csv"))
  if (!is.null(r)) {
    m <- merge(r, out, by = "treat", suffixes = c("_r", "_s"))
    dmax <- max(abs(c(m$b_obs_green_r - m$b_obs_green_s,
                      m$b_obs_dirty_r - m$b_obs_dirty_s)), na.rm = TRUE)
    dp <- max(abs(c(m$p_perm_green_r - m$p_perm_green_s,
                    m$p_perm_dirty_r - m$p_perm_dirty_s)), na.rm = TRUE)
    cat(sprintf("   coefficienti osservati vs R: scarto max = %.2e%s\n", dmax,
                if (dmax > 1e-6) "   *** ATTENZIONE ***" else ""))
    cat(sprintf("   p di permutazione vs R:      scarto max = %.4f  (atteso: granularita' del test, ~9 profili distinti)\n", dp))
  }
}

## ── 2. APEC: dall'omnibus al file dedicato ──────────────────────────────────
o <- rd(file.path(DIR_TS, "omnibus_collapsed_reghdfe.csv"))
if (is.null(o)) {
  cat("[2] omnibus_collapsed_reghdfe.csv assente: salto.\n")
} else {
  a <- o[o$spec == "apec" & o$var %in% c("ep_green", "ep_dirty", "td_green", "td_dirty"), ]
  if (!nrow(a)) {
    cat("[2] nessuna riga spec=apec nell'omnibus: salto.\n")
  } else {
    ## nomi dei termini identici a quelli scritti da R in 43_apec_egl_subsample.R
    termine <- function(var, treat) {
      xv <- if (treat == "WB") "WB_EP_Depth" else "TREND_EP_Count"
      switch(var,
        ep_green = paste0(xv, ":apec_green"),
        ep_dirty = paste0(xv, ":dirty_p"),
        td_green = "apec_green:TotalDepth_nonEnv",
        td_dirty = "dirty_p:TotalDepth_nonEnv")
    }
    ord <- c("ep_green", "ep_dirty", "td_green", "td_dirty")
    out <- do.call(rbind, lapply(c("WB", "TREND"), function(tr) {
      s <- a[a$treat == tr, ]
      s <- s[match(ord, s$var), ]
      data.frame(treat = tr, term = vapply(ord, termine, "", treat = tr),
                 coef = s$coef, se = s$stderr, pval = s$pval, nobs = s$N,
                 stringsAsFactors = FALSE)
    }))
    f <- file.path(DIR_TS, "tripledd_collapsed_apecgreen.csv")
    utils::write.csv(out, f, row.names = FALSE)
    cat(sprintf("[2] scritto %s (%d righe)\n", basename(f), nrow(out)))
    confronta("tripledd_collapsed_apecgreen.csv", out, paste(out$treat, out$term), out$coef)
  }
}

cat("\n=== 69 fatto ===\n")
