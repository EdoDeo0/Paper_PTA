########################################################################
###### Fase R-control — Sub-sample 1: C-prod-HS4 (≈ Control 3 di     ###
###### Caselli, Huang, Tomasi & Zhu)                                 ###
########################################################################

## Author: Edoardo Vitella
##
## INTUIZIONE
## ----------
## Oggi confrontiamo i 247 prodotti "verdi" (env_good == 1) con TUTTI gli
## altri ~4.752 prodotti non-verdi, qualunque essi siano. E' il confronto
## piu' ampio possibile e quindi il meno credibile: molti non-verdi non
## hanno nulla in comune con i verdi (mercati, dinamiche, shock diversi).
##
## Idea di questo script: ogni prodotto verde appartiene a una famiglia
## merceologica piu' larga, la categoria HS4 (i primi 4 digit del codice
## HS6). Es. HS6 440121 (legna da ardere certificata, verde) appartiene
## alla HS4 4401 (legna da ardere in generale). Restringiamo il controllo
## ai prodotti NON verdi che appartengono alla STESSA HS4 di almeno un
## prodotto verde -> il confronto resta dentro la stessa "famiglia" di
## beni, molto piu' comparabile del full sample.
##
## ATTENZIONE (limite reale, non cosmetico):
## Se un'impresa esporta sia il verde sia il non-verde della stessa HS4,
## e l'accordo la spinge a riallocare risorse dal non-verde al verde
## (spillover within-firm, Eckel et al. 2023), il "controllo" non e' piu'
## immune al trattamento: si muove anch'esso, nella direzione opposta.
## Per questo C-prod-HS4 NON va mai usato da solo: va sempre confrontato
## con C-overlap (script 10), che e' immune a questo problema.
##
## Lettura LEGGERA del .fst principale: solo 2 colonne (hs6, env_good).
## Esegue in sottoprocesso (pattern identico a 01_inference_fix.R) per
## tenere la memoria sotto controllo anche su 49,2M righe.
##
## Output:
##   New/Data/Subsamples/flag_prodHS4.csv
##     (hs6, hs4, env_good, in_HS4match, n_obs)
##   New/Output/Subsamples/prodHS4_diagnostics.txt

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here)

SHARED <- list(
  data_file  = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  green_file = here("New/Data/Concordance/Env_Codes_HS1996.csv"),
  out_data   = here("New/Data/Subsamples"),
  out_diag   = here("New/Output/Subsamples")
)

build_prodHS4 <- function(data_file, green_file, out_data, out_diag) {
  library(fst); library(data.table)
  threads_fst(1)
  dir.create(out_data, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_diag, recursive = TRUE, showWarnings = FALSE)

  ## ── 1. Lettura leggera: solo hs6 ─────────────────────────────────────
  ## env_good del pannello viene RICALCOLATO qui contro la lista green
  ## tradotta a HS1996 (03b_green_codes_to_hs1996.R) invece di usare la
  ## colonna env_good gia' presente nel .fst: quella e' stata costruita
  ## mergiando Data/Env_Codes_HS.dta (nativo HS2012) direttamente contro
  ## hs6 (trattato come HS1996), senza alcuna concordanza di vintage.
  cat("Loading hs6 (1 colonna, tutte le righe)...\n")
  d <- as.data.table(read_fst(data_file, columns = c("hs6")))
  d[, hs6_str := sprintf("%06d", as.integer(hs6))]

  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  green_codes <- unique(green$hs6_final)
  d[, env_good := as.integer(hs6_str %in% green_codes)]
  d[, hs6_str := NULL]

  ## hs4 = primi 4 digit del codice HS6 a 6 cifre (con zero padding)
  d[, hs4 := substr(sprintf("%06d", as.integer(hs6)), 1, 4)]

  ## ── 2. Famiglie HS4 che contengono almeno un prodotto verde ─────────
  green_hs4 <- unique(d[env_good == 1, hs4])
  cat(sprintf("HS4 con almeno un prodotto verde: %d\n", length(green_hs4)))

  ## flag a livello di RIGA: la riga sopravvive a C-prod-HS4 se:
  ##  - e' verde (sempre incluso, e' il trattato), OPPURE
  ##  - e' non-verde ma appartiene a una famiglia HS4 "verde"
  d[, in_HS4match := (env_good == 1) | (hs4 %in% green_hs4)]

  ## ── 3. Conteggi (la decisione si prende sui numeri, non a priori) ───
  prod_tab <- unique(d[, .(hs6, hs4, env_good)])
  n_obs_tab <- d[, .N, by = hs6]
  prod_tab <- merge(prod_tab, n_obs_tab, by = "hs6", all.x = TRUE)
  prod_tab[, in_HS4match := (env_good == 1) | (hs4 %in% green_hs4)]

  n_nongreen_tot   <- prod_tab[env_good == 0, .N]
  n_nongreen_match <- prod_tab[env_good == 0 & in_HS4match == TRUE, .N]
  n_rows_tot    <- nrow(d)
  n_rows_match  <- d[, sum(in_HS4match)]

  diag_txt <- c(
    "=== C-prod-HS4 — diagnostica (checkpoint 7.4.5) ===",
    sprintf("HS4 'verdi' (contengono >=1 prodotto verde): %d", length(green_hs4)),
    sprintf("Prodotti non-verdi: %d totali -> %d entro HS4 verdi (%.1f%%)",
            n_nongreen_tot, n_nongreen_match, 100 * n_nongreen_match / n_nongreen_tot),
    sprintf("Righe (osservazioni): %s totali -> %s sopravvivono a C-prod-HS4 (%.1f%%)",
            format(n_rows_tot, big.mark = ","), format(n_rows_match, big.mark = ","),
            100 * n_rows_match / n_rows_tot)
  )
  writeLines(diag_txt, file.path(out_diag, "prodHS4_diagnostics.txt"))
  cat(paste(diag_txt, collapse = "\n"), "\n")

  ## ── 4. Flag file leggero da mergiare dopo su hs6 ─────────────────────
  fwrite(prod_tab[, .(hs6, hs4, env_good, in_HS4match, n_obs = N)],
         file.path(out_data, "flag_prodHS4.csv"))
  cat("[OK] flag_prodHS4.csv —", nrow(prod_tab), "codici HS6\n")
}

stopifnot("Lista green HS1996 non trovata — eseguire prima 03b_green_codes_to_hs1996.R" = file.exists(SHARED$green_file))
callr::r(build_prodHS4, args = SHARED, show = TRUE)

cat("\n=== DONE C-prod-HS4 ===\n")
cat("Uso in 07_triple_diff.R: merge su hs6, poi d <- d[in_HS4match == TRUE] prima di stimare.\n")
cat("NB: riportare SEMPRE questa stima accanto a C-overlap (10_subsample_overlap.R), mai da sola.\n")
