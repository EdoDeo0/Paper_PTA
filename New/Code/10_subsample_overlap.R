########################################################################
###### Fase R-control — Sub-sample 3: C-overlap (≈ Control 1 di      ###
###### Caselli, Huang, Tomasi & Zhu — il piu' "pulito")              ###
########################################################################

## Author: Edoardo Vitella
##
## INTUIZIONE
## ----------
## Alcuni codici HS6 vengono esportati SOLO verso partner con un PTA
## (trattati), altri SOLO verso destinazioni senza PTA (controlli): per
## questi non esiste un "controfattuale osservato" — confrontarli vuol
## dire estrapolare fuori dal supporto comune dei dati, cosa che nessun
## metodo puo' fare in modo credibile.
##
## Questo script tiene SOLO i codici HS6 (verdi e non-verdi) che vengono
## esportati ANCHE verso destinazioni trattate E ANCHE verso destinazioni
## di controllo (common support prodotto x destinazione). E' la versione
## "Control 1" del paper di riferimento — quella che il paper stesso
## privilegia, perche' a differenza di C-prod-HS4/C-prod-match (script 08
## e 09) NON soffre dello spillover within-firm: qui non stiamo
## restringendo ai prodotti "vicini" nella stessa famiglia merceologica,
## stiamo solo togliendo i prodotti che non hanno un confronto osservato.
## Il prezzo: taglia MENO righe delle altre strategie (e' la leva piu'
## debole sul fronte computazionale, la piu' forte sul fronte identif.).
##
## "Destinazione trattata" = paese con WB_EP_Depth > 0 in almeno un anno
## (~19-25 partner PTA). "Destinazione di controllo" e' definita in DUE
## varianti, riportate entrambe (la scelta si fa dopo, sui numeri):
##   (a) control_loose  = qualunque destinazione mai trattata
##   (b) control_cem    = solo i controlli sopravvissuti al CEM-paese
##                         esistente (Output/CEM/matched_countries.csv) ->
##                         rinforza ANCHE il margine-destinazione, non
##                         solo quello-prodotto.
##
## Lettura LEGGERA: 4 colonne dal .fst (hs6, country_code, env_good,
## WB_EP_Depth) — NON serve leggere ln_export: la presenza di una riga
## prodotto x paese x anno nel dataset implica gia' un flusso commerciale
## registrato (positivo o nullo a seconda della costruzione del panel);
## qui basta sapere "questo hs6 appare mai per questa destinazione".
##
## Output:
##   New/Data/Subsamples/flag_overlap.csv
##     (hs6, env_good, overlap_loose, overlap_cem, n_obs)
##   New/Output/Subsamples/overlap_diagnostics.txt

if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr")
library(callr); library(here)

SHARED <- list(
  data_file = here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst"),
  cem_file  = here("Output/CEM/matched_countries.csv"),
  out_data  = here("New/Data/Subsamples"),
  out_diag  = here("New/Output/Subsamples")
)

build_overlap <- function(data_file, cem_file, out_data, out_diag) {
  library(fst); library(data.table)
  threads_fst(1)
  dir.create(out_data, recursive = TRUE, showWarnings = FALSE)
  dir.create(out_diag, recursive = TRUE, showWarnings = FALSE)

  cat("Loading hs6, country_code, env_good, WB_EP_Depth...\n")
  d <- as.data.table(read_fst(data_file,
        columns = c("hs6", "country_code", "env_good", "WB_EP_Depth")))

  ## ── 1. Universo "trattato" / "controllo" a livello di destinazione ──
  treated_dest <- unique(d[WB_EP_Depth > 0, country_code])
  cat(sprintf("Destinazioni trattate (WB_EP_Depth>0 in almeno un anno): %d\n",
              length(treated_dest)))

  ## variante (b): solo i controlli del CEM-paese esistente
  cem_controls <- integer(0)
  if (file.exists(cem_file)) {
    cem_dt <- fread(cem_file)
    cem_controls <- cem_dt[treated == 0 & !is.na(country_code), unique(country_code)]
    cat(sprintf("Destinazioni di controllo CEM (matched_countries.csv): %d\n",
                length(cem_controls)))
  } else {
    cat("[WARN] CEM matched_countries.csv non trovato — solo variante 'loose' disponibile.\n")
  }

  ## ── 2. Per ogni HS6: e' mai apparso verso un trattato? verso un
  ##      controllo (loose / CEM)? L'overlap richiede ENTRAMBI ──────────
  prod_dest <- unique(d[, .(hs6, country_code)])
  prod_dest[, is_treated_dest := country_code %in% treated_dest]
  prod_dest[, is_control_loose := !is_treated_dest]
  prod_dest[, is_control_cem := country_code %in% cem_controls]

  ov <- prod_dest[, .(
    ever_treated      = any(is_treated_dest),
    ever_control_loose = any(is_control_loose),
    ever_control_cem   = any(is_control_cem)
  ), by = hs6]
  ov[, overlap_loose := ever_treated & ever_control_loose]
  ov[, overlap_cem    := ever_treated & ever_control_cem]

  env_lookup <- unique(d[, .(hs6, env_good)])
  n_obs_tab  <- d[, .N, by = hs6]
  ov <- merge(ov, env_lookup, by = "hs6", all.x = TRUE)
  ov <- merge(ov, n_obs_tab, by = "hs6", all.x = TRUE)

  ## ── 3. Conteggi ──────────────────────────────────────────────────────
  n_hs6_tot <- nrow(ov)
  n_rows_tot <- nrow(d)
  n_rows_loose <- d[hs6 %in% ov[overlap_loose == TRUE, hs6], .N]
  n_rows_cem   <- d[hs6 %in% ov[overlap_cem == TRUE, hs6], .N]

  diag_txt <- c(
    "=== C-overlap — diagnostica (checkpoint 7.4.5) ===",
    sprintf("HS6 totali: %d", n_hs6_tot),
    sprintf("HS6 in overlap_loose (verso trattati E verso qualunque non-trattato): %d (%.1f%%)",
            sum(ov$overlap_loose), 100 * mean(ov$overlap_loose)),
    sprintf("HS6 in overlap_cem (verso trattati E verso controlli CEM): %d (%.1f%%)",
            sum(ov$overlap_cem), 100 * mean(ov$overlap_cem)),
    sprintf("Righe: %s totali -> %s con overlap_loose (%.1f%%) -> %s con overlap_cem (%.1f%%)",
            format(n_rows_tot, big.mark = ","),
            format(n_rows_loose, big.mark = ","), 100 * n_rows_loose / n_rows_tot,
            format(n_rows_cem, big.mark = ","), 100 * n_rows_cem / n_rows_tot)
  )
  writeLines(diag_txt, file.path(out_diag, "overlap_diagnostics.txt"))
  cat(paste(diag_txt, collapse = "\n"), "\n")

  fwrite(ov[, .(hs6, env_good, overlap_loose, overlap_cem, n_obs = N)],
         file.path(out_data, "flag_overlap.csv"))
  cat("[OK] flag_overlap.csv —", n_hs6_tot, "codici HS6\n")
}

callr::r(build_overlap, args = SHARED, show = TRUE)

cat("\n=== DONE C-overlap ===\n")
cat("Uso in 07_triple_diff.R: merge su hs6, poi d <- d[overlap_cem == TRUE] (preferito) o\n")
cat("d[overlap_loose == TRUE] prima di stimare. E' il controllo di riferimento da riportare\n")
cat("SEMPRE accanto a C-prod-HS4/C-prod-match per verificare l'eventuale spillover Eckel.\n")
