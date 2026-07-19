########################################################
###### 07 — Sub-campioni di controllo (prodHS4, overlap, deepshallow) ###
########################################################
## Author: Edoardo Vitella
## Sostituisce: 08_subsample_prodHS4.R + 10_subsample_overlap.R +
##              11_subsample_deepshallow.R. Run: ~3-5 min (tre letture
##              leggere del pannello raw, una per sotto-campione, ciascuna
##              in un sottoprocesso callr separato per tenere la memoria
##              sotto controllo su 49,2M righe).
##
## Cosa fa: costruisce tre sotto-campioni di controllo alternativi al
## confronto "tutti i verdi vs tutto il resto", ciascuno con un compromesso
## diverso tra forza dell'identificazione e perdita di osservazioni (§2.3
## del paper, tab:samples):
##
## A) C-prod-HS4: tiene solo i prodotti non-verdi che condividono la
##    famiglia merceologica (primi 4 digit HS6) di almeno un prodotto verde.
##    Limite: soffre di spillover within-firm (Eckel et al. 2023) - va
##    sempre riportato insieme a C-overlap.
## B) C-overlap: tiene solo gli HS6 esportati sia verso destinazioni
##    trattate sia verso destinazioni di controllo (common support). Non
##    soffre dello spillover di (A); e' il controllo di riferimento del paper.
## C) C-deepshallow: resta SOLO tra le destinazioni gia' trattate,
##    confrontando accordi con clausole ambientali "deep" (>= mediana)
##    contro "shallow" (< mediana). Elimina la selezione trattato-vs-mai-
##    trattato; da usare sempre insieme al controllo TotalDepth_nonEnv (04).
##
## Input:  Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root)
##         New/Data/Classifications/green_codes_hs1996.csv (da 01)
##         Output/CEM/matched_countries.csv (root, CEM v1 - riferimento del paper)
## Output: New/Data/Subsamples/flag_prodHS4.csv, flag_overlap.csv, flag_deepshallow.csv
##         New/Output/Subsamples/{prodHS4,overlap,deepshallow}_diagnostics.txt

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(callr)

## --- Parametri e percorsi --------------------------------------------------
DATA_FILE  <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
GREEN_FILE <- here("New/Data/Classifications/green_codes_hs1996.csv")
CEM_FILE   <- here("Output/CEM/matched_countries.csv")  # CEM v1, root, riferimento del paper
OUT_DATA   <- here("New/Data/Subsamples")
OUT_DIAG   <- here("New/Output/Subsamples")
dir.create(OUT_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIAG, recursive = TRUE, showWarnings = FALSE)
stopifnot("Lista green HS1996 non trovata - eseguire prima 01_green_goods_hs1996.R" = file.exists(GREEN_FILE))

## ===========================================================================
## Sezione A: C-prod-HS4
## ===========================================================================
build_prodHS4 <- function(data_file, green_file, out_data, out_diag) {
  library(fst)
  library(data.table)
  threads_fst(1)

  cat("[C-prod-HS4] Loading hs6 (1 colonna, tutte le righe)...\n")
  d <- as.data.table(read_fst(data_file, columns = c("hs6")))
  d[, hs6_str := sprintf("%06d", as.integer(hs6))]

  # env_good RICALCOLATO contro la lista green tradotta a HS1996 (01), non
  # la colonna env_good gia' presente nel .fst (quella confronta HS2012
  # nativo contro hs6 trattato come HS1996, senza concordanza di vintage)
  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  green_codes <- unique(green$hs6_final)
  d[, env_good := as.integer(hs6_str %in% green_codes)]
  d[, hs6_str := NULL]

  # hs4 = primi 4 digit del codice HS6 a 6 cifre (con zero padding)
  d[, hs4 := substr(sprintf("%06d", as.integer(hs6)), 1, 4)]

  # famiglie HS4 che contengono almeno un prodotto verde
  green_hs4 <- unique(d[env_good == 1, hs4])
  cat(sprintf("[C-prod-HS4] HS4 con almeno un prodotto verde: %d\n", length(green_hs4)))

  # flag a livello di riga: sopravvive se e' verde, oppure appartiene a una
  # famiglia HS4 che contiene un prodotto verde
  d[, in_HS4match := (env_good == 1) | (hs4 %in% green_hs4)]

  prod_tab  <- unique(d[, .(hs6, hs4, env_good)])
  n_obs_tab <- d[, .N, by = hs6]
  prod_tab  <- merge(prod_tab, n_obs_tab, by = "hs6", all.x = TRUE)
  prod_tab[, in_HS4match := (env_good == 1) | (hs4 %in% green_hs4)]

  n_nongreen_tot   <- prod_tab[env_good == 0, .N]
  n_nongreen_match <- prod_tab[env_good == 0 & in_HS4match == TRUE, .N]
  n_rows_tot   <- nrow(d)
  n_rows_match <- d[, sum(in_HS4match)]

  diag_txt <- c(
    "=== C-prod-HS4 - diagnostica ===",
    sprintf("HS4 'verdi' (contengono >=1 prodotto verde): %d", length(green_hs4)),
    sprintf("Prodotti non-verdi: %d totali -> %d entro HS4 verdi (%.1f%%)",
            n_nongreen_tot, n_nongreen_match, 100 * n_nongreen_match / n_nongreen_tot),
    sprintf("Righe (osservazioni): %s totali -> %s sopravvivono a C-prod-HS4 (%.1f%%)",
            format(n_rows_tot, big.mark = ","), format(n_rows_match, big.mark = ","),
            100 * n_rows_match / n_rows_tot)
  )
  writeLines(diag_txt, file.path(out_diag, "prodHS4_diagnostics.txt"))
  cat(paste(diag_txt, collapse = "\n"), "\n")

  fwrite(prod_tab[, .(hs6, hs4, env_good, in_HS4match, n_obs = N)],
         file.path(out_data, "flag_prodHS4.csv"))
  cat("[OK] flag_prodHS4.csv -", nrow(prod_tab), "codici HS6\n")
}

cat("=== A) C-prod-HS4 ===\n")
callr::r(build_prodHS4, args = list(data_file = DATA_FILE, green_file = GREEN_FILE,
                                     out_data = OUT_DATA, out_diag = OUT_DIAG), show = TRUE)

## ===========================================================================
## Sezione B: C-overlap
## ===========================================================================
build_overlap <- function(data_file, green_file, cem_file, out_data, out_diag) {
  library(fst)
  library(data.table)
  threads_fst(1)

  cat("[C-overlap] Loading hs6, country_code, WB_EP_Depth...\n")
  d <- as.data.table(read_fst(data_file, columns = c("hs6", "country_code", "WB_EP_Depth")))

  green <- fread(green_file, colClasses = list(character = "hs6_final"))
  green_codes <- unique(green$hs6_final)
  d[, hs6_str := sprintf("%06d", as.integer(hs6))]
  d[, env_good := as.integer(hs6_str %in% green_codes)]
  d[, hs6_str := NULL]

  # universo "trattato" / "controllo" a livello di destinazione
  treated_dest <- unique(d[WB_EP_Depth > 0, country_code])
  cat(sprintf("[C-overlap] Destinazioni trattate (WB_EP_Depth>0 in almeno un anno): %d\n",
              length(treated_dest)))

  # variante (b): solo i controlli del CEM-paese esistente (v1)
  cem_controls <- integer(0)
  if (file.exists(cem_file)) {
    cem_dt <- fread(cem_file)
    cem_controls <- cem_dt[treated == 0 & !is.na(country_code), unique(country_code)]
    cat(sprintf("[C-overlap] Destinazioni di controllo CEM: %d\n", length(cem_controls)))
  } else {
    cat("[C-overlap][WARN] CEM matched_countries.csv non trovato - solo variante 'loose'.\n")
  }

  # per ogni HS6: e' mai apparso verso un trattato? verso un controllo
  # (loose / CEM)? l'overlap richiede ENTRAMBI
  prod_dest <- unique(d[, .(hs6, country_code)])
  prod_dest[, is_treated_dest  := country_code %in% treated_dest]
  prod_dest[, is_control_loose := !is_treated_dest]
  prod_dest[, is_control_cem   := country_code %in% cem_controls]

  ov <- prod_dest[, .(
    ever_treated        = any(is_treated_dest),
    ever_control_loose  = any(is_control_loose),
    ever_control_cem    = any(is_control_cem)
  ), by = hs6]
  ov[, overlap_loose := ever_treated & ever_control_loose]
  ov[, overlap_cem    := ever_treated & ever_control_cem]

  env_lookup <- unique(d[, .(hs6, env_good)])
  n_obs_tab  <- d[, .N, by = hs6]
  ov <- merge(ov, env_lookup, by = "hs6", all.x = TRUE)
  ov <- merge(ov, n_obs_tab, by = "hs6", all.x = TRUE)

  n_hs6_tot    <- nrow(ov)
  n_rows_tot   <- nrow(d)
  n_rows_loose <- d[hs6 %in% ov[overlap_loose == TRUE, hs6], .N]
  n_rows_cem   <- d[hs6 %in% ov[overlap_cem == TRUE, hs6], .N]

  diag_txt <- c(
    "=== C-overlap - diagnostica ===",
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
  cat("[OK] flag_overlap.csv -", n_hs6_tot, "codici HS6\n")
}

cat("\n=== B) C-overlap ===\n")
callr::r(build_overlap, args = list(data_file = DATA_FILE, green_file = GREEN_FILE,
                                     cem_file = CEM_FILE, out_data = OUT_DATA,
                                     out_diag = OUT_DIAG), show = TRUE)

## ===========================================================================
## Sezione C: C-deepshallow
## ===========================================================================
build_deepshallow <- function(data_file, out_data, out_diag) {
  library(fst)
  library(data.table)
  threads_fst(1)

  cat("[C-deepshallow] Loading country_code, year, WB_EP_Depth...\n")
  d <- as.data.table(read_fst(data_file, columns = c("country_code", "year", "WB_EP_Depth")))

  # WB_EP_Depth e' (per costruzione del dataset) non decrescente nel tempo
  # per un dato accordo: il massimo storico = la profondita' "a regime"
  prof <- d[, .(max_EP_depth = max(WB_EP_Depth, na.rm = TRUE)), by = country_code]
  prof[, treated_dest := max_EP_depth > 0]

  treated <- prof[treated_dest == TRUE]
  cat(sprintf("[C-deepshallow] Destinazioni trattate: %d\n", nrow(treated)))

  # split deep vs shallow: mediana SOLO tra i trattati (confronto interno
  # al gruppo trattato, non rispetto ai mai-trattati)
  med_depth <- median(treated$max_EP_depth)
  prof[, group := fifelse(!treated_dest, "never_treated",
                    fifelse(max_EP_depth >= med_depth, "deep", "shallow"))]

  cat(sprintf("[C-deepshallow] Mediana profondita' ambientale (tra i trattati): %.2f\n", med_depth))
  cat("Distribuzione gruppi:\n"); print(prof[, .N, by = group])

  n_rows_tot     <- nrow(d)
  n_rows_treated <- d[country_code %in% treated$country_code, .N]
  diag_txt <- c(
    "=== C-deepshallow - diagnostica ===",
    sprintf("Paesi trattati: %d | mediana max_EP_depth: %.2f", nrow(treated), med_depth),
    sprintf("Gruppo 'deep' (>= mediana): %d paesi | 'shallow' (< mediana): %d paesi",
            sum(prof$group == "deep"), sum(prof$group == "shallow")),
    sprintf("Righe: %s totali -> %s nel sub-campione solo-trattati (%.1f%%)",
            format(n_rows_tot, big.mark = ","), format(n_rows_treated, big.mark = ","),
            100 * n_rows_treated / n_rows_tot),
    "",
    "ATTENZIONE - cluster nel gruppo 'shallow':",
    sprintf("Il gruppo 'shallow' ha solo %d paesi (split sbilanciato 'deep'=%d/'shallow'=%d perche'",
            sum(prof$group == "shallow"), sum(prof$group == "deep"), sum(prof$group == "shallow")),
    "diversi paesi condividono lo stesso max_EP_depth esattamente alla mediana, e per costruzione",
    "(>= mediana = deep) finiscono tutti nel gruppo 'deep'). E' un numero di cluster trattati ANCORA",
    "piu' piccolo dei 19-25 gia' segnalati come vincolo per l'inferenza nel paper (Wild Cluster",
    "Bootstrap): con 8 cluster nel confronto deep/shallow, anche il WCB puo' avere una copertura",
    "scadente. Da riportare esplicitamente come limite della stima 'deep:env_good', non da",
    "correggere nello script (e' una proprieta' dei dati, non un bug)."
  )
  writeLines(diag_txt, file.path(out_diag, "deepshallow_diagnostics.txt"))
  cat(paste(diag_txt, collapse = "\n"), "\n")

  fwrite(prof, file.path(out_data, "flag_deepshallow.csv"))
  cat("[OK] flag_deepshallow.csv -", nrow(prof), "paesi\n")
}

cat("\n=== C) C-deepshallow ===\n")
callr::r(build_deepshallow, args = list(data_file = DATA_FILE, out_data = OUT_DATA,
                                         out_diag = OUT_DIAG), show = TRUE)

cat("\n=== DONE: tre sotto-campioni costruiti ===\n")
