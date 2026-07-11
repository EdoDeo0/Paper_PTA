########################################################
###### Fase R2 — Classificazione dirty goods (HS6)  ####
########################################################

## Author: Edoardo Vitella
##
## Costruisce dirty_p a livello HS6 con due misure:
##   1. BINARIA (principale, comparabile con la letteratura): settori dirty
##      alla Mani-Wheeler (1998) / Low-Yeats (1992) — ISIC Rev.2:
##        341 pulp & paper | 351 industrial chemicals | 353 petroleum refining
##        371 iron & steel | 372 non-ferrous metals  | [esteso: 369 cemento/non-met. min.]
##      Concordanza: tabella ufficiale WITS/UNSD HS1996->ISIC Rev.3 (scaricata e
##      cachata in New/Data/Dirty/), con mapping documentato ISIC2->ISIC3 dei 6
##      settori qui sotto. (Il pacchetto `concordance` NON ha tabelle ISIC: la
##      strada ISIC2/ISIC3->HS via pacchetto e' impraticabile — verificato 2026-07-06.)
##   2. CONTINUA (robustezza): intensita' CO2 per industria da Shapiro (QJE 2021).
##      Replication: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CTUS2E
##      -> scaricare a mano il file delle intensita' e salvarlo in
##         New/Data/Dirty/shapiro2021_intensity.csv (colonne: industry_code, co2_intensity)
##      Lo script la integra se presente, altrimenti procede con la sola binaria.
##
## Output: New/Data/Dirty/dirty_goods_hs6.csv  (hs6, dirty, dirty_ext, co2_intensity?)
## Diagnostica: quota HS6 dirty, overlap con env_good (atteso ~0).
## Leggero (nessun accesso al .fst principale): eseguibile in qualsiasi momento.

library(here); library(data.table)

out_dir <- here("New/Data/Dirty")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## ── Settori dirty: ISIC Rev.2 (Mani-Wheeler) -> ISIC Rev.3 ────────────
## Mapping manuale, corrispondenze standard UNSD (una riga per settore ISIC2):
##   341 pulp & paper            -> 2101, 2102, 2109  (divisione 21)
##   351 industrial chemicals    -> 2411, 2412, 2413  (chimica di base)
##   353 petroleum refining      -> 2320
##   371 iron & steel            -> 2710, 2731        (2731 = fonderie ferrose, in 371 sotto ISIC2)
##   372 non-ferrous metals      -> 2720, 2732        (2732 = fonderie non ferrose)
##   369 other non-met. minerals -> 2693, 2694, 2695, 2696, 2699  (cemento ecc.; esteso)
dirty_isic3      <- c("2101", "2102", "2109", "2411", "2412", "2413",
                      "2320", "2710", "2731", "2720", "2732")          # core Mani-Wheeler
dirty_isic3_ext  <- c(dirty_isic3, "2693", "2694", "2695", "2696", "2699")

## ── ISIC3 -> HS6 via tabella ufficiale WITS/UNSD (HS1996 <-> ISIC Rev.3) ──
## Vintage HS1996: coerente con l'esito dell'audit R1 (pannello trattato come
## HS1996 uniforme; lista green gia' tradotta a HS1996 in 03b).
## La tabella viene scaricata una volta e cachata; fonte: WITS World Bank.
wits_zip <- file.path(out_dir, "Concordance_H1_to_I3.zip")
wits_dir <- file.path(out_dir, "wits_h1_i3")
if (!dir.exists(wits_dir)) {
  if (!file.exists(wits_zip)) {
    download.file("https://wits.worldbank.org/data/public/concordance/Concordance_H1_to_I3.zip",
                  wits_zip, mode = "wb")
  }
  unzip(wits_zip, exdir = wits_dir)
}
wits_csv <- list.files(wits_dir, pattern = "\\.CSV$", full.names = TRUE, ignore.case = TRUE)[1]
conc <- fread(wits_csv, colClasses = "character")
setnames(conc, c("hs6", "hs6_desc", "isic3", "isic3_desc"))
cat(sprintf("Tabella WITS HS1996->ISIC3: %d righe, %d HS6 distinti\n",
            nrow(conc), uniqueN(conc$hs6)))

cat("Selezione codici HS6 dirty...\n")
hs6_dirty     <- sort(unique(conc[isic3 %in% dirty_isic3,     hs6]))
hs6_dirty_ext <- sort(unique(conc[isic3 %in% dirty_isic3_ext, hs6]))
cat(sprintf("  core: %d codici HS6 | esteso: %d codici HS6\n",
            length(hs6_dirty), length(hs6_dirty_ext)))

dt <- data.table(hs6 = hs6_dirty_ext)
dt[, dirty     := as.integer(hs6 %in% hs6_dirty)]
dt[, dirty_ext := 1L]

## ── Shapiro (2021) intensita' continua, se disponibile ────────────────
shp_file <- file.path(out_dir, "shapiro2021_intensity.csv")
if (file.exists(shp_file)) {
  cat("Trovato file Shapiro: integrazione intensita' continua...\n")
  ## NB: adattare i nomi colonna al file effettivo del replication package
  shp <- fread(shp_file)
  cat("  Colonne:", paste(names(shp), collapse = ", "), "\n")
  cat("  -> completare il merge industry_code -> HS6 dopo ispezione del file.\n")
} else {
  cat("File Shapiro non presente (ok): solo classificazione binaria.\n")
}

## ── Diagnostica overlap con env_good ──────────────────────────────────
## Confronto con la lista green TRADOTTA A HS1996 (03b) — stessa vintage
## della mappa dirty qui sopra. (Prima usava Data/Env_Codes_HS.dta, nativo
## HS2012: vintage incoerente, fix 2026-07-03.)
env_file <- here("New/Data/Concordance/Env_Codes_HS1996.csv")
if (file.exists(env_file)) {
  env <- fread(env_file, colClasses = list(character = "hs6_final"))
  env_codes <- unique(env$hs6_final)
  overlap <- intersect(dt$hs6, env_codes)
  cat(sprintf("Overlap dirty/env_good: %d codici (atteso ~0)\n", length(overlap)))
  if (length(overlap) > 0) {
    fwrite(data.table(hs6 = overlap), file.path(out_dir, "overlap_dirty_green_CHECK.csv"))
    ## DECISIONE (2026-07-06): la lista green OCSE ha precedenza — e' curata a mano
    ## prodotto per prodotto (uso ambientale: isolanti, binari, tubi per acqua),
    ## mentre la classificazione dirty e' settoriale (emissioni di produzione).
    ## I codici in overlap escono dal dirty: le due categorie devono essere
    ## mutuamente esclusive nella triple-diff (07).
    dt <- dt[!hs6 %in% env_codes]
    cat(sprintf("  -> %d codici rimossi dal dirty (precedenza alla lista green); restano %d\n",
                length(overlap), nrow(dt)))
  }
} else {
  cat("[WARN] Env_Codes_HS1996.csv non trovato (eseguire prima 03b) — overlap check saltato.\n")
}

fwrite(dt, file.path(out_dir, "dirty_goods_hs6.csv"))
cat("[OK] dirty_goods_hs6.csv —", nrow(dt), "codici HS6\n")
cat("Merge previsto (Fase R3): su hs6 nel dataset di lavoro; dirty_p = dirty (core).\n")
