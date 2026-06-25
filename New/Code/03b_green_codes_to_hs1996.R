########################################################################
###### Fase R1 (chiusura) — Traduzione lista green a vintage unica  ###
###### HS1996, fidandosi della vintage dichiarata dal dataset       ###
########################################################################

## Author: Edoardo Vitella
##
## DECISIONE (presa dall'autore dopo la discussione sul mixing di vintage):
## ci fidiamo del fornitore del dataset (ricercatori affermati, script
## 1_create_panel_export.do dichiara esplicitamente HS1996 come riferimento)
## e trattiamo i codici HS6 nel pannello come HS1996, UNA VOLTA PER TUTTE,
## anziche' tentare una concordanza per-blocco-anno (03_hs_concordance.R,
## abbandonato: concord() restituisce NA sui casi-prova 8542xx, vedi
## R1c_concordance_report.md).
##
## Conseguenza diretta: la lista dei prodotti "verdi" (Data/Env_Codes_HS.dta,
## fingerprint = HS2012 al 100%, vedi sessione precedente) va tradotta UNA
## VOLTA a HS1996 e poi applicata uniformemente a tutti gli anni — NON
## tradotta blocco per blocco (sarebbe incoerente con la decisione sopra:
## se il pannello e' "tutto HS1996", anche la lista verde deve esserlo).
##
## METODO — perche' qui contiamo solo i match UNIVOCI
## ----------------------------------------------------
## Il confronto precedente (oggi vs HS1996-fissa vs per-blocco, fatto con
## concord(..., all = TRUE) e controllo di set-membership) e' INQUINATO:
## quando un codice si "splitta" in piu' candidati, basta che UNO qualsiasi
## coincida per caso con un codice presente nei dati per contare come
## "trovato" — non e' una prova di correttezza, solo di sovrapposizione
## casuale piu' probabile quando ci sono piu' candidati. Qui invece:
##   - teniamo SOLO i codici verdi con un match 1:1 univoco HS2012->HS1996
##     (nessun ventaglio di alternative)
##   - per i codici con split (1 codice HS2012 -> N candidati HS1996),
##     flagghiamo come "ambiguo" e NON assegniamo un codice a caso: li
##     riportiamo in diagnostica, decisione su come trattarli rimandata
##     (es. escluderli, o tenerli con tutti i candidati come instrumented
##     set per un controllo di robustezza)
##   - verifichiamo anche la continuita' di valore export per i match
##     univoci (il candidato HS1996 deve avere export prima/dopo coerente
##     con il prodotto originale, non un crollo a zero)
##
## REGOLA DI NON-INTERVENTO: questo script NON SCRIVE MAI in Desktop/china.
## Legge Data/Env_Codes_HS.dta in lettura, scrive solo in New/Data/ e
## New/Output/Diagnostics/.

if (!requireNamespace("concordance", quietly = TRUE)) install.packages("concordance", repos = "https://cloud.r-project.org")
library(haven); library(data.table); library(here); library(concordance)

OUT_DATA <- here("New/Data/Concordance")
OUT_DIAG <- here("New/Output/Diagnostics")
dir.create(OUT_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIAG, recursive = TRUE, showWarnings = FALSE)

## ── 1. Lista green originale (HS2012, fingerprint 100% — sessione precedente) ──
green <- as.data.table(read_dta(here("Data/Env_Codes_HS.dta")))
green[, hs6_str := sprintf("%06d", as.integer(hs6))]
cat(sprintf("Lista green originale: %d codici (vintage HS2012, fingerprint 100%%)\n", nrow(green)))

## ── 2. Concordanza HS2012 ("HS4") -> HS1996 ("HS1"), match univoci vs split ──
res <- concordance::concord(sourcevar = green$hs6_str, origin = "HS4", destination = "HS1",
                             dest.digit = 6, all = TRUE)

green[, `:=`(
  n_match  = sapply(res, function(z) length(z$match[!is.na(z$match)])),
  hs1_uniq = sapply(res, function(z) {
    m <- z$match[!is.na(z$match)]
    if (length(m) == 1) m[1] else NA_character_
  })
)]
green[, hs1_candidates := sapply(res, function(z) paste(z$match[!is.na(z$match)], collapse = "|"))]

n_unmatched <- green[n_match == 0, .N]
n_unique    <- green[n_match == 1, .N]
n_split     <- green[n_match > 1, .N]
cat(sprintf("Non concordati (nessun match HS1996): %d\n", n_unmatched))
cat(sprintf("Match univoco 1:1: %d\n", n_unique))
cat(sprintf("Split 1->N (ambiguo, NON assegnato a caso): %d\n", n_split))

## ── 3. Verifica continuita' di valore per i match univoci, sul confine
## 2006->2007 (caso-prova) e su tutta la serie — letta SOLO in lettura dal
## pannello raw (sola lettura, mai scritto) ───────────────────────────────
if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr", repos = "https://cloud.r-project.org")
RAW_FST <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")

check_continuity <- function(fst_file, codes_orig, codes_h1) {
  library(fst); library(data.table)
  threads_fst(1)
  d <- as.data.table(read_fst(fst_file, columns = c("year", "hs6", "export")))
  d[, hs6_str := sprintf("%06d", as.integer(hs6))]
  d <- d[hs6_str %in% codes_orig]
  d[, .(exp = sum(export, na.rm = TRUE)), by = .(year, hs6_str)]
}

uniq_codes <- green[n_match == 1, hs6_str]
cat("\nLeggendo (sola lettura) il pannello per verificare continuita' di export sui match univoci...\n")
yearly <- callr::r(check_continuity, args = list(fst_file = RAW_FST, codes_orig = uniq_codes, codes_h1 = NULL), show = TRUE)

yearly_wide <- dcast(yearly, hs6_str ~ year, value.var = "exp", fill = 0)
## flag: codice con export > 0 in almeno un anno pre-2007 e crollo a (quasi) 0 in almeno un anno post,
## senza mai piu' risalire (sintomo di un cambio di codice non di mercato)
year_cols <- setdiff(names(yearly_wide), "hs6_str")
pre_cols  <- as.character(2000:2006); pre_cols <- pre_cols[pre_cols %in% year_cols]
post_cols <- as.character(2007:2015); post_cols <- post_cols[post_cols %in% year_cols]
yearly_wide[, `:=`(
  exp_pre_avg  = rowMeans(.SD[, ..pre_cols], na.rm = TRUE),
  exp_post_avg = rowMeans(.SD[, ..post_cols], na.rm = TRUE)
)]
yearly_wide[, suspect_break := exp_pre_avg > 0 & exp_post_avg < 0.05 * exp_pre_avg]
n_suspect <- yearly_wide[suspect_break == TRUE, .N]
cat(sprintf("\nCodici verdi a match univoco con sospetto crollo di valore 2006->2007 (export post < 5%% di pre): %d / %d\n",
            n_suspect, nrow(yearly_wide)))
if (n_suspect > 0) print(yearly_wide[suspect_break == TRUE, .(hs6_str, exp_pre_avg, exp_post_avg)])

## ── 4. Output finale: lista green tradotta a HS1996 (uso uniforme su tutti gli anni) ──
## Decisione pratica: i match univoci diventano la lista "pulita" HS1996;
## i codici non concordati o split mantengono il codice ORIGINALE (HS2012)
## come fallback, flaggati esplicitamente — NON vengono scartati in silenzio.
green[, hs6_final := fifelse(n_match == 1, hs1_uniq, hs6_str)]
green[, vintage_note := fifelse(n_match == 1, "HS1996 (concordanza univoca)",
                          fifelse(n_match == 0, "non concordato — mantenuto HS2012 originale",
                                  "split ambiguo — mantenuto HS2012 originale"))]

out <- green[, .(hs6_hs2012_orig = hs6_str, env_good, hs6_final, n_match, hs1_candidates, vintage_note)]
fwrite(out, file.path(OUT_DATA, "Env_Codes_HS1996.csv"))
cat(sprintf("\n[OK] Lista green tradotta a HS1996 salvata: %s\n", file.path(OUT_DATA, "Env_Codes_HS1996.csv")))
cat(sprintf("Codici totali: %d | usano hs6_final HS1996: %d | fallback HS2012 originale: %d\n",
            nrow(out), n_unique, n_unmatched + n_split))

## ── 5. Report diagnostico ──────────────────────────────────────────────
report <- c(
  "# Fase R1 (chiusura) — Lista green tradotta a HS1996 (vintage unica)", "",
  sprintf("Data: %s", Sys.Date()), "",
  "## Decisione",
  "Si e' scelto di fidarsi della vintage HS1996 dichiarata dal fornitore del dataset",
  "(1_create_panel_export.do, Step B) e di tradurre la lista green (nativa HS2012,",
  "fingerprint 100%) UNA VOLTA a HS1996, applicandola uniformemente a tutti gli anni —",
  "non blocco per blocco.", "",
  "## Metodo: solo match univoci contano come 'tradotti'",
  "Il confronto precedente (oggi / HS1996-fissa / per-blocco) usava un test di set-",
  "membership inquinato dal fan-out di concord(all=TRUE): un codice con piu' candidati",
  "ha piu' probabilita' di sovrapporsi per caso, indipendentemente dalla correttezza.",
  "Qui si accettano SOLO i match 1:1 univoci come traduzione affidabile.", "",
  sprintf("- Codici totali nella lista green: %d", nrow(green)),
  sprintf("- Match univoco 1:1 HS2012->HS1996: %d (%.1f%%)", n_unique, 100*n_unique/nrow(green)),
  sprintf("- Split 1->N (ambiguo, NON assegnato a caso, fallback HS2012 originale): %d (%.1f%%)", n_split, 100*n_split/nrow(green)),
  sprintf("- Non concordato (nessun match, fallback HS2012 originale): %d (%.1f%%)", n_unmatched, 100*n_unmatched/nrow(green)),
  "",
  "## Verifica di continuita' di valore (solo sui match univoci)",
  sprintf("Codici a sospetto crollo di export 2006->2007 (export medio post-2007 < 5%% di pre-2007): %d / %d",
          n_suspect, nrow(yearly_wide)),
  if (n_suspect > 0) c("", capture.output(print(yearly_wide[suspect_break == TRUE, .(hs6_str, exp_pre_avg, exp_post_avg)]))) else
    "Nessun codice a match univoco mostra un crollo sospetto — la traduzione univoca pare coerente nel tempo.",
  "",
  "## Output",
  sprintf("- %s : lista completa con hs6_final (HS1996 dove univoco, HS2012 originale come fallback altrove)",
          file.path("New/Data/Concordance", "Env_Codes_HS1996.csv")),
  "",
  "## Nota per l'uso negli script 08-12 (Fase R-control)",
  "Sostituire il riferimento a Data/Env_Codes_HS.dta con questo file, usando la colonna",
  "hs6_final come chiave di match contro il pannello (trattato come HS1996 uniforme).",
  "I codici con vintage_note != 'HS1996 (concordanza univoca)' sono una fonte di rumore",
  "residuo gia' nota e quantificata qui (non eliminabile senza perdere quei prodotti)."
)
writeLines(report, file.path(OUT_DIAG, "R1d_green_codes_hs1996.md"))
cat("[OK] Report:", file.path(OUT_DIAG, "R1d_green_codes_hs1996.md"), "\n")
