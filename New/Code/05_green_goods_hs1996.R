########################################################
###### 01 — Lista green goods tradotta a HS1996      ####
########################################################
## Author: Edoardo Vitella
## Sostituisce: 03b_green_codes_to_hs1996.R + 03c_check_A2_continuity_fix.R
##              (in New/_legacy/code/). Run: ~2 min (legge il pannello raw
##              una volta, via callr, in sola lettura).
##
## Cosa fa: la lista dei prodotti "verdi" (Data/Env_Codes_HS.dta, 247 codici,
## nativa HS2012) va usata contro un pannello che il fornitore dichiara
## HS1996. Questo script la traduce UNA VOLTA a HS1996 (non blocco-per-anno:
## la concordanza per-blocco fu abbandonata, vedi _legacy/code/03_hs_concordance.R,
## concord() dava NA sui casi-prova 8542xx) e verifica che la traduzione non
## introduca discontinuita' di valore export intorno al confine 2006->2007.
## I green goods arrivano dal CLEG (OECD), Sauvage (2014).
##
## Nota storica (bug A2, fix integrato qui): la prima versione del continuity
## check (03b) filtrava il pannello sul codice ORIGINALE HS2012, non sul
## codice HS1996 tradotto (hs6_final) — per i 10/247 codici dove i due
## differiscono, il check verificava la continuita' del codice sbagliato.
## Qui il filtro e' su hs6_final fin dall'inizio: un solo continuity check,
## corretto, non due passaggi separati.
##
## Input:  Data/Env_Codes_HS.dta (root  , sola lettura)
##         Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst (root, sola lettura)
## Output: New/Data/Classifications/green_codes_hs1996.csv
##         New/Output/Diagnostics/05_green_goods_hs1996.md

## --- Setup ---------------------------------------------------------------
rm(list = ls())
if (!requireNamespace("concordance", quietly = TRUE)) install.packages("concordance", repos = "https://cloud.r-project.org")
if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr", repos = "https://cloud.r-project.org") ## legge il panel in un processo separato per evitare crash
library(haven)
library(data.table)
library(here)
library(concordance) ## https://github.com/insongkim/concordance

## --- Parametri e percorsi --------------------------------------------------
GREEN_DTA <- here("Data/Env_Codes_HS.dta")
RAW_FST <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
OUT_DATA <- here("New/Data/Classifications")
OUT_DIAG <- here("New/Output/Diagnostics")
dir.create(OUT_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIAG, recursive = TRUE, showWarnings = FALSE)

## --- Sezione 1: lista green originale (HS2012) ----------------------------
green <- as.data.table(read_dta(GREEN_DTA))
green[, hs6_str := sprintf("%06d", as.integer(hs6))]
cat(sprintf("Lista green originale: %d codici (vintage HS2012)\n", nrow(green)))

## --- Sezione 2: concordanza HS2012 -> HS1996 ------------------------------
# concord(all = TRUE) restituisce TUTTI i candidati per ogni codice, così
# possiamo distinguere un match univoco (1 candidato) da uno split ambiguo
# (piu' candidati, nessuno scelto a caso)
res <- concordance::concord(
  sourcevar = green$hs6_str, origin = "HS4", destination = "HS1",
  dest.digit = 6, all = TRUE
)

green[, `:=`(
  n_match = sapply(res, function(z) length(z$match[!is.na(z$match)])),
  hs1_uniq = sapply(res, function(z) {
    m <- z$match[!is.na(z$match)]
    if (length(m) == 1) m[1] else NA_character_
  })
)]
green[, hs1_candidates := sapply(res, function(z) paste(z$match[!is.na(z$match)], collapse = "|"))]

n_unmatched <- green[n_match == 0, .N]
n_unique <- green[n_match == 1, .N]
n_split <- green[n_match > 1, .N]
cat(sprintf(
  "Non concordati: %d | match univoco 1:1: %d | split ambiguo: %d\n",
  n_unmatched, n_unique, n_split
))

## --- Sezione 3: codice finale (hs6_final) ----------------------------------
# i match univoci diventano HS1996; i non concordati o split mantengono
# l'originale HS2012 come fallback, sempre flaggati (mai scartati in silenzio)
green[, hs6_final := fifelse(n_match == 1, hs1_uniq, hs6_str)]
green[, vintage_note := fifelse(
  n_match == 1, "HS1996 (concordanza univoca)",
  fifelse(
    n_match == 0, "non concordato - mantenuto HS2012 originale",
    "split ambiguo - mantenuto HS2012 originale"
  )
)]

## --- Sezione 4: verifica continuita' di export sul codice CORRETTO --------
# filtriamo il pannello su hs6_final (non sull'originale HS2012): un codice
# con match univoco ma diverso dall'originale (10/247 casi) nel pannello
# esiste solo sotto hs6_final, mai sotto il codice HS2012 di partenza
#
# PERCHE' PROPRIO IL 2006->2007: il 2007 e' l'anno in cui e' entrata in
# vigore la revisione doganale HS2007. Il fornitore del dataset dichiara che
# TUTTO il pannello 2000-2015 e' codificato uniformemente in HS1996, ma non
# lo sappiamo per certo - se in realta' le dogane avessero cambiato
# classificazione proprio al 2007 (comune nei dataset doganali), un codice
# HS1996 tradurrebbe erroneamente il commercio reale, che nel pannello
# finirebbe sotto un codice diverso. Sintomo tipico: export presente prima
# del 2007 che crolla quasi a zero dopo, non perche' il commercio sia
# sparito ma perche' lo si sta cercando sotto il codice sbagliato. Questo
# controllo verifica quindi, sui codici tradotti qui, se la vintage HS1996
# dichiarata dal fornitore regge davvero o e' solo un'assunzione.
codes_final <- unique(green[n_match == 1, hs6_final])

check_continuity <- function(fst_file, codes_final) {
  library(fst)
  library(data.table)
  threads_fst(1)
  d <- as.data.table(read_fst(fst_file, columns = c("year", "hs6", "export")))
  d[, hs6_str := sprintf("%06d", as.integer(hs6))]
  d <- d[hs6_str %in% codes_final]
  d[, .(exp = sum(export, na.rm = TRUE)), by = .(year, hs6_str)]
}

cat("\nLeggendo (sola lettura) il pannello per verificare continuita' export sui match univoci...\n")
yearly <- callr::r(check_continuity, args = list(fst_file = RAW_FST, codes_final = codes_final), show = TRUE)

yearly_wide <- dcast(yearly, hs6_str ~ year, value.var = "exp", fill = 0)
year_cols <- setdiff(names(yearly_wide), "hs6_str")
pre_cols <- intersect(as.character(2000:2006), year_cols)
post_cols <- intersect(as.character(2007:2015), year_cols)
yearly_wide[, exp_pre_avg := rowMeans(.SD, na.rm = TRUE), .SDcols = pre_cols]
yearly_wide[, exp_post_avg := rowMeans(.SD, na.rm = TRUE), .SDcols = post_cols]
# sospetto: export positivo pre-2007 che crolla a <5% post-2007 senza risalire
# mai (sintomo di un cambio di codice, non di un calo di mercato genuino)
yearly_wide[, suspect_break := exp_pre_avg > 0 & exp_post_avg < 0.05 * exp_pre_avg]

n_suspect <- yearly_wide[suspect_break == TRUE, .N]
cat(sprintf(
  "\nCodici a sospetto crollo di export 2006->2007 (post < 5%% di pre): %d / %d\n",
  n_suspect, nrow(yearly_wide)
))
if (n_suspect > 0) print(yearly_wide[suspect_break == TRUE, .(hs6_str, exp_pre_avg, exp_post_avg)])

## --- Sezione 5: salvataggio output ------------------------------------------
out <- green[, .(hs6_hs2012_orig = hs6_str, env_good, hs6_final, n_match, hs1_candidates, vintage_note)]
fwrite(out, file.path(OUT_DATA, "green_codes_hs1996.csv"))
cat(sprintf("\n[OK] Lista green tradotta a HS1996 salvata: %s\n", file.path(OUT_DATA, "green_codes_hs1996.csv")))

## --- Sezione 6: report diagnostico ------------------------------------------
n_risky <- green[hs6_final != hs6_str, .N]
report <- c(
  "# 01 - Lista green tradotta a HS1996 (vintage unica)", "",
  sprintf("Data: %s", Sys.Date()), "",
  "## Decisione",
  "Si e' scelto di fidarsi della vintage HS1996 dichiarata dal fornitore del dataset",
  "e di tradurre la lista green (nativa HS2012) UNA VOLTA a HS1996, applicandola",
  "uniformemente a tutti gli anni - non blocco per blocco.", "",
  "## Metodo: solo match univoci contano come 'tradotti'",
  "Si accettano SOLO i match 1:1 univoci come traduzione affidabile; i codici con",
  "split 1->N (piu' candidati) o senza match mantengono il codice HS2012 originale",
  "come fallback, sempre flaggati.", "",
  sprintf("- Codici totali nella lista green: %d", nrow(green)),
  sprintf("- Match univoco 1:1 HS2012->HS1996: %d (%.1f%%)", n_unique, 100 * n_unique / nrow(green)),
  sprintf("- Split 1->N (fallback HS2012 originale): %d (%.1f%%)", n_split, 100 * n_split / nrow(green)),
  sprintf("- Non concordato (fallback HS2012 originale): %d (%.1f%%)", n_unmatched, 100 * n_unmatched / nrow(green)),
  sprintf("- Codici dove hs6_final != codice originale (traduzione effettiva): %d / %d", n_risky, nrow(green)),
  "",
  "## Verifica di continuita' di valore (filtrata sul codice CORRETTO hs6_final)",
  sprintf(
    "Codici a sospetto crollo di export 2006->2007 (export medio post-2007 < 5%% di pre-2007): %d / %d",
    n_suspect, nrow(yearly_wide)
  ),
  if (n_suspect > 0) {
    c("", capture.output(print(yearly_wide[suspect_break == TRUE, .(hs6_str, exp_pre_avg, exp_post_avg)])))
  } else {
    "Nessun codice a match univoco mostra un crollo sospetto - la traduzione univoca e' coerente nel tempo."
  },
  "",
  "## Output",
  sprintf(
    "- %s : lista completa con hs6_final (HS1996 dove univoco, HS2012 originale come fallback altrove)",
    file.path("New/Data/Classifications", "green_codes_hs1996.csv")
  ),
  "",
  "## Nota per l'uso a valle",
  "Usare la colonna hs6_final come chiave di match contro il pannello (trattato come",
  "HS1996 uniforme). I codici con vintage_note diverso da 'HS1996 (concordanza univoca)'",
  "sono una fonte di rumore residuo gia' nota e quantificata qui."
)
writeLines(report, file.path(OUT_DIAG, "05_green_goods_hs1996.md"))
cat("[OK] Report:", file.path(OUT_DIAG, "05_green_goods_hs1996.md"), "\n")
