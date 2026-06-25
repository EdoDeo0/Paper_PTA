########################################################################
###### Fase R1 (chiusura) — Concordanza HS6 a vintage unica (HS1996) ###
########################################################################

## Author: Edoardo Vitella
##
## INTUIZIONE
## ----------
## 02_data_hygiene_audit.R + 02b_hs_vintage_check.R hanno DIMOSTRATO (non
## solo sospettato) che il pannello doganale grezzo
## (Desktop/china/final_dataset/export_fpdt_2000_2015.dta) mescola vintage
## HS diverse: i codici sono quelli "as-reported" anno per anno, e NON la
## versione armonizzata a HS1996 che lo script originale dei dati grezzi
## (chinese_customdata/1_create_panel_export.do, Step B) prevedeva di
## produrre. Esempio-prova: il codice 854213 (circuiti integrati) vale
## 17,26 mld $ nel 2006 e 0 nel 2007; nello stesso anno il codice (gia'
## esistente) 854230 salta da 1,11 a 22,46 mld $ — una differenza di
## ~21,35 mld $, quasi identica al valore "sparito" dai codici 854212/
## 854213/854240/854250 (~19,2 mld $ insieme). E' la revisione ufficiale
## HS2007 che ha fuso piu' sottocategorie di circuiti in una sola — non un
## fenomeno di mercato.
##
## REGOLA DI DECISIONE (gia' scritta nel progetto, New/working_paper_build.py
## §8.1, mai eseguita prima d'ora): se la discontinuita' supera il 3% del
## valore export a un confine di revisione, serve la CONCORDANZA COMPLETA
## del pannello. Il confine 2006->2007 e' al 6,03% — soglia superata.
##
## REGOLA DI NON-INTERVENTO (richiesta esplicita dell'autore): questo
## script NON SCRIVE MAI nulla dentro Desktop/china (i raw file). Legge
## SOLO in lettura da li'; tutti gli output vanno in New/Data/ e
## New/Output/Diagnostics/, mai altrove.
##
## METODO
## ------
## Replica la Step B del .do originale (stesso riferimento: HS1996 = "h1",
## la prima vintage osservata nel panel, 2000 e' il primo anno), ma con le
## tabelle di concordanza UFFICIALI del pacchetto `concordance` (stessa
## fonte UNSTAT richiamata nel commento del .do) invece delle tabelle
## locali introvabili (corr/h2_to_h1.dta ecc., assenti in Desktop/china).
##
## Blocchi anno -> vintage di partenza (come nel .do, Step B):
##   2000-2001 -> HS1996 (h1)            : nessuna conversione (e' il riferimento)
##   2002-2006 -> HS2002 (h2) -> HS1996  : concord(origin="HS2", destination="HS1")
##   2007-2011 -> HS2007 (h3) -> HS1996  : concord(origin="HS3", destination="HS1")
##   2012-2015 -> HS2012 (h4) -> HS1996  : concord(origin="HS4", destination="HS1")
##
## La concordanza si calcola sui CODICI DISTINTI per blocco (poche migliaia),
## non riga per riga sulle 49,2M osservazioni — poi si fa un merge, esattamente
## come il m:1 merge del .do originale. Per i casi non 1:1:
##   - 1 vecchio codice -> 1 nuovo codice (la maggioranza): riassegnazione diretta.
##   - 1 vecchio codice -> N nuovi codici (split): il valore export viene
##     diviso in parti EGUALI tra gli N codici nuovi (si conserva il totale,
##     non si inventa un criterio di allocazione che non abbiamo i dati per
##     giustificare — diverso dal .do originale che con "keep if _merge==3"
##     su una tabella m:1 avrebbe semplicemente scartato questi casi: qui
##     scegliamo di non perdere osservazioni, ma il costo e' un po' di
##     rumore sulla ripartizione del valore tra i nuovi codici, riportato
##     in diagnostica).
##   - Nessuna corrispondenza trovata: il codice resta quello originale,
##     flaggato `hs6_concorded = FALSE` (mai scartato silenziosamente) —
##     decisione su escluderlo o no rimandata alla fase di stima, con
##     sensitivity check.
##
## Output:
##   New/Data/Concordance/export_fpdt_2000_2015_HS1996.fst
##     (companyID, country_code, year, hs6_h1, export, exp_qua, hs6_concorded)
##   New/Output/Diagnostics/R1c_concordance_report.md

if (!requireNamespace("concordance", quietly = TRUE)) install.packages("concordance", repos = "https://cloud.r-project.org")
if (!requireNamespace("callr", quietly = TRUE)) install.packages("callr", repos = "https://cloud.r-project.org")
library(here)

RAW_FILE <- "C:/Users/edodr/Desktop/china/final_dataset/export_fpdt_2000_2015.dta"
stopifnot("Raw file non trovato (solo lettura, percorso invariato)" = file.exists(RAW_FILE))

OUT_DATA <- here("New/Data/Concordance")
OUT_DIAG <- here("New/Output/Diagnostics")
dir.create(OUT_DATA, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIAG, recursive = TRUE, showWarnings = FALSE)

## ─────────────────────────────────────────────────────────────────────
## SEZIONE A — lettura del raw file (sottoprocesso, e' un .dta da 49,2M
## righe: stesso pattern callr usato altrove nel progetto per il .fst
## grande). SOLO LETTURA: nessuna scrittura su RAW_FILE.
## ─────────────────────────────────────────────────────────────────────
load_raw <- function(raw_file) {
  library(haven); library(data.table)
  cat("Loading export_fpdt_2000_2015.dta (sola lettura)...\n")
  d <- as.data.table(read_dta(raw_file,
        col_select = c("companyID", "country_code", "year", "hs6", "export", "exp_qua")))
  cat(sprintf("Rows: %s\n", format(nrow(d), big.mark = ",")))
  d
}
cat("\n=== SEZIONE A: lettura raw (sola lettura) ===\n")
d <- callr::r(load_raw, args = list(raw_file = RAW_FILE), show = TRUE)

library(data.table); library(concordance)
d[, hs6_str := sprintf("%06d", as.integer(hs6))]

## ─────────────────────────────────────────────────────────────────────
## SEZIONE B — mappe di concordanza per blocco-anno, sui codici DISTINTI
## (non sulle righe) — stesso principio del merge m:1 del .do originale.
## ─────────────────────────────────────────────────────────────────────
build_block_map <- function(codes, origin) {
  ## codes: vettore di hs6 (string, 6 cifre) distinti per il blocco-anno
  if (origin == "HS1") {
    ## 2000-2001: e' il riferimento stesso, nessuna conversione
    return(data.table(hs6_str = codes, hs6_h1 = codes, n_match = 1L))
  }
  res <- tryCatch(
    concordance::concord(sourcevar = codes, origin = origin, destination = "HS1",
                          dest.digit = 6, all = TRUE),
    error = function(e) { cat("[WARN] concord() falli per", origin, ":", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(res)) return(data.table(hs6_str = codes, hs6_h1 = NA_character_, n_match = 0L))
  rbindlist(lapply(seq_along(codes), function(i) {
    m <- res[[i]]$match
    m <- m[!is.na(m)]
    if (length(m) == 0) return(data.table(hs6_str = codes[i], hs6_h1 = NA_character_, n_match = 0L))
    data.table(hs6_str = codes[i], hs6_h1 = m, n_match = length(m))
  }))
}

blocks <- list(
  list(years = 2000:2001, origin = "HS1"),
  list(years = 2002:2006, origin = "HS2"),
  list(years = 2007:2011, origin = "HS3"),
  list(years = 2012:2015, origin = "HS4")
)

cat("\n=== SEZIONE B: mappe di concordanza per blocco-anno ===\n")
out_list <- vector("list", length(blocks))
diag_lines <- c("# Fase R1 — Concordanza HS6 a vintage unica (HS1996)", "",
                 sprintf("Data: %s", Sys.Date()), "",
                 "## Blocchi anno -> vintage di partenza", "")

for (i in seq_along(blocks)) {
  bl <- blocks[[i]]
  cat(sprintf("\nBlocco %s (origin=%s)...\n", paste(range(bl$years), collapse = "-"), bl$origin))
  sub <- d[year %in% bl$years]
  codes <- unique(sub$hs6_str)
  cat(sprintf("  Codici distinti nel blocco: %d\n", length(codes)))

  map <- build_block_map(codes, bl$origin)

  ## split del valore export in parti eguali tra i match multipli (split reali)
  map[, n_match := pmax(n_match, 1L)]
  sub2 <- merge(sub, map, by = "hs6_str", all.x = TRUE, allow.cartesian = TRUE)
  sub2[is.na(hs6_h1), `:=`(hs6_h1 = hs6_str, hs6_concorded = FALSE, n_match = 1L)]
  sub2[!is.na(hs6_h1) & hs6_h1 != hs6_str | (hs6_h1 == hs6_str & n_match == 1L), hs6_concorded := TRUE]
  sub2[is.na(hs6_concorded), hs6_concorded := TRUE]
  sub2[, `:=`(export = export / n_match, exp_qua = exp_qua / n_match)]

  n_codes <- length(codes)
  n_unmatched <- sum(map$n_match == 0 | is.na(map$hs6_h1))
  n_split <- sum(map[, .N, by = hs6_str][N > 1, .N])
  share_exp_unmatched <- sub2[hs6_concorded == FALSE, sum(export, na.rm = TRUE)] / sub2[, sum(export, na.rm = TRUE)]
  share_exp_split <- sub2[n_match > 1, sum(export, na.rm = TRUE)] / sub2[, sum(export, na.rm = TRUE)]

  cat(sprintf("  Non concordati (nessun match, codice originale mantenuto): %d codici (%.2f%% del valore export blocco)\n",
              n_unmatched, 100 * share_exp_unmatched))
  cat(sprintf("  Split 1->N (valore diviso in parti eguali): %d codici origine (%.2f%% del valore export blocco)\n",
              n_split, 100 * share_exp_split))

  diag_lines <- c(diag_lines, sprintf(
    "- **%s** (origin %s): %d codici distinti; %d non concordati (%.2f%% valore); %d con split 1->N (%.2f%% valore)",
    paste(range(bl$years), collapse = "-"), bl$origin, n_codes, n_unmatched, 100 * share_exp_unmatched,
    n_split, 100 * share_exp_split))

  out_list[[i]] <- sub2[, .(companyID, country_code, year, hs6_h1, export, exp_qua, hs6_concorded)]
}

panel_h1 <- rbindlist(out_list)
cat(sprintf("\nRighe totali panel armonizzato: %s (vs %s raw — la differenza viene dagli split 1->N)\n",
            format(nrow(panel_h1), big.mark = ","), format(nrow(d), big.mark = ",")))

## ─────────────────────────────────────────────────────────────────────
## SEZIONE C — verifica diretta sul caso-prova (8542xx, confine 2006-2007)
## Se la concordanza ha funzionato, il salto 854213/854230 deve SPARIRE:
## i due codici devono confluire nello stesso hs6_h1 e quindi essere
## continui nel tempo (nessun crollo a zero da un anno all'altro).
## ─────────────────────────────────────────────────────────────────────
cat("\n=== SEZIONE C: verifica caso-prova 8542xx ===\n")
chk <- panel_h1[year %in% c(2006, 2007) & substr(hs6_h1, 1, 4) == "8542",
                 .(exp = sum(export, na.rm = TRUE)), by = .(year, hs6_h1)]
chk_wide <- dcast(chk, hs6_h1 ~ year, value.var = "exp", fill = 0)
setnames(chk_wide, c("hs6_h1", "exp2006", "exp2007"))
print(chk_wide[order(-exp2006)])
diag_lines <- c(diag_lines, "", "## Verifica caso-prova (8542xx, 2006-2007)", "",
                "Prima della concordanza: 854213 (17,26 mld $ nel 2006) crollava a 0 nel 2007,",
                "mentre 854230 saltava da 1,11 a 22,46 mld $. Dopo la concordanza a HS1996:", "",
                capture.output(print(chk_wide[order(-exp2006)])), "")

## ─────────────────────────────────────────────────────────────────────
## SEZIONE D — ripeti il check di stabilita' generale (come in
## 02_data_hygiene_audit.R sezione A) sul pannello armonizzato, per
## confermare che l'anomalia 2006->2007 sia sparita su TUTTO il panel,
## non solo sul caso 8542xx.
## ─────────────────────────────────────────────────────────────────────
cat("\n=== SEZIONE D: stabilita' HS6 sul pannello armonizzato ===\n")
hs_year_h1 <- panel_h1[, .(exp = sum(export, na.rm = TRUE)), by = .(year, hs6_h1)]
years <- sort(unique(hs_year_h1$year))
hs_stab_h1 <- rbindlist(lapply(seq_len(length(years) - 1), function(i) {
  y0 <- years[i]; y1 <- years[i + 1]
  c0 <- hs_year_h1[year == y0]; c1 <- hs_year_h1[year == y1]
  new_codes  <- setdiff(c1$hs6_h1, c0$hs6_h1)
  dead_codes <- setdiff(c0$hs6_h1, c1$hs6_h1)
  data.table(year_from = y0, year_to = y1,
             share_exp_new  = c1[hs6_h1 %in% new_codes,  sum(exp)] / c1[, sum(exp)],
             share_exp_dead = c0[hs6_h1 %in% dead_codes, sum(exp)] / c0[, sum(exp)])
}))
print(hs_stab_h1)
diag_lines <- c(diag_lines, "## Stabilita' HS6 sul pannello armonizzato (tutti gli anni)", "",
                capture.output(print(hs_stab_h1)), "",
                "Confrontare con New/Output/Diagnostics/R1_audit_report.md (panel raw, non armonizzato):",
                "se il picco 2006->2007 (era 6,03% su share_exp_dead) e' rientrato nel rumore di fondo",
                "degli altri anni, la concordanza ha risolto il problema diagnosticato.")

## ── Salvataggio (SOLO in New/, mai su Desktop/china) ──────────────────
if (!requireNamespace("fst", quietly = TRUE)) install.packages("fst", repos = "https://cloud.r-project.org")
fst::write_fst(panel_h1, file.path(OUT_DATA, "export_fpdt_2000_2015_HS1996.fst"))
writeLines(diag_lines, file.path(OUT_DIAG, "R1c_concordance_report.md"))
cat("\n[OK] Pannello armonizzato salvato in New/Data/Concordance/export_fpdt_2000_2015_HS1996.fst\n")
cat("[OK] Report in New/Output/Diagnostics/R1c_concordance_report.md\n")
cat("Raw file NON toccato:", RAW_FILE, "\n")
