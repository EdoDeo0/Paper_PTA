########################################################
###### Fase R1 (seguito) — Verifica vintage HS6 al    ##
###### confine 2006->2007 con tabella di concordanza   ##
###### ufficiale (pacchetto `concordance`)             ##
########################################################

## Author: Edoardo Vitella
##
## INTUIZIONE
## ----------
## 02_data_hygiene_audit.R ha trovato un'anomalia enorme al confine
## 2006->2007: 367 codici HS6 "muoiono" (contro una media di ~70 negli
## altri anni), portandosi dietro il 6% del valore export. E' compatibile
## con un cambio di nomenclatura (revisione HS2007) non tradotto a una
## vintage unica, ma e' solo un SOSPETTO statistico finora.
##
## Questo script verifica la diagnosi in modo diretto: prende i codici
## HS6 "morti" nel 2007 (presenti nel 2006, assenti nel 2007) e i codici
## "nati" nel 2007 (assenti nel 2006, presenti nel 2007), poi usa la
## tabella di concordanza UFFICIALE HS2002->HS2007 (pacchetto
## `concordance`, stessa fonte gia' usata in 05_dirty_goods.R per
## ISIC2->HS) per tradurre i codici morti e controllare se ricadono sui
## codici nati. Se la sovrapposizione e' alta, la diagnosi e' confermata:
## non e' un cambiamento di mercato reale, e' un cambio di etichetta.
##
## Lettura LEGGERA: 3 colonne dal .fst (year, hs6, export), solo anni
## 2006-2007.
##
## Output: New/Output/Diagnostics/R1b_hs_vintage_check.md

if (!requireNamespace("concordance", quietly = TRUE)) install.packages("concordance")
library(fst); library(data.table); library(here); library(concordance)
threads_fst(1)

data_file <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
out_dir   <- here("New/Output/Diagnostics")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat("Loading year, hs6, export (solo 2006-2007)...\n")
d <- as.data.table(read_fst(data_file, columns = c("year", "hs6", "export")))
d <- d[year %in% c(2006L, 2007L)]

## ── 1. Codici morti / nati al confine 2006->2007 (ricalcolo, come in
## 02_data_hygiene_audit.R sezione A, ma qui salviamo le LISTE non solo i
## conteggi) ─────────────────────────────────────────────────────────────
hs_year <- d[, .(exp = sum(export, na.rm = TRUE)), by = .(year, hs6)]
c2006 <- hs_year[year == 2006]
c2007 <- hs_year[year == 2007]

dead_codes <- setdiff(c2006$hs6, c2007$hs6)   # presenti nel 2006, assenti nel 2007
new_codes  <- setdiff(c2007$hs6, c2006$hs6)   # assenti nel 2006, presenti nel 2007

cat(sprintf("Codici morti (2006->2007): %d | Codici nati: %d\n",
            length(dead_codes), length(new_codes)))

## hs6 nel dataset e' a 6 cifre con zero padding (coerente con gli script
## 08-11 di questa Fase R-control)
dead_codes_str <- sprintf("%06d", as.integer(dead_codes))
new_codes_str  <- sprintf("%06d", as.integer(new_codes))

## ── 2. Concordanza UFFICIALE HS2002 ("HS2") -> HS2007 ("HS3") ─────────
## Ipotesi di lavoro: i dati 2000-2006 sono in HS2002, quelli 2007+ in
## HS2007 (la transizione coincide esattamente con l'anno dell'anomalia).
## Se i codici morti, tradotti da HS2002 a HS2007, ricadono sui codici
## nati, la diagnosi e' confermata.
cat("\nTraducendo i codici morti da HS2002 (HS2) a HS2007 (HS3) con la tabella ufficiale...\n")
translated <- tryCatch(
  concordance::concord(sourcevar = dead_codes_str, origin = "HS2", destination = "HS3",
                        dest.digit = 6, all = TRUE),
  error = function(e) { cat("[WARN] concord() fallito:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(translated)) {
  translated_codes <- unique(unlist(lapply(translated, function(z) z$match)))
  translated_codes <- translated_codes[!is.na(translated_codes)]
  n_translated <- length(translated_codes)
  n_matched_to_new <- length(intersect(translated_codes, new_codes_str))
  n_dead_explained <- sum(sapply(translated, function(z) {
    any(z$match %in% new_codes_str)
  }))

  cat(sprintf("Codici morti tradotti con successo dalla tabella: %d / %d\n",
              sum(sapply(translated, function(z) length(z$match) > 0 && !all(is.na(z$match)))),
              length(dead_codes_str)))
  cat(sprintf("Codici morti la cui traduzione HS2007 e' tra i codici NATI nel 2007: %d / %d (%.1f%%)\n",
              n_dead_explained, length(dead_codes_str), 100 * n_dead_explained / length(dead_codes_str)))

  ## quota di VALORE export (sui codici morti) spiegata dalla concordanza
  dead_exp <- c2006[hs6 %in% dead_codes]
  dead_exp[, hs6_str := sprintf("%06d", as.integer(hs6))]
  dead_exp[, explained := sapply(hs6_str, function(h) {
    idx <- match(h, dead_codes_str)
    if (is.na(idx)) return(FALSE)
    any(translated[[idx]]$match %in% new_codes_str)
  })]
  share_exp_explained <- dead_exp[explained == TRUE, sum(exp)] / dead_exp[, sum(exp)]
  cat(sprintf("Quota di VALORE export (sui codici morti) spiegata dalla concordanza ufficiale: %.1f%%\n",
              100 * share_exp_explained))
} else {
  n_dead_explained <- NA; share_exp_explained <- NA
}

## ── 3. Report ───────────────────────────────────────────────────────────
report <- c(
  "# Fase R1 (seguito) — Verifica vintage HS6 al confine 2006->2007", "",
  sprintf("Data: %s", Sys.Date()), "",
  sprintf("Codici morti (presenti 2006, assenti 2007): %d", length(dead_codes)),
  sprintf("Codici nati (assenti 2006, presenti 2007): %d", length(new_codes)), "",
  if (!is.null(translated)) c(
    sprintf("Codici morti la cui traduzione ufficiale HS2002->HS2007 e' tra i nati: %d / %d (%.1f%%)",
            n_dead_explained, length(dead_codes_str), 100 * n_dead_explained / length(dead_codes_str)),
    sprintf("Quota di valore export (sui codici morti) spiegata dalla concordanza ufficiale: %.1f%%",
            100 * share_exp_explained), "",
    "## Interpretazione",
    "- Se la quota spiegata e' ALTA (es. >70-80%): la diagnosi e' CONFERMATA — il salto del 2007",
    "  e' un cambio di nomenclatura (HS2002->HS2007) non tradotto a una vintage unica, non un",
    "  fenomeno di mercato reale. Passo successivo: ricodificare l'intero pannello su un'unica",
    "  vintage HS prima di qualunque stima (incluse le Fase R-control 08-12).",
    "- Se la quota spiegata e' BASSA: l'anomalia 2007 ha un'altra origine (da indagare oltre la",
    "  semplice revisione di nomenclatura: es. cambio di fonte/rilevazione doganale)."
  ) else "[WARN] concord() non disponibile/fallito — verifica manuale necessaria."
)
writeLines(report, file.path(out_dir, "R1b_hs_vintage_check.md"))
cat("\n[OK] Report scritto in", file.path(out_dir, "R1b_hs_vintage_check.md"), "\n")
