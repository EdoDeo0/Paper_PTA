########################################################
###### 02 — Classificazione dirty goods (HS6)         ##
########################################################
## Author: Edoardo Vitella
## Sostituisce: 05_dirty_goods.R. Run: pochi secondi (nessun accesso al
##              pannello raw da 49M righe).
##
## Cosa fa: costruisce dirty_p a livello HS6 con due misure:
##   1. BINARIA (principale): settori dirty alla Mani-Wheeler (1998) /
##      Low-Yeats (1992), ISIC Rev.2 -> ISIC Rev.3 -> HS6 via la tabella
##      ufficiale WITS/UNSD (HS1996 <-> ISIC Rev.3):
##        341 pulp & paper | 351 industrial chemicals | 353 petroleum refining
##        371 iron & steel | 372 non-ferrous metals | [esteso: 369 cemento]
##   2. ESTESA: core + settore 369 (altri minerali non metallici, cemento).
## La vintage HS1996 e' coerente con la lista green (01) e con il pannello
## (trattato come HS1996 uniforme, vedi audit R1).
## Dove i due elenchi si sovrappongono, la lista green ha precedenza (e'
## curata a mano prodotto per prodotto; il dirty e' una classificazione
## settoriale grezza) - i codici in overlap escono dal dirty.
##
## Input:  New/Data/Classifications/wits_h1_i3/*.CSV (tabella WITS, gia' cachata)
##         New/Data/Classifications/green_codes_hs1996.csv (da 01_green_goods_hs1996.R)
## Output: New/Data/Classifications/dirty_goods_hs6.csv (hs6, dirty, dirty_ext)
##         New/Data/Classifications/overlap_dirty_green_CHECK.csv (se overlap > 0)
##
## Fonti bibliografiche dei settori dirty (vedi anche wiki/ManiWheeler1998_...
## e wiki/LowYeats1992_... per le paper card complete):
##   Mani, M., & Wheeler, D. (1998). In search of pollution havens? Dirty
##     industry in the world economy, 1960-1995. Journal of Environment &
##     Development, 7(3), 215-247. https://doi.org/10.1177/107049659800700302
##   Low, P., & Yeats, A. (1992). Do dirty industries migrate? In P. Low
##     (Ed.), International trade and the environment (pp. 89-103).
##     World Bank Discussion Paper 159. Washington, DC: World Bank.
##     (nessun DOI: capitolo pre-1992, non indicizzato su Crossref/OpenAlex)
## ATTENZIONE - core qui sotto vs. Tabella 1 originale di Mani-Wheeler:
## il core scelto in questo script (petrolio dentro, cemento fuori) e' in
## realta' l'OPPOSTO di quello che direbbe la Tabella 1 del paper originale:
##   - il CEMENTO (369) nella Tabella 1 di Mani-Wheeler compare stabilmente
##     tra i settori piu' inquinanti (alto in tutte e 4 le classifiche:
##     aria/acqua/metalli/overall) - ma qui e' relegato alla sola versione
##     ESTESA, non al core.
##   - il PETROLIO (353) e' invece esplicitamente ESCLUSO da Mani-Wheeler
##     dalla loro stessa analisi regionale ("very few countries are actually
##     involved in its production", nota 3 del paper) - ma qui e' nel core.
## Non e' un errore: questo script segue la lista "5 settori Mani-Wheeler"
## COSI' COME E' DIVENTATA STANDARD nella letteratura successiva (pulp&paper,
## chimica, petrolio, siderurgia, metalli non ferrosi come core; cemento solo
## come variante estesa) - e' la convenzione con cui la maggior parte dei
## paper che citano "Mani-Wheeler" definisce i settori dirty, non una lettura
## letterale della Tabella 1 del paper del 1998. Se un referee contesta
## questa scelta, il punto di partenza e' questa nota, non un bug da correggere.

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(here)
library(data.table)

## --- Parametri e percorsi --------------------------------------------------
OUT_DIR   <- here("New/Data/Classifications")
WITS_ZIP  <- file.path(OUT_DIR, "Concordance_H1_to_I3.zip")
WITS_DIR  <- file.path(OUT_DIR, "wits_h1_i3")
GREEN_FILE <- file.path(OUT_DIR, "green_codes_hs1996.csv")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

## --- Sezione 1: mapping settori dirty ISIC Rev.2 -> ISIC Rev.3 -----------
# mapping manuale, corrispondenze standard UNSD (una riga per settore ISIC2)
#   341 pulp & paper            -> 2101, 2102, 2109  (divisione 21)
#   351 industrial chemicals    -> 2411, 2412, 2413  (chimica di base)
#   353 petroleum refining      -> 2320
#   371 iron & steel            -> 2710, 2731        (2731 = fonderie ferrose)
#   372 non-ferrous metals      -> 2720, 2732        (2732 = fonderie non ferrose)
#   369 other non-met. minerals -> 2693-2696, 2699   (cemento ecc.; solo ESTESA)
dirty_isic3     <- c("2101", "2102", "2109", "2411", "2412", "2413",
                      "2320", "2710", "2731", "2720", "2732")          # core Mani-Wheeler
dirty_isic3_ext <- c(dirty_isic3, "2693", "2694", "2695", "2696", "2699")

## --- Sezione 2: ISIC3 -> HS6 via tabella ufficiale WITS/UNSD -------------
if (!dir.exists(WITS_DIR)) {
  if (!file.exists(WITS_ZIP)) {
    download.file("https://wits.worldbank.org/data/public/concordance/Concordance_H1_to_I3.zip",
                  WITS_ZIP, mode = "wb")
  }
  unzip(WITS_ZIP, exdir = WITS_DIR)
}
wits_csv <- list.files(WITS_DIR, pattern = "\\.CSV$", full.names = TRUE, ignore.case = TRUE)[1]
conc <- fread(wits_csv, colClasses = "character")
setnames(conc, c("hs6", "hs6_desc", "isic3", "isic3_desc"))
cat(sprintf("Tabella WITS HS1996->ISIC3: %d righe, %d HS6 distinti\n",
            nrow(conc), uniqueN(conc$hs6)))

hs6_dirty     <- sort(unique(conc[isic3 %in% dirty_isic3,     hs6]))
hs6_dirty_ext <- sort(unique(conc[isic3 %in% dirty_isic3_ext, hs6]))
cat(sprintf("HS6 dirty: core %d | esteso %d\n", length(hs6_dirty), length(hs6_dirty_ext)))

dt <- data.table(hs6 = hs6_dirty_ext)
dt[, dirty     := as.integer(hs6 %in% hs6_dirty)]
dt[, dirty_ext := 1L]

## --- Sezione 3: overlap con la lista green (la green ha precedenza) ------
if (file.exists(GREEN_FILE)) {
  green <- fread(GREEN_FILE, colClasses = list(character = "hs6_final"))
  green_codes <- unique(green$hs6_final)
  overlap <- intersect(dt$hs6, green_codes)
  cat(sprintf("Overlap dirty/env_good: %d codici (atteso ~0)\n", length(overlap)))
  if (length(overlap) > 0) {
    fwrite(data.table(hs6 = overlap), file.path(OUT_DIR, "overlap_dirty_green_CHECK.csv"))
    # le due categorie devono essere mutuamente esclusive nella triple-diff:
    # un prodotto non puo' essere sia green che dirty
    dt <- dt[!hs6 %in% green_codes]
    cat(sprintf("  -> %d codici rimossi dal dirty (precedenza alla lista green); restano %d\n",
                length(overlap), nrow(dt)))
  }
} else {
  cat("[WARN] green_codes_hs1996.csv non trovato (eseguire prima 01_green_goods_hs1996.R) - overlap check saltato.\n")
}

## --- Sezione 4: salvataggio -----------------------------------------------
fwrite(dt, file.path(OUT_DIR, "dirty_goods_hs6.csv"))
cat("[OK] dirty_goods_hs6.csv -", nrow(dt), "codici HS6\n")
