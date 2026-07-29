########################################################
###### _sample_config — variante di campione HK/Macao ###
########################################################
## Author: Edoardo Vitella
##
## ############################################################################
## ##  UNICA COSA DA TOCCARE: la riga qui sotto.                             ##
## ##    "excl" -> Hong Kong e Macao ESCLUSI  (specifica principale)         ##
## ##    "incl" -> Hong Kong e Macao INCLUSI  (robustezza d'appendice)       ##
## ############################################################################

SAMPLE <- "excl"

## ############################################################################
##
## Perche' esiste: HK (country_code 110) e Macao (121) sono entrepot di
## riesportazione - il flusso registrato verso di loro non e' domanda finale di
## quel mercato, quindi un effetto di composizione stimato li' non e'
## interpretabile. Sono 3.463.793 osservazioni, il 7,0% del campione
## (49.245.304 totali -> 45.781.211 esclusi HK+MO). La specifica principale del
## paper li esclude, la robustezza d'appendice li include.
##
## Questo file e' la sorgente unica di verita' su quale variante e' attiva:
## il filtro non e' piu' duplicato inline in ogni script.
##
## COME PRODURRE LE DUE VARIANTI (funziona in qualunque IDE, su Win e Mac,
## semplicemente aprendo gli script e premendo Run - nessuna variabile
## d'ambiente, nessun argomento da riga di comando):
##   1. lasciare SAMPLE <- "excl", eseguire gli script che servono
##   2. cambiare in SAMPLE <- "incl", salvare, rieseguire gli stessi script
## Gli output della seconda passata prendono il suffisso "_inclHKMO" e non
## sovrascrivono quelli della prima.
##
## Uso dentro uno script (dopo library(here)):
##   source(here("New/Code/_sample_config.R"))
##   d <- hkmo_filter(d)                       # invece del filtro hardcoded
##   fwrite(x, out_path(".../tabella.csv"))    # su OGNI output E OGNI cache
##
## ATTENZIONE - out_path() va applicato anche ai path di CACHE, non solo agli
## output finali: molti script saltano il ricalcolo se il file di cache esiste,
## quindi senza suffisso un run "incl" leggerebbe la cache "excl" e
## restituirebbe i numeri sbagliati SENZA sollevare alcun errore.

if (!SAMPLE %in% c("excl", "incl")) {
  stop(sprintf("SAMPLE deve essere 'excl' o 'incl', trovato: '%s'", SAMPLE))
}

HKMO_CODES    <- c(110L, 121L)
HKMO_DROP     <- SAMPLE == "excl"
SAMPLE_SUFFIX <- if (SAMPLE == "incl") "_inclHKMO" else ""

## Applica (o no) l'esclusione HK+MO. Accetta data.table o data.frame con la
## colonna country_code.
hkmo_filter <- function(d) {
  if (!HKMO_DROP) return(d)
  stopifnot("country_code assente: hkmo_filter() non applicabile" =
              "country_code" %in% names(d))
  d[!as.integer(d$country_code) %in% HKMO_CODES, ]
}

## Inserisce SAMPLE_SUFFIX prima dell'estensione: "a/b.csv" -> "a/b_inclHKMO.csv".
## Su una directory (nessuna estensione) il suffisso va in coda.
out_path <- function(path) {
  if (SAMPLE_SUFFIX == "") return(path)
  ext <- tools::file_ext(path)
  if (ext == "") return(paste0(path, SAMPLE_SUFFIX))
  sub(sprintf("\\.%s$", ext), sprintf("%s.%s", SAMPLE_SUFFIX, ext), path)
}

cat(sprintf("[campione] %s - HK+MO %s%s\n", SAMPLE,
            if (HKMO_DROP) "esclusi" else "inclusi",
            if (SAMPLE_SUFFIX != "") sprintf(" - suffisso output: %s", SAMPLE_SUFFIX) else ""))
