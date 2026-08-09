########################################################
###### _sample_config — campione e controllo depth      ###
########################################################
## Author: Edoardo Vitella
##
## ############################################################################
## ##  DUE COSE DA TOCCARE (una per asse):                                   ##
## ##                                                                         ##
## ##  1. CAMPIONE HK/Macao:                                                 ##
## ##       "excl" -> HK e Macao ESCLUSI  (specifica principale)             ##
## ##       "incl" -> HK e Macao INCLUSI  (robustezza)                       ##
## ##                                                                         ##
## ##  2. CONTROLLO DI PROFONDITA':                                           ##
## ##       "totaldepth" -> TotalDepth_nonEnv, WB (specifica principale)     ##
## ##       "desta"      -> DESTA_depth_index, fonte indipendente (rob.)     ##
## ############################################################################

SAMPLE <- "incl"
DEPTH  <- "totaldepth"

## ############################################################################
##
## Asse 1 — HK/Macao: entrepot di riesportazione, il flusso verso di loro
## non e' domanda finale. Sono 3.463.793 obs, il 7,0% del campione.
## La spec principale li esclude, la robustezza li include.
##
## Asse 2 — Depth control: TotalDepth e' costruito dalla stessa fonte di EP
## (WB) e ha una correlazione within 0,96 (VIF 5,71). DESTA e' una misura
## indipendente (Dur, Baccini & Elsig 2014) che riduce il VIF a 1,92 e dimezza
## gli errori standard. Timor Est (country_code=144) ha un PTA cinese ma non
## e' nel DESTA: nelle stime desta riceve NA -> le sue celle escono dal campione
## (~4181 celle, 0,11%). Le destinazioni mai trattate ricevono 0 in entrambi.
##
## COME PRODURRE LE 4 VARIANTI (nessuna variabile d'ambiente, nessun arg da CLI):
##   Run 1: SAMPLE="excl", DEPTH="totaldepth"  -> spec principale
##   Run 2: SAMPLE="incl", DEPTH="totaldepth"  -> robustezza campione
##   Run 3: SAMPLE="excl", DEPTH="desta"       -> robustezza depth
##   Run 4: SAMPLE="incl", DEPTH="desta"       -> robustezza doppia
##
## Uso dentro uno script (dopo library(here)):
##   source(here("New/Code/_sample_config.R"))
##   d <- hkmo_filter(d)
##   fwrite(x, out_path(".../tabella.csv"))   # su OGNI output E OGNI cache
##
## ATTENZIONE - out_path() va su path di CACHE oltre che sugli output finali:
## senza suffisso, una run "incl" o "desta" legge la cache della run
## precedente e restituisce numeri sbagliati SENZA errore.

if (!SAMPLE %in% c("excl", "incl"))
  stop(sprintf("SAMPLE deve essere 'excl' o 'incl', trovato: '%s'", SAMPLE))
if (!DEPTH %in% c("totaldepth", "desta"))
  stop(sprintf("DEPTH deve essere 'totaldepth' o 'desta', trovato: '%s'", DEPTH))

## --- Asse 1: HK/Macao -------------------------------------------------------
HKMO_CODES    <- c(110L, 121L)
HKMO_DROP     <- SAMPLE == "excl"
SAMPLE_SUFFIX <- if (SAMPLE == "incl") "_inclHKMO" else ""

hkmo_filter <- function(d) {
  if (!HKMO_DROP) return(d)
  stopifnot("country_code assente" = "country_code" %in% names(d))
  d[!as.integer(d$country_code) %in% HKMO_CODES, ]
}

## --- Asse 2: depth control --------------------------------------------------
DEPTH_VALS <- list(
  totaldepth = list(
    file             = here::here("New/Data/TotalDepth/wb_totaldepth_country_year.csv"),
    var              = "TotalDepth_nonEnv",
    suffix           = "",
    drop_unmeasured  = FALSE
  ),
  desta = list(
    file             = here::here("New/Data/TotalDepth/desta_depth_country_year.csv"),
    var              = "DESTA_depth_index",
    suffix           = "_desta",
    drop_unmeasured  = TRUE
  )
)
DEPTH_FILE            <- DEPTH_VALS[[DEPTH]]$file
DEPTH_VAR             <- DEPTH_VALS[[DEPTH]]$var
DEPTH_SUFFIX          <- DEPTH_VALS[[DEPTH]]$suffix
DEPTH_DROP_UNMEASURED <- DEPTH_VALS[[DEPTH]]$drop_unmeasured

## --- Suffisso combinato (ordine fisso: _inclHKMO poi _desta) ----------------
## Dai 4 casi: "", "_inclHKMO", "_desta", "_inclHKMO_desta"
OUT_SUFFIX <- paste0(SAMPLE_SUFFIX, DEPTH_SUFFIX)

## Inserisce OUT_SUFFIX prima dell'estensione. Su directory (no ext): in coda.
out_path <- function(path) {
  if (OUT_SUFFIX == "") return(path)
  ext <- tools::file_ext(path)
  if (ext == "") return(paste0(path, OUT_SUFFIX))
  sub(sprintf("\\.%s$", ext), sprintf("%s.%s", OUT_SUFFIX, ext), path)
}

cat(sprintf("[config] campione: %s (HK+MO %s) | depth: %s%s\n",
            SAMPLE,
            if (HKMO_DROP) "esclusi" else "inclusi",
            DEPTH_VAR,
            if (OUT_SUFFIX != "") sprintf(" | suffisso: %s", OUT_SUFFIX) else ""))
