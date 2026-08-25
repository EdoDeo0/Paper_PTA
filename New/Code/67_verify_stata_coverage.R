########################################################
###### 67 — Verifica della copertura Stata delle tabelle  ###
########################################################
## Author: Edoardo Vitella
##
## COSA FA. Due controlli su tutti i CSV prodotti dalla campagna di copertura
## Stata (script 61-66):
##
##   1. INTEGRITA': ogni file ha il numero di righe che dovrebbe avere?
##      Serve perche' i do-file scrivono l'intestazione del CSV all'INIZIO del
##      blocco e vi appendono le righe man mano. Se il processo si interrompe a
##      meta' (questa macchina ha storia di riavvii improvvisi), resta un file
##      valido ma TRONCATO, e la logica resume-safe lo salterebbe credendolo
##      completo. Questo controllo intercetta esattamente quel caso.
##
##   2. ACCORDO CON R: per ogni file con un gemello R, confronta i coefficienti
##      e riporta lo scarto massimo. I coefficienti devono coincidere
##      (identita' numerica); i p bootstrap/permutazione no, perche' hanno
##      errore Monte Carlo - per quelli si riporta lo scarto senza giudizio.
##
## USO: dopo ogni tornata della coda.
##   & "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New/Code/67_verify_stata_coverage.R
##
## Esce con stato 1 se trova file troncati o scarti sui coefficienti > 1e-6:
## utilizzabile come gate in uno script di coda.

rm(list = ls())
suppressWarnings(suppressMessages(library(here)))

DIR_T  <- here("New/Output/TripleDiff/Tables")
DIR_TS <- here("New/Output/TripleDiff/Tables_Stata")

SFX <- c("", "_inclHKMO", "_desta", "_inclHKMO_desta")

## righe DATI attese (senza intestazione). NA = dipende dalla variante.
## Il leave-one-out ha 2 spec fisse + senza_alta_dose + una riga per paese
## trattato: 23 trattati escl. HK/Macao, 25 includendoli.
attese <- function(base, sfx) {
  incl <- grepl("inclHKMO", sfx)
  switch(base,
    tripledd_collapsed      = 8,
    wcb_collapsed           = 4,
    subindices_collapsed    = 28,
    r711_shapiro_intensity  = 4,
    r79_desttrends          = 8,
    r79b_wcb_trends         = 4,
    r79c_pretrends          = 4,
    ppml_extensive          = 8,
    dirty_leaveoneout       = 3 + if (incl) 25 else 23,
    NA_integer_)
}

FAMIGLIE <- c("tripledd_collapsed", "wcb_collapsed", "subindices_collapsed",
              "r711_shapiro_intensity", "r79_desttrends", "r79b_wcb_trends",
              "r79c_pretrends", "dirty_leaveoneout", "ppml_extensive")

## chiave di appaiamento R <-> Stata, per famiglia
chiave <- function(base, d) {
  if (base == "subindices_collapsed") return(paste(d$sub_index, d$term))
  if (base == "dirty_leaveoneout")    return(as.character(d$spec))
  if (all(c("treat", "term") %in% names(d))) return(paste(d$treat, d$term))
  return(as.character(seq_len(nrow(d))))
}

problemi <- 0L

## C'e' una sessione Stata in esecuzione? Se si', i file appena iniziati sono
## blocchi in corso, non file corrotti.
stata_attivo <- tryCatch({
  out <- suppressWarnings(system2("tasklist", c("/FI", "\"IMAGENAME eq StataSE-64.exe\""),
                                  stdout = TRUE, stderr = NULL))
  any(grepl("StataSE-64", out, fixed = TRUE))
}, error = function(e) FALSE)

cat("=========================================================================\n")
cat(" VERIFICA COPERTURA STATA —", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
if (stata_attivo)
  cat(" NB: Stata e' IN ESECUZIONE — i file a 0 righe sono blocchi in corso,\n",
      "    non file corrotti. Per un verdetto definitivo rilanciare a coda ferma.\n")
cat("=========================================================================\n\n")
cat(sprintf("%-34s %-8s %7s %7s  %s\n", "file", "stato", "righe", "attese", "accordo con R"))

for (base in FAMIGLIE) {
  for (sfx in SFX) {
    nome <- paste0(base, sfx, ".csv")
    p_s  <- file.path(DIR_TS, nome)
    if (!file.exists(p_s)) {
      cat(sprintf("%-34s %-8s %7s %7s  %s\n", nome, "assente", "-", "-", "-"))
      next
    }
    d_s <- tryCatch(utils::read.csv(p_s, stringsAsFactors = FALSE),
                    error = function(e) NULL)
    if (is.null(d_s)) {
      cat(sprintf("%-34s %-8s %7s %7s  %s\n", nome, "ILLEGG.", "-", "-", "-"))
      problemi <- problemi + 1L; next
    }
    att <- attese(base, sfx)
    ok_righe <- is.na(att) || nrow(d_s) == att
    ## Un file con la sola intestazione e' quasi sempre un blocco IN CORSO, non
    ## un residuo di crash: i do-file scrivono l'intestazione all'inizio del
    ## blocco. Distinguere i due casi e' importante, perche' il rimedio per un
    ## file troncato e' cancellarlo — e cancellare un file che Stata sta
    ## scrivendo in quel momento farebbe danno.
    in_corso <- nrow(d_s) == 0L && stata_attivo
    stato <- if (ok_righe) "ok" else if (in_corso) "in corso" else "TRONCATO"
    if (!ok_righe && !in_corso) problemi <- problemi + 1L

    ## accordo con il gemello R
    acc <- "-"
    p_r <- file.path(DIR_T, nome)
    if (file.exists(p_r)) {
      d_r <- tryCatch(utils::read.csv(p_r, stringsAsFactors = FALSE),
                      error = function(e) NULL)
      if (!is.null(d_r) && "coef" %in% names(d_r) && "coef" %in% names(d_s)) {
        kr <- chiave(base, d_r); ks <- chiave(base, d_s)
        com <- intersect(kr, ks)
        if (length(com)) {
          a <- d_r$coef[match(com, kr)]
          b <- d_s$coef[match(com, ks)]
          dmax <- suppressWarnings(max(abs(a - b), na.rm = TRUE))
          acc <- sprintf("n=%d  |d|max=%.1e", length(com), dmax)
          if (is.finite(dmax) && dmax > 1e-6) {
            acc <- paste(acc, "*** SCARTO ***")
            problemi <- problemi + 1L
          }
        } else acc <- "chiavi non appaiate"
      }
    }
    cat(sprintf("%-34s %-8s %7d %7s  %s\n", nome, stato, nrow(d_s),
                if (is.na(att)) "?" else as.character(att), acc))
  }
}

cat("\n-------------------------------------------------------------------------\n")
if (problemi == 0L) {
  cat("Nessun problema: file completi e coefficienti in accordo con R.\n")
} else {
  cat(sprintf("PROBLEMI RILEVATI: %d. Un file TRONCATO va CANCELLATO prima di\n", problemi))
  cat("rilanciare la coda, altrimenti il blocco viene saltato credendolo completo.\n")
  cat("ATTENZIONE: cancellare solo a CODA FERMA. Un file che Stata sta scrivendo\n")
  cat("in questo momento non e' troncato, e' semplicemente incompleto.\n")
}
cat("-------------------------------------------------------------------------\n")

quit(status = if (problemi == 0L) 0L else 1L)
