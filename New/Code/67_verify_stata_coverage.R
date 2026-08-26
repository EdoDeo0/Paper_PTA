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
## NB DESTA: Timor-Leste (144) non ha copertura DESTA, quindi nella variante
## `desta` le sue celle trattate vengono eliminate e il paese esce dalla lista
## dei trattati. Stata produce percio' UNA riga leave-one-out in meno. Non e' un
## troncamento: R quella riga la scrive lo stesso, ma rimuove solo ~50 celle gia'
## non trattate, quindi e' di fatto una ripetizione del baseline.
attese <- function(base, sfx) {
  incl <- grepl("inclHKMO", sfx)
  dst  <- grepl("desta", sfx)
  switch(base,
    tripledd_collapsed      = 8,
    wcb_collapsed           = 4,
    subindices_collapsed    = 28,
    r711_shapiro_intensity  = 4,
    r79_desttrends          = 8,
    r79b_wcb_trends         = 4,
    r79c_pretrends          = 4,
    ppml_extensive          = 8,
    dirty_leaveoneout       = 3 + (if (incl) 25 else 23) - (if (dst) 1 else 0),
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
incompleti <- 0L

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
  cat(" NB: Stata e' IN ESECUZIONE. I file incompleti sono marcati 'incompl.' e\n",
      "    NON fanno scattare allarmi: mentre una stima gira non si puo' dire se\n",
      "    un file sia a meta' o troncato. Verdetto definitivo solo a macchina ferma.\n")
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
    ## "In corso di scrittura" e "troncato da un crash" NON sono distinguibili
    ## in modo affidabile mentre Stata gira: una singola ppmlhdfe su 8 milioni
    ## di celle puo' stare piu' di mezz'ora senza toccare il file, quindi ne'
    ## il numero di righe ne' l'orario di modifica bastano. (Ci ho provato con
    ## entrambi: due falsi allarmi.)
    ## Regola onesta: con Stata attivo un file incompleto e' "incompleto" e non
    ## fa scattare nulla; il verdetto si da' a macchina ferma. Cosi' non si
    ## rischia di cancellare un file che e' semplicemente a meta'.
    in_corso <- stata_attivo
    stato <- if (ok_righe) "ok" else if (in_corso) "incompl." else "TRONCATO"
    if (!ok_righe && !in_corso) problemi <- problemi + 1L
    if (!ok_righe && in_corso) incompleti <- incompleti + 1L

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
          ## Eccezioni note: righe in cui il CSV R e' DIMOSTRATO corrotto e
          ## Stata e' l'autorita'. Si escludono dal confronto invece di
          ## sopprimere l'allarme, cosi' un nuovo disaccordo resta visibile.
          ## Prova dell'arbitrato in MISTAKES.md (voce 2026-08-26): R ristimato
          ## in processi isolati riproduce i valori Stata a 12 cifre.
          if (nome == "dirty_leaveoneout_desta.csv") {
            corrotte <- c("senza_111", "senza_127")
            tieni <- !(com %in% corrotte)
            if (any(!tieni)) acc_nota <- sprintf(" [escluse %d righe R corrotte]",
                                                 sum(!tieni)) else acc_nota <- ""
            a <- a[tieni]; b <- b[tieni]; com <- com[tieni]
          } else acc_nota <- ""
          dmax <- suppressWarnings(max(abs(a - b), na.rm = TRUE))
          acc <- sprintf("n=%d  |d|max=%.1e%s", length(com), dmax, acc_nota)
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

## ─────────────────────────────────────────────────────────────────────
## FAMIGLIE CON NOMI DIVERSI FRA R E STATA
## ─────────────────────────────────────────────────────────────────────
## Event study e Sun-Abraham non seguono la convenzione "stesso nome in due
## cartelle": il gemello R si chiama diversamente e vive in Diagnostics. Vanno
## quindi mappati a mano, ma il controllo è lo stesso (righe attese + accordo
## sui coefficienti).
DIR_D <- here("New/Output/TripleDiff/Diagnostics")

cat("\n")
cat(sprintf("%-34s %-8s %7s %7s  %s\n", "file (nomi non allineati)", "stato", "righe", "attese", "accordo con R"))

## chiave comune per l'event study: (t, quale)
key_es <- function(d, vcol) {
  ## Le righe di riferimento (t = -1, coefficiente 0) esistono solo nel file
  ## Stata. Attenzione: se la colonna `source` non c'è, `d$source` è NULL e
  ## `NULL %in% "reference"` restituisce logical(0), che nel subset azzera
  ## l'intero data frame invece di non filtrare nulla. Serve il controllo
  ## esplicito sull'esistenza della colonna.
  if ("source" %in% names(d)) d <- d[d$source != "reference", , drop = FALSE]
  stats::setNames(as.numeric(d[[vcol]]), paste(d$t, d$quale))
}
## Sun-Abraham: R usa "year::-6"/"ATT_aggregato", Stata "g_m6"/"ATT_aggregato"
key_sa <- function(d, is_stata) {
  if (is_stata) {
    tt <- ifelse(grepl("^g_m", d$term), -suppressWarnings(as.integer(sub("^g_m", "", d$term))),
          ifelse(grepl("^g_p", d$term),  suppressWarnings(as.integer(sub("^g_p", "", d$term))), NA))
    lab <- ifelse(is.na(tt), "ATT", as.character(tt))
    stats::setNames(as.numeric(d$coef), paste(d$spec, lab))
  } else {
    tt <- suppressWarnings(as.integer(sub(".*year::(-?[0-9]+).*", "\\1", d$term)))
    lab <- ifelse(is.na(tt), "ATT", as.character(tt))
    stats::setNames(as.numeric(d$coef), paste(d$outcome, lab))
  }
}

for (sfx in SFX) {
  for (spec in list(
      list(s = paste0("eventstudy_twfe_stata", sfx, ".csv"),
           r = file.path(DIR_D, paste0("eventstudy_collapsed", sfx, ".csv")),
           att = 24, f = "es"),
      list(s = paste0("sunab_stata", sfx, ".csv"),
           r = file.path(DIR_T, paste0("sunab_gap", sfx, ".csv")),
           att = 58, f = "sa"))) {
    p_s <- file.path(DIR_TS, spec$s)
    if (!file.exists(p_s)) {
      cat(sprintf("%-34s %-8s %7s %7s  %s\n", spec$s, "assente", "-", "-", "-")); next
    }
    d_s <- utils::read.csv(p_s, stringsAsFactors = FALSE)
    ok  <- nrow(d_s) == spec$att
    stato <- if (ok) "ok" else if (stata_attivo) "incompl." else "TRONCATO"
    if (!ok && !stata_attivo) problemi <- problemi + 1L
    if (!ok && stata_attivo)  incompleti <- incompleti + 1L
    acc <- "-"
    if (file.exists(spec$r)) {
      d_r <- utils::read.csv(spec$r, stringsAsFactors = FALSE)
      a <- if (spec$f == "es") key_es(d_r, "b")  else key_sa(d_r, FALSE)
      b <- if (spec$f == "es") key_es(d_s, "coef") else key_sa(d_s, TRUE)
      com <- intersect(names(a), names(b))
      if (length(com)) {
        dmax <- max(abs(a[com] - b[com]), na.rm = TRUE)
        acc <- sprintf("n=%d  |d|max=%.1e", length(com), dmax)
        if (is.finite(dmax) && dmax > 1e-6) {
          acc <- paste(acc, "*** SCARTO ***"); problemi <- problemi + 1L
        }
      } else acc <- "chiavi non appaiate"
    }
    cat(sprintf("%-34s %-8s %7d %7d  %s\n", spec$s, stato, nrow(d_s), spec$att, acc))
  }
}

cat("\n-------------------------------------------------------------------------\n")
if (problemi == 0L) {
  if (incompleti > 0L) {
    cat(sprintf("Nessun problema fra i file completati. %d ancora incompleti (Stata sta\n",
                incompleti))
    cat("girando): rilanciare a coda ferma per il verdetto definitivo.\n")
  } else {
    cat("Nessun problema: file completi e coefficienti in accordo con R.\n")
  }
} else {
  cat(sprintf("PROBLEMI RILEVATI: %d. Un file TRONCATO va CANCELLATO prima di\n", problemi))
  cat("rilanciare la coda, altrimenti il blocco viene saltato credendolo completo.\n")
  cat("ATTENZIONE: cancellare solo a CODA FERMA. Un file che Stata sta scrivendo\n")
  cat("in questo momento non e' troncato, e' semplicemente incompleto.\n")
}
cat("-------------------------------------------------------------------------\n")

quit(status = if (problemi == 0L) 0L else 1L)
