########################################################################
###### 44 — Generatore di tabelle LaTeX dai CSV delle stime          ###
########################################################################

## Author: Edoardo Vitella
##
## SCOPO
## -----
## Legge i CSV prodotti dalla campagna di stima (script 16-43 R + 17/17b/18
## Stata) e scrive un frammento .tex per ogni tabella in New/Paper/Tabelle/.
## Nessun numero viene trascritto a mano: se un CSV cambia, basta rilanciare
## questo script e le tabelle si aggiornano.
##
## PERCHE' ESISTE
## --------------
## Il paper aveva 32 tabelle battute a mano e zero \input{}: con 4 varianti
## di stima x ~10 output ciascuna, la trascrizione manuale era il principale
## punto di rischio del progetto (ROADMAP §10).
##
## LE 4 VARIANTI (matrice 2x2)
##   asse 1 — campione:  escluse / incluse Hong Kong e Macao
##   asse 2 — controllo: TotalDepth (Banca Mondiale) / DESTA (fonte esterna)
##
## NOTA ONESTA: la struttura delle fixed effects e il livello di clustering
## NON sono registrati nei CSV (ROADMAP §10 punto 3): sono presi dagli script
## che li hanno prodotti e scritti a mano nelle note qui sotto. Se cambia uno
## script, va aggiornata la costante FE_* corrispondente.
##
## Output: New/Paper/Tabelle/tab_*.tex

ROOT <- tryCatch(here::here(), error = function(e) getwd())
DIR_T <- file.path(ROOT, "New/Output/TripleDiff/Tables")
DIR_D <- file.path(ROOT, "New/Output/TripleDiff/Diagnostics")
DIR_G <- file.path(ROOT, "New/Output/Diagnostics")
OUT   <- file.path(ROOT, "New/Paper/Tabelle")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

## ─────────────────────────────────────────────────────────────────────
## HELPER DI FORMATTAZIONE
## ─────────────────────────────────────────────────────────────────────

## numero con d decimali; NA -> cella vuota
fmt <- function(x, d = 5) {
  ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = d))
}
## interi con separatore delle migliaia
fmt_n <- function(x) {
  ifelse(is.na(x), "", formatC(as.numeric(x), format = "d", big.mark = ","))
}
## p-value: sotto 0.001 non si stampano zeri inutili
fmt_p <- function(p) {
  p <- as.numeric(p)
  ifelse(is.na(p), "",
    ifelse(p < 0.001, "$<$0.001", formatC(p, format = "f", digits = 3)))
}
## stelle di significativita'
st <- function(p) {
  p <- as.numeric(p)
  ifelse(is.na(p), "",
    ifelse(p < 0.01, "$^{***}$",
      ifelse(p < 0.05, "$^{**}$",
        ifelse(p < 0.10, "$^{*}$", ""))))
}
## coefficiente + stelle
cst <- function(b, p, d = 5) paste0(fmt(b, d), st(p))
## escape dei caratteri speciali LaTeX (per nomi di file/variabili nelle note)
esc <- function(s) {
  s <- gsub("\\\\", "\\\\textbackslash{}", s)
  s <- gsub("([_$%&#{}])", "\\\\\\1", s)
  s
}

## lettura CSV tollerante: se il file non c'e', restituisce NULL
rd <- function(path) {
  if (!file.exists(path)) { message("[manca] ", basename(path)); return(NULL) }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

## ─────────────────────────────────────────────────────────────────────
## COSTANTI DEL DISEGNO (dagli script, non dai CSV — vedi nota in testa)
## ─────────────────────────────────────────────────────────────────────
FE_FULL      <- "impresa$\\times$prodotto$\\times$destinazione, impresa$\\times$destinazione$\\times$anno, prodotto$\\times$anno"
FE_COLL      <- "prodotto$\\times$destinazione, destinazione$\\times$anno, prodotto$\\times$anno"
CLUSTER      <- "destinazione"
NOTE_STARS   <- "$^{*}$ $p<0.10$, $^{**}$ $p<0.05$, $^{***}$ $p<0.01$."
NOTE_SE      <- "Errore standard fra parentesi tonde, $p$-value fra parentesi quadre."

## Le 4 varianti: suffisso del file e intestazione di colonna
VAR <- list(
  list(sfx = "",                lab = "(1)", desc = "Escl.\\ HK/Macao, controllo TotalDepth (baseline)"),
  list(sfx = "_inclHKMO",       lab = "(2)", desc = "Incl.\\ HK/Macao, controllo TotalDepth"),
  list(sfx = "_desta",          lab = "(3)", desc = "Escl.\\ HK/Macao, controllo DESTA"),
  list(sfx = "_inclHKMO_desta", lab = "(4)", desc = "Incl.\\ HK/Macao, controllo DESTA")
)
NVAR <- length(VAR)

## Etichette leggibili dei termini (niente nomi di variabile grezzi in tabella)
LAB <- c(
  ## collassato / R
  "WB_EP_Depth:env_good"        = "Profondit\\`a EP (WB) $\\times$ Verde",
  "WB_EP_Depth:dirty_p"         = "Profondit\\`a EP (WB) $\\times$ Sporco",
  "TREND_EP_Count:env_good"     = "Conteggio EP (TREND) $\\times$ Verde",
  "TREND_EP_Count:dirty_p"      = "Conteggio EP (TREND) $\\times$ Sporco",
  "env_good:TotalDepth_nonEnv"  = "Profondit\\`a accordo $\\times$ Verde",
  "dirty_p:TotalDepth_nonEnv"   = "Profondit\\`a accordo $\\times$ Sporco",
  "env_good:DESTA_depth_index"  = "Profondit\\`a accordo $\\times$ Verde",
  "dirty_p:DESTA_depth_index"   = "Profondit\\`a accordo $\\times$ Sporco",
  "env_good:TotalDepth_targeted"= "Profondit\\`a mirata $\\times$ Verde",
  "dirty_p:TotalDepth_targeted" = "Profondit\\`a mirata $\\times$ Sporco",
  "WB_EP_Depth:apec_green"      = "Profondit\\`a EP (WB) $\\times$ Verde (APEC)",
  "apec_green:TotalDepth_nonEnv"= "Profondit\\`a accordo $\\times$ Verde (APEC)",
  "TREND_EP_Count:apec_green"   = "Conteggio EP (TREND) $\\times$ Verde (APEC)",
  "SUB:env_good"                = "Sotto-indice $\\times$ Verde",
  "SUB:dirty_p"                 = "Sotto-indice $\\times$ Sporco",
  "EP_share:env_good"           = "Quota EP $\\times$ Verde",
  "EP_share:dirty_p"            = "Quota EP $\\times$ Sporco",
  ## full panel / Stata
  "wb_green" = "Profondit\\`a EP (WB) $\\times$ Verde",
  "wb_dirty" = "Profondit\\`a EP (WB) $\\times$ Sporco",
  "tr_green" = "Conteggio EP (TREND) $\\times$ Verde",
  "tr_dirty" = "Conteggio EP (TREND) $\\times$ Sporco",
  "td_green" = "Profondit\\`a accordo $\\times$ Verde",
  "td_dirty" = "Profondit\\`a accordo $\\times$ Sporco",
  "tariffs"  = "Dazio MFN (log)",
  "ln_hhi_baci" = "Concentrazione di mercato (log HHI)",
  "AD_pdt"   = "Esposizione ad antidumping",
  "WB_EP_Depth"    = "Profondit\\`a EP (WB)",
  "TREND_EP_Count" = "Conteggio EP (TREND)",
  "totaldepth_nonenv" = "Profondit\\`a accordo",
  ## bootstrap / permutazione
  "ep_green" = "Profondit\\`a EP $\\times$ Verde",
  "ep_dirty" = "Profondit\\`a EP $\\times$ Sporco",
  "ep_co2"   = "Profondit\\`a EP $\\times$ Intensit\\`a CO$_2$"
)
lab_of <- function(t) {
  out <- LAB[t]
  ifelse(is.na(out), paste0("\\texttt{", esc(t), "}"), out)
}

## ─────────────────────────────────────────────────────────────────────
## COSTRUTTORE GENERICO: tabella a 4 colonne (una per variante)
##   blocks: lista di list(title=..., terms=c(...))
##   getter(sfx) -> data.frame con colonne term, coef, se, pval (+ nobs, r2)
## ─────────────────────────────────────────────────────────────────────
tab_variants <- function(getter, blocks, caption, label, notes,
                         show_p = TRUE, digits = 5, extra_stats = NULL) {
  dat <- lapply(VAR, function(v) getter(v$sfx))
  names(dat) <- sapply(VAR, function(v) v$sfx)

  L <- c(
    "\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    paste0("\\caption{", caption, "}"), paste0("\\label{", label, "}"),
    "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"),
    "\\midrule"
  )

  for (bk in blocks) {
    if (!is.null(bk$title)) {
      L <- c(L, paste0("\\multicolumn{", NVAR + 1, "}{l}{\\textit{", bk$title, "}} \\\\"))
    }
    for (tm in bk$terms) {
      row_b <- character(NVAR); row_s <- character(NVAR); row_p <- character(NVAR)
      for (i in seq_len(NVAR)) {
        d <- dat[[i]]
        r <- if (is.null(d)) NULL else d[d$term == tm, , drop = FALSE]
        if (is.null(r) || nrow(r) == 0) { row_b[i] <- ""; row_s[i] <- ""; row_p[i] <- "" } else {
          row_b[i] <- cst(r$coef[1], r$pval[1], digits)
          row_s[i] <- paste0("(", fmt(r$se[1], digits), ")")
          row_p[i] <- paste0("[", fmt_p(r$pval[1]), "]")
        }
      }
      L <- c(L, paste0(lab_of(tm), " & ", paste(row_b, collapse = " & "), " \\\\"),
                paste0(" & ", paste(row_s, collapse = " & "), " \\\\"))
      if (show_p) L <- c(L, paste0(" & ", paste(row_p, collapse = " & "), " \\\\"))
      L <- c(L, "\\addlinespace")
    }
  }

  ## righe di statistiche
  L <- c(L, "\\midrule")
  nobs <- sapply(dat, function(d) if (is.null(d) || is.null(d$nobs)) NA else d$nobs[1])
  L <- c(L, paste0("Osservazioni & ", paste(fmt_n(nobs), collapse = " & "), " \\\\"))
  if (!is.null(dat[[1]]$r2)) {
    r2 <- sapply(dat, function(d) if (is.null(d) || is.null(d$r2)) NA else d$r2[1])
    L <- c(L, paste0("$R^2$ & ", paste(fmt(r2, 4), collapse = " & "), " \\\\"))
  }
  if (!is.null(extra_stats)) for (es in extra_stats) {
    L <- c(L, paste0(es$label, " & ", paste(es$values, collapse = " & "), " \\\\"))
  }

  L <- c(L, "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc,
           "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    paste0("\\item ", notes),
    paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  L
}

wr <- function(lines, file) {
  writeLines(lines, file.path(OUT, file))
  cat("[ok]", file, "\n")
}

cat("\n=== Generazione tabelle in", OUT, "===\n")

## normalizza i nomi dei termini: il controllo di profondita' cambia nome fra
## le varianti (TotalDepth_nonEnv vs DESTA_depth_index) ma svolge lo stesso
## ruolo, quindi in tabella deve stare sulla stessa riga
norm_term <- function(t) {
  t <- gsub("TotalDepth_nonEnv", "DEPTH", t, fixed = TRUE)
  t <- gsub("DESTA_depth_index", "DEPTH", t, fixed = TRUE)
  t
}
LAB["env_good:DEPTH"] <- "Profondit\\`a accordo $\\times$ Verde"
LAB["dirty_p:DEPTH"]  <- "Profondit\\`a accordo $\\times$ Sporco"

## ── getter per le due unita' di analisi ──────────────────────────────
get_full <- function(sfx, tr) {
  d <- rd(file.path(DIR_T, paste0("tripledd_full_reghdfe", sfx, ".csv")))
  if (is.null(d)) return(NULL)
  d <- d[d$treat == tr & d$var != "_cons", , drop = FALSE]
  if (!nrow(d)) return(NULL)
  data.frame(term = norm_term(d$var), coef = d$coef, se = d$stderr,
             pval = d$pval, nobs = d$N, r2 = d$r2, stringsAsFactors = FALSE)
}
get_coll <- function(sfx, tr) {
  d <- rd(file.path(DIR_T, paste0("tripledd_collapsed", sfx, ".csv")))
  if (is.null(d)) return(NULL)
  d <- d[d$treat == tr, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  data.frame(term = norm_term(d$term), coef = d$coef, se = d$se,
             pval = d$pval, nobs = d$nobs, stringsAsFactors = FALSE)
}

########################################################################
## T1 — Mappa del trattamento
########################################################################
{
  d <- rd(file.path(DIR_G, "B_treatment_entry.csv"))
  d <- d[order(d$entry_year, d$country), ]
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Le destinazioni trattate: quando l'accordo entra in vigore e quanto contenuto ambientale contiene}",
    "\\label{tab:trattamento}", "\\begin{threeparttable}",
    "\\begin{tabular}{llcc}", "\\toprule",
    "Destinazione & Codice & Anno di entrata & Profondit\\`a EP massima \\\\",
    " & & in vigore & WB \\quad / \\quad TREND \\\\", "\\midrule")
  for (i in seq_len(nrow(d))) {
    L <- c(L, sprintf("%s & %d & %d & %d \\quad / \\quad %d \\\\",
                      esc(d$country[i]), d$country_code[i], d$entry_year[i],
                      d$max_WB[i], d$max_TREND[i]))
  }
  L <- c(L, "\\midrule",
    sprintf("\\textbf{Totale} & %d destinazioni & & \\\\", nrow(d)),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    "\\item Ogni riga \\`e una destinazione con cui la Cina ha un accordo commerciale in vigore fra il 2000 e il 2015.",
    "\\item \\textit{Profondit\\`a EP} = quante disposizioni ambientali contiene l'accordo, secondo due codifiche indipendenti: quella della Banca Mondiale (WB) e quella del database accademico TREND.",
    "\\item Le 11 destinazioni ASEAN condividono lo stesso accordo (2005) e quindi gli stessi valori: gli accordi realmente distinti sono circa 14, non 25. \\`E questo il vincolo che governa tutta l'inferenza del lavoro.",
    "\\item Hong Kong e Macao sono economie di transito: met\\`a del valore esportato verso destinazioni trattate passa da l\\`i. Per questo il campione principale le esclude e una variante le reinserisce.",
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_01_trattamento.tex")
}

########################################################################
## T2 — Saturation ladder (effetto di livello)
########################################################################
{
  src <- file.path(ROOT, "New/Output/OLS/Tables/OLS_Ladder_FE.tex")
  if (file.exists(src)) {
    inner <- readLines(src, warn = FALSE)
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Perch\\'e l'effetto \\emph{di livello} non \\`e stimabile: la scala di saturazione}",
      "\\label{tab:ladder}", "\\begin{threeparttable}", inner,
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Variabile dipendente: logaritmo del valore esportato. Ogni riga aggiunge effetti fissi pi\\`u stringenti, cio\\`e elimina confronti sempre meno credibili.",
      "\\item Notazione: \\textit{f} = impresa, \\textit{p} = prodotto, \\textit{d} = destinazione, \\textit{t} = anno.",
      "\\item Il coefficiente \\`e piccolo ovunque e la significativit\\`a compare solo nella specificazione intermedia, per poi sparire quando si satura. \\`E il segno tipico della \\emph{selezione}: la Cina firma accordi con i partner verso cui gi\\`a esporta molto. Da qui la scelta di abbandonare l'effetto di livello e passare alla composizione.",
      "\\item Errore standard fra parentesi, raggruppato per destinazione.",
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_02_ladder.tex")
  }
}

########################################################################
## T3 — Specificazione principale, FULL PANEL
########################################################################
{
  ## due pannelli (WB e TREND) nella stessa tabella
  gA <- function(sfx) get_full(sfx, "WB")
  gB <- function(sfx) get_full(sfx, "TREND")
  datA <- lapply(VAR, function(v) gA(v$sfx)); datB <- lapply(VAR, function(v) gB(v$sfx))
  rowset <- function(dat, terms) {
    out <- character(0)
    for (tm in terms) {
      b <- s <- p <- character(NVAR)
      for (i in seq_len(NVAR)) {
        d <- dat[[i]]; r <- if (is.null(d)) NULL else d[d$term == tm, , drop = FALSE]
        if (is.null(r) || !nrow(r)) { b[i] <- s[i] <- p[i] <- "" } else {
          b[i] <- cst(r$coef[1], r$pval[1]); s[i] <- paste0("(", fmt(r$se[1]), ")")
          p[i] <- paste0("[", fmt_p(r$pval[1]), "]")
        }
      }
      out <- c(out, paste0(lab_of(tm), " & ", paste(b, collapse = " & "), " \\\\"),
                    paste0(" & ", paste(s, collapse = " & "), " \\\\"),
                    paste0(" & ", paste(p, collapse = " & "), " \\\\"), "\\addlinespace")
    }
    out
  }
  nobs <- sapply(datA, function(d) if (is.null(d)) NA else d$nobs[1])
  r2   <- sapply(datA, function(d) if (is.null(d)) NA else d$r2[1])
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Specificazione principale --- dati a livello di singola transazione (\\emph{full panel})}",
    "\\label{tab:main-full}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello A --- profondit\\`a ambientale misurata dalla Banca Mondiale}} \\\\"),
    "\\addlinespace",
    rowset(datA, c("wb_green","wb_dirty","td_green","td_dirty")),
    "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello B --- profondit\\`a ambientale misurata da TREND}} \\\\"),
    "\\addlinespace",
    rowset(datB, c("tr_green","tr_dirty","td_green","td_dirty")),
    "\\midrule",
    paste0("Osservazioni & ", paste(fmt_n(nobs), collapse = " & "), " \\\\"),
    paste0("$R^2$ & ", paste(fmt(r2, 4), collapse = " & "), " \\\\"),
    paste0("Effetti fissi & \\multicolumn{", NVAR, "}{c}{impresa$\\times$prod.$\\times$dest.; impresa$\\times$dest.$\\times$anno; prod.$\\times$anno} \\\\"),
    paste0("Raggruppamento errori & \\multicolumn{", NVAR, "}{c}{destinazione} \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item Variabile dipendente: logaritmo del valore esportato dall'impresa \\textit{f} del prodotto \\textit{p} verso la destinazione \\textit{d} nell'anno \\textit{t}.",
    "\\item L'effetto fisso impresa$\\times$destinazione$\\times$anno assorbe \\emph{tutto} ci\\`o che accade a quell'impresa su quel mercato in quell'anno --- compreso l'accordo commerciale stesso, i tagli tariffari e gli shock di domanda. Resta identificato solo il confronto fra prodotti verdi, sporchi e neutri all'interno della stessa cella.",
    "\\item I coefficienti sono in punti logaritmici per unit\\`a di profondit\\`a: $-0.005$ significa circa $-0.5\\%$ di esportazioni per ogni disposizione ambientale in pi\\`u, rispetto ai prodotti neutri.",
    "\\item \\textbf{Attenzione alle scale.} Le righe \\emph{Profondit\\`a accordo} non sono confrontabili fra le colonne (1)--(2) e (3)--(4): nelle prime due il controllo \\`e un conteggio di disposizioni della Banca Mondiale, nelle altre due \\`e l'indice DESTA, costruito su una scala del tutto diversa. Le righe della profondit\\`a \\emph{ambientale}, che sono quelle di interesse, usano invece la stessa misura in tutte e quattro le colonne e restano quindi pienamente confrontabili.",
    "\\item Stima con \\texttt{reghdfe} (Stata), rimozione iterativa dei singleton.",
    paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_03_main_full.tex")
}

########################################################################
## T4 — Specificazione principale, PANNELLO COLLASSATO
########################################################################
{
  datA <- lapply(VAR, function(v) get_coll(v$sfx, "WB"))
  datB <- lapply(VAR, function(v) get_coll(v$sfx, "TREND"))
  rowset <- function(dat, terms) {
    out <- character(0)
    for (tm in terms) {
      b <- s <- p <- character(NVAR)
      for (i in seq_len(NVAR)) {
        d <- dat[[i]]; r <- if (is.null(d)) NULL else d[d$term == tm, , drop = FALSE]
        if (is.null(r) || !nrow(r)) { b[i] <- s[i] <- p[i] <- "" } else {
          b[i] <- cst(r$coef[1], r$pval[1]); s[i] <- paste0("(", fmt(r$se[1]), ")")
          p[i] <- paste0("[", fmt_p(r$pval[1]), "]")
        }
      }
      out <- c(out, paste0(lab_of(tm), " & ", paste(b, collapse = " & "), " \\\\"),
                    paste0(" & ", paste(s, collapse = " & "), " \\\\"),
                    paste0(" & ", paste(p, collapse = " & "), " \\\\"), "\\addlinespace")
    }
    out
  }
  nobs <- sapply(datA, function(d) if (is.null(d)) NA else d$nobs[1])
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Specificazione principale --- dati aggregati per prodotto, destinazione e anno (\\emph{pannello collassato})}",
    "\\label{tab:main-coll}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello A --- profondit\\`a ambientale misurata dalla Banca Mondiale}} \\\\"),
    "\\addlinespace",
    rowset(datA, c("WB_EP_Depth:env_good","WB_EP_Depth:dirty_p","env_good:DEPTH","dirty_p:DEPTH")),
    "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello B --- profondit\\`a ambientale misurata da TREND}} \\\\"),
    "\\addlinespace",
    rowset(datB, c("TREND_EP_Count:env_good","TREND_EP_Count:dirty_p","env_good:DEPTH","dirty_p:DEPTH")),
    "\\midrule",
    paste0("Celle & ", paste(fmt_n(nobs), collapse = " & "), " \\\\"),
    paste0("Effetti fissi & \\multicolumn{", NVAR, "}{c}{prod.$\\times$dest.; dest.$\\times$anno; prod.$\\times$anno} \\\\"),
    paste0("Ponderazione & \\multicolumn{", NVAR, "}{c}{numero di osservazioni impresa nella cella} \\\\"),
    paste0("Raggruppamento errori & \\multicolumn{", NVAR, "}{c}{destinazione} \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item Stessa logica della Tabella \\ref{tab:main-full}, ma l'unit\\`a di osservazione \\`e la cella prodotto--destinazione--anno anzich\\'e la singola transazione d'impresa. Serve a verificare che il risultato non dipenda dal livello di aggregazione e rende eseguibili stime altrimenti troppo pesanti.",
    "\\item Il $R^2$ non \\`e stato esportato per questa specificazione (i CSV non lo contengono).",
    "\\item Ogni cella \\`e pesata per il numero di osservazioni impresa che contiene: le celle pi\\`u grandi contano di pi\\`u. \\`E la differenza chiave rispetto al \\emph{full panel}, dove ogni osservazione pesa uno.",
    paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_04_main_collapsed.tex")
}

########################################################################
## T5 — Inferenza robusta: wild cluster bootstrap
########################################################################
{
  gc_ <- function(sfx, tr) {
    d <- rd(file.path(DIR_T, paste0("wcb_collapsed", sfx, ".csv")))
    if (is.null(d)) return(NULL); d[d$treat == tr, , drop = FALSE]
  }
  gf_ <- function(sfx) {
    p <- file.path(ROOT, paste0("New/Output/OLS", sfx, "/Bootstrap/wcb_fullpanel", sfx, ".csv"))
    rd(p)
  }
  cellf <- function(d, term, col) {
    if (is.null(d)) return("")
    r <- d[d$term == term | d$spec == term, , drop = FALSE]
    if (!nrow(r)) return("")
    if (col == "b") fmt(r$coef[1]) else fmt_p(r$p_wcb[1])
  }
  rowline <- function(getter, term, what) {
    v <- sapply(VAR, function(x) {
      d <- getter(x$sfx)
      if (is.null(d)) return("")
      r <- if ("term" %in% names(d)) d[d$term == term, , drop = FALSE] else d[d$spec == term, , drop = FALSE]
      if (!nrow(r)) return("")
      if (what == "b") fmt(r$coef[1]) else fmt_p(r$p_wcb[1])
    })
    paste(v, collapse = " & ")
  }
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Inferenza robusta: \\emph{wild cluster bootstrap} con 9.999 ripetizioni}",
    "\\label{tab:wcb}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello A --- pannello collassato (indice WB)}} \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Verde: coefficiente & ", rowline(function(s) gc_(s,"WB"), "ep_green", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(function(s) gc_(s,"WB"), "ep_green", "p"), " \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Sporco: coefficiente & ", rowline(function(s) gc_(s,"WB"), "ep_dirty", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(function(s) gc_(s,"WB"), "ep_dirty", "p"), " \\\\"), "\\addlinespace",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello B --- pannello collassato (indice TREND)}} \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Verde: coefficiente & ", rowline(function(s) gc_(s,"TREND"), "ep_green", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(function(s) gc_(s,"TREND"), "ep_green", "p"), " \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Sporco: coefficiente & ", rowline(function(s) gc_(s,"TREND"), "ep_dirty", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(function(s) gc_(s,"TREND"), "ep_dirty", "p"), " \\\\"),
    "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello C --- \\emph{full panel} (indice WB)}} \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Verde: coefficiente & ", rowline(gf_, "WB_green", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(gf_, "WB_green", "p"), " \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Sporco: coefficiente & ", rowline(gf_, "WB_dirty", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(gf_, "WB_dirty", "p"), " \\\\"), "\\addlinespace",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello D --- \\emph{full panel} (indice TREND)}} \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Verde: coefficiente & ", rowline(gf_, "TREND_green", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(gf_, "TREND_green", "p"), " \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Sporco: coefficiente & ", rowline(gf_, "TREND_dirty", "b"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rowline(gf_, "TREND_dirty", "p"), " \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item \\textbf{A cosa serve.} I $p$-value ordinari sono affidabili quando i gruppi su cui si raggruppano gli errori sono molti. Qui i gruppi trattati sono 25 e gli accordi realmente distinti circa 14: in questa situazione i $p$-value ordinari tendono a dichiarare significativo ci\\`o che non lo \\`e. Il \\emph{wild cluster bootstrap} ricostruisce la distribuzione del coefficiente rimescolando i dati 9.999 volte e restituisce un $p$-value molto pi\\`u prudente.",
    "\\item Il confronto con la Tabella \\ref{tab:main-coll} \\`e istruttivo: dove il $p$-value ordinario \\`e $<0.001$, quello bootstrap pu\\`o salire sopra 0.05.",
    "\\item Pannelli C e D calcolati con \\texttt{boottest} in Stata su dati residualizzati (Frisch--Waugh), perch\\'e il comando non regge pi\\`u di un insieme di effetti fissi assorbiti.",
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_05_wcb.tex")
}

########################################################################
## T6 — Test di permutazione
########################################################################
{
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Test di permutazione: 1.000 riassegnazioni casuali del contenuto ambientale}",
    "\\label{tab:perm}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule")
  get_perm <- function(sfx, tr) {
    d <- rd(file.path(DIR_T, paste0("r710_permutation_summary", sfx, ".csv")))
    if (is.null(d)) return(NULL); r <- d[d$treat == tr, , drop = FALSE]
    if (!nrow(r)) NULL else r
  }
  for (tr in c("WB", "TREND")) {
    ttl <- if (tr == "WB") "Pannello A --- indice WB" else "Pannello B --- indice TREND"
    L <- c(L, paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{", ttl, "}} \\\\"), "\\addlinespace")
    for (mg in c("green", "dirty")) {
      nm <- if (mg == "green") "Verde" else "Sporco"
      bs <- sapply(VAR, function(v) { r <- get_perm(v$sfx, tr); if (is.null(r)) "" else fmt(r[[paste0("b_obs_", mg)]][1]) })
      ps <- sapply(VAR, function(v) { r <- get_perm(v$sfx, tr); if (is.null(r)) "" else fmt_p(r[[paste0("p_perm_", mg)]][1]) })
      ns <- sapply(VAR, function(v) { r <- get_perm(v$sfx, tr); if (is.null(r)) "" else fmt_n(r[[paste0("n_used_", mg)]][1]) })
      L <- c(L, paste0("Profondit\\`a EP $\\times$ ", nm, ": coefficiente osservato & ", paste(bs, collapse = " & "), " \\\\"),
                paste0("\\quad $p$-value di permutazione & ", paste(ps, collapse = " & "), " \\\\"),
                paste0("\\quad permutazioni valide & ", paste(ns, collapse = " & "), " \\\\"), "\\addlinespace")
    }
  }
  L <- c(L, "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item \\textbf{Come funziona.} Si prende il profilo di contenuto ambientale di ciascun accordo (quanto \\`e profondo e in quali anni) e lo si riassegna a caso fra le destinazioni gi\\`a trattate, 1.000 volte. Ogni volta si ristima il modello. Il $p$-value \\`e la quota di riassegnazioni casuali che produce un coefficiente pi\\`u grande in valore assoluto di quello vero.",
    "\\item \\textbf{Come si legge.} Un $p$-value alto significa: ``lo stesso numero si sarebbe ottenuto etichettando a caso questi paesi'', cio\\`e il risultato non dipende dal contenuto ambientale. Un $p$-value basso significa che il numero osservato spicca fra i mille finti.",
    "\\item \\`E il test pi\\`u severo dei tre usati nel lavoro, perch\\'e non assume nulla sulla distribuzione degli errori.",
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_06_permutation.tex")
}

########################################################################
## T7 — Matrice di sintesi 2x2 (WB x Sporco, stesso metodo ovunque)
########################################################################
{
  getb <- function(sfx, unita) {
    if (unita == "coll") {
      d <- rd(file.path(DIR_T, paste0("wcb_collapsed", sfx, ".csv")))
      if (is.null(d)) return(c("", ""))
      r <- d[d$treat == "WB" & d$term == "ep_dirty", , drop = FALSE]
    } else {
      d <- rd(file.path(ROOT, paste0("New/Output/OLS", sfx, "/Bootstrap/wcb_fullpanel", sfx, ".csv")))
      if (is.null(d)) return(c("", ""))
      r <- d[d$spec == "WB_dirty", , drop = FALSE]
    }
    if (!nrow(r)) return(c("", ""))
    c(fmt(r$coef[1]), fmt_p(r$p_wcb[1]))
  }
  rowu <- function(unita, nome) {
    vals <- lapply(VAR, function(v) getb(v$sfx, unita))
    c(paste0(nome, " & ", paste(sapply(vals, `[`, 1), collapse = " & "), " \\\\"),
      paste0("\\quad $p$-value bootstrap & ", paste(sapply(vals, `[`, 2), collapse = " & "), " \\\\"),
      "\\addlinespace")
  }
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Sintesi: il coefficiente sui prodotti sporchi nelle quattro varianti, con lo stesso metodo di inferenza}",
    "\\label{tab:matrice}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule",
    rowu("coll", "Pannello collassato: coefficiente"),
    rowu("full", "\\emph{Full panel}: coefficiente"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item Coefficiente dell'interazione fra profondit\\`a ambientale (indice WB) e prodotti sporchi. \\`E l'unico coefficiente del lavoro che si muove: quello sui prodotti verdi \\`e nullo ovunque.",
    "\\item \\textbf{Tutti i $p$-value in questa tabella vengono dal \\emph{wild cluster bootstrap}}, cos\\`i le otto celle sono confrontabili fra loro. \\`E una precisazione necessaria: altrove nel progetto le colonne (1) e (2) erano state riassunte con il $p$-value ordinario, che \\`e molto pi\\`u basso e non \\`e paragonabile.",
    "\\item \\textbf{Come si legge.} Sul \\emph{full panel} il risultato dipende da quale controllo di profondit\\`a si usa: con il controllo della Banca Mondiale il $p$-value resta sopra 0.17, con il controllo DESTA scende sotto 0.05. I due controlli hanno correlazione diversa con la profondit\\`a ambientale (0.86 il primo, 0.69 il secondo): pi\\`u un controllo \\`e sovrapposto alla variabile di interesse, meno variazione indipendente resta e pi\\`u grande diventa l'errore standard.",
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_07_matrice.tex")
}

########################################################################
## T8 — Event study (pannello collassato)
########################################################################
{
  d <- rd(file.path(DIR_D, "eventstudy_collapsed.csv"))
  if (!is.null(d)) {
    tt <- sort(unique(d$t))
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Studio dell'evento: andamento anno per anno attorno all'entrata in vigore dell'accordo}",
      "\\label{tab:eventstudy}", "\\begin{threeparttable}",
      "\\begin{tabular}{rcccc}", "\\toprule",
      "Anni dall' & \\multicolumn{2}{c}{Prodotti verdi vs neutri} & \\multicolumn{2}{c}{Prodotti sporchi vs neutri} \\\\",
      "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
      "entrata & Coefficiente & Errore std. & Coefficiente & Errore std. \\\\", "\\midrule")
    for (k in tt) {
      g <- d[d$t == k & d$quale == "green", , drop = FALSE]
      b <- d[d$t == k & d$quale == "dirty", , drop = FALSE]
      L <- c(L, sprintf("%s & %s & (%s) & %s & (%s) \\\\",
        ifelse(k >= 0, paste0("+", k), as.character(k)),
        if (nrow(g)) fmt(g$b[1], 4) else "", if (nrow(g)) fmt(g$se[1], 4) else "",
        if (nrow(b)) fmt(b$b[1], 4) else "", if (nrow(b)) fmt(b$se[1], 4) else ""))
    }
    L <- c(L, "\\midrule", "$-1$ & \\multicolumn{4}{c}{\\textit{anno di riferimento (normalizzato a zero)}} \\\\",
      "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Variante baseline (escl.\\ HK/Macao, controllo TotalDepth), pannello collassato, effetti fissi prod.$\\times$dest., dest.$\\times$anno, prod.$\\times$anno; errori raggruppati per destinazione.",
      "\\item \\textbf{A cosa serve.} Prima dell'entrata in vigore l'accordo non pu\\`o avere effetto: se i coefficienti \\emph{prima} dell'anno zero fossero gi\\`a diversi da zero, vorrebbe dire che prodotti verdi e neutri stavano gi\\`a divergendo per altri motivi, e il confronto non sarebbe credibile.",
      "\\item \\textbf{Come si legge.} Valori vicini a zero e non significativi prima dell'entrata (righe negative) sostengono il disegno; l'assenza di un salto all'anno zero e dopo \\`e il risultato.",
      "\\item L'anno $-1$ \\`e il termine di paragone: tutti gli altri coefficienti misurano la distanza da quell'anno.",
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_08_eventstudy.tex")
  }
}

########################################################################
## T9 — Sun-Abraham sul divario verde/sporco vs neutri
########################################################################
{
  d <- rd(file.path(DIR_T, "sunab_gap.csv"))
  if (!is.null(d)) {
    d$k <- suppressWarnings(as.integer(gsub(".*year::(-?[0-9]+).*", "\\1", d$term)))
    d <- d[!is.na(d$k), ]
    kk <- sort(unique(d$k)); kk <- kk[kk >= -10 & kk <= 8]
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Stimatore di Sun e Abraham applicato al divario di composizione}",
      "\\label{tab:sunab}", "\\begin{threeparttable}",
      "\\begin{tabular}{rcccc}", "\\toprule",
      "Anni dall' & \\multicolumn{2}{c}{Divario verdi $-$ neutri} & \\multicolumn{2}{c}{Divario sporchi $-$ neutri} \\\\",
      "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
      "entrata & Coefficiente & $p$-value & Coefficiente & $p$-value \\\\", "\\midrule")
    for (k in kk) {
      g <- d[d$k == k & d$outcome == "gap_green", , drop = FALSE]
      b <- d[d$k == k & d$outcome == "gap_dirty", , drop = FALSE]
      L <- c(L, sprintf("%s & %s & %s & %s & %s \\\\",
        ifelse(k >= 0, paste0("+", k), as.character(k)),
        if (nrow(g)) cst(g$coef[1], g$pval[1], 4) else "", if (nrow(g)) fmt_p(g$pval[1]) else "",
        if (nrow(b)) cst(b$coef[1], b$pval[1], 4) else "", if (nrow(b)) fmt_p(b$pval[1]) else ""))
    }
    L <- c(L, "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Variante baseline. Finestra mostrata: da $-10$ a $+8$ anni; il file completo copre un intervallo pi\\`u ampio.",
      "\\item \\textbf{Perch\\'e serve.} Quando gli accordi entrano in vigore in anni diversi (qui: dal 2002 al 2015), lo stimatore tradizionale pu\\`o confondere l'effetto vero con il confronto fra chi \\`e stato trattato prima e chi dopo. Sun e Abraham (2021) propongono una correzione che calcola l'effetto separatamente per ogni gruppo di ingresso e poi lo aggrega.",
      "\\item \\textbf{Come si legge.} La variabile dipendente qui non \\`e l'export ma il \\emph{divario}: media dei prodotti verdi meno media dei neutri, nella stessa destinazione e anno. Cos\\`i il disegno diventa un confronto scaglionato ordinario, a cui il metodo si applica direttamente.",
      "\\item Un coefficiente significativo \\emph{prima} dell'anno zero \\`e un segnale di allarme, non un risultato: va discusso apertamente.",
      paste0("\\item ", NOTE_STARS),
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_09_sunab.tex")
  }
}

########################################################################
## T10 — Gruppi di controllo alternativi
########################################################################
{
  grp_lab <- c(prodHS4 = "Solo prodotti della stessa famiglia merceologica",
               deepshallow = "Solo partner con accordo: profondi vs superficiali",
               cem_v1 = "Solo destinazioni appaiate per caratteristiche")
  d0 <- rd(file.path(DIR_T, "tripledd_stability.csv"))
  if (!is.null(d0)) {
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Gruppi di controllo alternativi: il risultato cambia se cambia il termine di paragone?}",
      "\\label{tab:stability}", "\\begin{threeparttable}",
      "\\begin{tabular}{lcccc}", "\\toprule",
      " & \\multicolumn{2}{c}{Indice WB} & \\multicolumn{2}{c}{Indice TREND} \\\\",
      "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
      "Gruppo di controllo & $\\times$ Verde & $\\times$ Sporco & $\\times$ Verde & $\\times$ Sporco \\\\", "\\midrule")
    for (g in names(grp_lab)) {
      s <- d0[d0$group == g, , drop = FALSE]
      if (!nrow(s)) next
      pick <- function(tr, tm) {
        r <- s[s$treat == tr & s$term == tm, , drop = FALSE]
        if (!nrow(r)) return(c("", "", ""))
        c(cst(r$coef[1], r$pval[1]), paste0("(", fmt(r$se[1]), ")"), paste0("[", fmt_p(r$pval[1]), "]"))
      }
      a <- pick("WB", "WB_EP_Depth:env_good"); b <- pick("WB", "WB_EP_Depth:dirty_p")
      c1 <- pick("TREND", "TREND_EP_Count:env_good"); d1 <- pick("TREND", "TREND_EP_Count:dirty_p")
      nb <- fmt_n(s$nobs[1])
      L <- c(L,
        paste0(grp_lab[[g]], " & ", a[1], " & ", b[1], " & ", c1[1], " & ", d1[1], " \\\\"),
        paste0(" & ", a[2], " & ", b[2], " & ", c1[2], " & ", d1[2], " \\\\"),
        paste0(" & ", a[3], " & ", b[3], " & ", c1[3], " & ", d1[3], " \\\\"),
        paste0("\\quad \\textit{osservazioni: ", nb, "} & & & & \\\\"), "\\addlinespace")
    }
    L <- c(L, "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Variante baseline, dati a livello di transazione, effetti fissi come nella Tabella \\ref{tab:main-full}; errori raggruppati per destinazione.",
      "\\item \\textbf{A cosa serve.} Nella specificazione principale i prodotti verdi sono confrontati con \\emph{tutti} gli altri. Qui il paragone viene ristretto in tre modi diversi e sempre pi\\`u severi. Se il risultato dipendesse dalla scelta del termine di paragone, cambierebbe passando da una riga all'altra.",
      "\\item \\textit{Stessa famiglia merceologica}: i non-verdi ammessi come confronto sono solo quelli che condividono le prime quattro cifre del codice doganale con un prodotto verde.",
      "\\item \\textit{Profondi vs superficiali}: si tengono solo i partner che hanno gi\\`a un accordo, e si confrontano quelli con contenuto ambientale sopra la mediana con quelli sotto. Cos\\`i sparisce del tutto il confronto fra chi ha un accordo e chi non ce l'ha.",
      "\\item \\textit{Destinazioni appaiate}: si tengono solo i paesi di controllo simili ai trattati per reddito, crescita e livello dei dazi nel 2000 (\\emph{coarsened exact matching}).",
      paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_10_stability.tex")
  }
}

########################################################################
## T11 — Robustezza sul full panel (varianti di campione e controlli)
########################################################################
{
  d <- rd(file.path(DIR_T, "tripledd_robustness_reghdfe.csv"))
  if (!is.null(d)) {
    mod_lab <- c(
      A_WB_controls      = "Con controlli aggiuntivi (dazi, concentrazione, antidumping)",
      B_WB_noASEAN       = "Escludendo le 11 destinazioni ASEAN",
      C_WB_inclHKMO      = "Reinserendo Hong Kong e Macao",
      D_WB_overlap       = "Solo prodotti con supporto comune",
      D_TREND_overlap    = "Solo prodotti con supporto comune (indice TREND)",
      E_TREND_deepshallow= "Solo partner con accordo, profondi vs superficiali (TREND)",
      G_WB_withinfirm    = "Quota verde nel paniere dell'impresa (indice WB)",
      G_TREND_withinfirm = "Quota verde nel paniere dell'impresa (TREND)")
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Robustezza sul \\emph{full panel}: varianti di campione, controlli e variabile dipendente}",
      "\\label{tab:robust-full}", "\\begin{threeparttable}",
      "\\begin{tabular}{lccrc}", "\\toprule",
      "Variante & $\\times$ Verde & $\\times$ Sporco & Osservazioni & $R^2$ \\\\", "\\midrule")
    for (m in names(mod_lab)) {
      s <- d[d$model == m, , drop = FALSE]
      if (!nrow(s)) next
      gvar <- intersect(c("wb_green","tr_green"), s$var)
      bvar <- intersect(c("wb_dirty","tr_dirty"), s$var)
      if (length(gvar)) {
        rg <- s[s$var == gvar[1], ]; rb <- s[s$var == bvar[1], ]
        L <- c(L,
          sprintf("%s & %s & %s & %s & %s \\\\", mod_lab[[m]],
                  cst(rg$coef[1], rg$pval[1]), cst(rb$coef[1], rb$pval[1]),
                  fmt_n(rg$N[1]), fmt(rg$r2[1], 4)),
          sprintf(" & (%s) & (%s) & & \\\\", fmt(rg$stderr[1]), fmt(rb$stderr[1])),
          sprintf(" & [%s] & [%s] & & \\\\", fmt_p(rg$pval[1]), fmt_p(rb$pval[1])), "\\addlinespace")
      } else {
        ## modelli G: variabile dipendente = quota verde, un solo regressore
        rv <- s[s$var %in% c("WB_EP_Depth","TREND_EP_Count"), , drop = FALSE]
        if (nrow(rv)) L <- c(L,
          sprintf("%s & \\multicolumn{2}{c}{%s} & %s & %s \\\\", mod_lab[[m]],
                  cst(rv$coef[1], rv$pval[1]), fmt_n(rv$N[1]), fmt(rv$r2[1], 4)),
          sprintf(" & \\multicolumn{2}{c}{(%s)} & & \\\\", fmt(rv$stderr[1])),
          sprintf(" & \\multicolumn{2}{c}{[%s]} & & \\\\", fmt_p(rv$pval[1])), "\\addlinespace")
      }
    }
    L <- c(L, "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Variante baseline (escl.\\ HK/Macao, controllo TotalDepth). Stima \\texttt{reghdfe}; effetti fissi e raggruppamento come nella Tabella \\ref{tab:main-full}, salvo dove indicato.",
      "\\item \\textit{Supporto comune}: si tengono solo i prodotti che vengono esportati sia verso destinazioni con accordo sia verso destinazioni senza. Per gli altri non esiste un termine di paragone osservato.",
      "\\item \\textit{Quota verde nel paniere}: qui la variabile dipendente cambia. Non \\`e pi\\`u l'export, ma la frazione di fatturato che l'impresa realizza in prodotti verdi verso quella destinazione. Effetti fissi impresa$\\times$destinazione e anno. Risponde a una domanda diversa: l'impresa \\emph{ricompone} il proprio paniere?",
      paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_11_robustness_full.tex")
  }
}

########################################################################
## T12 — Trend specifici per destinazione + test sui pre-trend
########################################################################
{
  gtr <- function(sfx, tr) {
    d <- rd(file.path(DIR_T, paste0("r79_desttrends", sfx, ".csv")))
    if (is.null(d)) return(NULL)
    key <- if (tr == "WB") "WB_EP_Depth" else "TREND_EP_Count"
    s <- d[d$treat == key, , drop = FALSE]; if (!nrow(s)) NULL else s
  }
  gpre <- function(sfx, tr) {
    d <- rd(file.path(DIR_T, paste0("r79c_pretrends", sfx, ".csv")))
    if (is.null(d)) return(NULL)
    s <- d[d$treat == tr, , drop = FALSE]; if (!nrow(s)) NULL else s
  }
  rl <- function(fun, tr, term, field) {
    paste(sapply(VAR, function(v) {
      s <- fun(v$sfx, tr); if (is.null(s)) return("")
      r <- s[s$term == term, , drop = FALSE]; if (!nrow(r)) return("")
      switch(field, b = cst(r$coef[1], if ("pval" %in% names(r)) r$pval[1] else r$p_asy[1]),
             se = paste0("(", fmt(if ("se" %in% names(r)) r$se[1] else r$se_asy[1]), ")"),
             p  = paste0("[", fmt_p(if ("pval" %in% names(r)) r$pval[1] else r$p_asy[1]), "]"),
             pw = fmt_p(r$p_wcb[1]))
    }), collapse = " & ")
  }
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Andamenti specifici per destinazione e verifica dei pre-trend}",
    "\\label{tab:trends}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello A --- stima con andamenti lineari destinazione$\\times$tipo di prodotto (indice WB)}} \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Verde & ", rl(gtr, "WB", "WB_EP_Depth:env_good", "b"), " \\\\"),
    paste0(" & ", rl(gtr, "WB", "WB_EP_Depth:env_good", "se"), " \\\\"),
    paste0(" & ", rl(gtr, "WB", "WB_EP_Depth:env_good", "p"), " \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Sporco & ", rl(gtr, "WB", "WB_EP_Depth:dirty_p", "b"), " \\\\"),
    paste0(" & ", rl(gtr, "WB", "WB_EP_Depth:dirty_p", "se"), " \\\\"),
    paste0(" & ", rl(gtr, "WB", "WB_EP_Depth:dirty_p", "p"), " \\\\"), "\\addlinespace",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello B --- test formale sui pre-trend (indice WB)}} \\\\"), "\\addlinespace",
    paste0("Pendenza pre-accordo, Verde & ", rl(gpre, "WB", "ep_green", "b"), " \\\\"),
    paste0(" & ", rl(gpre, "WB", "ep_green", "se"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rl(gpre, "WB", "ep_green", "pw"), " \\\\"), "\\addlinespace",
    paste0("Pendenza pre-accordo, Sporco & ", rl(gpre, "WB", "ep_dirty", "b"), " \\\\"),
    paste0(" & ", rl(gpre, "WB", "ep_dirty", "se"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rl(gpre, "WB", "ep_dirty", "pw"), " \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item \\textbf{Pannello A --- a quale obiezione risponde.} Gli effetti fissi del disegno principale non catturano una cosa: uno shock che riguardi \\emph{i prodotti verdi in una specifica destinazione} e che cresca nel tempo. \\`E il caso in cui un paese firma clausole ambientali proprio mentre la sua domanda di beni verdi sta salendo. Qui si aggiunge un andamento lineare per ogni destinazione e per ogni tipo di prodotto, che assorbe esattamente quella dinamica.",
    "\\item \\textbf{Pannello B --- come si legge.} Si stima la pendenza del divario di composizione nel periodo \\emph{precedente} all'accordo. Se fosse diversa da zero, verdi e neutri divergevano gi\\`a prima e il confronto sarebbe viziato. I $p$-value riportati sono quelli bootstrap, i pi\\`u prudenti.",
    paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_12_desttrends.tex")
}

########################################################################
## T13 — Sotto-indici: quali disposizioni avrebbero un meccanismo?
########################################################################
{
  d <- rd(file.path(DIR_T, "subindices_collapsed.csv"))
  if (!is.null(d)) {
    sub_lab <- c(
      WB_GreenLiberalization  = "Liberalizzazione dei beni verdi (WB)",
      TREND_GreenMarketAccess = "Accesso al mercato per i beni verdi (TREND)",
      WB_EnforcementDSM       = "Meccanismo di risoluzione controversie (WB)",
      TREND_EnforcementDSM    = "Meccanismo di risoluzione controversie (TREND)",
      TREND_Hard              = "Disposizioni vincolanti (TREND)",
      TREND_Soft              = "Disposizioni di sola cooperazione (TREND)",
      TREND_RegulatorySpace   = "Clausole di spazio regolatorio (TREND)")
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Scomposizione per tipo di disposizione: quali clausole avrebbero un meccanismo commerciale?}",
      "\\label{tab:subindices}", "\\begin{threeparttable}",
      "\\begin{tabular}{lcc}", "\\toprule",
      "Sotto-indice & $\\times$ Verde & $\\times$ Sporco \\\\", "\\midrule")
    for (s in names(sub_lab)) {
      r <- d[d$sub_index == s, , drop = FALSE]
      if (!nrow(r)) next
      g <- r[r$term == "SUB:env_good", , drop = FALSE]
      b <- r[r$term == "SUB:dirty_p", , drop = FALSE]
      L <- c(L,
        sprintf("%s & %s & %s \\\\", sub_lab[[s]],
                if (nrow(g)) cst(g$coef[1], g$pval[1], 4) else "",
                if (nrow(b)) cst(b$coef[1], b$pval[1], 4) else ""),
        sprintf(" & (%s) & (%s) \\\\", if (nrow(g)) fmt(g$se[1], 4) else "", if (nrow(b)) fmt(b$se[1], 4) else ""),
        sprintf(" & [%s] & [%s] \\\\", if (nrow(g)) fmt_p(g$pval[1]) else "", if (nrow(b)) fmt_p(b$pval[1]) else ""),
        "\\addlinespace")
    }
    nb <- fmt_n(d$nobs[1])
    L <- c(L, "\\midrule", paste0("Celle (tutte le righe) & \\multicolumn{2}{c}{", nb, "} \\\\"),
      "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Variante baseline, pannello collassato. Ogni riga \\`e una stima separata in cui l'indice complessivo \\`e sostituito dal sotto-indice indicato; il controllo di profondit\\`a resta incluso.",
      "\\item \\textbf{L'obiezione a cui risponde.} Un indice che somma tutte le disposizioni mescola clausole che potrebbero davvero spostare il commercio (abbassare i dazi sui beni verdi, imporre standard vincolanti) con dichiarazioni di intenti che non hanno alcun meccanismo. Se l'effetto esiste, dovrebbe concentrarsi nelle prime.",
      "\\item \\textbf{Il limite, che \\`e esso stesso un risultato.} Negli accordi cinesi del periodo le disposizioni dotate di un meccanismo commerciale sono rarissime: i due sotto-indici della Banca Mondiale che ne hanno uno sono diversi da zero in soli tre anni-paese e risultano perfettamente sovrapposti fra loro. Non \\`e quindi possibile distinguerne gli effetti: non per un difetto del metodo, ma perch\\'e quel contenuto negli accordi quasi non c'\\`e.",
      "\\item Le righe \\emph{sola cooperazione} e \\emph{spazio regolatorio} funzionano da controllo negativo: sono clausole senza meccanismo commerciale, e l\\`i non dovrebbe emergere nulla.",
      paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_13_subindices.tex")
  }
}

########################################################################
## T14 — Margine estensivo (PPML con zeri)
########################################################################
{
  gp <- function(sfx, tr) {
    d <- rd(file.path(DIR_T, paste0("ppml_extensive", sfx, ".csv")))
    if (is.null(d)) return(NULL)
    s <- d[d$treat == tr, , drop = FALSE]; if (!nrow(s)) NULL else s
  }
  rlp <- function(tr, term, field) {
    paste(sapply(VAR, function(v) {
      s <- gp(v$sfx, tr); if (is.null(s)) return("")
      r <- s[s$term == term, , drop = FALSE]; if (!nrow(r)) return("")
      switch(field, b = cst(r$coef[1], r$pval[1], 4),
             se = paste0("(", fmt(r$se[1], 4), ")"), p = paste0("[", fmt_p(r$pval[1]), "]"))
    }), collapse = " & ")
  }
  nobs <- sapply(VAR, function(v) { s <- gp(v$sfx, "WB"); if (is.null(s)) "" else fmt_n(s$nobs[1]) })
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Margine estensivo: nascono flussi commerciali che prima non c'erano?}",
    "\\label{tab:ppml}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello A --- indice WB}} \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Verde & ", rlp("WB", "WB_EP_Depth:env_good", "b"), " \\\\"),
    paste0(" & ", rlp("WB", "WB_EP_Depth:env_good", "se"), " \\\\"),
    paste0(" & ", rlp("WB", "WB_EP_Depth:env_good", "p"), " \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Sporco & ", rlp("WB", "WB_EP_Depth:dirty_p", "b"), " \\\\"),
    paste0(" & ", rlp("WB", "WB_EP_Depth:dirty_p", "se"), " \\\\"),
    paste0(" & ", rlp("WB", "WB_EP_Depth:dirty_p", "p"), " \\\\"), "\\addlinespace",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello B --- indice TREND}} \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Verde & ", rlp("TREND", "TREND_EP_Count:env_good", "b"), " \\\\"),
    paste0(" & ", rlp("TREND", "TREND_EP_Count:env_good", "se"), " \\\\"),
    paste0(" & ", rlp("TREND", "TREND_EP_Count:env_good", "p"), " \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Sporco & ", rlp("TREND", "TREND_EP_Count:dirty_p", "b"), " \\\\"),
    paste0(" & ", rlp("TREND", "TREND_EP_Count:dirty_p", "se"), " \\\\"),
    paste0(" & ", rlp("TREND", "TREND_EP_Count:dirty_p", "p"), " \\\\"),
    "\\midrule",
    paste0("Celle (con zeri) & ", paste(nobs, collapse = " & "), " \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item Stima Poisson pseudo-massima verosimiglianza (PPML) su una griglia prodotto--destinazione--anno completata con gli zeri; effetti fissi prod.$\\times$dest., dest.$\\times$anno, prod.$\\times$anno; errori raggruppati per destinazione.",
    "\\item \\textbf{Perch\\'e serve.} Le stime in logaritmo usano solo i flussi gi\\`a esistenti: se una clausola ambientale facesse \\emph{nascere} esportazioni verdi verso un mercato dove prima non ce n'erano, quelle stime non lo vedrebbero. Il PPML tiene dentro anche gli zeri e quindi coglie questo margine.",
    "\\item I coefficienti PPML si leggono come variazioni percentuali approssimate.",
    paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_14_ppml.tex")
}

########################################################################
## T15 — Intensita' di CO2 come misura continua di "sporco"
########################################################################
{
  gco <- function(sfx, tr) {
    d <- rd(file.path(DIR_T, paste0("r711_shapiro_intensity", sfx, ".csv")))
    if (is.null(d)) return(NULL)
    s <- d[d$treat == tr, , drop = FALSE]; if (!nrow(s)) NULL else s
  }
  rlc <- function(tr, term, field) {
    paste(sapply(VAR, function(v) {
      s <- gco(v$sfx, tr); if (is.null(s)) return("")
      r <- s[s$term == term, , drop = FALSE]; if (!nrow(r)) return("")
      switch(field, b = cst(r$coef[1], r$p_asy[1]),
             se = paste0("(", fmt(r$se_asy[1]), ")"),
             p  = paste0("[", fmt_p(r$p_asy[1]), "]"),
             pw = fmt_p(r$p_wcb[1]))
    }), collapse = " & ")
  }
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Misura continua di intensit\\`a inquinante al posto della classificazione s\\`i/no}",
    "\\label{tab:co2}", "\\begin{threeparttable}",
    paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
    paste0(" & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello A --- indice WB}} \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Verde & ", rlc("WB", "ep_green", "b"), " \\\\"),
    paste0(" & ", rlc("WB", "ep_green", "se"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rlc("WB", "ep_green", "pw"), " \\\\"), "\\addlinespace",
    paste0("Profondit\\`a EP $\\times$ Intensit\\`a CO$_2$ & ", rlc("WB", "ep_co2", "b"), " \\\\"),
    paste0(" & ", rlc("WB", "ep_co2", "se"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rlc("WB", "ep_co2", "pw"), " \\\\"), "\\addlinespace",
    paste0("\\multicolumn{", NVAR+1, "}{l}{\\textit{Pannello B --- indice TREND}} \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Verde & ", rlc("TREND", "ep_green", "b"), " \\\\"),
    paste0(" & ", rlc("TREND", "ep_green", "se"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rlc("TREND", "ep_green", "pw"), " \\\\"), "\\addlinespace",
    paste0("Conteggio EP $\\times$ Intensit\\`a CO$_2$ & ", rlc("TREND", "ep_co2", "b"), " \\\\"),
    paste0(" & ", rlc("TREND", "ep_co2", "se"), " \\\\"),
    paste0("\\quad $p$-value bootstrap & ", rlc("TREND", "ep_co2", "pw"), " \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
    "\\item \\textbf{L'obiezione a cui risponde.} Classificare un prodotto come ``sporco'' s\\`i o no \\`e grossolano: l'acciaio e la carta non inquinano allo stesso modo. Qui la variabile binaria \\`e sostituita da una misura continua di intensit\\`a di CO$_2$ per settore, cos\\`i i prodotti si ordinano lungo una scala invece che in due caselle.",
    "\\item Un coefficiente negativo significherebbe che, dove il contenuto ambientale \\`e pi\\`u profondo, le esportazioni calano di pi\\`u per i prodotti pi\\`u inquinanti.",
    "\\item Si riporta anche il $p$-value bootstrap perch\\'e, come altrove, quello ordinario \\`e troppo generoso con cos\\`i pochi gruppi trattati.",
    paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_15_co2.tex")
}

########################################################################
## T16 — Leave-one-out: il risultato dipende da un solo paese?
########################################################################
{
  nm <- rd(file.path(DIR_G, "B_treatment_entry.csv"))
  cname <- if (is.null(nm)) NULL else setNames(nm$country, as.character(nm$country_code))
  dats <- lapply(VAR, function(v) rd(file.path(DIR_T, paste0("dirty_leaveoneout", v$sfx, ".csv"))))
  base <- dats[[1]]
  if (!is.null(base)) {
    ## unione delle specifiche su tutte le varianti: le colonne che includono
    ## Hong Kong e Macao hanno due righe in piu' (senza_110, senza_121), che
    ## altrimenti resterebbero invisibili
    specs <- unique(unlist(lapply(dats, function(d)
      if (is.null(d)) character(0) else d$spec[grepl("^senza_", d$spec)])))
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Prova di esclusione: che cosa resta del risultato togliendo un paese alla volta}",
      "\\label{tab:loo}", "\\begin{threeparttable}",
      paste0("\\begin{tabular}{l", strrep("c", NVAR), "}"), "\\toprule",
      paste0("Paese escluso & ", paste(sapply(VAR, function(v) v$lab), collapse = " & "), " \\\\"), "\\midrule")
    ## riga di riferimento
    rowfor <- function(sp) {
      paste(sapply(dats, function(d) {
        if (is.null(d)) return("")
        r <- d[d$spec == sp, , drop = FALSE]; if (!nrow(r)) return("")
        cst(r$coef[1], r$pval[1], 4)
      }), collapse = " & ")
    }
    L <- c(L, paste0("\\textit{Nessuno (riferimento)} & ", rowfor("lista_estesa"), " \\\\"), "\\midrule")
    for (sp in specs) {
      cc <- sub("^senza_", "", sp)
      nmx <- if (!is.null(cname) && cc %in% names(cname)) cname[[cc]] else cc
      L <- c(L, sprintf("%s & %s \\\\", esc(nmx), rowfor(sp)))
    }
    L <- c(L, "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      paste0("\\item Colonne: (1) ", VAR[[1]]$desc, "; (2) ", VAR[[2]]$desc, "; (3) ", VAR[[3]]$desc, "; (4) ", VAR[[4]]$desc, "."),
      "\\item Coefficiente dell'interazione fra profondit\\`a ambientale (WB) e prodotti sporchi, pannello collassato, ristimato escludendo ogni volta una destinazione trattata.",
      "\\item \\textbf{A cosa serve.} Con pochi accordi, un singolo partner molto grande pu\\`o generare da solo tutto il risultato. Se il coefficiente crolla togliendo un paese, la conclusione poggia su quel paese e non sul fenomeno generale.",
      "\\item \\textbf{Come si legge.} Le stelle indicano la significativit\\`a in ciascuna riga: righe che perdono le stelle segnalano i paesi da cui il risultato dipende di pi\\`u.",
      "\\item Le righe di Hong Kong e Macao sono vuote nelle colonne (1) e (3) perch\\'e in quelle varianti i due territori sono gi\\`a esclusi dal campione: non si possono togliere due volte.",
      "\\item Gli errori standard e il numero di osservazioni non sono presenti nei file esportati per questa prova: \\`e una lacuna nota degli export, non un dato mancante.",
      paste0("\\item ", NOTE_STARS),
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_16_leaveoneout.tex")
  }
}

########################################################################
## T17 — Come si controlla la profondita' dell'accordo
########################################################################
{
  nod <- rd(file.path(DIR_T, "tripledd_collapsed_nodepth.csv"))
  tgt <- rd(file.path(DIR_T, "tripledd_collapsed_targeted.csv"))
  eps <- rd(file.path(DIR_T, "tripledd_epshare_treatedonly.csv"))
  bas <- rd(file.path(DIR_T, "tripledd_collapsed.csv"))
  cell3 <- function(d, tr, tm) {
    if (is.null(d)) return(c("", "", ""))
    s <- if ("treat" %in% names(d)) d[d$treat == tr & d$term == tm, , drop = FALSE] else d[d$term == tm, , drop = FALSE]
    if (!nrow(s)) return(c("", "", ""))
    c(cst(s$coef[1], s$pval[1]), paste0("(", fmt(s$se[1]), ")"), paste0("[", fmt_p(s$pval[1]), "]"))
  }
  b1 <- cell3(bas, "WB", "WB_EP_Depth:env_good"); b2 <- cell3(bas, "WB", "WB_EP_Depth:dirty_p")
  n1 <- cell3(nod, "WB", "WB_EP_Depth:env_good"); n2 <- cell3(nod, "WB", "WB_EP_Depth:dirty_p")
  t1 <- cell3(tgt, "WB", "WB_EP_Depth:env_good"); t2 <- cell3(tgt, "WB", "WB_EP_Depth:dirty_p")
  e1 <- cell3(eps, NA,   "EP_share:env_good");    e2 <- cell3(eps, NA,   "EP_share:dirty_p")
  L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
    "\\caption{Quanto conta il modo in cui si controlla la profondit\\`a complessiva dell'accordo}",
    "\\label{tab:depthctrl}", "\\begin{threeparttable}",
    "\\begin{tabular}{lcc}", "\\toprule",
    "Specificazione & $\\times$ Verde & $\\times$ Sporco \\\\", "\\midrule",
    paste0("(a) Baseline: controllo TotalDepth & ", b1[1], " & ", b2[1], " \\\\"),
    paste0(" & ", b1[2], " & ", b2[2], " \\\\"),
    paste0(" & ", b1[3], " & ", b2[3], " \\\\"), "\\addlinespace",
    paste0("(b) Senza alcun controllo di profondit\\`a & ", n1[1], " & ", n2[1], " \\\\"),
    paste0(" & ", n1[2], " & ", n2[2], " \\\\"),
    paste0(" & ", n1[3], " & ", n2[3], " \\\\"), "\\addlinespace",
    paste0("(c) Controllo mirato alle sole aree rilevanti & ", t1[1], " & ", t2[1], " \\\\"),
    paste0(" & ", t1[2], " & ", t2[2], " \\\\"),
    paste0(" & ", t1[3], " & ", t2[3], " \\\\"), "\\addlinespace",
    paste0("(d) Quota ambientale sul totale, solo trattati & ", e1[1], " & ", e2[1], " \\\\"),
    paste0(" & ", e1[2], " & ", e2[2], " \\\\"),
    paste0(" & ", e1[3], " & ", e2[3], " \\\\"),
    "\\bottomrule", "\\end{tabular}",
    "\\begin{tablenotes}[flushleft]\\footnotesize",
    "\\item Indice WB, variante baseline, pannello collassato.",
    "\\item \\textbf{Il problema.} Gli accordi con molte clausole ambientali sono spesso accordi profondi \\emph{in tutto}. Se non si tiene conto della profondit\\`a complessiva, si rischia di attribuire al contenuto ambientale un effetto che appartiene all'accordo nel suo insieme. Ma il controllo \\`e a sua volta molto correlato con la variabile di interesse (0.86), quindi ne assorbe parte della variazione.",
    "\\item Le righe mostrano quanto il risultato dipenda da questa scelta: (b) toglie del tutto il controllo, (c) lo restringe alle sole aree dell'accordo che potrebbero plausibilmente interagire con la composizione, (d) cambia impostazione e misura la \\emph{quota} ambientale sul totale, stimata solo fra i partner che hanno gi\\`a un accordo.",
    "\\item In (d) i coefficienti sono su una scala diversa (la quota varia fra 0 e 1) e non sono confrontabili in valore assoluto con le altre righe.",
    paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
    "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
  wr(L, "tab_17_depthcontrols.tex")
}

########################################################################
## T18 — Definizione alternativa di "bene verde" (lista APEC)
########################################################################
{
  d <- rd(file.path(DIR_T, "tripledd_collapsed_apecgreen.csv"))
  bas <- rd(file.path(DIR_T, "tripledd_collapsed.csv"))
  if (!is.null(d)) {
    cc <- function(dd, tr, tm) {
      s <- dd[dd$treat == tr & dd$term == tm, , drop = FALSE]
      if (!nrow(s)) return(c("", "", ""))
      c(cst(s$coef[1], s$pval[1]), paste0("(", fmt(s$se[1]), ")"), paste0("[", fmt_p(s$pval[1]), "]"))
    }
    a1 <- cc(bas, "WB", "WB_EP_Depth:env_good"); a2 <- cc(d, "WB", "WB_EP_Depth:apec_green")
    b1 <- cc(bas, "TREND", "TREND_EP_Count:env_good"); b2 <- cc(d, "TREND", "TREND_EP_Count:apec_green")
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Definizione alternativa di bene ambientale: lista OCSE contro lista APEC}",
      "\\label{tab:apec}", "\\begin{threeparttable}",
      "\\begin{tabular}{lcc}", "\\toprule",
      " & Lista OCSE & Lista APEC \\\\",
      " & (usata nel lavoro) & (54 codici) \\\\", "\\midrule",
      paste0("Profondit\\`a EP (WB) $\\times$ Verde & ", a1[1], " & ", a2[1], " \\\\"),
      paste0(" & ", a1[2], " & ", a2[2], " \\\\"),
      paste0(" & ", a1[3], " & ", a2[3], " \\\\"), "\\addlinespace",
      paste0("Conteggio EP (TREND) $\\times$ Verde & ", b1[1], " & ", b2[1], " \\\\"),
      paste0(" & ", b1[2], " & ", b2[2], " \\\\"),
      paste0(" & ", b1[3], " & ", b2[3], " \\\\"),
      "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Variante baseline, pannello collassato.",
      "\\item \\textbf{A cosa serve.} Non esiste una definizione unica di ``bene ambientale''. Il lavoro usa la lista OCSE; qui la si sostituisce con la lista APEC, molto pi\\`u ristretta e concordata politicamente nel 2012. Se il risultato dipendesse da quali prodotti chiamiamo verdi, le due colonne divergerebbero.",
      paste0("\\item ", NOTE_SE, " ", NOTE_STARS),
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_18_apec.tex")
  }
}

########################################################################
## T19 — Quanto grande dovrebbe essere un effetto per essere visto (MDE)
########################################################################
{
  f <- file.path(DIR_G, "33_mde_equivalence.md")
  if (file.exists(f)) {
    ln <- readLines(f, warn = FALSE)
    rows <- ln[grepl("^\\| *(WB|TREND) *\\| *(green|dirty)", ln)]
    L <- c("\\begin{table}[htbp]", "\\centering", "\\footnotesize",
      "\\caption{Quanto dovrebbe essere grande un effetto perch\\'e questo disegno riuscisse a vederlo}",
      "\\label{tab:mde}", "\\begin{threeparttable}",
      "\\begin{tabular}{llcccc}", "\\toprule",
      "Indice & Margine & Errore std. & Effetto minimo & Effetto minimo & Intervallo di confidenza \\\\",
      " & & asintotico & rilevabile (1 dev.\\ std.) & rilevabile, bootstrap & bootstrap (per unit\\`a) \\\\", "\\midrule")
    for (r in rows) {
      p <- trimws(strsplit(r, "|", fixed = TRUE)[[1]])
      p <- p[nzchar(p)]
      if (length(p) < 9) next
      mg <- if (p[2] == "green") "Verde" else "Sporco"
      ## il simbolo % va sempre protetto: in LaTeX aprirebbe un commento
      pc <- function(x) gsub("%", "\\\\%", x)
      L <- c(L, sprintf("%s & %s & %s & %s & %s & %s \\\\",
                        p[1], mg, p[3], pc(p[7]), pc(p[8]), pc(p[9])))
    }
    L <- c(L, "\\bottomrule", "\\end{tabular}",
      "\\begin{tablenotes}[flushleft]\\footnotesize",
      "\\item Campione di stima: pannello collassato, variante escl.\\ HK/Macao. Deviazione standard dei regressori calcolata sul campione effettivo e pesata.",
      "\\item \\textbf{Perch\\'e questa tabella \\`e importante.} Un risultato nullo pu\\`o significare due cose molto diverse: che l'effetto non c'\\`e, oppure che c'\\`e ma \\`e troppo piccolo perch\\'e i dati lo distinguano dal rumore. Questa tabella dice quale delle due.",
      "\\item \\textbf{Come si legge.} L'\\emph{effetto minimo rilevabile} \\`e la dimensione al di sotto della quale il disegno non \\`e in grado di dire nulla. Sul margine verde, l'intervallo bootstrap esclude effetti superiori a circa il 3\\% per disposizione: sopra quella soglia possiamo dire che l'effetto non c'\\`e; sotto, il disegno semplicemente non discrimina.",
      "\\item Ne discende una formulazione pi\\`u corretta del risultato: non ``non troviamo alcun effetto'', ma ``possiamo escludere effetti superiori a questa soglia''.",
      "\\end{tablenotes}", "\\end{threeparttable}", "\\end{table}")
    wr(L, "tab_19_mde.tex")
  }
}

cat("\n=== Fatto. File generati in", OUT, "===\n")
