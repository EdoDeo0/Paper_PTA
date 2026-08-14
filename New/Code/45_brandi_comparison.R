########################################################################
###### 45 — Confronto riproducibile con Brandi et al. (2020)         ###
########################################################################

## Author: Edoardo Vitella (con Claude, Fase A del piano 2026-08-14 fase2)
##
## SCOPO
## -----
## §4.1 di draft_paper.tex confronta le nostre stime con l'effetto trovato
## da Brandi, Schwab, Berger & Morin (2020, World Development), finora
## fatto a mano nel testo. Questo script lo rende riproducibile: legge i
## due numeri pubblicati da Brandi et al. (input esterno, non calcolabile
## dalla nostra pipeline — vedi fonte sotto), li converte nella metrica di
## questo paper (log-punti sul margine composizione), e li confronta con
## i nostri CSV di stima gia' su disco. Stile coerente con
## 44_make_tables_tex.R: nessun numero nostro trascritto a mano.
##
## FONTE DEI NUMERI BRANDI (input esterno, non generabile dalla pipeline)
## ------------------------------------------------------------------------
## Brandi, C., Schwab, J., Berger, A., & Morin, J.-F. (2020). "Do
## Environmental Provisions in Trade Agreements Make Exports from
## Developing Countries Greener?" World Development, 129, 104899.
## https://doi.org/10.1016/j.worlddev.2020.104899
## Numeri verificati su `wiki/Brandi2020_EPsGreenExports.md` (paper card),
## sezione "Results":
##   - green: +0.4 punti percentuali per provisione LIBERALE, pari a +17%
##     della quota media di export green (mean green share)
##   - dirty: -0.72 punti percentuali per provisione TRADE-RESTRICTIVE,
##     pari a circa -5% della quota media di export dirty (mean = 14%)
## Se questi due numeri cambiassero (nuova lettura del paper, correzione),
## vanno aggiornati SOLO qui sotto — il resto dello script segue.
##
## Input:  New/Output/TripleDiff/Tables/tripledd_full_reghdfe.csv (asint., da Stata 17)
##         New/Output/OLS/Bootstrap/wcb_fullpanel.csv (WCB, da Stata 17b)
## Output: New/Paper/Tabelle/tab_20_brandi.tex

library(here)
library(data.table)

ROOT <- here()
OUT  <- file.path(ROOT, "New/Paper/Tabelle")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

## --- 1. Numeri Brandi (esterni, vedi fonte sopra) --------------------------
brandi_green_pct <-  0.17   # +17% della quota media green, per provisione liberale
brandi_dirty_pct <- -0.05   # -5% della quota media dirty, per provisione trade-restrictive

## Conversione nella metrica di questo paper: la variabile dipendente e'
## ln(export), quindi una variazione percentuale di quota si legge come
## variazione in log-punti via ln(1+x) (stessa trasformazione gia' usata
## nel testo, §4.1: "+17%" -> "≈+0.16 log points").
brandi_green_logpts <- log(1 + brandi_green_pct)
brandi_dirty_logpts <- log(1 + brandi_dirty_pct)

cat(sprintf("[Brandi] green: %.0f%% -> %.4f log-punti equivalenti\n",
            brandi_green_pct * 100, brandi_green_logpts))
cat(sprintf("[Brandi] dirty: %.0f%% -> %.4f log-punti equivalenti\n",
            brandi_dirty_pct * 100, brandi_dirty_logpts))

## --- 2. Nostre stime full panel, spec principale WB (asintotico + WCB) ----
tri <- fread(file.path(ROOT, "New/Output/TripleDiff/Tables/tripledd_full_reghdfe.csv"))
wcb <- fread(file.path(ROOT, "New/Output/OLS/Bootstrap/wcb_fullpanel.csv"))

wb_green_asym <- tri[var == "wb_green"]
wb_dirty_asym <- tri[var == "wb_dirty"]
wb_green_wcb  <- wcb[spec == "WB_green"]
wb_dirty_wcb  <- wcb[spec == "WB_dirty"]

stopifnot(nrow(wb_green_asym) == 1, nrow(wb_dirty_asym) == 1,
          nrow(wb_green_wcb) == 1, nrow(wb_dirty_wcb) == 1)

## --- 3. Confronto: quota dell'intervallo/punto rispetto all'equivalente Brandi
## Sul margine green il coefficiente non e' distinguibile da zero: il confronto
## informativo e' quanto dell'effetto Brandi l'estremo superiore dell'IC esclude.
frac_green_asym <- wb_green_asym$ci_upper / brandi_green_logpts
frac_green_wcb  <- wb_green_wcb$ci_high   / brandi_green_logpts

## Sul margine dirty si confronta anche la point estimate (piu' informativa
## di un IC che include lo zero), come gia' nel testo.
frac_dirty_point <- wb_dirty_asym$coef / brandi_dirty_logpts

cat(sprintf("\n[Confronto] margine green, estremo sup. IC asintotico / equiv. Brandi: %.3f (~1/%.0f)\n",
            frac_green_asym, 1 / frac_green_asym))
cat(sprintf("[Confronto] margine green, estremo sup. IC WCB / equiv. Brandi: %.3f (~1/%.0f)\n",
            frac_green_wcb, 1 / frac_green_wcb))
cat(sprintf("[Confronto] margine dirty, point estimate / equiv. Brandi: %.3f (~1/%.0f)\n",
            frac_dirty_point, 1 / frac_dirty_point))

## --- 4. Frammento LaTeX -----------------------------------------------------
fmt <- function(x, d = 4) formatC(as.numeric(x), format = "f", digits = d)

tex <- c(
"% Auto-generato da New/Code/45_brandi_comparison.R — non editare a mano.",
"% Numeri Brandi da wiki/Brandi2020_EPsGreenExports.md (Brandi et al. 2020, World Development).",
"\\begin{table}[htbp]",
"\\centering",
"\\caption{Benchmark against Brandi et al. (2020): equivalent effect and comparison with the confidence intervals}",
"\\label{tab:brandi}",
"\\small",
"\\begin{tabular}{lrrrr}",
"\\toprule",
"Margin & Brandi effect (log points) & Asy.\\ CI upper & WCB CI upper/coef. & Ratio \\\\",
"\\midrule",
sprintf("Green (liberal) & %s & %s & %s & 1/%.0f (WCB) \\\\",
        fmt(brandi_green_logpts), fmt(wb_green_asym$ci_upper), fmt(wb_green_wcb$ci_high),
        1 / frac_green_wcb),
sprintf("Dirty (restrictive) & %s & %s (point est.) & -- & 1/%.0f \\\\",
        fmt(brandi_dirty_logpts), fmt(wb_dirty_asym$coef), 1 / frac_dirty_point),
"\\bottomrule",
"\\end{tabular}",
"\\end{table}"
)
writeLines(tex, file.path(OUT, "tab_20_brandi.tex"))
cat("\n[OK]", file.path(OUT, "tab_20_brandi.tex"), "\n")
