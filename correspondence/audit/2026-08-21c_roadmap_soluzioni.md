# Roadmap soluzioni — audit 2026-08-21c (notturno)
**Scopo:** istruzioni autosufficienti, eseguibili da un altro modello senza indecisioni. Ogni item: file esatti, passi, criterio di accettazione. 🛑 = decisione dell'utente, fermarsi e chiedere.

**Regole vincolanti per chi esegue (ereditate dal progetto, non negoziabili):**
1. Stime R solo su Windows (macchina canonica); **ogni stima R nuova va confermata con Stata o cross-run prima di essere citata** (regola M8, `MISTAKES.md` 21/08).
2. Nessun `git commit`/`push` senza richiesta esplicita dell'utente.
3. Dopo ogni edit a `draft_paper.tex`: 2 passate `pdflatex`, 0 errori, 0 riferimenti irrisolti. Path pdflatex: `%LOCALAPPDATA%\Programs\MiKTeX\miktex\bin\x64\pdflatex.exe`, working dir `New/Paper/`.
4. Nessun numero nel testo senza CSV sorgente; mai patch manuali ai CSV; mai p asintotici in colonne `p_wcb`.
5. Prima di sovrascrivere un CSV che ha una colonna `source` che inizia con `reghdfe_stata` o `stata_fw_boottest`: fermarsi. Quei file sono ground truth.

**Ordine di esecuzione:** P1 → P2 (10 min, testo+PDF) → P3 (1–2 h, il critico) → P4 → P5 (30 min) → P6–P8 (igiene) → 🛑 P9.

---

## P1 · [WARNING W1] Correggere i numeri del trimming in `draft_paper.tex`

**Problema.** §"Outcome trimming" (~riga 1107) dice: *"The collapsed panel retains 3,605,798 cells (from 3,786,234; a loss of 4.8\%)"*. La base 3.786.234 non esiste (il panel intero ha 3.773.498 celle); il 4,8% somma trim e rimozione singleton; il 3.605.798 è il campione di stima post-singleton, non ciò che il trim conserva.

**Numeri veri (verificati da questo audit, 21/08 notte, su `panel_pdt_collapsed.fst`):**
- base: 3.773.498 celle; trim p1/p99 su y = [3,5851; 13,6360]; post-trim: 3.698.033 (−2,00%); campione di stima post-singleton: 3.605.798 (228 cluster; ≡ `stata_check_46_47_collapsed.csv` e `wcb_trimmed_collapsed.csv`).

**Azione.** In `New/Paper/draft_paper.tex`, sostituire la frase:

```latex
The collapsed panel retains 3,605,798 cells (from 3,786,234; a loss of 4.8\%);
the full panel retains 44.8 million observations.
```

con:

```latex
Trimming removes 2.0\% of collapsed-panel cells (3,773,498 to 3,698,033); after the
usual iterative singleton removal the estimation sample is 3,605,798 cells. The full
panel retains 44.8 million observations.
```

(Il resto della sottosezione non si tocca: tutti gli altri numeri sono ≡ CSV verificati.)

**Accettazione:** grep di `3,786,234` nel `.tex` = 0 occorrenze; grep di `3,698,033` = 1; i tre conteggi nel testo coincidono con la tabella qui sopra.

## P2 · [WARNING W2] Ricompilare il PDF

Dopo P1: 2 passate pdflatex su `draft_paper.tex` da `New/Paper/`. Il PDF attuale (21/08 10:36) non contiene le sottosezioni "Outcome trimming" e "Export value decomposition" (aggiunte al tex dopo, ultima modifica 19:50).

**Accettazione:** `draft_paper.pdf` con mtime > mtime del `.tex`; log con 0 errori e 0 `undefined references`; nel PDF compaiono entrambe le sottosezioni (cercare "Outcome trimming" nel testo estratto).

## P3 · [CRITICAL C1] Allineare la pipeline allo stato verificato e proteggere i CSV ground-truth

Tre sotto-item, tutti necessari.

**P3a — guardia anti-sovrascrittura in 46 e 47.** In testa a `New/Code/46_robustness_trim.R` (dopo il blocco setup, ~riga 45) e `New/Code/47_outcome_decomposition.R` (posizione analoga), aggiungere:

```r
## Guardia: non sovrascrivere CSV verificati cross-software (regola M8).
## Per rigenerarli davvero: FORCE_OVERWRITE_VERIFIED <- TRUE (e poi ri-arbitrare con Stata).
FORCE_OVERWRITE_VERIFIED <- FALSE
.protected <- c("tripledd_trimmed_collapsed.csv", "tripledd_decomp_collapsed.csv",
                "wcb_trimmed_collapsed.csv", "wcb_decomp_collapsed.csv",
                "wcb_trimmed_fullpanel.csv")
for (.f in file.path(OUT_DIR, .protected)) {
  if (file.exists(.f) && !FORCE_OVERWRITE_VERIFIED) {
    .src <- tryCatch(names(fread(.f))  , error = function(e) character())
    if ("source" %in% .src)
      stop(sprintf("%s ha colonna 'source' (verificato Stata). Questo script lo sovrascriverebbe con output R non verificato. Usare 49/50/48e, o FORCE_OVERWRITE_VERIFIED=TRUE.", basename(.f)))
  }
}
```

Nota per 47: `OUT_DIR` deve essere definito prima della guardia; se in 47 il path è costruito diversamente, adattare i path assoluti ma NON la logica. In 46 la lista rilevante sono i 3 file trim; in 47 i 2 decomp; va bene lasciare la lista completa in entrambi (il check `file.exists` filtra da sé).

**P3b — aggiornare `run_pipeline.R`.** In `New/Code/run_pipeline.R`:
1. Rimuovere `wcb_decomp_fullpanel.csv` dalla lista artefatti dello step 47 (riga ~357) — il file non esiste per scelta (mai verificato, vedi P5).
2. Nello stesso blocco, aggiungere un commento e la catena verificata:

```r
## NB (2026-08-21): i CSV trim/decomp citabili NON escono da 46/47 ma dalla catena
## verificata cross-software: 48_trim_export_dta.R -> stata/48_trim_check.do ->
## 48c_build_verified_csvs.R -> 49_wcb_trim_verified.R -> 50_wcb_decomp_verified.R;
## full panel: 48e_export_fullpanel_dta.R -> stata/48e_fullpanel_boottest.do.
## 46/47 restano come generatori dei dataset intermedi e dei CSV non-verified,
## e sono protetti da guardia anti-sovrascrittura (P3a).
```

3. Aggiungere step espliciti (stesso pattern degli step Stata già presenti: stampa comando, stop finché l'artefatto non compare) per: `48_trim_export_dta.R`, `stata/48_trim_check.do` (artefatto: `stata_check_46_47_collapsed.csv`, 24 righe), `48c_build_verified_csvs.R` (artefatti: i 2 tripledd verified con colonna source), `49_wcb_trim_verified.R` (artefatto: `wcb_trimmed_collapsed.csv` con nclust_pre), `50_wcb_decomp_verified.R` (artefatto: `wcb_decomp_collapsed.csv`, 8 righe), `48e_export_fullpanel_dta.R` + `stata/48e_fullpanel_boottest.do` (artefatti: `stata_check_trim_fullpanel.csv`, `wcb_trimmed_fullpanel.csv` con source).
4. Parse-check finale: `Rscript -e "invisible(parse('New/Code/run_pipeline.R'))"` → nessun errore.

**P3c — verifica.** Lanciare SOLO il parse-check di 46/47/run_pipeline (`Rscript -e "invisible(parse(...))"` per ciascuno). NON eseguire 46/47 per intero (nessuna rigenerazione richiesta: i CSV sono già verificati).

**Accettazione:** i 3 parse-check passano; eseguire `Rscript New/Code/46_robustness_trim.R` in un terminale di prova deve fermarsi subito con il messaggio della guardia (poi Ctrl-C se serve); `run_pipeline.R` non menziona più `wcb_decomp_fullpanel.csv`.

## P4 · [WARNING W3] Risolvere il conflitto sulle stime full-panel trimmate

**Problema.** `tripledd_trimmed_fullpanel.csv` (run R, senza `source`) ≠ Stata: WB green −0.005234 vs −0.005971, WB dirty −0.011562 vs −0.011698; nclust 236 (R, conteggio grezzo) vs 229 (Stata, post-singleton reghdfe). Causa più probabile: campione singleton diverso (feols tiene i singleton, reghdfe li droppa). Il paper cita i valori Stata ✓.

**Azione (nessuna stima nuova richiesta):** riscrivere il CSV dai valori Stata già esistenti. Script una-tantum (eseguire con `Rscript`, poi cancellare):

```r
library(data.table)
s <- fread("New/Output/TripleDiff/Tables/stata_check_trim_fullpanel.csv")
map <- c(wb_green="WB_EP_Depth:env_good", wb_dirty="WB_EP_Depth:dirty_p",
         tr_green="TREND_EP_Count:env_good", tr_dirty="TREND_EP_Count:dirty_p",
         td_green="env_good:TotalDepth_nonEnv", td_dirty="dirty_p:TotalDepth_nonEnv")
out <- s[, .(treat, var = map[var], coef, se, pval, nobs, nclust,
             source = "stata_fw_boottest_48e")]
stopifnot(nrow(out) == 8, !any(is.na(out$var)))
fwrite(out, "New/Output/TripleDiff/Tables/tripledd_trimmed_fullpanel.csv")
```

Attenzione: le righe td_* di TREND e WB in `stata_check_trim_fullpanel.csv` mappano sulla stessa etichetta var — va bene, il CSV finale distingue per colonna `treat` (come i gemelli collassati). Verificare a mano che le 8 righe abbiano treat/var coerenti col gemello collassato.

**Accettazione:** `tripledd_trimmed_fullpanel.csv` con 8 righe, colonna `source="stata_fw_boottest_48e"`, coef ≡ `stata_check_trim_fullpanel.csv`, nclust=229. Grep nel `.tex`: nessun numero del full-panel trim cambia (il paper citava già i valori Stata).

## P5 · [WARNING W4] Neutralizzare `tripledd_decomp_fullpanel.csv`

**Problema.** Unico CSV inferenziale superstite del run 20/08 (il run che ha prodotto le corruzioni accertate), mai verificato, senza WCB gemello, senza source, committato. Il paper non lo cita.

**Azione raccomandata: eliminarlo.** `Remove-Item New/Output/TripleDiff/Tables/tripledd_decomp_fullpanel.csv` (la cancellazione entrerà nel prossimo commit deciso dall'utente). Motivo per non "verificarlo con Stata invece": richiederebbe un export ~3 GB + 8 demean reghdfe su 45M righe per un risultato che il paper non usa — costo senza beneficio. Se in futuro servirà la decomposizione full-panel, la si produce col pattern 48e (export .dta → reghdfe demean → reg + boottest → CSV con source).

Annotare la cancellazione nel session-log con il motivo ("mai verificato, run 20/08, paper non lo cita").

**Accettazione:** file assente; `run_pipeline.R` già non lo richiede più (P3b — rimuovere anche `tripledd_decomp_fullpanel.csv` dalla lista artefatti, riga ~355, stesso edit).

## P6 · [NOTE N1, opzionale] Agganciare i numeri di trimming/decomposizione a un generatore

Oggi i numeri delle due sottosezioni sono prosa trascritta (verificata ≡ CSV da questo audit). Se si vuole l'aggancio automatico: aggiungere in `44_make_tables_tex.R` un blocco che legge i 4 CSV verified e scrive `New/Paper/fragments/ptab_trim_decomp.tex` (stesso pattern di `ptab_robust`), poi citarlo nel paper con `\input{}`. Non bloccante; farlo solo se l'utente lo chiede o se i numeri dovranno cambiare di nuovo.

## P7 · [NOTE N2/N3/N5] Igiene

1. **Eliminare `New/Code/46b2_wcb_fullpanel_rerun.R`** (superato dal flusso 48e; se eseguito riprodurrebbe W3). È già successo che venisse cancellato senza conferma: stavolta la cancellazione è motivata da questa roadmap, ma 🛑 chiedere comunque conferma all'utente citando questo item.
2. **🛑 Temporanei pesanti (~4,7 GB)** in `New/Data/Collapsed/`: `tmp_check_trim.dta`, `tmp_check_decomp_qua.dta`, `tmp_check_decomp_uv.dta` (271 MB ×3), `tmp_check_trim_fullpanel.dta` (3 GB), `tmp_trim_fullpanel.fst` (850 MB), `tmp_trim_collapsed.fst`, `tmp_decomp_*.fst`. Sono input dei check Stata già completati; rigenerabili con 48/48e. Proporre all'utente di cancellarli; NON cancellare senza conferma.
3. **Rigenerare `New/Output/Diagnostics/43_apec_egl_subsample.md`**: `Rscript New/Code/43_apec_egl_subsample.R` (chiude i «247» residui). Prima verificare che lo script sia solo descrittivo/leggero; se contiene feols sul collassato, va bene comunque (pattern lean già presente) ma confrontare l'output col `.md` precedente e spiegare ogni riga cambiata.

## P8 · [NOTE N4] Documentare la semantica nobs/nclust

In `New/ROADMAP.md`, sezione dedicata (o nuova §"Convenzioni CSV"), scrivere la tabella una volta per tutte:

| Contesto | nobs | nclust |
|---|---|---|
| CSV asintotici collassati (fixest) | post-singleton feols | 236 = grezzo (uniqueN) |
| CSV asintotici da Stata (source=reghdfe_stata_48) | e(N) post-singleton | 228 = e(N_clust) |
| WCB collassati (49/50) | post-singleton | nclust_pre=236, nclust=228 |
| Full panel R (fpd+fdt+pt, stata/17) | e(N) | 225 (excl) / 227 (incl) |
| Full panel trim Stata (48e) | 44.787.612 | 229 |

e la regola: nelle tabelle del paper si riporta sempre il conteggio del disegno (236 collassato / 225 full panel baseline) con nota sul post-singleton, come già fa `ptab_main`.

## P9 · 🛑 Decisioni utente (invariate)

1. **R10** — comprimere §3.1 (procedura nella roadmap 18/08). L'utente il 21/08 ha detto di lasciarla: riaprire solo se lo chiede.
2. **R12** — stimatore continuous-dose alla Callaway: parcheggiato "on demand"; la motivazione nel paper è già adeguata.
3. **MemTest86 notturno** — mai eseguito, costo zero, spiegherebbe (o escluderebbe) la RAM come causa della corruzione silenziosa. Proporlo di nuovo all'utente.
4. **Commit** — P1–P8 producono modifiche al working tree; il commit resta decisione dell'utente.
