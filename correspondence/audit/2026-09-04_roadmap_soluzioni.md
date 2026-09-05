# Roadmap Soluzioni — Audit Paper_PTA 2026-09-04

**Data:** 2026-09-04
**Riferimento:** `2026-09-04_audit_report.md`
**Regola:** ogni soluzione è autocontenuta.

---

## [P1] `tab_05_wcb` inclusa senza \ref

**Gravità:** WARNING
**File:** `New/Paper/paper_v3/paper_v3.tex`, riga 729

**Problema:** La tabella WCB è inclusa via `\input{Tabelle/tab_05_wcb}` nella nota a piè di pagina che descrive il WCB, ma nessun `\ref{tab:wcb}` esiste nel testo. La tabella appare nel PDF senza che il lettore sia mai indirizzato a essa.

**Soluzione:** Aggiungere un riferimento esplicito nel testo. Cercare la frase nella nota a piè di pagina che introduce il WCB (attorno a riga 707-729) e aggiungere alla fine della nota:

```latex
Wild cluster bootstrap $p$-values are reported in Table~\ref{tab:wcb}.
```

Oppure, se la tabella è troppo grande per stare in una nota, spostarla fuori dalla nota e citarla nel corpo del testo con:

```latex
Table~\ref{tab:wcb} reports wild cluster bootstrap $p$-values.
```

**Verifica:** `grep -n "ref{tab:wcb}" paper_v3.tex` deve restituire almeno una occorrenza.

---

## [P2] `tab_20_brandi` inclusa senza \ref

**Gravità:** WARNING
**File:** `New/Paper/paper_v3/paper_v3.tex`, riga 773

**Problema:** La tabella di confronto con Brandi et al. è inclusa via `\input{Tabelle/tab_20_brandi}` dopo il paragrafo sui MDE bounds, ma `\ref{tab:brandi}` non esiste nel testo.

**Soluzione:** Nella frase che cita i bounds di Brandi (attorno a riga 770-772), aggiungere un riferimento alla tabella. Trovare la frase che cita il confronto con Brandi et al. (2020) e aggiungere in coda:

```latex
Table~\ref{tab:brandi} reports the bound comparison.
```

Oppure, se il paragrafo già cita il confronto con parole, aggiungere alla fine del paragrafo prima del `\input`:

```latex
Appendix Table~\ref{tab:brandi} summarizes the comparison.
```

**Verifica:** `grep -n "ref{tab:brandi}" paper_v3.tex` deve restituire almeno una occorrenza.

---

## [P3] Chiarire lo status dei 10 file Tabelle/ non inclusi

**Gravità:** NOTE
**File:** `New/Paper/paper_v3/Tabelle/tab_01, tab_03, tab_04, tab_07, tab_08, tab_10, tab_11, tab_17, tab_18, tab_19`

**Problema:** 10 file generati da `44_make_tables_tex.R` esistono nella cartella `Tabelle/` ma non sono inclusi via `\input` nel paper. Non è chiaro se siano intenzionalmente esclusi (tabelle in appendice non ancora aggiunte) o dimenticati.

**Soluzione:** Per ciascuno dei 10 file, decidere esplicitamente una delle tre opzioni:
1. **Includi:** aggiungere `\input{Tabelle/tab_XX}` nell'appendice corretta + `\ref` nel testo
2. **Escludi intenzionalmente:** aggiungere un commento in `44_make_tables_tex.R` accanto al blocco che genera quel file: `# tab_XX: generato ma non incluso nel paper — [motivo]`
3. **Elimina dalla generazione:** rimuovere il blocco da `44` se la tabella è obsoleta

Le tabelle più probabilmente da includere (se non già coperte inline): `tab_17_depthcontrols` (corrisponde a `\ref{tab:depthbounds}` definito inline?), `tab_18_apec`, `tab_19_mde`.

**Verifica:** dopo la decisione, `grep -rn "input{Tabelle/" paper_v3.tex | wc -l` deve coprire tutte le tabelle che si vogliono nel PDF.

---

## [W10] 5 script con `.md` diagnostici non suffissati

**Gravità:** NOTE
**File:** i 5 script `New/Code/` tra 33 e 43 non ancora aggiornati

**Problema:** Rieseguire con `SAMPLE="incl"` sovrascrive il `.md` del baseline.

**Soluzione:** Negli script non ancora aggiornati, sostituire il path bare:

```r
# Prima:
writeLines(report, here("New", "Output", "Diagnostics", "33_diagnostic.md"))

# Dopo:
writeLines(report, out_path(here("New", "Output", "Diagnostics"), "33_diagnostic", ".md"))
```

`out_path()` è già definito in `_sample_config.R`. I 6 script già aggiornati mostrano il pattern corretto — copiare da quelli.

**Verifica:** rieseguire con `SAMPLE="incl"`: il file di output deve avere suffisso `_inclHKMO.md` e non sovrascrivere il baseline.

---

## [W11] Ladder appendix: non distingue WB vs TREND significance

**Gravità:** NOTE
**File:** `New/Paper/paper_v3/paper_v3.tex`, sezione `\section{Saturation ladder}` (riga ~1225)

**Problema:** Il testo dice "reaches nominal significance only in the fpt+pd structure" senza dire che entrambe le colonne TREND in quella riga sono p<0.05 mentre WB è p=0.09. Un referee che guarda la tabella lo noterà.

**Soluzione:** Nella frase dell'appendice che cita la significatività, aggiungere il dettaglio WB vs TREND:

Trovare la frase: `"reaches nominal significance only in the \textit{fpt}+\textit{pd} structure"`

Sostituire con:

```latex
reaches nominal significance only in the \textit{fpt}+\textit{pd} structure (WB index: $p = 0.09$; TREND index: $p < 0.05$)
```

**Verifica:** rileggere il paragrafo — il lettore deve poter capire che TREND è più significativo di WB nella stessa struttura FE.

---

## [W12] LOO: lettura dose-response non menzionata

**Gravità:** NOTE
**File:** `New/Paper/paper_v3/paper_v3.tex`, sezione §5.4 (leave-one-out), attorno a riga 871-874

**Problema:** Il paper interpreta la fragilità LOO di Australia e South Korea come "thin slice of identifying variation" senza notare che sono anche le destinazioni con EP depth più alto. L'interpretazione dose-response alternativa (effetto reale ma sotto-alimentato nelle destinazioni meno trattate) non viene mai nominata.

**Soluzione:** Dopo la frase "A genuine identified effect does not behave this way." (fine paragrafo LOO, riga ~874), aggiungere:

```latex
Australia and South Korea also happen to be among the destinations with the deepest EP coverage
in the sample (WB depth 12 and 17, respectively). A dose-response reading --- that the dirty
effect is real but concentrated in the highest-dose observations --- is not ruled out by the
leave-one-out geometry alone. The DESTA depth control, which shifts the pivotal observation
from Australia to South Korea, does not separate these two interpretations. We flag this
ambiguity without resolving it.
```

**Verifica:** il testo deve presentare entrambe le letture (fragility e dose-response) come possibili, senza privilegiarne una.

---

## [W13] Nessun framework di test multipli

**Gravità:** WARNING
**File:** `New/Paper/paper_v3/paper_v3.tex`, sezione §5 (robustness) o conclusione

**Problema:** Con ~40 test nella batteria di robustezza, il paper non discute il tasso di falsa scoperta. Il segnale RegulatorySpace (WCB p=0.046/0.022) è il solo risultato robusto e non viene contestualizzato rispetto alla null globale.

**Soluzione:** Aggiungere un breve paragrafo a fine sezione §5 (robustness), prima della conclusione, o come footnote nell'ultima sottosezione robustness:

```latex
Across the full robustness battery, approximately 40 hypothesis tests are reported.
Under a global null with independent tests, two rejections at the 5\% level are
expected by chance. The RegulatorySpace result ($p = 0.046$) is the only point
of nominal significance that survives bootstrap inference; it is internally
consistent across WB and TREND codings, but this consistency cannot be
distinguished from correlated false discovery given the high collinearity among
sub-indices. We report this caveat without discounting the finding.
```

**Verifica:** il testo deve rendere esplicita la coerenza tra il numero di test, i falsi positivi attesi, e il segnale unico trovato.

---

## [W14] "Content, not chapters" senza caveat causale

**Gravità:** NOTE
**File:** `New/Paper/paper_v3/paper_v3.tex`, conclusione, righe ~1184-1190

**Problema:** La conclusione dice "The policy implication is direct: a chapter in an agreement is not itself the instrument. What the chapter contains is." Questa inferenza si basa sul conteggio descrittivo delle provision nella Tabella dei subindici + null aggregato — non su un contrasto causale in-sample (impossibile per collinearità perfetta). Manca una frase che dica esplicitamente su cosa si basa l'inferenza.

**Soluzione:** Dopo "What the chapter contains is." aggiungere:

```latex
This reading rests on descriptive counting of provision types (Table~\ref{tab:subindices})
and the aggregate null result, not on a within-sample causal contrast between
mechanism-bearing and cooperation-only provisions: the collinearity structure documented
in Section~\ref{sec:bundling} makes that test infeasible with this design.
```

**Verifica:** il lettore deve capire che "content matters" è un'inferenza indiretta (counting + null), non una stima causale.

---

## Ordine di implementazione suggerito

| Priorità | Issue | Tempo stimato |
|----------|-------|---------------|
| 1 | P1 + P2: aggiungere \ref per wcb e brandi | 5 min |
| 2 | W13: paragrafo test multipli | 10 min |
| 3 | W14: frase di umiltà nella conclusione | 5 min |
| 4 | W11: WB vs TREND nella ladder | 5 min |
| 5 | W12: dose-response alternativa nel LOO | 10 min |
| 6 | P3: decidere status dei 10 file Tabelle/ | 15 min (decisione) |
| 7 | W10: 5 script .md suffisso | 20 min |
