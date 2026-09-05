# Roadmap Soluzioni — Audit 2026-09-05

Ogni fix è descritto nel dettaglio minimo necessario per essere implementato senza ambiguità.

**File di riferimento:** `New/Paper/paper_v2/paper_v2.tex` salvo diversa indicazione.

---

## FIX 1 — Refusi nell'abstract e nell'introduzione

**File:** `paper_v2.tex`

Cercare e sostituire:

```
"incresignly" → "increasingly"          (abstract, riga ~57)
"againts"     → "against"               (abstract, riga ~57)
"matter"      → "matters"               (abstract, riga ~57, soggetto singolare "the content")
"difficoult"  → "difficult"             (intro, riga ~97)
"enforcable"  → "enforceable"           (4 occorrenze: abstract + intro)
"they the mere presence" → "the mere presence"  (intro, riga ~97)
"contration"  → "contraction"           (intro, riga ~99)
"represents"  → "represent"             (intro, riga ~103, soggetto "three categories")
"EP enter"    → "EPs enter"             (intro, riga ~103)
"significat"  → "significant"           (intro, riga ~107)
"The reminder of" → "The remainder of"  (intro, riga ~109)
```

**Verifica:** compilare il .tex e rileggere abstract + intro cercando parole sottolineate in rosso.

---

## FIX 2 — Coefficiente green: −0.0022 → −0.0023

**File:** `paper_v2.tex`, riga ~562

Cercare:
```latex
$-0.0022$ (s.e. 0.0039)
```

Sostituire con:
```latex
$-0.0023$ (s.e. 0.0039)
```

**Verifica:** aprire `New/Paper/paper_v2/fragments/ptab_main.tex` e controllare che il coefficiente green nel Panel A (full panel, colonna WB) sia effettivamente −0.0023.

---

## FIX 3 — VIF: verificare 5.8 vs 5.7

**File:** `paper_v2.tex`, riga ~455

**Azione:**
1. Aprire `New/Output/Diagnostics/` e cercare il file che contiene il VIF (probabilmente output di `14_descriptives_collinearity.R` o `41_vif_subindices.R`)
2. Verificare il valore esatto
3. Allineare testo e nota tabella allo stesso numero

Se il VIF corretto è 5.7, sostituire "5.8" con "5.7" nel testo. Se è 5.8, aggiornare la nota di `ptab_depthbounds.tex`.

---

## FIX 4 — PPML: chiarire 8.2M vs 7.9M

**File:** `paper_v2.tex`, riga ~814

Cercare il passaggio che dice "8.2 million cells" e aggiungere una precisazione:

```latex
8.2 million cells (7.9 million after singleton removal by the PPML estimator)
```

**Verifica:** controllare `New/Output/TripleDiff/Tables/ppml_extensive.csv` per il numero esatto di osservazioni nel modello stimato.

---

## FIX 5 — Nomi comandi fuori dalle note

**File:** `paper_v2.tex`

### 5a. Riga ~645-646 (corpo del testo, sezione Sun-Abraham)

Cercare il passaggio che menziona `eventstudyinteract` e `fixest::sunab` nel corpo del testo.

Riscrivere così (esempio):
```latex
The standard errors from the Stata implementation of \citet{sunab2021}
differ from those of the R implementation because the former estimates
cohort shares, while the latter treats them as known weights.\footnote{%
  Specifically, we compare \texttt{eventstudyinteract} in Stata with
  \texttt{fixest::sunab} in R. See Appendix~\ref{app:sunab}.}
```

Il principio: il nome del pacchetto/comando va in `\footnote{}`, il corpo del testo descrive il concetto.

### 5b. Appendice (~l.1082-1099)

Qui è più tollerabile perché l'appendice è tecnica. Se si vuole essere coerenti con la regola, spostare i nomi dei comandi in una nota anche lì. Altrimenti, lasciare — l'appendice è il luogo naturale per i dettagli software.

---

## FIX 6 — Rimuovere path interni dalle note tabelle

**File:** `paper_v2.tex`

### 6a. Riga ~234 (nota Table 2)

Cercare:
```latex
\texttt{New/Output/Diagnostics/B\_treatment\_entry.csv}
```

Eliminare l'intero riferimento al path, oppure sostituire con:
```latex
Source: authors' elaboration on WB and TREND data.
```

### 6b. Riga ~740 (nota Table 8)

Cercare:
```latex
\texttt{Data/Merged/Merged\_TREND\_WB\_Indices\_Only.csv}
```

Stessa azione: eliminare o sostituire con una descrizione generica della fonte.

---

## FIX 7 — Pronome: scegliere "I"

**File:** `paper_v2.tex`

Questo paper è single-authored. Usare "I" ovunque.

**Azione:** cercare `\bwe\b` (case sensitive) nel .tex e sostituire ogni "we" con "I" dove appropriato. Attenzione: alcune occorrenze di "we" possono essere nel senso di "noi come comunità scientifica" — quelle vanno lasciate. Sostituire solo dove "we" = "io, l'autore".

Passaggi principali da controllare:
- l.105: "we" → "I"
- l.284: "we" → "I"
- l.508: "we" → "I"

**Verifica:** rileggere il paper con find-and-replace per assicurarsi che non rimangano "we" autoriali.

---

## FIX 8 — Definire "bounded null"

**File:** `paper_v2.tex`

Alla prima occorrenza di "bounded null" (probabilmente nell'abstract o nella sezione risultati), aggiungere una definizione in linea. Esempio:

```latex
a \emph{bounded null}---a null result whose magnitude is bounded from above
by the wild cluster bootstrap confidence interval, ruling out effects
larger than [±X\%] at the 95\% level
```

Adattare il numero [±X%] al valore effettivo dell'intervallo bootstrap.

---

## FIX 9 — Discutere t=-6 nel Sun-Abraham dirty

**File:** `paper_v2.tex`, sezione Sun-Abraham (dopo la descrizione dei risultati event study)

Aggiungere 1-2 frasi. Esempio:

```latex
The dirty coefficient at $t=-6$ is marginally significant.
A leave-one-cohort-out exercise shows this is driven by [specificare la coorte],
and the coefficient falls to insignificance when the estimation window is truncated
to $[-5, +5]$.
```

**Fonte dati:** aprire `New/Code/23_eventstudy_sunab.R`, sezione B (righe 122-209), dove la diagnostica leave-one-cohort-out è già implementata. Usare i risultati di quella sezione.

**Perché è importante:** un referee vedrà il blip nella figura e chiederà. Meglio affrontarlo proattivamente.

---

## FIX 10 — Descrivere il CEM nella metodologia

**File:** `paper_v2.tex`, sezione dati o metodologia

Aggiungere un paragrafo breve (3-4 frasi) che descriva:
1. Le variabili di matching (cercarle in `New/Code/stata/12_cem_matching_stata.do`)
2. Il risultato: 16 trattati + 40 controlli
3. Un riferimento alla Table 4 per i dettagli

Esempio:
```latex
To construct the matched sample (Table~\ref{tab:samples}), I apply Coarsened
Exact Matching \citep{iacus2012} on [variabile 1], [variabile 2], and [variabile 3],
retaining 16 treated and 40 control destinations.
```

---

## FIX 11 — Rigenerare .fst con env_good aggiornato (non urgente)

**File:** `New/Code/29b_build_ppml_zerofill.R` e `New/Code/10_collapsed_panel.R`

**Azione:** la colonna `env_good` nei file .fst è calcolata con 238 codici green anziché 246 (la lista aggiornata in `green_codes_hs1996.csv`). Tutti gli script di analisi riclassificano a runtime, quindi i risultati sono corretti. Ma per igiene:

1. Impostare `REBUILD_FST=TRUE` in `run_pipeline.R`
2. Rilanciare gli step 10 (panel collassato) e 29b (griglia PPML)

**Attenzione:** richiede ore di calcolo e può provocare crash dell'allocatore. Fare solo quando si ha tempo e stabilità. Non urgente per la submission.

---

## FIX 12 — Riordinare step 44 dopo step 69-70

**File:** `New/Code/run_pipeline.R`

Spostare il blocco step 44 (righe 427-431) DOPO il blocco step 69-70 (righe 443-449).

In pratica: tagliare le righe:
```r
run_rscript("44", "Generatore CSV -> frammenti LaTeX",
  script = "New/Code/44_make_tables_tex.R",
  artifacts = file.path(ROOT, "New/Paper/Tabelle"))
```

E incollarle DOPO:
```r
run_rscript("70", "Statistiche descrittive per il paper",
  ...
```

Questo elimina la necessità del doppio lancio.

---

## FIX 13 — FE e clustering nei CSV (miglioramento, non urgente)

**File:** tutti gli script di stima (16, 19, 22, 23, 25, 26-31, 36, 38, 39, 42, 43, 46, 47)

**Azione ideale:** aggiungere due colonne ai CSV di output: `fe_structure` e `cluster_var`. Esempio:

```r
out$fe_structure <- "pd + dt + pt"
out$cluster_var  <- "country_code"
```

Poi in `44_make_tables_tex.R`, leggere queste colonne anziché usare le costanti hardcoded `FE_FULL`, `FE_COLL`, `CLUSTER`.

**Non urgente:** il rischio è solo se qualcuno cambia la struttura FE senza aggiornare 44. Dato che il progetto è maturo, il rischio è basso.

---

## FIX 14 — Rimuovere blocchi commentati dall'abstract

**File:** `paper_v2.tex`

Eliminare le righe commentate ~55-56 e ~59-86 (vecchie versioni dell'abstract). Non influenzano la compilazione ma sono rumore nel sorgente.

---

## FIX 15 — Referenziare tab:brandi nel testo

**File:** `paper_v2.tex`

Cercare il punto dove si discute il confronto con Brandi et al. (2020) e aggiungere un `\ref{tab:brandi}`. Esempio:

```latex
Table~\ref{tab:brandi} compares our results with those of \citet{brandi2020}.
```

---

## FIX supplementari (NOTE, non bloccanti)

### S1 — Chiarire 25 vs 23 al primo uso
Alla riga ~141 ("25 destination economies"), aggiungere:
```latex
25 destination economies (23 after excluding Hong Kong and Macao, which...)
```

### S2 — Aggiungere discussione validità esterna nella conclusione
2-3 frasi su cosa questi risultati dicono (o non dicono) su PTA di altri paesi o futuri accordi cinesi.

### S3 — Definire la notazione p/g
Alla riga ~413, chiarire che `p` indicizza prodotti HS6 e `g` indica la categoria green/dirty/neutral, non un secondo indice prodotto.

### S4 — Titolo "false positive"
Valutare se rinominare la sezione 4.4 in qualcosa di meno provocatorio, ad esempio "Anatomy of an asymptotically fragile result" o "Robustness of the green null". Un referee potrebbe obiettare che senza conoscere il vero effetto non si può chiamarlo "false positive".

### S5 — Data sul frontespizio
Il paper dice "August 2026". Aggiornare a "September 2026" o alla data effettiva di submission.

### S6 — Commento italiano in tab_20_brandi.tex
Riga 1: `% Auto-generato da New/Code/45_brandi_comparison.R — non editare a mano.`
Tradurre: `% Auto-generated by New/Code/45_brandi_comparison.R — do not edit manually.`

---

## Ordine di esecuzione consigliato

1. **FIX 1** (refusi) — 10 min, find-and-replace
2. **FIX 2** (coeff green) — 2 min
3. **FIX 6** (path interni) — 2 min
4. **FIX 14** (blocchi commentati) — 2 min
5. **FIX 7** (I/we) — 15 min, richiede lettura attenta
6. **FIX 5** (comandi in nota) — 10 min
7. **FIX 3** (VIF) — 5 min, richiede verifica dato
8. **FIX 4** (PPML singleton) — 5 min
9. **FIX 8** (bounded null) — 5 min
10. **FIX 9** (t=-6) — 10 min, richiede consultazione output script 23
11. **FIX 10** (CEM) — 10 min, richiede lettura script 12 Stata
12. **FIX 15** (ref Brandi) — 2 min
13. **FIX 12** (ordine pipeline) — 2 min
14. **FIX S1-S6** — a discrezione

**Tempo totale stimato:** ~80 minuti per i fix principali (1-15), tutti testuali.
