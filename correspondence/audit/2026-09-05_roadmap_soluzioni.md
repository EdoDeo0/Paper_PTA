# Roadmap Soluzioni — Audit 2026-09-05 (paper_v3)

Ogni fix è descritto nel dettaglio minimo necessario per essere implementato senza ambiguità.

**File di riferimento:** `New/Paper/paper_v3/paper_v3.tex` salvo diversa indicazione.

---

## FIX C1 — Tradurre 5 tabelle italiane in inglese

**Priorità: CRITICO — blocca la submission**

Tradurre interamente (didascalia, intestazioni colonne, note a piè di tabella) i seguenti 5 file:

### C1.1 — `Tabelle/tab_01_trattamento.tex`

```
Caption:
  IT: "Le destinazioni trattate: quando l'accordo entra in vigore e quanto contenuto ambientale contiene"
  EN: "Treated destinations: PTA entry-into-force year and environmental content"

Column headers:
  IT: Destinazione / Codice / Anno di entrata in vigore / Profondità EP massima
  EN: Destination / Code / Year of entry into force / Maximum EP depth

Row "Totale":
  IT: "25 destinazioni"
  EN: "25 destinations"

Row "East Timor":
  → Rinominare in "Timor-Leste" per coerenza con il testo principale (riga 204)

Table notes — tradurre parola per parola:
  nota 1: IT "Ogni riga è una destinazione..." → EN "Each row is a destination with which China has a trade agreement in force between 2000 and 2015."
  nota 2: IT "Profondità EP = quante disposizioni ambientali..." → EN "EP depth = the number of environmental provisions in the agreement, according to two independent coding systems: the World Bank (WB) and the academic database TREND."
  nota 3: IT "Le 11 destinazioni ASEAN..." → EN "The 11 ASEAN destinations share the same agreement (2005) and therefore the same values: the truly distinct agreements are approximately 14, not 25. This is the constraint that governs all inference in this paper."
  nota 4: IT "Hong Kong e Macao sono economie di transito..." → EN "Hong Kong and Macao are transit economies: roughly half of the export value to treated destinations passes through them. The main sample therefore excludes them, and a variant reintroduces them."
```

### C1.2 — `Tabelle/tab_07_matrice.tex`

```
Caption:
  IT: "Sintesi: il coefficiente sui prodotti sporchi nelle quattro varianti, con lo stesso metodo di inferenza"
  EN: "Summary: dirty-product coefficient across four specification variants, with common inference method"

Tradurre tutte le intestazioni di riga e colonna e le note.
```

### C1.3 — `Tabelle/tab_11_robustness_full.tex`

```
Caption:
  IT: "Robustezza sul full panel: varianti di campione, controlli e variabile dipendente"
  EN: "Robustness on the full panel: sample, control, and dependent-variable variants"

Tradurre tutte le intestazioni di riga e colonna e le note.
```

### C1.4 — `Tabelle/tab_18_apec.tex`

```
Caption:
  IT: "Definizione alternativa di bene ambientale: lista OCSE contro lista APEC"
  EN: "Alternative environmental-good definition: OECD list vs. APEC list"

Nota: "OCSE" è l'acronimo italiano di OECD. Cambiare in "OECD" ovunque.
Tradurre tutte le note a piè di tabella.
```

### C1.5 — `Tabelle/tab_19_mde.tex`

```
Caption:
  IT: "Quanto dovrebbe essere grande un effetto perché questo disegno riuscisse a vederlo"
  EN: "How large an effect would the design need to detect it"

Column headers:
  IT: Indice / Margine / Errore std. asintotico / Effetto minimo rilevabile (1 dev. std.) / Semi-ampiezza IC bootstrap (1 dev. std.) / Intervallo di confidenza bootstrap (per unità)
  EN: Index / Margin / Asymptotic std. error / Minimum detectable effect (1 s.d.) / Bootstrap CI half-width (1 s.d.) / Bootstrap CI (per unit)

Row labels:
  IT: Verde / Sporco
  EN: Green / Dirty

Table notes — tradurre parola per parola:
  nota 1: IT "Campione di stima: pannello collassato..." → EN "Estimation sample: collapsed panel, excl. HK/Macao variant. Standard deviation of regressors computed on the effective sample and weighted."
  nota 2 "Perché questa tabella è importante": → EN "Why this table matters. A null result can mean two very different things: either the effect does not exist, or it exists but is too small for the data to distinguish it from noise. This table says which."
  nota 3 "Come si legge": → EN "How to read it. The minimum detectable effect (column 4) is the threshold below which the design is uninformative, computed from the asymptotic method (2.8 × standard error). Column 5 reports the half-width of the wild cluster bootstrap confidence interval: this is not an 80%-power MDE (which would be 1.43× larger), but the bootstrap margin of error. The informative bound for the precision estimate is the [bootstrap CI] column: on the green margin, it rules out effects larger than about 3% per provision at 95% confidence."
  nota 4: → EN "This yields a more precise statement of the result: not 'we find no effect,' but 'we can rule out effects larger than this threshold.'"
```

**Regola generale:** conservare tutti i comandi LaTeX (`\emph{}`, `\textbf{}`, `\textit{}`, `\citet{}`, `\citep{}`), i marker `\toprule`, `\midrule`, `\bottomrule`, e la struttura `threeparttable`. Cambiare SOLO il testo naturale.

---

## FIX C2 — Riga "alta_dose" in tab_16

**File:** `Tabelle/tab_16_leaveoneout.tex`, riga 14

Due opzioni:

**Opzione A (se è un test di sottocampione high-dose):**
1. Rinominare "alta\_dose" → "High EP depth only"
2. Aggiungere una nota che spiega il sottocampione: "High EP depth only: estimated on the subsample of destinations with above-median WB EP depth."
3. Separare visivamente questa riga dalle righe LOO standard (con un `\midrule` aggiuntivo)

**Opzione B (se è un residuo di editing):**
1. Rimuovere la riga intera (riga 14 del .tex)
2. Aggiornare il `\midrule` circostante se necessario

**Come decidere:** cercare "alta_dose" nel codice R/Stata per capire cosa genera questa riga. Se è generata da uno script di stima, è Opzione A. Se non c'è, è Opzione B.

```bash
grep -r "alta.dose\|alta_dose\|high.dose" New/Code/
```

---

## FIX C3 — Spiegare il gap 45.8M → 21.5M

**File:** `paper_v3.tex`

Aggiungere una frase nella Sezione 4 (Empirical Strategy), subito dopo la presentazione del modello e prima dei risultati. Posizione suggerita: dopo la descrizione dei FE, prima della prima tabella di risultati.

Testo suggerito:

```latex
The three-way fixed-effect structure absorbs a large number of singletons
--- observations whose identifying variation is fully accounted for by at
least one fixed effect.  Iterative singleton removal
\citep{correia2017} reduces the estimation sample from 45.8 million
raw observations to 21,519,511 in the full panel (a 53\% reduction)
and from 3,773,498 cells to 3,681,023 in the collapsed panel.
```

---

## FIX C4 — Uniformare pronomi

**File:** `paper_v3.tex`

Cercare tutte le occorrenze di "I " (maiuscola, seguita da spazio) nel file che si riferiscono all'autore (non alla variabile I o all'articolo inglese):

```bash
grep -n '\bI [a-z]' paper_v3.tex | grep -v '\\' | head -30
```

Sostituire ogni "I find/show/use/compute/estimate..." con "we find/show/use/compute/estimate...".

Controllare anche: "my" → "our", "me" → "us".

---

## FIX W1 — VIF 5.8 vs 5.7

**File:** `paper_v3.tex` (riga ~663) e/o `fragments/ptab_depthbounds.tex`

1. Verificare il valore reale. Il VIF è calcolato nello script di stima — cercare:

```bash
grep -rn "vif\|VIF" New/Code/
```

2. Una volta trovato il valore corretto, aggiornare il file che riporta il numero sbagliato.

---

## FIX W2 — 27.7% → 27.8%

**File:** `paper_v3.tex`, riga ~849

Cercare: `27.7\%`  
Sostituire con: `27.8\%`

Alternativa: se la percentuale viene arrotondata di proposito, scrivere "approximately 28%" e rimuovere il conflitto.

---

## FIX W3 — Bootstrap p = 0.012 vs 0.015

**File:** `paper_v3.tex` (sezione destination trends)

Cercare il passaggio che cita "p = 0.012" nel contesto del TREND green interaction con destination trends.

Due opzioni:
1. Aggiornare il testo a "p = 0.015" per corrispondere a tab_12
2. Se il valore nel testo proviene da un run più recente: aggiornare tab_12 e rigenerare la tabella dal codice

Per verificare quale sia il valore corretto:
```bash
grep -rn "0.012\|0.015" New/Code/ | grep -i "trend\|dest"
```

---

## FIX W4 — Deep/shallow 7 vs 9

**File:** `paper_v3.tex` e `fragments/ptab_stability.tex`

Il testo dice "16 deep, 7 shallow" (campione excl. HK/MO). La nota di ptab_stability dice "16 deep vs. 9 shallow" (campione incl. HK/MO).

Soluzione: uniformare alla convenzione del campione principale (excl. HK/MO → 7 shallow).

In `ptab_stability.tex`, modificare la nota da:
```
16 deep vs. 9 shallow
```
a:
```
16 deep vs.\ 7 shallow (excl.\ HK/Macao; 9 shallow in the variant that includes them)
```

---

## FIX W5 — Obs count in ptab_main

**File:** `fragments/ptab_main.tex`

Riga 25: attualmente riporta `21{,}519{,}511` come N per tutte e 4 le colonne.

Opzione A (mostrare conteggi separati):
```latex
\midrule
Observations & \multicolumn{2}{c}{21,519,511} & \multicolumn{2}{c}{21,517,666} \\
```

Opzione B (aggiungere nota):
```latex
Observations & \multicolumn{4}{c}{21,519,511 (WB) / 21,517,666 (TREND)} \\
```

---

## FIX W6 — SE Australia non verificabile

**File:** `Tabelle/tab_16_leaveoneout.tex`

Aggiungere una colonna "SE" alla tabella LOO, accanto alla colonna dei coefficienti. In alternativa, aggiungere una nota:

```
The baseline standard error on EP$\times$dirty is 0.0030 (asymptotic). Excluding
Australia increases it to 0.0087, reflecting Australia's 2015 entry --- the
only late entrant --- and its disproportionate leverage on the treated-vs-control
comparison in the final years of the panel.
```

---

## FIX W7 — CEM 14.0M vs 13.7M

**File:** `paper_v3.tex`, righe ~538 e ~563

Sostituire "14.0 million" con "13.7 million" (il valore post-singleton riportato nelle tabelle).

Oppure chiarire:
```latex
... reduces the sample to approximately 14.0 million observations
before singleton removal (13,728,510 after iterative removal of singletons).
```

---

## FIX W8 — "~9 EP profiles" vs 13

**File:** `Tabelle/tab_06_permutation.tex`, nella nota

Cercare "roughly nine distinct EP profiles" e sostituire con una formulazione più precisa:

```
13 distinct EP profiles (of which approximately 9 are effectively
independent, since the 10 ASEAN destinations share a single agreement).
```

---

## FIX N1 — Rimuovere 16 bib entries non citate

**File:** `references.bib`

Rimuovere i seguenti blocchi `@article{...}` o `@book{...}`:

```
baccini2017
baghdadi2013
bertrand2004
brunnermeier2004
callaway2021
conley2011
copelandtaylor2004
dean2009
dechezlepretre2017
fisher1935
headmayer2014
jaffe1997
kellenberg2014
medvedev2010
neri2023
rajan1998
```

Prima di rimuovere, verificare con:
```bash
for key in baccini2017 baghdadi2013 bertrand2004 brunnermeier2004 callaway2021 conley2011 copelandtaylor2004 dean2009 dechezlepretre2017 fisher1935 headmayer2014 jaffe1997 kellenberg2014 medvedev2010 neri2023 rajan1998; do
  echo -n "$key: "
  grep -c "$key" paper_v3.tex
done
```

Se il conteggio è 0 per tutte, rimuovere. Se qualcuna ha count > 0, mantenerla.

---

## FIX N2 — Chiavi bib

**File:** `references.bib`

Opzionale ma consigliato per chiarezza:
- Rinominare `morin2018` → `morin2017` (e aggiornare tutte le `\cite{morin2018}` nel .tex)
- Rinominare `gutsch2024` → `gutsch2025` (e aggiornare tutte le `\cite{gutsch2024}`)

**Attenzione:** cercare prima tutti i riferimenti nel .tex:
```bash
grep -c "morin2018" paper_v3.tex
grep -c "gutsch2024" paper_v3.tex
```

---

## FIX N3 — Timor-Leste vs East Timor

**File:** `Tabelle/tab_01_trattamento.tex`, `Tabelle/tab_16_leaveoneout.tex`

Cercare "East Timor" e sostituire con "Timor-Leste" in entrambi i file.

Verificare che il testo principale (riga ~204) usi già "Timor-Leste" (dovrebbe).

---

## FIX N4 — Figure inutilizzate

Spostare in una sottocartella `figures/_unused/`:
```bash
mkdir -p New/Paper/paper_v3/Figures/_unused
mv New/Paper/paper_v3/Figures/fig_ep_distribution_wb.pdf New/Paper/paper_v3/Figures/_unused/
mv New/Paper/paper_v3/Figures/fig_ep_distribution_trend.pdf New/Paper/paper_v3/Figures/_unused/
mv New/Paper/paper_v3/Figures/fig_ep_timeline_twopanel.pdf New/Paper/paper_v3/Figures/_unused/
mv New/Paper/paper_v3/Figures/fig_ep_timeline_twopanel.png New/Paper/paper_v3/Figures/_unused/
```

---

## FIX N5 — Data nel paper

**File:** `paper_v3.tex`, frontmatter

Cercare "August 2026" e aggiornare alla data di sottomissione effettiva.

---

## FIX N6 — run_pipeline.R cross-platform

**File:** `New/Code/run_pipeline.R`, riga ~40

Sostituire:
```r
RSCRIPT_BIN <- file.path(R.home("bin"), "Rscript.exe")
```
con:
```r
RSCRIPT_BIN <- file.path(R.home("bin"), "Rscript")
```

`Rscript` (senza `.exe`) funziona su tutti i sistemi operativi. Su Windows, il sistema risolve automaticamente l'estensione.

---

## FIX N7 — Lockfile versioni R

Dalla directory del progetto, eseguire:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

Questo crea `renv.lock` con tutte le versioni dei pacchetti installati. Includere `renv.lock` nel repository.

---

## Ordine di esecuzione raccomandato

1. **C1** — Traduci le 5 tabelle (è il lavoro più grosso, ~2 ore)
2. **C2** — Risolvi "alta_dose" (cerca nel codice prima)
3. **C3** — Aggiungi frase singleton removal
4. **C4** — Uniforma pronomi
5. **W1–W8** — Correzioni numeriche puntuali (30 min totali)
6. **N1–N8** — Pulizia (30 min totali)
7. Ricompila il PDF e verifica visivamente tutte le tabelle tradotte
8. Verifica che `biber` non produca warning su chiavi mancanti

Nessun fix richiede di rieseguire stime, rigenerare dati, o modificare il codice di analisi (eccetto N6 e N7 che toccano l'orchestratore e l'ambiente, non le stime).
