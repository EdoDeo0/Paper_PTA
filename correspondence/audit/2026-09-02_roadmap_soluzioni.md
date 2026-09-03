# Roadmap delle soluzioni — audit 2026-09-02

**Come si usa questo documento.** Ogni voce è autosufficiente: dice cosa c'è ora, cosa deve diventare, i passi esatti e come si verifica. Un modello che esegue una voce non deve prendere nessuna decisione autonoma. Se un passo non torna, si ferma e lo segnala invece di improvvisare.

**Regole valide per tutte le voci:**

1. Non toccare mai i file in `New/Output/`, `Output/`, `Data/`. Sono risultati, non sorgenti.
2. Non rilanciare nessuna stima per applicare una correzione al testo. I numeri esistono già su disco.
3. Ogni volta che si scrive un numero nel `.tex`, va preso da un file su disco, mai ricalcolato a mente.
4. Dopo ogni modifica al `.tex`: ricompilare con `pdflatex` + `biber` + `pdflatex` ×2 e controllare che il log non abbia nuovi errori né `Citation ... undefined`.
5. Non fare `git commit` né `git push` senza che l'utente lo chieda esplicitamente in quel momento.

**Percorsi.** `ROOT = C:\Work\projects\Paper_PTA`. Il paper è `ROOT/New/Paper/paper_v3/paper_v3.tex`.

---

# PRIORITÀ 0 — Prima di far leggere il paper a chiunque

Sono quattro correzioni al testo. Nessuna stima da rifare. Tempo stimato: 2-3 ore in totale, più 1 ora per lo script di P0.1.

---

## P0.1 — Quota green: 11,0 % → 11,5 %, e rendere riproducibili le summary statistics

**Rilievo:** C1.

### Il problema in una frase

Le regressioni usano `env_good` **ricalcolato** dalla lista `green_codes_hs1996.csv` (media 0,1154). Le tabelle descrittive usano la colonna `env_good` **stantia** dentro il `.dta` originale (media 0,1096). Il paper quindi descrive una variabile diversa da quella che stima. In più i file `sumstats_*.csv` non hanno uno script che li generi.

### Prove su disco

| File | Contenuto |
|---|---|
| `New/Output/17_main_tripledd_fullpanel.log`, righe 277-287 | `Righe: 45781211`, `green: 11.5%`, `dirty: 7.0%` |
| `New/Output/Diagnostics/15_descriptives_sample.md` | `Quota green (su N oss.) 11,54% pre-singleton` |
| `New/Paper/paper_v3/sumstats_fullpanel_exHKMO.csv` | `env_good, 45781211, 0.10961573296958, ...` |
| `New/Code/stata/17_main_tripledd_fullpanel.do`, intestazione | *"env_good RICALCOLATO dalla lista green HS1996 (05) ... NON le colonne stantie del .dta originale"* |

### Passo 1 — Creare `New/Code/70_sumstats_paper.R`

Nuovo file. Non modificare nessuno script esistente.

Requisiti funzionali, in ordine:

1. `library(here); library(fst); library(data.table)`.
2. Leggere le liste canoniche:
   - green: `here("New/Data/Classifications/green_codes_hs1996.csv")`, colonna `hs6_final`, letta come `character` (`colClasses = list(character = "hs6_final")`), poi `unique()`.
   - dirty: `here("New/Data/Classifications/dirty_goods_hs6.csv")`, tenere solo le righe con `dirty == 1` e prendere `hs6`.
3. Leggere dal panel `here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")` **solo** queste colonne, con `fst::read_fst(..., columns = ...)`:
   `ln_export`, `ln_export_qua`, `ln_export_value`, `hs6`, `country_code`, `year`, `WB_EP_Depth`, `TREND_EP_Count`, `tariffs`, `ln_hhi_baci`.
   **Non** leggere `env_good` né `dirty` dal `.fst`: sono le colonne stantie, è esattamente l'errore da eliminare.
4. Costruire in memoria: `env_good := as.integer(hs6_str %in% green_codes)` e `dirty_p := as.integer(hs6_str %in% dirty_codes)`, dove `hs6_str` è `hs6` formattato a 6 cifre come stringa (usare la stessa formattazione di `52_export_collapsed_dta.R`, riga ~58: leggere quel file e copiarne il metodo, non inventarne uno).
5. Produrre due versioni: `incl` (tutto) ed `exHKMO` (`country_code` diverso da 110 e 121).
6. Per ogni variabile calcolare N (non missing), media, mediana, sd, min, max. Per `env_good` e `dirty_p` calcolare N su tutte le righe.
7. Scrivere, sovrascrivendo:
   - `New/Paper/paper_v3/sumstats_fullpanel.csv`
   - `New/Paper/paper_v3/sumstats_fullpanel_exHKMO.csv`
   con **le stesse colonne e gli stessi nomi di variabile dei file attuali** (`variable,N,mean,median,sd,min,max`), così il diff è leggibile.
8. Per il panel collassato leggere `here("New/Data/Collapsed/collapsed_omnibus.dta")` (se il file non c'è, fermarsi con `stop()` e segnalarlo; **non** ricostruire il panel) e scrivere `New/Paper/paper_v3/sumstats_collapsed.csv` con le stesse colonne.
9. Alla fine stampare a schermo un blocco di controllo:
   ```
   CONTROLLO vs 17.do (atteso: righe 45781211, green 11.5%, dirty 7.0%)
   ```
   con i valori ottenuti accanto.

### Passo 2 — Verifica prima di toccare il `.tex`

Lanciare:

```bash
"/c/Program Files/R/R-4.5.2/bin/Rscript.exe" New/Code/70_sumstats_paper.R
```

Il blocco di controllo **deve** dare righe = 45.781.211, green ≈ 11,54 %, dirty ≈ 7,01 %.

Se green esce 10,96 % vuol dire che si sta leggendo la colonna stantia: fermarsi e correggere lo script.

### Passo 3 — Aggiornare le tre tabelle nel `.tex`

Nel file `New/Paper/paper_v3/paper_v3.tex`, sostituire i valori di `env_good`/`dirty` in tutte e tre le tabelle di summary statistics con quelli del CSV rigenerato. Le righe da toccare:

- Tabella `tab:sumstats` (intorno a riga 390): riga `$g_p$ (green)` e riga `$b_p$ (dirty)`.
- Tabella `tab:sumstats_collapsed` (intorno a riga 425).
- Tabella `tab:sumstats_combined` (intorno a riga 452).

Valori attesi per il full panel escl. HK-Macao (confermare dal CSV, non copiare da qui alla cieca): media green `0.115`, sd green `0.320`.

### Passo 4 — Aggiornare le due frasi nel testo

**Riga 341-345**, testo attuale:

> `Green products account for 11.0\% of firm-level observations (excluding Hong Kong and Macao) and dirty products for 7.0\%.`

Sostituire `11.0\%` con il valore rigenerato (atteso `11.5\%`). Lasciare `7.0\%` invariato.

**Nella stessa tabella `tab:descriptives`** (intorno a riga 510), la riga `Share of observations: green products & 11.0\%` va allineata.

### Passo 5 — Aggiungere una nota che chiuda il problema alla radice

Nelle note di `tab:sumstats` aggiungere:

> `\item Green and dirty indicators are recomputed from the canonical HS1996 code lists (\texttt{green\_codes\_hs1996.csv}, \texttt{dirty\_goods\_hs6.csv}), the same lists used in estimation, not from the pre-computed columns of the source panel.`

### Verifica finale

```bash
grep -n '11.0\\%\|0.110' New/Paper/paper_v3/paper_v3.tex
```

Non deve restituire nulla che si riferisca alla quota green.

---

## P0.2 — Riga "Log export value / TREND" della tabella outcomes

**Rilievo:** C2.

### Il problema

Nella tabella `tab:outcomes` (§5.13, intorno a riga 1340) la riga *Log export value* per la colonna TREND riporta numeri presi dalla regressione sul **valore unitario**, non dal baseline. La nota della tabella dichiara che quella riga è il baseline della Tabella 3, quindi il paper si contraddice.

### Numeri corretti

Fonte autorevole: `New/Output/TripleDiff/Tables_Stata/OMNI_baseline_TREND.dta` (coefficienti) e `New/Output/TripleDiff/Tables_Stata/wcb_collapsed.csv` (*p* bootstrap). Sono gli stessi numeri già stampati nella Tabella 3 Panel B, colonne (3) e (4).

| Riga | Coeff. | Boot. *p* |
|---|---|---|
| × green | `+0.0018` | `0.39` |
| × dirty | `+0.0004` | `0.86` |

### Modifica esatta

Cercare nel `.tex` il blocco:

```
Log export value   & $\times$green & $-0.0046$ & 0.65 & $-0.0002$ & 0.87 \\
                   & $\times$dirty & $-0.0119$ & 0.07 & $-0.0015$ & 0.86 \\
```

Sostituire con:

```
Log export value   & $\times$green & $-0.0046$ & 0.65 & $+0.0018$ & 0.39 \\
                   & $\times$dirty & $-0.0119$ & 0.07 & $+0.0004$ & 0.86 \\
```

**Non toccare** le righe *Log export quantity*: sono corrette (verificate su `New/Output/TripleDiff/Tables/wcb_decomp_collapsed.csv`).

### Correzione collegata (rilievo N2)

Nelle note della stessa tabella, la frase:

> `Asymptotic $p$-values for log unit value are between 0.85 and 0.95 on all four interactions except TREND$\times$dirty ($p = 0.16$).`

I valori 0,85-0,95 e 0,16 sono **bootstrap**, non asintotici. Gli asintotici sono 0,854 / 0,880 / 0,868 / 0,076. Sostituire con:

> `Bootstrap $p$-values for log unit value are between 0.85 and 0.95 on all four interactions except TREND$\times$dirty ($p = 0.16$).`

### Correzione collegata (rilievo N3)

Sostituire `exact coefficients available from the author` con un riferimento vero:

> `exact coefficients in \texttt{tripledd\_decomp\_collapsed.csv}`

Oppure, meglio, aggiungere quattro numeri alla tabella prendendoli da `New/Output/TripleDiff/Tables/tripledd_decomp_collapsed.csv`, righe con `outcome = ln_export_value`: WB green `+0.0005`, WB dirty `+0.0006`, TREND green `−0.0001`, TREND dirty `+0.0009`.

---

## P0.3 — Riscrivere la descrizione della saturation ladder

**Rilievo:** C3. È la correzione più importante delle quattro, perché tocca l'argomento che regge la scelta di specificazione.

### I fatti, presi da `New/Output/OLS/Tables_Stata/OLS_Ladder_FE_reghdfe.csv`

Colonna WB, spec senza interazione (`NI`), outcome `ln_export`, senza controlli (`ctrl=0`):

| Struttura FE | Coefficiente | Errore std | *p* | N |
|---|---|---|---|---|
| `fpd + year` | 0,003106 | 0,002505 | 0,216 | 29.477.365 |
| `fpt + pd` | 0,004628 | 0,002700 | **0,088** | 35.607.036 |
| `fpt + fpd` | 0,000097 | 0,003262 | 0,976 | 22.927.402 |
| `fpd + pt` | 0,000872 | 0,002248 | 0,698 | 29.473.145 |

Quattro strutture, non dodici. Nessuna contiene `fdt`. La sequenza non è monotona. L'unica nominalmente significativa è `fpt + pd`. I campioni sono diversi fra loro.

### Modifica 1 — §4, apertura (righe 655-661)

Testo attuale:

> `The empirical argument comes from a saturation ladder exercise. Under sparse fixed effects (firm--product--destination and year dummies), the EP-depth coefficient is small, positive, and nominally significant. As the fixed-effects structure is enriched---adding product--year and pair-level interactive effects---the coefficient falls monotonically to a precisely estimated zero. Significance that survives only in under-saturated specifications is the classic signature of omitted variable bias from selection into agreements, not a causal effect \citep{bertrand2004, goodmanbacon2021}.`

Testo nuovo:

> `The empirical argument comes from a saturation ladder exercise (Appendix~\ref{app:ladder}). Across four fixed-effects structures the EP-depth coefficient is always small---between $+0.0001$ and $+0.0046$ log points per provision---and is nominally significant in exactly one of them, the structure that combines firm--product--year with product--destination effects ($p=0.09$). It is indistinguishable from zero in the three others, including the two most saturated. A coefficient that reaches conventional significance in one fixed-effects structure out of four, with no stable sign pattern across them, is the signature of residual selection into agreements rather than of a treatment effect \citep{bertrand2004, goodmanbacon2021}. Two features of the exercise limit how much weight it can bear, and both are stated here rather than in a footnote: none of the four structures includes the firm--destination--year absorption of equation~\eqref{eq:main}, and each structure induces a different iterative singleton removal, so the four estimation samples range from 22.9 to 35.6 million observations. The exercise is therefore suggestive, and the structural argument that follows is the one that carries the section.`

### Modifica 2 — Appendice B (righe 1437-1442)

Testo attuale:

> `Table~\ref{tab:ladder} reports EP-depth coefficients on log exports across twelve fixed-effects structures, from the sparsest (product--destination and year) to the most saturated (adding firm--destination--year and product--year). Moving from left to right, the coefficient falls monotonically, reaching a precise zero in the three most saturated structures. This is the empirical documentation of the collinearity argument of Section~\ref{sec:strategy}: the pattern is the signature of selection, not treatment.`

Testo nuovo:

> `Table~\ref{tab:ladder} reports EP-depth coefficients on log export value across four fixed-effects structures, from the sparsest (firm--product--destination plus year dummies) to structures that combine two high-dimensional sets. Columns (1) and (3) are without controls, columns (2) and (4) add the non-environmental TotalDepth control. The coefficient never exceeds $+0.005$ log points per provision and reaches nominal significance only in the \textit{fpt}+\textit{pd} structure. Two caveats apply. First, none of the four structures includes firm--destination--year effects, so the ladder does not reach the saturation of equation~\eqref{eq:main}; the argument that a destination--year intercept absorbs environmental depth entirely is structural, not empirical, and is made in Section~\ref{sec:strategy}. Second, iterative singleton removal differs across structures, so the samples are not nested: N ranges from 22.9 million (\textit{fpt}+\textit{fpd}) to 35.6 million (\textit{fpt}+\textit{pd}). The comparison across rows therefore mixes the effect of the fixed effects with the effect of the sample.`

### Modifica 3 — nota della tabella

In `New/Paper/Tabelle/tab_02_ladder.tex` **e** `New/Paper/paper_v3/Tabelle/tab_02_ladder.tex` (devono restare identici) la nota attuale dice:

> `disappearing once firm--dest--year absorption is included`

Va sostituita, perché nessuna riga la include:

> `no structure in this table includes firm--dest--year absorption; that is the specification of Table~\ref{tab:main}`

Aggiungere anche una colonna o una riga con gli N, oppure una frase nella nota:

> `Estimation samples differ across rows because iterative singleton removal depends on the fixed-effects structure: N = 29.5M, 35.6M, 22.9M and 29.5M from top to bottom.`

**Attenzione:** `tab_02_ladder.tex` è generato da `44_make_tables_tex.R`. La modifica va fatta **nello script** (cercare la stringa della nota dentro `44_make_tables_tex.R`), poi rigenerando la tabella, non a mano sul `.tex` — altrimenti la prossima esecuzione dello script la sovrascrive. Se rigenerare `44` non è praticabile in questa sessione, modificare a mano **entrambe** le copie e annotare in `session-log.md` che `44` va allineato.

### Opzione da valutare con l'utente (non eseguire senza conferma)

I dati per una ladder vera esistono già: `19d_ladder_tripledd_fullpanel.do` ha prodotto 288 righe di triple-diff su quattro coppie di FE. Si potrebbe sostituire la ladder di livello con quella triple-diff, che è la specifica del paper. Ma è una scelta di contenuto, non una correzione: va proposta all'utente, non decisa.

---

## P0.4 — Il numero di unità indipendenti di variazione

**Rilievo:** C4.

### I fatti, contati su `New/Output/Diagnostics/B_treatment_map.csv`

Escludendo Hong Kong (110) e Macao (121):

- **23** destinazioni trattate
- **12** accordi distinti: Bangkok/APTA, ASEAN, Cile, Pakistan, Nuova Zelanda, Singapore, Perù, Costa Rica, Islanda, Svizzera, Australia, Corea
- **13** profili (dose, tempistica) distinti:
  1. Bangkok-only, WB=1 dal 2002 — Bangladesh, India, Sri Lanka
  2. ASEAN-only, WB=6 dal 2005 — Brunei, Cambogia, Timor Est, Indonesia, Malesia, Myanmar, Filippine, Thailandia, Vietnam
  3. Laos: WB=1 dal 2002, WB=6 dal 2005
  4. Singapore: WB=6 dal 2005, WB=7 dal 2009
  5. Corea: WB=1 dal 2002, WB=17 dal 2015
  6-13. Cile, Pakistan, Nuova Zelanda, Perù, Costa Rica, Islanda, Svizzera, Australia (uno ciascuno)

**14** è il conteggio degli accordi con HK e Macao inclusi. **Nove** è il numero di livelli distinti dell'indice WB, {1,3,4,5,6,7,12,14,17}, che ignora la tempistica.

### Decisione da prendere

Usare **13 profili distinti** come numero unico in tutto il paper. È la quantità corretta perché il test di permutazione permuta dose e tempistica insieme, ed è la quantità che l'argomento sull'inferenza richiede.

### Modifiche

Sono cinque punti. Sostituire ovunque `14 independent units of EP variation` con `13 independent EP profiles`.

| Riga | Testo attuale | Testo nuovo |
|---|---|---|
| ~215 | `the total of 23 treated destinations thus corresponds to 14 independent units of EP variation` | `the total of 23 treated destinations thus corresponds to 13 distinct EP profiles---combinations of depth and entry timing---of which the ASEAN profile alone covers nine destinations` |
| ~666 | `With only 14 independent units of EP variation and near-zero within-destination change over time` | `With only 13 distinct EP profiles and near-zero within-destination change over time` |
| ~859 | `only 23 are treated, corresponding to 14 independent units of EP variation` | `only 23 are treated, corresponding to 13 distinct EP profiles` |
| ~1121 | `With 14 independent units of EP variation, this design cannot.` | `With 13 distinct EP profiles, this design cannot.` |
| Tabella `tab:descriptives`, ~riga 510 | `Independent units of EP variation & 14 \\` | `Distinct EP profiles (depth $\times$ timing) & 13 \\` |

### Modifica al passaggio sulla permutazione (riga 900-908)

Testo attuale:

> `Of the 23 treated destinations, 11 are ASEAN members that signed the same agreement with identical EP content at the same time. Swapping any two ASEAN destinations produces the same data, so there are only about nine distinct EP profiles among the 23 treated destinations.`

Il "11" è difendibile (undici destinazioni sono parti dell'ACFTA in qualche momento) ma di quelle undici, Laos e Singapore hanno traiettorie proprie. Testo nuovo:

> `Of the 23 treated destinations, eleven are parties to the ASEAN--China agreement, and nine of them have that agreement as their only source of EP content throughout the sample: Laos entered through the Bangkok Agreement first, and Singapore signed a bilateral agreement in 2009. Swapping any two of those nine produces the same data, so the 23 treated destinations carry only 13 distinct EP profiles.`

### Verifica

```bash
grep -n 'independent units of EP variation\|about nine distinct' New/Paper/paper_v3/paper_v3.tex
```

Non deve restituire nulla.

---

## P0.5 — Le quattro correzioni brevi

Tutte in `paper_v3.tex`, nessun calcolo richiesto.

### W1 — Cluster del panel collassato (riga 848-854)

Testo attuale:

> `The difference in the number of destination clusters between the two panels---225 in the full panel, 236 in the collapsed---is a consequence of singleton removal. With the high-dimensional fixed effects of the full panel ($\theta_{fpd}$, $\theta_{fdt}$, $\theta_{pt}$), iterative removal drops all observations from 11 small destinations; the collapsed panel's lower-dimensional fixed effects ($pd$, $dt$, $pt$) retain all 236.`

Testo nuovo:

> `The difference in the number of destination clusters between the two panels---225 in the full panel, 228 in the collapsed---is a consequence of singleton removal. With the high-dimensional fixed effects of the full panel ($\theta_{fpd}$, $\theta_{fdt}$, $\theta_{pt}$), iterative removal drops all observations from 11 of the 236 destinations; the collapsed panel's lower-dimensional fixed effects ($pd$, $dt$, $pt$) are less demanding and drop only 8.`

Fonte: `New/Output/Diagnostics/stata_logs/52_omnibus_collapsed.log`, riga 683: `(Std. err. adjusted for 228 clusters in country_code)`.

### W2 — C-overlap (riga 617-620 e riga 969)

Nella descrizione di §3.3 aggiungere, dopo `(98.5\% of all HS6 codes, 21.5 million observations)`:

> `In practice the restriction is close to non-binding: it removes 314 of the 21,519,511 observations in the estimation sample, so this row is best read as a confirmation that no identifying variation comes from products traded with only one group of destinations, not as an independent robustness design.`

Nella nota di `ptab_stability.tex` (da modificare in `44_make_tables_tex.R`, poi rigenerare, oppure in entrambe le copie con annotazione in `session-log.md`) aggiungere la stessa avvertenza in forma breve:

> `The common-support row removes 314 observations from the baseline estimation sample and is a check, not an independent design.`

Fonte dei numeri: `New/Output/TripleDiff/Tables/tripledd_full_reghdfe.csv` (N = 21.519.511) e `tripledd_robustness_reghdfe.csv`, modello `D_WB_overlap` (N = 21.519.197).

### W3 — Intervallo della tabella stability (riga 969)

Testo attuale:

> `the EP$\times$green coefficient stays between $-0.0009$ and $-0.0046$`

Testo nuovo:

> `the EP$\times$green coefficient stays between $-0.0002$ and $-0.0046$`

### W4 — Deep contro shallow (riga 630 e nota di `ptab_stability`)

Testo attuale in §3.3:

> `splits the remaining PTA partners at the median EP depth (16 deep, 9 shallow partner countries; 5.3 million observations)`

Testo nuovo:

> `splits the remaining PTA partners at the median EP depth (16 deep, 7 shallow partner countries in the baseline sample; 5.3 million observations)`

Nella nota di `ptab_stability.tex`, `16 deep vs.\ 9 shallow partner destinations` diventa `16 deep vs.\ 7 shallow partner destinations (9 when Hong Kong and Macao are included)`.

Fonte: `New/Data/Subsamples/flag_deepshallow.csv` — 16 deep e 9 shallow in totale, ma i codici 110 (HK, shallow) e 121 (Macao, shallow) sono esclusi dal baseline.

### W6 — Timor-Leste (riga 204-206)

Testo attuale:

> `Timor-Leste is coded as an ASEAN--China party in the source databases; it accounts for 0.02\% of observations.`

Testo nuovo:

> `Timor-Leste is assigned to the ASEAN--China agreement in the country lists used to expand agreements into destination--year rows, although it acceded to ASEAN only in 2022. It accounts for 0.02\% of observations, and excluding it moves the dirty coefficient by less than $10^{-6}$ (leave-one-out row \texttt{senza\_144}).`

Fonte del numero: `New/Output/TripleDiff/Tables_Stata/dirty_leaveoneout.csv`, riga `senza_144` = −0,011873196 contro baseline −0,011873387.

### W11 — La nota "produced twice" (riga ~915, nota di §4.3)

Testo attuale:

> `Every estimate was produced twice: in \textsc{Stata} (...) and in \textsc{R} (...). Point estimates agree to at least eight significant digits throughout.`

Testo nuovo:

> `Estimates on the collapsed and destination-level panels were produced twice, in \textsc{Stata} (\texttt{reghdfe}, \citealp{correia2017}; \texttt{boottest}, \citealp{roodman2019}; \texttt{ppmlhdfe}, \citealp{correia2020}; \texttt{eventstudyinteract}, \citealp{sun2021}) and in \textsc{R} (\texttt{fixest}, \texttt{fwildclusterboot}); across 44 result files the two implementations agree on every point estimate to at least eight significant digits. Estimates on the 45.8-million-row firm-level panel exist only in \textsc{Stata}: \texttt{fixest} cannot complete them on the available hardware. They are validated instead by an algebraic identity---the firm-level panel estimated with $pd+dt+pt$ fixed effects reproduces the weighted collapsed-panel estimate to nine significant digits (Appendix~\ref{app:pddt})---which checks the data construction of the two pipelines against each other. All reported numbers are the \textsc{Stata} ones.`

### N4 — Arrotondamento (riga ~932)

`the EP$\times$green coefficient is $-0.0022$` → `$-0.0023$`, per allinearsi alla Tabella 3 e al sorgente (−0,0022564).

### N5 — Nota alla Tabella 1

Aggiungere alle `tablenotes` di `tab:treatment`:

> `Laos is listed under the Bangkok Agreement, its first agreement; it is also a party to ASEAN--China from 2005, so eleven destinations are ASEAN--China parties in total.`

### N6 — PPML (§5.4)

`On a zero-filled HS6--destination--year grid (8.2 million cells)` → `On a zero-filled HS6--destination--year grid (8.2 million cells, of which 7.9 million enter the estimation after \texttt{ppmlhdfe} drops separated and singleton observations)`.

Fonte: `New/Output/TripleDiff/Tables_Stata/ppml_extensive.csv`, `nobs = 7895543`.

### E3 — Dichiarare il controllo di profondità nella spec TREND

In §4.1, dopo la descrizione di `TD_{dt}`, aggiungere:

> `The same World Bank non-environmental depth control is used in both the WB and the TREND specifications: TREND does not code non-environmental provisions, so no TREND-based analogue exists.`

Verificare prima leggendo `New/Code/stata/17_main_tripledd_fullpanel.do`, righe 143-150: `td_green` e `td_dirty` sono costruiti da `$DEPTHVAR` = `totaldepth_nonenv` in entrambi i blocchi.

---

# PRIORITÀ 1 — Prima di rilanciare la pipeline da zero

Queste voci non cambiano nessun numero pubblicato. Servono a impedire che un rilancio produca numeri sbagliati senza accorgersene.

---

## P1.1 — Mettere una rete sotto la costruzione del trattamento

**Rilievo:** C5. È il rischio più grave del repository.

### Il problema

In `New/Code/02_build_dataset_wb_trend_merge.R`, righe 187-197, la mappa accordo → paesi → anno di entrata in vigore è costruita per **posizione di riga** dopo un `pivot_wider()`:

```r
df_wb$Merge_ID <- c(8, 15, 10, 1, 9, 2, 12, 3, 4, 7, 13, 5, 6, 11)
df_wb$Year_WB  <- c(2005, 2002, 2015, 2006, 2011, 2003, 2015, 2003, 2008, 2009, 2014, 2007, 2010, 2014)
Country_WB <- list( ... )
```

Se l'ordine restituito dal pivot cambia — per un aggiornamento della fonte, per una versione diversa di `tidyr`, per una riga in più nell'Excel — il trattamento si sposta in silenzio.

### Intervento: aggiungere asserzioni, non riscrivere

**Non riscrivere la logica.** Un rewrite introduce rischio senza risolvere il problema. Aggiungere invece un blocco di controllo che fallisce se il mondo cambia.

Subito **dopo** la riga `Country_WB <- list(...)` e prima di `df_wb_country_year <- ...`, inserire:

```r
## ---- GUARDIA POSIZIONALE (audit 2026-09-02, rilievo C5) ---------------------
## La mappa accordo -> paesi -> anno qui sopra e' allineata per POSIZIONE
## all'ordine delle righe restituito da pivot_wider(). Se quell'ordine cambia,
## il trattamento si sposta senza errori. Queste asserzioni lo intercettano.
stopifnot(
  "df_wb deve avere esattamente 14 accordi" = nrow(df_wb) == 14,
  "lunghezza Merge_ID"  = length(df_wb$Merge_ID) == 14,
  "lunghezza Year_WB"   = length(df_wb$Year_WB)  == 14,
  "lunghezza Country_WB"= length(Country_WB)     == 14
)
## Impronta attesa: WBID nell'ordine in cui il pivot li restituisce.
## Se questa fallisce, l'ordine e' cambiato: NON aggiornare l'impronta senza
## aver prima riverificato a mano la corrispondenza WBID <-> accordo <-> paesi.
WBID_ATTESI <- c(<DA_RIEMPIRE>)
stopifnot("ordine WBID cambiato rispetto alla verifica del 2026" =
            identical(as.integer(df_wb$WBID), as.integer(WBID_ATTESI)))
```

**Come riempire `<DA_RIEMPIRE>`:** eseguire lo script fino a quel punto e stampare `df_wb$WBID`. Copiare il vettore risultante. Non inventarlo.

### Seconda guardia: sull'output, non sull'input

Alla fine dello stesso script, dopo la costruzione di `WB_EP_Depth` e `TREND_EP_Count`, aggiungere un confronto con la mappa già validata:

```r
## Confronto con la mappa verificata a mano (audit 2026-09-02).
ref <- data.table::fread(here("New/Output/Diagnostics/B_treatment_entry.csv"))
now <- <costruire dallo stesso oggetto: country_code, entry_year, max_WB, max_TREND>
cmp <- merge(ref, now, by = "country_code", all = TRUE, suffixes = c("_ref", "_new"))
bad <- cmp[is.na(entry_year_ref) | is.na(entry_year_new) |
           entry_year_ref != entry_year_new | max_WB_ref != max_WB_new |
           max_TREND_ref != max_TREND_new]
if (nrow(bad) > 0) { print(bad); stop("La mappa del trattamento e' cambiata.") }
```

Questa è la guardia che conta: confronta il **risultato**, non l'ordine delle righe. Se un giorno la mappa cambia legittimamente (nuova versione TREND), l'errore obbliga a guardare il diff invece di subirlo.

### Le altre due fragilità dello stesso file

- Riga ~174: `df_wb <- df_wb[-c(1, 7, 15, 20, 22, 34, 51), ]`. Aggiungere prima `stopifnot(nrow(df_wb) == <N_atteso>)` e un commento che elenchi i nomi delle sette righe eliminate, così che un cambio d'ordine si veda.
- Righe 78-107: selezione degli accordi con `grepl` su etichette. Aggiungere dopo ogni filtro `stopifnot(length(selected_vars) == <atteso>)`.

---

## P1.2 — Guardia Frisch-Waugh negli script di bootstrap full panel

**Rilievo:** W8.

### Il modello da copiare

`New/Code/stata/52_omnibus_collapsed.do`, righe ~470-480, contiene già il pattern corretto:

```stata
if abs(`coef_ep_green_dm_wb' - (-0.0045685)) > 1e-4 | ///
   abs(`coef_ep_dirty_dm_wb' - (-0.0118734)) > 1e-4 {
    di as error "FWL non riproduce il baseline WB (-0.0045685 / -0.0118734)."
    di as error "  ottenuto: " `coef_ep_green_dm_wb' " / " `coef_ep_dirty_dm_wb'
    exit 9
}
```

### Dove va applicato

**`New/Code/stata/17b_wcb_fullpanel.do`.** Lo script già salva i coefficienti diretti in `b_wbg` e `b_wbd` (righe ~174-175) e poi esegue `regress` sui residui FWL (riga ~185). Manca il confronto. Inserire **subito dopo** la riga `regress ...`:

```stata
* Guardia FWL (audit 2026-09-02, rilievo W8): i coefficienti sui residui
* devono riprodurre quelli del reghdfe diretto. Se non lo fanno, il demeaning
* e' andato storto e i p bootstrap che seguono sarebbero calcolati sul nulla.
if abs(_b[`ewbg'] - `b_wbg') > 1e-8 | abs(_b[`ewbd'] - `b_wbd') > 1e-8 {
    di as error "FWL non riproduce reghdfe diretto (WB)."
    di as error "  diretto: " `b_wbg' " / " `b_wbd'
    di as error "  FWL:     " _b[`ewbg'] " / " _b[`ewbd']
    exit 9
}
```

Ripetere il blocco identico nel ramo TREND (dopo il secondo `regress`), con `b_trg` / `b_trd` e i relativi `tempvar`.

**Tolleranza:** `1e-8` è giustificata: nel log verificato la differenza osservata è zero a sette cifre stampate. Se la guardia scatta con `1e-8`, alzarla a `1e-6` **solo dopo** aver stampato i due valori e constatato che la differenza è di arrotondamento.

**Stessi interventi in:**
- `New/Code/stata/48e_fullpanel_boottest.do` (blocchi WB e TREND)
- `New/Code/stata/57_wcb_ladder_fullpanel.do` (il commento a riga 170 dice già che i coefficienti coincidono a sette cifre: trasformare l'affermazione in un test)

### Verifica

Non richiede di rilanciare le stime pesanti. Basta controllare che il do-file continui a compilare sintatticamente (`. do <file>` su un campione ridotto, oppure semplice revisione). La guardia si attiverà al prossimo run vero.

---

## P1.3 — Log Stata per variante, e sotto controllo di versione

**Rilievo:** W9.

### Problema

Nessun do-file contiene `log using`. Il log prende il nome del `.do`, quindi la variante `_inclHKMO_desta` ha sovrascritto quella baseline in `17b` e `18`.

### Intervento

In **ogni** do-file che accetta le varianti (`17`, `17b`, `17c`, `18`, `19b`, `19c`, `19d`, `52`, `54`, `58`, `60`, `61`, `63`, `65`, `66`), subito dopo la definizione di `$OUTSFX`, inserire:

```stata
cap mkdir "$ROOT/New/Output/Diagnostics/stata_logs"
cap log close _all
log using "$ROOT/New/Output/Diagnostics/stata_logs/<nome_script>$OUTSFX.log", replace text
```

e in fondo al file:

```stata
cap log close _all
```

Sostituire `<nome_script>` con il nome del do-file senza estensione.

### Intervento su `.gitignore`

Riga attuale da rimuovere:

```
New/Output/Diagnostics/stata_logs/
```

I log sono la prova materiale dei run: vanno versionati. Se il volume preoccupa, comprimere invece di ignorare.

Valutare anche di togliere `*.dta` dal `.gitignore` limitatamente a `New/Output/**/Tables_Stata/*.dta` (sono 210 file di risultato, non dati grezzi), con una riga di eccezione:

```
!New/Output/TripleDiff/Tables_Stata/*.dta
!New/Output/OLS/Tables_Stata/*.dta
```

**Chiedere conferma all'utente prima di toccare `.gitignore`:** cambia cosa entra nel repository.

---

## P1.4 — Diagnostica sui merge

**Rilievo:** W7.

### In R — `New/Code/02_build_dataset_wb_trend_merge.R`

Dopo la riga 262 (`df_merged <- df_wb %>% inner_join(df_trend, ...)`) inserire:

```r
## Diagnostica merge (audit 2026-09-02, rilievo W7)
solo_wb    <- dplyr::anti_join(df_wb, df_trend,
                 by = c("Country_WB" = "Country_TREND", "Year"))
solo_trend <- dplyr::anti_join(df_trend, df_wb,
                 by = c("Country_TREND" = "Country_WB", "Year"))
cat(sprintf("[merge WBxTREND] wb=%d trend=%d merged=%d | solo WB=%d solo TREND=%d\n",
            nrow(df_wb), nrow(df_trend), nrow(df_merged),
            nrow(solo_wb), nrow(solo_trend)))
if (nrow(solo_wb) > 0)    print(unique(solo_wb$Country_WB))
if (nrow(solo_trend) > 0) print(unique(solo_trend$Country_TREND))
stopifnot("il merge WBxTREND ha perso righe inattese" = nrow(df_merged) > 0)
```

Dopo il `left_join` con `country_codes` (riga 268) inserire:

```r
na_cc <- df_merged$Country_WB[is.na(df_merged$country_code)]
if (length(na_cc) > 0) {
  print(unique(na_cc))
  stop("Destinazioni senza country_code: uscirebbero dal gruppo trattato in silenzio.")
}
```

**Attenzione:** la seconda guardia va inserita **prima** del `select(-...)` che elimina `Country_WB`, altrimenti la colonna non esiste più.

### In Stata

Nei do-file, sostituire ogni `merge m:1 ... keep(master match) nogen` con la forma che conserva `_merge` e la controlla. Esempio da `17_main_tripledd_fullpanel.do`, riga 124:

```stata
merge m:1 hs6 using `green', keep(master match)
qui count if _merge == 3
di as text "[merge green] righe appaiate: " r(N)
drop _merge
```

Applicare almeno ai tre merge di `17.do` (green, dirty, depth) e agli stessi in `18.do` e `58.do`. Sono merge di arricchimento, quindi non ci si aspetta perdite: serve solo che il numero sia stampato nel log.

---

## P1.5 — Seed sempre impostato in `56_permutation_collapsed.do`

**Rilievo:** E5.

Riga 146, attuale:

```stata
if `start_rep' == 1 set seed 42
```

Il problema: un run ripreso da un checkpoint parte da uno stato RNG indefinito.

Sostituire con la stessa logica di `56b` e `66b`, cioè un seed che dipende solo dal numero di replica. All'interno del ciclo sulle repliche:

```stata
set seed `= 1000000 + `b' * 7919'
```

e rimuovere il `set seed 42` condizionale.

**Conseguenza da segnalare all'utente prima di applicare:** i numeri prodotti da `56` cambierebbero (non i coefficienti osservati, ma i *p* di permutazione, per errore Monte Carlo). I numeri pubblicati vengono da `56b` e `66b`, che già usano il seed per replica, quindi il paper non è toccato. Se si preferisce non alterare `56`, l'alternativa minima è aggiungere un commento che dichiari lo script non riproducibile in modalità resume.

---

# PRIORITÀ 2 — Pacchetto di replica

Da fare quando si prepara il rilascio, non prima.

---

## P2.1 — Rendere `New/` autosufficiente

**Rilievo:** W12.

### Stato attuale

Tutti gli input della pipeline `New/` vivono nella root legacy:

| Percorso letto | Da chi |
|---|---|
| `Data/Final Dataset/final_dataset_pta_env_indices_compressed.{dta,fst}` | 17, 17b, 17c, 18, 19b-d, 57, 58 e una decina di script R |
| `Data/Final Dataset/ppml_agg_pdt_zerofill.fst` | 29b, 30, 55, 64, 65 |
| `Data/Merged/Merged_TREND_WB_Indices_Only.csv` | vari |
| `Data/WB/`, `Data/TREND/`, `Data/Country_Codes_Custom_Data.csv` | 02 |
| `Data/Matching/wdi_data.csv`, `Data/Matching/mfn_tariffs_2000.csv` | 12 |
| `Output/CEM/matched_countries.csv` | 58 |

### Intervento consigliato: copy-only, non spostare

Esiste già un piano precedente per questo: `correspondence/audit/2026-08-28_piano_riordino.md`. Riprenderlo invece di riscriverlo.

Il principio da rispettare: **non spostare né rinominare nulla nel repository di lavoro.** Costruire una cartella nuova `Paper_PTA_pkg/` per copia, e verificare che la pipeline giri lì dentro. Spostare i file nel repository attivo rischia di invalidare cache resume-safe e percorsi hardcoded in un momento in cui il paper è ancora in revisione.

## P2.2 — Percorsi Stata portabili

**Rilievo:** W12.

19 do-file su 30 hardcodano `C:\Work\projects\Paper_PTA` senza ramo per sistema operativo. Gli altri 11 usano già questo blocco:

```stata
if c(os) == "Windows" { global ROOT "C:\Work\projects\Paper_PTA" }
if c(os) == "MacOSX"  { global ROOT "~/Documents/work/projects/Paper_PTA" }
if c(os) == "Unix"    { global ROOT "~/work/projects/Paper_PTA" }
```

Copiare quel blocco nei 19 restanti. Elenco dei file senza ramo OS:

`03`, `19b_assemble_only`, `19b_saturation_ladder_fullpanel`, `48_trim_check`, `48e_fullpanel_boottest`, `52`, `54`, `55`, `56`, `56b`, `57`, `58`, `59`, `60`, `61`, `63`, `65`, `66`, `66b`, `68`.

Meglio ancora: estrarre il blocco in `New/Code/stata/_paths.do` e sostituire in ogni file con `include "_paths.do"`. Ma richiede che tutti i file siano lanciati dalla stessa directory: **verificare prima** come li lancia `run_full_stata_coverage.ps1`.

## P2.3 — Un solo entry point

**Rilievo:** W12.

Oggi: `run_pipeline.R` copre solo la parte R e dichiara esplicitamente di non lanciare Stata; `run_full_stata_coverage.ps1` copre solo 63, 65, 66.

Creare `New/Code/stata/run_all_stata.ps1` che esegua, nell'ordine, con le varianti esplicite:

```
01 -> 03 -> 17 (4 varianti) -> 17b (4) -> 17c (4) -> 18 (4) -> 19b -> 19c (2)
   -> 19d (4) -> 52 -> 54 (4) -> 55 -> 56b -> 57 -> 58 (4) -> 59 -> 60 (4)
   -> 61 -> 63 (4) -> 65 -> 66b+66c (3) -> 68
```

Requisiti:
- ogni riga stampa la variante (`PTA_SAMPLE`, `PTA_DEPTH`) e il file di log atteso;
- resume-safe: salta se l'output esiste già (i do-file lo sono già);
- alla fine lancia `67_verify_stata_coverage.R` e propaga il suo exit code.

Copiare la struttura di `run_full_stata_coverage.ps1`, che ha già la funzione `Run-Stata` con gli argomenti posizionali.

## P2.4 — Eliminare i quattro `19d_*.do` identici

**Rilievo:** W12.

I quattro file sono byte-identici (verificato con `diff`). La variante arriva dalle variabili d'ambiente `PTA_SAMPLE` e `PTA_DEPTH`.

Intervento: tenere **solo** `19d_ladder_tripledd_fullpanel.do`, applicargli P1.3 (log con `$OUTSFX`), e far lanciare le quattro varianti da `run_all_stata.ps1` con le env var impostate. Cancellare gli altri tre **solo dopo** aver confermato con l'utente che i CSV già prodotti restano dove sono.

## P2.5 — Provenienza dimostrata invece che dichiarata

**Rilievo:** W10.

Due modifiche, entrambe piccole:

1. Negli script `17`, `17b`, `18`, `58`, `19b`, `19c`, `19d`, `57`: aggiungere una colonna `source` ai CSV, con il valore `reghdfe_stata_<numero>`, come già fa `52`. Nel comando `regsave` si aggiunge a `addlabel(...)`.
2. In `44_make_tables_tex.R`: cambiare `rd_pref()` in modo che la provenienza venga letta dalla colonna `source` del file quando c'è, e solo in mancanza di quella si consulti `STATA_NATIVE_IN_DIR_T`. Marcare esplicitamente nel rapporto di provenienza le righe che dipendono dalla whitelist, così che il numero "53 su 53" distingua verificato da dichiarato.

3. Facoltativo ma consigliato: far scrivere gli script full panel in `New/Output/TripleDiff/Tables_Stata/` invece che in `Tables/`, così la separazione fra cartella R e cartella Stata torna vera. **Richiede** di aggiornare i percorsi in `44_make_tables_tex.R` e in `67_verify_stata_coverage.R`, e di spostare i CSV esistenti. Da fare in una sessione dedicata, non insieme ad altro.

## P2.6 — Estendere `67_verify_stata_coverage.R` al full panel

**Rilievo:** W6 dell'audit (buco di copertura).

Non è possibile aggiungere un confronto R contro Stata sul full panel: R non completa quelle stime. Si può però aggiungere un **controllo di identità interno**, che è più forte di un semplice conteggio righe.

Aggiungere a `67` un blocco finale:

```r
## Identita' collassato / full panel (audit 2026-09-02).
## tripledd_full_pddt.csv e' il full panel con FE pd+dt+pt: per costruzione
## deve riprodurre la regressione pesata sul panel collassato.
pddt <- rd(file.path(DIR_T,  "tripledd_full_pddt.csv"))
coll <- rd(file.path(DIR_TS, "tripledd_collapsed.csv"))
## appaiare wb_green<->ep_green e wb_dirty<->ep_dirty, confrontare i coef
## soglia: 1e-7 (l'accordo osservato il 2026-09-02 e' a 9 cifre)
```

Con soglia `1e-7` e un messaggio esplicito se fallisce. Aggiungere anche i file full panel al controllo di **numero di righe attese**:

| File | Righe dati attese |
|---|---|
| `tripledd_full_reghdfe.csv` | 10 (5 per indice × 2 indici) |
| `tripledd_full_pddt.csv` | 5 |
| `joint_F_fullpanel.csv` | 2 |
| `stability_fullpanel_reghdfe.csv` | 30 (3 gruppi × 2 indici × 5 righe) |
| `tripledd_robustness_reghdfe.csv` | variabile — verificare contando i `model` distinti |

---

# PRIORITÀ 3 — Miglioramenti di contenuto (proporre, non eseguire)

Queste non sono correzioni: sono scelte editoriali. Vanno **proposte all'utente**, mai applicate d'iniziativa.

## P3.1 — Formalizzare la decomposizione between-firm / within-firm

Il paper dice a parole che "circa tre quinti" del coefficiente dirty collassato è composizione fra imprese. Il numero esatto è già su disco:

- full panel con `fpd + fdt + pt`: −0,0043521 (within-firm)
- full panel con `pd + dt + pt`: −0,0118734 (totale)
- differenza: −0,0075213, cioè il 63,3 % del totale

Presentarlo come decomposizione esplicita in una tabella a tre righe rende l'argomento contro Zhu-Sun (2026) verificabile invece che asserito. È probabilmente il contributo metodologico più forte del paper ed è oggi nascosto in una frase.

## P3.2 — Pesare il CEM

**Rilievo:** W5. `58_stability_fullpanel.do` usa il campione CEM come filtro e ignora `cem_out$w`.

Aggiungere `[aw=cem_weight]` alla `reghdfe` del gruppo `cem_v1` richiede: (a) esportare i pesi da `matched_countries.csv` nel tempfile `$F_CEM`, (b) rilanciare quel blocco (ore di calcolo sul full panel).

Prima di farlo, decidere se serve: la riga CEM è una delle nove della tabella stability, e il coefficiente pesato quasi certamente resterà indistinguibile da zero come tutti gli altri. Valutare il costo.

## P3.3 — Chiarire lo stato di CEM v2

`New/Code/12_cem_matching.R` produce `New/Output/CEM_v2/matched_countries_v2.csv` (8 trattati, 21 controlli, quattro covariate) che **nessuno legge**. Le stime usano `Output/CEM/matched_countries.csv` (v1, 16 e 40, tre covariate).

Tre opzioni, da sottoporre all'utente:
1. Documentare in testa a `12_cem_matching.R` che il v2 è esplorativo e che il paper usa il v1, indicando dove sta il v1.
2. Rilanciare la riga CEM con il v2 e riportare entrambi.
3. Rimuovere `12_cem_matching.R` dalla pipeline e conservarlo in `New/_legacy/`.

L'opzione 1 costa dieci minuti e chiude l'ambiguità. Le altre due costano ore o rimuovono lavoro fatto.

---

# Checklist di chiusura

Da spuntare prima di dichiarare l'audit chiuso.

```
PRIORITÀ 0 (paper)
[ ] P0.1  70_sumstats_paper.R creato, eseguito, green = 11,5%
[ ] P0.1  tre tabelle sumstats aggiornate nel .tex
[ ] P0.1  due frasi nel testo aggiornate
[ ] P0.2  riga TREND di tab:outcomes corretta (+0.0018 / +0.0004)
[ ] P0.2  nota "asymptotic" -> "bootstrap"
[ ] P0.3  §4 apertura riscritta
[ ] P0.3  Appendice B riscritta
[ ] P0.3  nota di tab_02_ladder corretta in 44_make_tables_tex.R e in entrambe le copie
[ ] P0.4  cinque occorrenze di "14 independent units" -> "13 distinct EP profiles"
[ ] P0.4  passaggio sulla permutazione riscritto
[ ] P0.5  W1 cluster 236 -> 228
[ ] P0.5  W2 C-overlap: aggiunta l'avvertenza
[ ] P0.5  W3 intervallo -0,0002 … -0,0046
[ ] P0.5  W4 "16 deep / 7 shallow"
[ ] P0.5  W6 Timor-Leste riformulato
[ ] P0.5  W11 nota "produced twice" riformulata
[ ] P0.5  N4, N5, N6, E3 applicati
[ ] PDF ricompilato: 0 errori, 0 citazioni indefinite
[ ] grep di controllo: nessuna occorrenza residua dei valori vecchi

PRIORITÀ 1 (pipeline)
[ ] P1.1  guardie posizionali in 02.R
[ ] P1.1  confronto con B_treatment_entry.csv in coda a 02.R
[ ] P1.2  guardia FWL in 17b, 48e, 57
[ ] P1.3  log per variante nei do-file
[ ] P1.3  .gitignore aggiornato (previa conferma dell'utente)
[ ] P1.4  diagnostica merge in 02.R e nei do-file principali
[ ] P1.5  seed in 56.do (previa conferma dell'utente)

PRIORITÀ 2 (replica)
[ ] P2.1  pacchetto copy-only secondo 2026-08-28_piano_riordino.md
[ ] P2.2  ramo OS nei 19 do-file
[ ] P2.3  run_all_stata.ps1
[ ] P2.4  19d ridotto a un file
[ ] P2.5  colonna source + rd_pref() che la legge
[ ] P2.6  67 esteso all'identità pddt e ai conteggi full panel

PRIORITÀ 3 (da proporre)
[ ] P3.1  decomposizione formale between/within
[ ] P3.2  CEM pesato
[ ] P3.3  stato di CEM v2 chiarito
```
