# Roadmap Soluzioni — Audit 2026-09-07 (paper_v4)

Ogni fix è descritto in modo da poter essere eseguito da un altro modello senza decisioni da prendere. Dove serve una scelta, è già fatta e motivata.

**File di riferimento:** `New/Paper/paper_v4/paper_v4.tex` (righe indicate sulla versione del 7/9/2026 01:09). Le tabelle sono in `New/Paper/paper_v4/Tabelle/`. I CSV di verità sono quelli in `New/Output/TripleDiff/Tables_Stata/` dove esistono, altrimenti in `New/Output/TripleDiff/Tables/` (solo i file scritti da `.do`: `tripledd_full_*`, `subindices_fullpanel*`, `joint_F_*`, `tripledd_robustness_*`) e `New/Output/OLS*/Bootstrap/wcb_fullpanel*.csv`.

**Regola generale:** non ristimare nulla. Tutti i fix sono editing di `.tex` o di `.do/.R`. Dopo ogni blocco ricompilare (`pdflatex → biber → pdflatex ×2`) e controllare che il log non contenga `undefined` e che le tabelle toccate compaiano.

---

## BLOCCO A — Critici (fare per primi, in quest'ordine)

### FIX C1 — Chiudere la frase troncata (§4.2)
**Dove:** `paper_v4.tex` righe 743–744.

Sostituire:
```latex
Bootstrap and permutation $p$-values accompany every estimate in Section~\ref{sec:results}; the
complete set, across both panels, both indices and all four sample--depth variants, is collected in
```
con:
```latex
Bootstrap and permutation $p$-values accompany every estimate in Section~\ref{sec:results}; the
complete set, across both panels, both indices and all four sample--depth variants, is collected in
Appendix~\ref{app:inference} (Tables~\ref{tab:wcb} and~\ref{tab:perm}).
```

### FIX C2 — Etichetta e nota della riga "High EP depth only" (Tab. 5)
**Dove:** `Tabelle/tab_04_leaveoneout.tex`.

1. Riga 20 circa: sostituire `\textit{High EP depth only}` con `\textit{Excluding the three deepest (Peru, Switzerland, Korea)}`.
2. Nella nota, sostituire:
```
\item \textit{High EP depth only}: estimated on the subsample excluding Peru, Switzerland, and Korea jointly --- the three destinations with above-median WB EP depth.
```
con:
```
\item \textit{Excluding the three deepest}: estimated dropping Peru, Switzerland and Korea jointly --- the three destinations with the highest WB EP depth (12, 14 and 17 provisions). The coefficient roughly doubles and the standard error quadruples: the three deepest agreements supply most of the identifying variation.
```
(Verifica: CSV `dirty_leaveoneout.csv`, riga `senza_alta_dose`: coef −0.0271, se 0.0133 vs baseline se 0.0030.)

3. Il testo di §5.2 non cita questa riga: nessuna modifica lì.

### FIX C3 — Nota della Tab. A7 (event study TWFE) e frase in §5.5
**Dove:** `Tabelle/tab_A07_eventstudy_twfe.tex`, nota; `paper_v4.tex` §5.5 (riga ~1097) e App. F (riga ~1263).

1. Sostituire l'intestazione delle colonne:
```
 & \multicolumn{2}{c}{WB depth control} & \multicolumn{2}{c}{DESTA depth control} \\
```
con:
```
 & \multicolumn{2}{c}{Baseline sample} & \multicolumn{2}{c}{DESTA-coverage sample} \\
```
e `Period & $EP \times g_p$ & $EP \times b_p$ & ...` con `Period & $1[t] \times g_p$ & $1[t] \times b_p$ & $1[t] \times g_p$ & $1[t] \times b_p$ \\`.

2. Sostituire la nota (prima `\item`) con:
```
\item \emph{Notes:} TWFE event study on the collapsed panel. Treatment is binary: $1[\text{rel.\ time}=t]$ equals one for treated destinations $t$ years from PTA entry into force ($t=0$), and zero for never-treated destinations. The coefficients are $1[\text{rel.\ time}=t] \times g_p$ (cols.\ 1, 3) and $1[\text{rel.\ time}=t] \times b_p$ (cols.\ 2, 4), estimated jointly; EP depth does not enter, and no depth control is included. $t=-1$ is the omitted reference period; endpoint bins accumulate ($\leq -6$, $\geq +5$). FE: $pd + dt + pt$; weights: cell counts. Standard errors clustered at the destination level in parentheses. Columns (3)--(4) re-estimate on the sample used for the DESTA variants, which drops the one treated destination (Timor-Leste) not covered by DESTA; this is why they differ from columns (1)--(2) only at the fourth decimal.
```
3. Nella riga `$t=-6$` e `$t=5$` della tabella scrivere `$t\leq-6$` e `$t\geq5$`.
4. In §5.5, sostituire *"That estimator answers a narrower question than equation~\eqref{eq:main}: it binarises treatment, discards depth, carries no depth control, and operates on a gap collapsed across products"* con *"Both event studies answer a narrower question than equation~\eqref{eq:main}: they binarise treatment and discard depth; the Sun--Abraham version additionally operates on a gap collapsed across products"*.
5. Nella nota della Fig. 5 (`fig:es`), aggiungere dopo "FE $pd + dt + pt$": `; binary treatment, no depth control`.

### FIX C4 — Frase sulle venti celle verdi (§5.3)
**Dove:** `paper_v4.tex` riga ~968; stessa frase in `summary_v4.tex` (§"The green margin").

Sostituire:
```
The green coefficient stays between $-0.0023$ and $+0.0005$ in all twenty green cells and its
bootstrap $p$-value never falls below 0.58.
```
con:
```
Across the twelve green cells of the three subsamples the coefficient stays between $-0.0023$ and
$+0.0005$ and its bootstrap $p$-value never falls below 0.58; including the two reference rows, the
range widens only to $[-0.0046, +0.0018]$ and the lowest $p$-value is 0.39.
```
(Verifica: Tab. 6, righe collapsed: −0.0046/+0.0018 Panel A, −0.0043/+0.0016 Panel B; p minimo 0.390.)

### FIX C5 — Il benchmark "un quarto"
**Dove:** intro riga 126, conclusione riga 1156, App. MDE (riga ~1471).

Scelta: **reinserire il confronto quantitativo** con Brandi et al. (2020) come una frase in App. MDE, non come tabella. Il file v3 `New/Paper/paper_v3/Tabelle/tab_20_brandi.tex` contiene il numero di riferimento usato in v3: leggerlo e riportare **lo stesso numero** (non ricalcolare). Procedura:
1. `cat New/Paper/paper_v3/Tabelle/tab_20_brandi.tex` e annotare il benchmark (effetto per provision in Brandi et al.) e il rapporto bound/benchmark.
2. Aggiungere in App. MDE, dopo *"…says nothing about anything smaller."*:
```
For scale, \citet{brandi2020} report an effect of [X] log points per trade-relevant provision on the
green export share in their aggregate panel; the upper bound of 3.2 percent per provision here is
roughly [Y] of that figure.
```
con [X] e [Y] presi dalla tabella v3.
3. Se `tab_20_brandi.tex` non contiene un numero utilizzabile (verificare), **eliminare** la frase "one quarter of the benchmark" dall'intro e dalla conclusione e sostituirla con *"rules out effects larger than about three percent per provision at 95 percent confidence"*. Non lasciare un claim senza numero.

### FIX C6 — Definizione unica di C-prod-HS4
**Dove:** `Tabelle/tab_05_subsamples.tex` nota; `Tabelle/tab_A19_cprodhs4.tex` nota; `paper_v4.tex` §5.3 riga ~957.

Definizione corretta (da `11_subsamples.R`): *green products plus all non-green products belonging to one of the 106 HS4 headings that contain at least one green code (20.5 percent of observations).*

1. In `tab_05_subsamples.tex` sostituire `\textit{C-prod-HS4}: restricted to HS6 products with a comparable HS4-level match across classification revisions.` con `\textit{C-prod-HS4}: green products plus the non-green products in the 106 HS4 headings that contain at least one green code, so that each green product is compared with its four-digit neighbours.`
2. In `tab_A19_cprodhs4.tex` sostituire `C-prod-HS4 subsample: restricted to HS6 products with a comparable HS4-level match across classification revisions.` con la stessa frase.
3. In §5.3 sostituire *"restricts the sample to HS6 lines with a comparable match at the four-digit level, so that a solar panel is compared with its neighbours inside the same heading rather than with the entire neutral universe"* con *"restricts the neutral group to products in the same HS4 heading as a green code, so that a solar panel is compared with its four-digit neighbours rather than with the entire neutral universe"*.

### FIX C7 — Hong Kong "a third of the increase"
**Dove:** `paper_v4.tex` riga ~267 (nota §3.2) e riga ~1390 (App. Hong Kong).

Riga 267, sostituire *"the collapsed-panel dirty coefficient roughly doubles, a third of the increase attributable to Hong Kong alone"* con *"the collapsed-panel dirty coefficient roughly doubles, and dropping Hong Kong alone reverses almost all of the increase"*.

Riga ~1390, sostituire:
```
show that dropping Hong Kong
alone moves the coefficient from $-0.0189$ to $-0.0129$, so a third of the increase is Hong Kong by
itself.
```
con:
```
show that dropping Hong Kong
alone moves the coefficient from $-0.0189$ to $-0.0129$: of the 0.0070 increase relative to the
baseline of $-0.0119$, Hong Kong accounts for 0.0060, and Macao for almost none ($-0.0183$ when
dropped).
```

### FIX C8 — Numeri del campione CEM
**Dove:** `paper_v4.tex` riga ~575.

Sostituire *"(approximately 14.0 million observations before singleton removal; 13,728,510 after iterative removal of singletons)"* con *"(13,992,396 observations after iterative singleton removal)"*. Verificare che Tab. 7 (`tab:samples`) dica "14.0M" per CEM: è coerente, lasciare.

---

## BLOCCO B — Warning di testo (numeri e frasi)

### FIX W1 — Ladder, colonna citata
§5.1 riga ~760: sostituire *"the TREND green interaction is positive and significant at $+0.0021$ in the same column"* con *"at $+0.0018$ in the same column"*; riga ~778: *"the TREND green coefficient from $+0.0021$ to $-0.0001$"* → *"from $+0.0018$ to $-0.0001$"*. Riga ~761: *"in column~(3) both are again positive, one of them significantly so"* → *"in column~(3) both are again positive and significant at the ten percent level or better"*.
Stesso fix in `summary_v4.tex` (§"the fixed effects earn their keep": sostituire +0.0021 con +0.0018 in entrambe le occorrenze, oppure specificare "column (3)").

### FIX W2 — "Six times narrower"
§5.1 riga ~858: *"is roughly six times narrower"* → *"is roughly four and a half times narrower"*.

### FIX W3 — "The single coefficient… to survive the bootstrap"
Tre occorrenze: §5.5 riga ~1105, App. G riga ~1310, `summary_v4.tex`. Sostituire *"the single coefficient in the entire robustness battery to survive the bootstrap"* con *"the only green coefficient in the robustness battery to survive the bootstrap"*. (Vero: nessun altro coefficiente verde ha p bootstrap < 0.05.)

### FIX W4 — Nota Tab. 2
`tab_01_ladder.tex`, ultima `\item` prima di Source: sostituire *"Quantity and unit-value outcomes, and the versions including Hong Kong and Macao, are reported in Appendix Tables…"* con *"Quantity and unit-value outcomes are reported in Appendix Tables…"*.

### FIX W5 — R² / Adj. R²
1. `tab_A11_fullpanel_inclHKMO.tex`: due righe `Adj.\ $R^2$` → `$R^2$` (i valori 0.871/0.910/0.963 sono R²; CSV `tripledd_full_alldepvars_reghdfe_inclHKMO*.csv`, colonna `r2`).
2. `tab_06_subindex_fullpanel.tex` e `tab_A19/A20/A21`: lasciare "Adj. R²" (è ciò che il CSV contiene) ma aggiungere nella nota di Tab. 8: *"Adjusted $R^2$; the unadjusted $R^2$ of the reference specification is 0.873 (Table~\ref{tab:fullpanel})."*

### FIX W6 — Tab. A16 con numeri Stata
`tab_A16_mde.tex`, riga WB Green: sostituire `[-1.77\%, 3.19\%]` con `[-1.85\%, 3.18\%]` (da `Tables_Stata/wcb_collapsed.csv`: conf_low −0.018500, conf_high 0.031841). Le altre tre righe già coincidono a due decimali. Aggiungere alla nota: *"Bootstrap intervals from the Stata implementation."* Opzionale: in `33_mde_equivalence.R` cambiare `WCB <- out_path(here("New/Output/TripleDiff/Tables/wcb_collapsed.csv"))` in `.../Tables_Stata/wcb_collapsed.csv`.

### FIX W7 — Descrizione del test di permutazione
Tre punti da correggere, stessa idea: il profilo permutato include il controllo di profondità.
1. §4.2 (riga ~733), dopo *"Because depth and timing are permuted jointly, the test cannot separate the effect of environmental content from the effect of entry timing."* aggiungere: *"The non-environmental depth control is permuted together with the environmental profile, so the null hypothesis is that, given which destinations have an agreement and when, it does not matter which agreement profile --- environmental and non-environmental content alike --- each of them received."*
2. §5.2 riga ~896: sostituire *"because the permutation distribution is built by shuffling treatment rather than by changing the control, and it therefore returns the same verdict under both depth measures"* con *"because the permutation distribution is built by reassigning whole agreement profiles rather than by partialling out a control, and it returns the same verdict under both depth measures (0.28 and 0.15)"*.
3. Conclusione riga ~1165: sostituire *"which is indifferent to the choice of control because it shuffles treatment rather than covariates"* con *"which reassigns whole agreement profiles and therefore does not depend on how well the depth control is measured"*.

### FIX W8 — Bias da under-control, lato dirty
§5.2, dopo il paragrafo *"One ambiguity we do not resolve…"* (riga ~935) aggiungere un paragrafo:
```
A second consideration cuts the other way. The under-control argument of Section~\ref{sec:strategy}
--- an imperfect depth control lets $EP_{dt}$ absorb part of the effect of general agreement depth
--- biases both interactions upward. For the green margin this makes the null conservative; for the
dirty margin it implies that the true coefficient is, if anything, more negative than the estimate.
We note this rather than resolve it: the permutation test, which reassigns environmental and
non-environmental content together, is unaffected by the quality of the control, and it is the test
on which the reading above rests.
```

### FIX W9 — Numeri di §5.4 e tabella di composizione
1. *"The regulatory-space index also accounts for 71.5 percent of the TREND count across treated destination–years and correlates 0.90 with total depth"* (footnote §5.4): sostituire con *"The regulatory-space index also accounts for 36 percent of the TREND count across treated destination--years and correlates 0.87 with the aggregate TREND count and 0.90 with World Bank non-environmental depth"*. (Fonte: `Data/Merged/Merged_TREND_WB_Indices_Only.csv`, country-year con WB_EP_Depth > 0 e escl. 110/121: somma RegSpace / somma TREND_EP_Count = 0.362; corr(RegSpace, TREND_EP_Count) = 0.868. Il 0.90 con TotalDepth era in v3 riga 984.)
2. *"a binding environmental obligation appears in exactly one of the fourteen Chinese PTAs"* e Panel B di `tab:mechanism`: indicare la variabile. Procedura: `grep -n "Binding\|binding" New/Code/*.R New/_legacy/code/*.R` per trovare quale colonna TREND (`X2_*`?) ha generato il conteggio. Se si trova, aggiungere nella nota di `tab:mechanism`: *"Binding obligations: TREND variable [nome], coded 1 for [descrizione]"*. Se non si trova entro 15 minuti, **togliere** Panel B dalla tabella e la frase dal testo: un numero senza fonte non va in un paper.
3. `tab:mechanism` Panel B: se resta, scrivere "14" (non "14–15") e "7.1\%".
4. Tabella `tab:subindex_composition`: allineare alle colonne di Tab. 8. Sostituire il corpo con nove righe: Green Liberalization (WB), Standards Non-Regression (WB, *"collinear with Green Liberalization in-sample, not estimated separately"*), Green Market Access (TREND) nel blocco trade-mechanism; Enforcement & DSM (WB), Enforcement & DSM (TREND) nel blocco enforcement; Hard obligations (TREND), Soft obligations (TREND), Regulatory Space (TREND) nel blocco restante. Eliminare le righe "Binding Obligations (TREND)" e "Regulatory Space (WB)" (non stimate). Contenuti da `02_build_dataset_wb_trend_merge.R` sezione D1/D2: Hard = X2_*, X5_*, X10_*, X14_* al netto dei Soft; Soft = X1_*, X7_09, X5_01_02; RegSpace TREND = X1_07–X1_09, X8_*; GreenMkt = X7_01_01, X7_01_02_*, X8_09_04; Enf TREND = X5_*, X11_*, X12_*, X13_*; WB Enf = WB_13–16; WB GreenLib = WB_10; WB Std = WB_2, 8, 9.

### FIX W10 — Descrizione della Fig. 3
§3.2 riga ~440: sostituire *"The two groups track each other closely throughout the sample period, with no visible divergence after agreements enter into force"* con *"For green products the two groups track each other closely throughout, with no visible divergence after agreements enter into force. The dirty share is about twice as high toward treated destinations and rises faster between 2002 and 2008, which reflects the product mix of the early partners (India, ASEAN) rather than a response to environmental content: the level difference is absorbed by the destination fixed effects, and the pre-treatment coefficients of the event study (Figure~\ref{fig:es}) are flat."*

### FIX W11 — "Four earliest cohorts"
§5.5 riga ~1092: sostituire *"identified only by the four earliest cohorts --- ASEAN in 2005, Chile in 2006, Pakistan in 2007, New Zealand in 2008"* con *"identified only by the cohorts that entered by 2010, and dominated by the two early ones (Bangkok 2002 and ASEAN 2005)"*.

### FIX W12 — Tabelle descrittive
1. Eliminare `tab:sumstats_collapsed` e `tab:sumstats_combined` (righe ~379–433) e tenere solo `tab:sumstats` aggiungendole due righe per il collassato: `$\bar{y}_{pdt}$ (collapsed) & 3,773,498 & 8.974 & 9.029 & 1.957 & [0, 20.7]` e `Cell count $n_{pdt}$ & 3,773,498 & 12.13 & 3 & 43.64 & [1, 8,946]`, con `\midrule` e riga di intestazione *"Collapsed panel"*.
2. Sostituire la frase *"Mean, median, and standard deviation omitted for binary variables with more than two categories."* con *"Thirty observations with zero recorded export value have no log export value."*
3. Aggiungere in §3.1 dopo *"(Table~\ref{tab:treatment})"*: `; Figure~\ref{fig:map} maps them`.

### FIX W13 — Nomi paese
- `tab_04_leaveoneout.tex`: `Macau` → `Macao` (2 volte); `Korea Rep.` → `South Korea`; `Laos, PDR` → `Laos`.
- `tab_A01_trattamento.tex`: `Korea Rep.` → `South Korea`; `Laos,PDR` → `Laos`; `HongKong` → `Hong Kong`; `Macau` → `Macao`. Eliminare la colonna "Code" (codici interni del dataset, inutili al lettore) o rinominarla "Dataset code".
- `tab:treatment` nel tex: `S.~Korea` → `South Korea`.

### FIX W14 — "the authors"
Riga ~1008 e nota di `tab:subindex_composition`: `constructed by the authors` → `constructed here`.

### FIX W15 — Tabelle fuori margine
`tab_A05_ladder_wb.tex` e `tab_A06_ladder_trend.tex`: dopo `\begin{table}[htbp]` aggiungere `\scriptsize` al posto di `\footnotesize` e `\setlength{\tabcolsep}{3pt}`; se ancora sfora (controllare nel log), avvolgere il `tabular` in `\resizebox{\textwidth}{!}{...}`. `tab_A13_robustness_full.tex`: `\setlength{\tabcolsep}{4pt}` e abbreviare la prima colonna ("With additional controls (tariffs, concentration, antidumping)" → "Additional controls$^{a}$", con nota a).

### FIX W16 — Settori dirty
App. CO₂ riga ~1431: *"six pollution-intensive sectors"* → *"five pollution-intensive sectors"*.

### FIX W17 — Conclusione, errore standard
Riga ~1168: *"while the standard error nearly triples when either of two destinations is dropped"* → *"while the standard error doubles or triples when one of two destinations is dropped"*.

### FIX W18 — Tab. A13 simmetrica
Aggiungere a Panel A (WB) la riga *"PTA partners only, deep vs. shallow"*: coef −0.00222 (0.00351) [0.534] green; −0.00344 (0.00289) [0.246] dirty; N 5,262,293; R² 0.8839 (da `tripledd_robustness_reghdfe.csv`, model `E_WB_deepshallow`; se la riga WB manca nel CSV usare `stability_fullpanel_reghdfe.csv` groupname `deepshallow`, treat WB: −0.0022157/0.0035067/0.534; −0.0034386/0.0028862/0.246). Aggiungere a Panel B (TREND) la riga *"Including Hong Kong and Macao"*: −0.00011 (0.00100) [0.913]; −0.00101 (0.00063) [0.112]; N 23,560,110; R² 0.8706 (da `tripledd_full_alldepvars_reghdfe_inclHKMO.csv`, outcome ln_export, TREND).

### FIX W19 — App. B
Riga ~1219: *"lists the 23 treated destinations (excluding Hong Kong and Macao)"* → *"lists the 25 destinations covered by a Chinese PTA, including Hong Kong and Macao, which the main sample excludes"*.

---

## BLOCCO C — Codice e dati

### FIX K1 — Generare le tabelle v4 da script (priorità alta, non urgente per la circolazione)
Scrivere `New/Code/44b_make_tables_v4.R` che legga i CSV elencati in testa a questa roadmap e scriva i 28 file in `New/Paper/paper_v4/Tabelle/`. Specifica minima:
- Una funzione `fmt(x, d)` per coefficiente/SE/p con 4 decimali (coefficiente), 4 (SE), 3 (p in parentesi quadre); stelle asintotiche da `pval`.
- Una funzione `six_col_table(csv_base, csv_desta, wcb_base, wcb_desta, label, caption, fe_rows)` che produce il layout di `tab_02_fullpanel.tex` (3 outcome × 2 indici, 2 panel). Riusarla per tab_02, tab_03 (aggiungendo la riga permutazione), A11, A12, A19, A20, A21.
- Le note vanno scritte come stringhe nel codice, una per tabella, così restano allineate alla specifica (è il modo in cui C3 e C6 non si ripetono).
- Test finale: `diff` tra i file generati e quelli attuali deve mostrare solo differenze di formattazione, non di numeri. Se un numero differisce, la versione attuale è sbagliata: annotarlo nel session-log.

### FIX K2 — `68_subindices_fullpanel.do`
1. Sostituire il blocco vuoto
```stata
    if `drop_unmeasured' {
        * no WB_EP_Depth in memory — approximate: ...
    }
```
con: caricare anche `WB_EP_Depth` nella `use` (riga `use ln_export hs6 country_code year fpd fdt pt ...` → aggiungere `WB_EP_Depth`), poi
```stata
    if `drop_unmeasured' {
        drop if missing(`depthvar') & WB_EP_Depth > 0
    }
    drop WB_EP_Depth
```
2. `merge m:1 country_code year using `subidx', nogen` → `merge m:1 country_code year using `subidx', keep(master match) nogen`.
3. Rilanciare solo la variante DESTA (`subindices_fullpanel_desta.csv`; ~1 h). Aggiornare Tab. 8 Panel B: N diventa 21,517,666; i coefficienti cambiano al quarto decimale al più. Se non si rilancia, aggiungere alla nota di Tab. 8: *"Panel B keeps Timor-Leste in the sample (N = 21,519,511), unlike Table~\ref{tab:fullpanel} Panel B."*

### FIX K3 — `env_good` stantio nel .fst
Non toccare i dati. Aggiungere in `04_build_dataset_convert_fst.R`, in testa alla sezione colonne, il commento:
```r
## ATTENZIONE: la colonna env_good del .dta/.fst viene dalla lista HS2012 grezza
## (03.do merge con Env_Codes_HS.dta) e NON e' quella usata nelle stime. Tutti gli
## script di stima la ricalcolano da New/Data/Classifications/green_codes_hs1996.csv.
## Non usare env_good del .fst per nessuna statistica del paper.
```
e la stessa frase nel README di `New/`.

### FIX K4 — Guardia posizionale TREND in `02_build_dataset_wb_trend_merge.R`
Dopo `df_trend$Year_trend <- c(...)` aggiungere:
```r
stopifnot("df_trend deve avere 15 accordi" = nrow(df_trend) == 15,
          "lunghezza Country_TREND" = length(Country_TREND) == 15)
TREND_ATTESI <- c("Chile", "HongKong", "Macau", "New Zealand", "Pakistan", "Peru", "Singapore",
                  "ASEAN", "Costa Rica", "Australia", "Switzerland", "Korea Rep.", "Iceland",
                  "Bangkok", "APTA")
## impronta: prima parola del nome accordo nell'ordine del CSV
stopifnot(identical(substr(df_trend$Trade.Agreement, 1, 3),
                    substr(TREND_ATTESI_AGREEMENT_NAMES, 1, 3)))
```
dove `TREND_ATTESI_AGREEMENT_NAMES` va riempito una volta con `df_trend$Trade.Agreement` letto dal CSV attuale (`cat Data/TREND/TREND_China_2000_2015.csv | cut -d, -f1-3 | head -20`). Scopo: se l'ordine delle righe cambia, lo script si ferma invece di spostare gli anni di trattamento.

### FIX K5 — Timor-Leste
Scelta consigliata: **lasciare** i dati come sono (ristimare tutto per 0.02% di osservazioni non vale) ma spostare la spiegazione dalla nota di `tab:treatment` a una frase esplicita in §3.1: *"Timor-Leste is coded as an ASEAN--China party in the agreement lists although it is not one; it accounts for 0.02 percent of observations and its exclusion moves no coefficient beyond the sixth decimal (Table~\ref{tab:loo_new})."* Nessuna modifica al codice.

### FIX K6 — Deep/shallow: dichiarare il campione della mediana
Nessuna ristima. In §3.3, sostituire *"(16 deep, 7 shallow partner countries in the baseline sample; 5.3 million observations)"* con *"(median computed over the 25 PTA partners including Hong Kong and Macao; 16 deep and 9 shallow, of which 16 and 7 remain in the baseline sample; 5.3 million observations)"*. Nella nota di Tab. 6 aggiungere: *"Deep vs.\ shallow: 23 clusters, of which 7 shallow; bootstrap coverage with so few clusters on one side is imperfect."*

---

## BLOCCO D — Note e pulizia

- **N1:** intro: `allows to check` → `makes it possible to check`; `Because EP enter` → `Because EPs enter`; `reflects proper environmental content` → `reflects environmental content proper`; `In what the authors claim to be the first article` → `In the first study of the trade effects of environmental provisions in PTAs,`.
- **N4:** footnote §4.2: *"the key dirty coefficient has a bootstrap p-value of 0.07 regardless of which approximation applies"* → *"the collapsed-panel dirty coefficient has a bootstrap p-value of 0.07 and the full-panel one of 0.19 regardless of which approximation applies"*.
- **N7:** dalla root, in R: `renv::init(); renv::snapshot()`; committare `renv.lock`.
- **N8:** dopo i fix C4, W1, W3, W11 rileggere `summary_v4.tex` e allinearlo (contiene le stesse frasi).
- **N9:** `mv New/Paper/Results_v4_draft.* New/Paper/Results_v5_draft.* New/_legacy/docs/`.
- **N6:** in `make_figures_v3.do` (mappa) usare una scala a 5 classi invece di 10, oppure una palette a colori; rigenerare `fig_map_treated.pdf`.

---

## Ordine di esecuzione e tempi

1. Blocco A (C1–C8): ~2 ore. Ricompilare. → paper circolabile.
2. Blocco B (W1–W19): ~3 ore. Ricompilare, verificare log (nessun `Overfull` > 10pt, nessun `undefined`).
3. Blocco D (N1, N4, N8, N9): ~30 min.
4. Blocco C: K2 (~1 h + 1 h di Stata), K3/K4/K5/K6 (~30 min), K1 (~1 giorno, quando si ha tempo; va fatto prima della submission).
5. Rilanciare un `/audit` sul solo paper dopo 1–3 per confermare che i critici sono chiusi.

Nessun passo di 1–3 tocca dati, stime o CSV.
