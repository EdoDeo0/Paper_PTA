# Audit di verifica — paper_v4 dopo l'applicazione della roadmap (2026-09-07b)

**Data:** 2026-09-07 (pomeriggio)
**Oggetto:** `New/Paper/paper_v4/` (paper_v4.tex, summary_v4.tex, 27 tabelle in `Tabelle/`) e `New/Code/33_mde_equivalence.R`.
**Documenti di riferimento:** `2026-09-07_audit_report.md` (cosa era sbagliato), `2026-09-07_roadmap_soluzioni.md` (cosa fare), `2026-09-07_implementazione_roadmap.md` (cosa si dichiara fatto).
**Metodo:** diff completo tra la versione attuale e il backup pre-fix (`scratchpad/backup_paper_v4/` della sessione e966c99b, 08:27) — quindi ogni riga toccata è stata letta, non solo quelle citate dal report. Ogni numero nuovo o spostato confrontato col CSV che lo produce (Tables_Stata/ dove esiste; altrimenti i CSV scritti da `.do` in Tables/ e `OLS*/Bootstrap/`). Ricompilazione completa e confronto del log col log del backup. Nessun file del paper, del codice o dei dati modificato; nessuna stima rieseguita.
**Fonti dei CSV verificate prima dell'uso:** `Tables_Stata/dirty_leaveoneout*.csv` (63.do), `Tables_Stata/stability_fullpanel_reghdfe.csv` (58.do), `Tables_Stata/cem_alldepvars.csv` (48j.do), `Tables_Stata/wcb_collapsed.csv` e `tripledd_collapsed.csv` (Stata), `Tables/tripledd_robustness_reghdfe.csv` (18.do), `Tables/tripledd_full_alldepvars_reghdfe_inclHKMO.csv` (17c.do), `Tables_Stata/eventstudy_twfe_stata*.csv` (54.do), `OLS/Bootstrap/wcb_fullpanel.csv`.

---

## 1. Verdetto in tre righe

**Gli 8 critici sono chiusi nella sostanza: 7 bene, 1 (C5) chiuso con un'aritmetica che non torna.** I 19 warning sono quasi tutti chiusi; quattro (W3, W6, W9, W15) sono chiusi male e vanno ripresi. Nessuna frase rotta, nessun `\ref` orfano, nessun numero di tabella spostato male: i sei numeri nuovi di A13, le due righe di sumstats, le note C2/C3/C6/C7 sono tutti corretti contro i CSV.

**Ci sono però quattro regressioni introdotte dai fix**, di cui due visibili nel PDF (Tab. 9 e Tab. 27 sforano la pagina: il numero di pagina si sovrappone alle note e l'ultima riga di nota di A13 è tagliata) e una di sostanza (la nota di Tab. A16 dichiara "intervalli dalla implementazione Stata" ma 3 righe su 4 restano R).

**Circolabile come working paper dopo mezz'ora di correzioni** (§6): due float da ridimensionare, una frase in App. MDE da riscrivere, tre celle di A16 da aggiornare, un numero senza fonte (0.90) da togliere o riprodurre.

---

## 2. Critici C1–C8

| # | Stato | Prova |
|---|---|---|
| C1 | **CHIUSO** | `paper_v4.tex:700-703`: la frase termina con "…is collected in Appendix~\ref{app:inference} (Tables~\ref{tab:wcb} and~\ref{tab:perm})." Le tre etichette esistono (`app:inference` riga 1193, `tab:wcb` in tab_A02, `tab:perm` in tab_A03). Log: 0 undefined. |
| C2 | **CHIUSO** (con regressione di layout, §4.1) | `tab_04_leaveoneout.tex:18`: riga "Excluding the three deepest (Peru, Switzerland, Korea)", −0.0271**. CSV `Tables_Stata/dirty_leaveoneout.csv`, riga `senza_alta_dose,434+331+133`: coef −.02706, se .01333 vs baseline se .00295 (×4.5: "quadruples" ✓; coef ×2.3: "roughly doubles" ✓). Codici 434/331/133 = Peru/Svizzera/Corea (tab_A01), profondità 12/14/17 ✓. |
| C3 | **CHIUSO** (due imprecisioni minori nella nota nuova, §4.3) | Nota di tab_A07 riscritta: trattamento binario, niente dose, niente controllo di profondità, `[aw=n]` ✓ (54.do riga 126), never-treated a rel_time=−1 ✓ (riga 95). Intestazioni `1[t]×g_p`; righe `t≤−6`, `t≥5`; seconda `\item` allineata (follow-up §7). §5.5: "Both event studies … binarise treatment and discard depth; the Sun–Abraham version additionally operates on a gap" ✓ (60.do stima sul gap, 54.do sulle celle). Nota Fig. 5: "binary treatment, no depth control" ✓. |
| C4 | **CHIUSO** | §5.3 riga 933-935 e summary riga 160-162. Verificato su tab_05: le 12 celle verdi dei tre sottocampioni stanno in [−0.0023, +0.0005] con p min 0.585; le 8 celle di riferimento portano il range a [−0.0046, +0.0018] con p min 0.390 ✓. |
| C5 | **CHIUSO MALE** | App. MDE riga 1441: "brandi2020 report an effect of 0.157 log points per trade-relevant provision … the upper bound of 3.2 percent per provision here is roughly one quarter of that figure". 0.032/0.157 = 0.20 = un quinto, non un quarto. Il "1/4" della tabella v3 (`tab_20_brandi.tex`, generata da `45_brandi_comparison.R`) nasce da 0.0355/0.157 = 0.226, dove 0.0355 è il limite superiore WCB **del full panel** (`OLS/Bootstrap/wcb_fullpanel.csv`: ci_high .0355059), lo stesso che §5.1 riga 816 cita come ±3.5%. Il 3.2% è invece il bound del panel collassato (Tab. A16). Inoltre 0.157 non è un numero che Brandi et al. "report": è log(1.17), la conversione fatta in 45.R del loro +17% sulla quota green per provisione *liberalizzante* (wiki `Brandi2020_EPsGreenExports.md`; 45.R righe 45-53). Riscrittura proposta in §4.4. |
| C6 | **CHIUSO** | Tre punti allineati alla definizione di 11_subsamples.R (106 HS4 con ≥1 verde, 20.5% delle righe: `Output/Subsamples/prodHS4_diagnostics.txt` righe 2 e 4). `grep "classification revisions"` = 0 occorrenze. |
| C7 | **CHIUSO** | Nota §3.2 riga 267 e App. HK/MO righe 1361-1363. CSV `Tables_Stata/dirty_leaveoneout_inclHKMO.csv`: baseline −.018871; senza_110 (HK) −.012860; senza_121 (Macao) −.018331. Aumento 0.0070 ✓, HK 0.0060 ✓, Macao −0.0183 ✓. |
| C8 | **CHIUSO** | §3.3 riga 530: "13,992,396 observations after iterative singleton removal". `cem_alldepvars.csv` e `stability_fullpanel_reghdfe.csv` (cem_v1): nobs 13992396 ✓. Tab. 7 "14.0M" coerente. `grep 13,728` = 0. |

---

## 3. Warning W1–W19, N1, N4, N8, N9, K5, K6

| # | Stato | Prova |
|---|---|---|
| W1 | **CHIUSO** | §5.1 righe 717 e 733: +0.0018 (tab_01 col. 4: 0.0018**) ✓; "in column (3) both are again positive and significant at the ten percent level or better": col. 3 WB dirty 0.0058*, TREND green 0.0021** ✓. Summary riga 137 "+0.0021 (p = 0.021, column 3)": col. 3 0.0021 (0.0009) → p≈0.02 ✓; riga 143 +0.0018 ✓. Scelta discrezionale §4.1 del report corretta. |
| W2 | **CHIUSO** | 0.0708/0.0155 = 4.57 → "four and a half times" ✓ (riga 821). CI full panel confermato in `wcb_fullpanel.csv`: [−.035338, +.035506]. |
| W3 | **CHIUSO MALE** | "the only green coefficient in the robustness battery to survive the bootstrap" (§5.5 riga 1069, App. G riga 1282, summary riga 240). La roadmap lo giustifica con "nessun altro coefficiente verde ha p bootstrap < 0.05". **Falso:** la footnote di §5.4 (riga 1031-1032) dice che sul panel collassato il TREND regulatory-space × green ha bootstrap p = 0.045 (Tab. A10, +0.0242). Un coefficiente verde con p = 0.045 sopravvive al bootstrap. Regge solo se "robustness battery" esclude la scomposizione per sotto-indici, e il testo non lo dice. Fix: "the only green coefficient among the robustness checks of Appendix G to survive the bootstrap" (e analogo in §5.5 e summary), oppure "outside the sub-index decomposition of Section 5.4". |
| W4 | **CHIUSO** | tab_01 nota: rimosso "and the versions including Hong Kong and Macao" ✓. |
| W5 | **CHIUSO** | tab_A11: "R²" 0.871/0.910/0.963; CSV inclHKMO r2 .8706/.9101/.9627 ✓. tab_06 nota: aggiunto "Adjusted R²; the unadjusted R² of the reference specification is 0.873". Stile: la frase è appesa dopo la legenda delle stelle e inizia con un frammento nominale ("Adjusted R²;") — leggibile, non elegante. |
| W6 | **CHIUSO MALE** | tab_A16: riga WB Green [−1.85%, 3.18%] ✓ (`Tables_Stata/wcb_collapsed.csv`: −.0184997/.0318412). Ma la nota ora dice "Bootstrap intervals from the Stata implementation" e **le altre tre righe non sono Stata**. Stata: WB Dirty [−1.85%, 0.17%] (−.0184596/.0016918), tabella [−1.84%, 0.18%]; TREND Green [−0.17%, 0.71%] (−.0017200/.0071089), tabella [−0.18%, 0.71%]; TREND Dirty [−0.31%, 0.56%] (−.0031394/.0055866), tabella [−0.32%, 0.54%]. L'affermazione della roadmap "le altre tre righe già coincidono a due decimali" era sbagliata (già nell'audit del mattino) e il correttore l'ha presa per buona senza aprire il CSV. Cinque celle da aggiornare. Vedi anche §5 per la colonna semi-ampiezza. |
| W7 | **CHIUSO** | Tre punti applicati; §5.2 "(0.28 and 0.15)" coerente con 27.8/14.6%. Effetto collaterale: in §4.2 (righe 681-689) l'ipotesi nulla è ora enunciata due volte nello stesso paragrafo, prima come "which destination received which EP profile" e subito dopo come "which agreement profile — environmental and non-environmental content alike". La seconda supera la prima; la prima andrebbe tolta. Non è un errore, è una ridondanza. |
| W8 | **CHIUSO** | Paragrafo aggiunto a §5.2 riga 900-906, testo della roadmap, grammaticalmente completo. |
| W9 | **CHIUSO MALE** (3 punti su 4) | (1) 36% ✓: ricalcolato da `Merged_TREND_WB_Indices_Only.csv`, country-year con WB_EP_Depth>0 escl. 110/121: 0.362. "0.87 with the aggregate TREND count": ricalcolo 0.878 escl. HK/MO, 0.868 incl.; la frase parla di "treated destination–years", che nel paper sono i 23, quindi 0.88. Borderline. **"0.90 with World Bank non-environmental depth": nessuno script lo produce** (grep su New/Code e _legacy: nulla; v3 riga 984 lo asseriva senza fonte). Ricalcolo con `Data/TotalDepth/wb_totaldepth_country_year.csv` merge su country-year: **0.70** (escl. HK/MO), 0.63 (incl.). Il numero va tolto o riprodotto con uno script. (2) Binding: `X5.01.01.Binding.obligations` = 1 in 1 riga su 15 del CSV TREND (909_China Korea_2015) ✓. Ma la nota dice "Panel B counts Chinese PTAs in the TREND database" e nel database sono 15 (Bangkok e APTA separati); il "14" e il 7.1% valgono contando Bangkok/APTA come uno. Va detto ("14 distinct agreements, Bangkok/APTA counted once"), altrimenti 1/15 = 6.7% — che era il vecchio numero. (3) 14 / 7.1% ✓. (4) Composizione: le 8 righe stimate corrispondono a 02.R sezioni D1/D2 riga per riga (GreenLib=WB_10; Std=WB_2,8,9; GreenMkt=X7_01_01, X7_01_02_01-02, X8_09_04; Enf WB=WB_13-16; Enf TREND=X5_*,X11_*,X12_*,X13_*; Hard=X2_*,X5_*,X10_*,X14_* − Soft; Soft=X1_*,X7_09,X5_01_02; RegSpace=X1_07/08/09_*, X8_*) ✓ e alle 7 colonne di Tab. 8/A10 ✓. **La nona riga, "Cooperation (TREND)", non esiste**: nessun indice `Cooperation` in 02.R, nessuna stima in 68.do o A10. È lo stesso difetto delle righe "Binding Obligations" e "Regulatory Space (WB)" appena rimosse. Il correttore l'ha tenuta per far tornare "nove righe" (report §4.2): la roadmap ne elencava otto, ed erano otto. Togliere la riga. |
| W10 | **CHIUSO** | Testo riscritto. Verificato su `green_dirty_shares_by_year.csv` (quote in valore): dirty verso trattati 13.6%→19.9% (2002-08), verso non trattati 5.5%→9.1%; green 6.0→12.9 vs 6.3→12.1 ✓. Pre-trend piatti in A07 ✓. La spiegazione "reflects the product mix of the early partners (India, ASEAN)" è un'interpretazione senza test a supporto; plausibile, ma la coorte 2002 è Bangkok (5 paesi), non "India". |
| W11 | **CHIUSO** | "cohorts that entered by 2010": corretto (Costa Rica 2011 ha t max = 4; Peru 2010 entra nel bin ≥5 con il solo 2015). "dominated by the two early ones (Bangkok 2002 and ASEAN 2005)": plausibile (5 + 11 destinazioni su 23), nessuno script pesa le coorti. Residuo di sostituzione: resta un "---" orfano ("…ASEAN 2005) --- and it disappears…"), grammaticalmente accettabile ma è il trattino di chiusura del vecchio inciso. |
| W12 | **CHIUSO** (fonte da annotare) | Due tabelle eliminate, nessun `\ref` orfano (verificato su tutte le etichette). Le due righe nuove di `tab:sumstats` (3,773,498 / 8.974 / 9.029 / 1.957 / [0, 20.7]; 12.13 / 3 / 43.64 / [1, 8,946]) sono identiche a quelle della tabella eliminata. **Nessun CSV nel repo le produce**: `paper_v4/sumstats_collapsed.csv` ha N=0 per ln_export; le medie 0.669/1.438/0.084/0.140 sì, le righe ȳ e n no. Non contraddette, non tracciabili (K1). Nota "Thirty observations…" ✓ (45,781,211 − 45,781,181 = 30). §3.1 "Figure~\ref{fig:map} maps them" ✓. Follow-up §7.3 (spiegazione 11.5/8.4 e 7.0/14.0 nella nota di `tab:descriptives`): i numeri compaiono nella tabella stessa ✓. |
| W13 | **CHIUSO** | grep Macau/Korea Rep./S.~Korea/Laos, PDR/HongKong: 0 occorrenze in tex e tabelle. Colonna "Dataset code" ✓. |
| W14 | **CHIUSO** | Le due occorrenze sostituite. L'unico "the authors" residuo (riga 138) si riferisce a Baier–Bergstrand: corretto. |
| W15 | **CHIUSO MALE** | A5/A6: sforamenti azzerati ✓. A13: sforamento orizzontale azzerato ✓, ma ora la tabella **sfora verticalmente** ("Float too large for page by 76.45pt", vedi §4.1): il fix W15 + le due righe di W18 hanno spostato il problema da larghezza ad altezza. |
| W16 | **CHIUSO** | "five pollution-intensive sectors" ✓. |
| W17 | **CHIUSO** | "doubles or triples when one of two destinations is dropped" ✓ (Australia ×2.9, Corea ×2.0 dal CSV leave-one-out). Nel summary riga 192 resta "nearly triples" riferito alla sola Australia: corretto. |
| W18 | **CHIUSO** (con regressione di layout) | Sei numeri verificati. Panel A deep/shallow da `stability_fullpanel_reghdfe.csv` (groupname deepshallow, WB): −.0022157 (.0035067) [.534]; −.0034386 (.0028862) [.246]; N 5,262,293; r2 .88391 ✓. Panel B incl. HK/MO da `tripledd_full_alldepvars_reghdfe_inclHKMO.csv` (TREND, ln_export): −.0001091 (.0009960) [.913]; −.0010053 (.0006301) [.112]; N 23,560,110; r2 .8706 ✓. Le parentesi quadre di A13 sono p asintotici (coerenti con tutte le altre righe), quindi il 0.534 di A13 e il 0.585 di Tab. 6 per la stessa cella non si contraddicono. |
| W19 | **CHIUSO** | App. B riga 1189: "lists the 25 destinations … including Hong Kong and Macao, which the main sample excludes" ✓. |
| N1 | **CHIUSO** | Quattro sostituzioni applicate. Nota: "In the first study of the trade effects…" afferma come fatto ciò che v4 attribuiva agli autori; l'abstract di Brandi et al. lo rivendica, quindi è difendibile. |
| N4 | **CHIUSO** | "collapsed-panel dirty coefficient has a bootstrap p-value of 0.07 and the full-panel one of 0.19": 0.072 e 0.185 ✓. |
| N8 | **CHIUSO** | Summary allineato su C4, W1, W3, W7, W11; ricompilato: 7 pagine, 0 undefined. Eredita il problema W3. |
| N9 | **CHIUSO** | `Results_v4_draft.*` e `Results_v5_draft.*` ora in `New/_legacy/docs/` (git status), assenti da `New/Paper/`. |
| K5 | **CHIUSO** | §3.1 righe 202-205. Verificato: `senza_144` (Timor-Leste) dirty −.0118732 vs baseline −.0118734; green −.0045688 vs −.0045685 — sesto decimale invariato su entrambi ✓. Il rimando a `tab:loo_new` mostra però solo il dirty a 4 decimali: il lettore non può verificare "no coefficient beyond the sixth decimal" dalla tabella. Accettabile. |
| K6 | **CHIUSO** | §3.3 riga 535: "median computed over the 25 PTA partners including Hong Kong and Macao; 16 deep and 9 shallow, of which 16 and 7 remain". `deepshallow_diagnostics.txt`: 25 trattati, mediana 6.00, 16/9 ✓; HK (4) e Macao (5) sono shallow, quindi 7 nel baseline ✓. Nota Tab. 6 aggiunta ✓. |

---

## 4. Regressioni introdotte dai fix

### 4.1 Due tabelle sforano la pagina (nuovo, non dichiarato)

Il log attuale contiene due warning assenti dal log del backup:

```
LaTeX Warning: Float too large for page by 23.02406pt on input line 59.   ← Tabelle/tab_04_leaveoneout.tex
LaTeX Warning: Float too large for page by 76.45097pt on input line 75.   ← Tabelle/tab_A13_robustness_full.tex
```

Il report di implementazione dichiara "5 Overfull \hbox, gli stessi preesistenti" ed è vero, ma ha guardato solo gli `Overfull \hbox`. Ho renderizzato le due pagine:

- **Tab. 9 (leave-one-out, p. 30):** la legenda delle stelle, ultima riga delle note, si sovrappone al numero di pagina. Causa: FIX C2 (etichetta su tre righe con `p{3.2cm}` + nota più lunga). Effetto collaterale: l'etichetta è giustificata a blocco su 3.2 cm e viene fuori "Excluding    the    three / deepest (Peru, Switzer- / land, Korea)". Fix: `\raggedright` dentro la `p{}` e `\scriptsize` → dimezzare `\arraystretch`, oppure spostare le due note "Note on inference"/"How to read" (già ridondanti col testo) fuori dalla tabella.
- **Tab. 27 (A13, p. 67):** il numero di pagina "67" è stampato sopra la nota *Common support* e l'ultima nota ("Standard error in parentheses, p-value in brackets. * p<0.10…") è **tagliata** dal bordo inferiore. Causa: FIX W18 (due righe da tre linee ciascuna) + nota `a` di FIX W15. Fix: `\scriptsize` al posto di `\footnotesize`, oppure `[p]` e `\renewcommand{\arraystretch}{0.9}`.

### 4.2 Nota di Tab. A16 che dichiara una fonte diversa da quella dei numeri (W6)

Descritta in §3. È la categoria di errore che ha generato C3 e C6: nota scritta a mano che non corrisponde al contenuto. Prima del fix la tabella era internamente coerente (tutta R); ora è mista e dichiara di essere Stata.

### 4.3 Imprecisioni nella nuova nota di Tab. A7 (C3)

- "drops the one treated destination (Timor-Leste)" e "differ … only at the fourth decimal". In 54.do riga 83 il filtro è `drop if missing(DESTA_depth_index) & WB_EP_Depth > 0`: elimina le celle *trattate* di Timor-Leste e tiene quelle pre-2005, che restano nel campione come non trattate. Per questo i cluster sono 228 in entrambe le colonne (CSV `eventstudy_twfe_stata_desta.csv`: nclust 228, nobs 3,677,333) mentre la nota lascia intendere che una destinazione sparisca. E a t=4 la tabella mostra −0.039 vs −0.040 (CSV: −.03936 vs −.03959): la differenza è al quarto decimale come dice la nota, ma l'arrotondamento la rende visibile al terzo. Riformulare: "drops the post-entry cells of Timor-Leste (the one treated destination not covered by DESTA); the cluster count is unchanged and coefficients differ at most at the fourth decimal".

### 4.4 C5: benchmark Brandi con aritmetica incoerente

Il paper dice ora in tre punti "one quarter" (intro riga 126, conclusione riga 1125, App. MDE riga 1441) e nell'unico punto in cui mostra i numeri usa 3.2% (collassato) invece del 3.55% (full panel) da cui il quarto deriva. Proposta di riscrittura della frase in App. MDE:

> For scale, \citet{brandi2020} find that one additional green-liberalisation provision raises the green export share by about 17 percent (0.157 log points) in their aggregate bilateral panel. The full-panel upper bound of Section~\ref{sec:fullpanel}, 3.5 percent per provision, is roughly one quarter of that figure; the collapsed-panel bound in Table~\ref{tab:mde}, 3.2 percent, roughly one fifth.

Così intro e conclusione ("one quarter") restano vere, il numero di Brandi è attribuito correttamente (il +17% è loro, i 0.157 log points sono la conversione fatta in 45.R), e il lettore vede entrambi i bound. Da non fare: "3.55 percent … one quarter" senza dire che è il full panel, perché la frase sta nell'appendice del collassato.

Nota a margine, preesistente: la frase di riga 1439 "It is estimated on the collapsed panel, where the standard errors are larger than in the full panel, so the bounds it reports are looser than those quoted in Section 5.1" è vera per gli SE asintotici (0.0070 vs 0.0039) ma falsa per il bound bootstrap, che nel collassato è più stretto (3.18% < 3.55%). Fuori mandato, ma la riscrittura sopra la rende evidente.

### 4.5 Regressioni minori di stile (non bloccanti)

- §4.2: ipotesi nulla enunciata due volte (W7, §3).
- §5.5: "---" orfano dopo la parentesi (W11, §3).
- Tab. 8 nota: frammento "Adjusted R²;" (W5, §3).
- Tab. 9: etichetta giustificata su tre righe (§4.1).

### 4.6 Cose controllate e a posto

Frasi troncate o senza verbo: nessuna (letto ogni hunk del diff, 47 KB). `\ref` orfani: nessuno; etichette eliminate `tab:sumstats_collapsed`/`tab:sumstats_combined`: 0 riferimenti residui. Stringhe stantie (13,728; 71.5; six times; twenty green; third of; Macau; Korea Rep.; four earliest; six pollution; nearly triples nel corpo; allows to; EP enter; High EP depth; indifferent; shuffles; classification revisions): 0 occorrenze nel corpo del paper (il "shuffles" residuo a riga 679 è "reshuffles EP profiles", corretto; "nearly triples" a riga 99 è in un commento `%`). Compilazione: 75 pagine, 0 undefined, 0 multiply defined, 0 warning biber, 5 Overfull \hbox identici al backup per posizione e ampiezza, nessuno in A5/A6/A13.

---

## 5. Colonna "Bootstrap CI half-width (1 s.d.)" di Tab. A16

**Il ragionamento del §7 del report di implementazione è sbagliato nella premessa, quindi i valori attesi sono sbagliati.** Il correttore ha calcolato il rapporto Stata/R delle semi-ampiezze usando `Tables/wcb_collapsed.csv` (versione R attuale) come base, e ha riscalato i 5.90% ecc. della tabella. Ma la tabella non deriva da quel file: il report generato dallo script, `New/Output/Diagnostics/33_mde_equivalence.md` (12 agosto), mostra IC [−1.77%, 3.19%] e semi-ampiezza 0.0248, mentre il CSV R attuale ha [−1.82%, 3.16%] e semi-ampiezza 0.02493. Il CSV R è stato rigenerato dopo il 12 agosto; la tabella è rimasta al run vecchio. Il rapporto 1.0095 misura quindi lo scarto tra Stata e un file che la tabella non ha mai usato.

Il calcolo esatto non richiede il `.fst`: serve solo la SD pesata dei regressori, che è deterministica e sta nel report del 12 agosto (WB_EP_Depth 2.3827; TREND_EP_Count 8.1645; non dipende dal bootstrap). Semi-ampiezza Stata × SD:

| Riga | In tabella | Semi-amp. Stata (per unità) | × SD | Corretto | Stima del report §7 |
|---|---|---|---|---|---|
| WB Green | 5.90% | (0.0318412 + 0.0184997)/2 = 0.025170 | × 2.3827 | **6.00%** | ~5.96% (sbagliato) |
| WB Dirty | 2.40% | (0.0184596 + 0.0016918)/2 = 0.010076 | × 2.3827 | **2.40%** | ~2.44% (sbagliato) |
| TREND Green | 3.62% | (0.0017200 + 0.0071089)/2 = 0.004414 | × 8.1645 | **3.60%** | ~3.62% (sbagliato) |
| TREND Dirty | 3.53% | (0.0031394 + 0.0055866)/2 = 0.004363 | × 8.1645 | **3.56%** | ~3.57–3.58% (impreciso) |

L'arrotondamento della SD a 4 decimali sposta i risultati di meno di 0.005 punti: i quattro valori sono stabili alla seconda cifra. Le colonne "Asymptotic std. error" (0.0070 = .0069576 Stata ✓) e "MDE (1 s.d.)" (4.64% = 2.8 × .0069576 × 2.3827 ✓) sono già coerenti con Stata e non cambiano.

**Da scrivere in tab_A16, in un solo passaggio:** semi-ampiezze 6.00 / 2.40 / 3.60 / 3.56 e IC [−1.85, 3.18] / [−1.85, 0.17] / [−0.17, 0.71] / [−0.31, 0.56]. A quel punto la nota "Bootstrap intervals from the Stata implementation" diventa vera e la tabella coincide con ciò che `33_mde_equivalence.R` produrrebbe con i percorsi nuovi. La nota "rules out effects larger than about 3% per provision" resta vera.

**Sul cambio di percorso in `33_mde_equivalence.R`:** corretto. `out_path()` inserisce il suffisso di variante prima dell'estensione e i quattro gemelli esistono in Tables_Stata (`wcb_collapsed{,_desta,_inclHKMO,_inclHKMO_desta}.csv`, idem `tripledd_collapsed`); le colonne usate (`treat`, `term`, `se`, `conf_low`, `conf_high`) e i valori di `term` (`WB_EP_Depth:env_good`, `ep_green`) coincidono nei due file. Il commento in testa è accurato. Unico rilievo: lo script ora legge output Stata dentro una pipeline R (`run_pipeline.R`), quindi va eseguito dopo 52/63 e non prima; da annotare nel README, non nel codice.

---

## 6. Problemi preesistenti ancora aperti (fuori mandato, solo elenco)

- **K1** — le 27 tabelle di `paper_v4/Tabelle/` non sono generate da script. Questo audit lo conferma di nuovo: le righe collassate di `tab:sumstats` (W12) e le colonne 4-5 di A16 (§5) non hanno un CSV nel repo che le produca, e A16 era rimasta a un run del 12 agosto senza che nessuno se ne accorgesse.
- **K2** — `68_subindices_fullpanel.do`: ramo `drop_unmeasured` vuoto; Tab. 8 Panel B N = 21,519,511 contro 21,517,666 di Tab. 3 Panel B.
- **K3** — `env_good` stantio nel `.fst`.
- **K4** — mappatura posizionale TREND senza guardia in `02.R`.
- **N6** — legenda della mappa.
- **N7** — `renv.lock` assente.
- (preesistente, notato in §4.4) riga 1439: "bounds looser than Section 5.1" falso per il bound bootstrap.

---

## 7. Verdetto finale

**Non ancora circolabile così com'è; circolabile dopo le sei correzioni sotto, tutte di editing `.tex`, stimate in 30-45 minuti.** Nessuna richiede stime. In ordine di visibilità per un lettore:

1. **Tab. A13 e Tab. 9 sforano la pagina** (§4.1): riga tagliata e numero di pagina sovrapposto. Un referee lo vede alla prima sfogliata.
2. **Tab. A16** (§5, W6): aggiornare tre IC e quattro semi-ampiezze con i numeri della tabella in §5, così la nota "from the Stata implementation" diventa vera.
3. **App. MDE, benchmark Brandi** (C5, §4.4): riscrivere la frase come proposto; altrimenti "3.2 percent … one quarter" è un errore aritmetico esposto.
4. **§5.4 footnote, "0.90 with World Bank non-environmental depth"** (W9): nessuno script lo produce e il ricalcolo dà 0.70. Togliere il numero (basta "correlates 0.87 with the aggregate TREND count") o produrlo con uno script e citarlo.
5. **Tabella di composizione, riga "Cooperation (TREND)"** (W9): indice mai costruito né stimato. Togliere.
6. **"The only green coefficient in the robustness battery to survive the bootstrap"** (W3, tre occorrenze): contraddetto dalla footnote di §5.4 (RegSpace × green, bootstrap p = 0.045). Restringere a "among the robustness checks of Appendix G".

Facoltativi ma da fare prima di una submission: nota di `tab:mechanism` con "14 distinct agreements (Bangkok/APTA counted once)"; nota di Tab. A7 riformulata come in §4.3; le quattro ridondanze di stile in §4.5; la frase preesistente di riga 1439.

**Giudizio sul lavoro di correzione.** Le 34 sostituzioni sono state eseguite con cura: ancore complete, nessuna frase spezzata, tutti i numeri copiati dai CSV giusti, scelte discrezionali dichiarate e ragionevoli. Le quattro cose chiuse male hanno un tratto comune: il correttore ha preso per buone affermazioni della roadmap senza aprire il file ("le altre tre righe coincidono", "nessun altro coefficiente verde ha p < 0.05", "0.90 era in v3"), e ha controllato il log per `Overfull \hbox` ma non per `Float too large`. È la stessa lezione della voce del 2026-09-06 in MISTAKES.md, spostata dal CSV alla roadmap: un documento che prescrive un numero non è la fonte del numero.
