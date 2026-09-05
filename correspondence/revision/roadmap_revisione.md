# Roadmap di revisione — paper_v3.tex

File target: `New/Paper/paper_v3/paper_v3.tex` (1482 righe).
Ogni task ha: cosa fare, dove farlo (righe), e come.

---

## 0. Modifiche globali (tutto il documento)

### 0.1 Pacchetto footnote spacing
**Azione:** Aggiungere nel preambolo LaTeX il pacchetto `\usepackage[bottom]{footmisc}`. Questo spinge le note a piè di pagina in fondo alla pagina con spazio bianco tra testo e note.

### 0.2 Formattazione tabelle e figure
**Azione su tutte le tabelle/figure del documento:**
- Caption (`Table X:` / `Figure X:`) va SOPRA la tabella/figura
- Aggiungere `\vspace{6pt}` tra caption e contenuto
- Aggiungere `Notes:` sotto ogni tabella (se non presente)
- Aggiungere fonte dati (es. "Source: Chinese Customs Data, World Bank DTA Database") — mai nomi di file CSV
- Nelle figure: rimuovere il titolo generato da R/Stata (usare solo il caption LaTeX)
- Uniformare font dei grafici (Times o simile, compatibile col documento)
- Usare colori b/n-friendly (scale di grigio, pattern, tratteggi) in TUTTE le figure
- Tabelle troppo larghe: ridurre con `\resizebox` o `\small`/`\footnotesize`

### 0.3 Nomi variabili e funzioni
**Azione:** Cercare nel testo (NON nelle equazioni/tabelle) occorrenze di nomi di variabili dataset (es. `env_good`, `dirty_good`, `n_pdt`, `AD_pdt`). Sostituire con la denominazione dalla Table 2 (variable descriptions). Stessa cosa per nomi di funzioni R/Stata (`feols`, `reghdfe`, `boottest`, `fixest`): rimuoverli dal testo, accettabili SOLO in note a piè di pagina se strettamente necessario.

### 0.4 Linguaggio LLM
**Azione:** Fare un passaggio completo del documento cercando ed eliminando espressioni tipiche da LLM. Lista non esaustiva di espressioni da cercare e sostituire:
- "warrants" → riformulare
- "bears noting" → riformulare
- "may obscure" → riformulare
- "mechanism-bearing" → MAI usare
- "mirror position" → riformulare
- "sits comfortably" → riformulare
- "is instructive" → riformulare
- "trickles in" → riformulare
- "In light of these findings" → riformulare
- "The key difference lies in" → riformulare
- Qualsiasi frase che suoni come un sommario generato

### 0.5 Variare terminologia EP
**Azione:** Non usare sempre "EP" o "EP content". Alternare con "environmental provisions", "environmental content", "environmental clauses" per variare.

---

## 1. Sezione 2: Related Literature

### 1.1 Accorciamento generale
**Azione:** Accorciare la sezione del ~15-20%. Non tagliare drasticamente, ma eliminare ridondanze.

### 1.2 Riga ~95: "that accounts for the endogeneity..."
**Azione:** Sostituire con espressione più breve, es. "that addresses PTA endogeneity" o "that controls for self-selection into agreements".

### 1.3 Riga ~97: "a finding substantially larger..."
**Azione:** Aggiungere "as the authors themselves note" prima della frase, OPPURE riformulare in modo generico se non verificabile nel paper originale.

### 1.4 Riga ~100: "Surveying this literature more broadly"
**Azione:** Sostituire con "Another survey from [autore]..." Togliere "more broadly".

### 1.5 Riga ~105: Mattoo, Mulabdic, and Ruta (2022) + DESTA
**Azione:** Riformulare il paragrafo per chiarire che Mattoo et al. usano il WB DTA database, NON DESTA. DESTA è citato come framework indipendente. Separare i due concetti in frasi distinte.

### 1.6 Riga ~112: "consistent with the pollution haven prediction..."
**Azione:** Aggiungere breve spiegazione: "...the econometric difficulties — chiefly endogenous abatement costs and omitted environmental regulation — that limited earlier studies."

### 1.7 Riga ~115: "this tradition"
**Azione:** Sostituire con "the pollution-intensive sector taxonomy" o equivalente esplicito.

### 1.8 Riga ~118: "a handful of empirical studies have produced results..."
**Azione:** Riscrivere. Proposta: "A few empirical studies point in the same direction" o simile. Togliere "In light of these findings".

### 1.9 Riga ~122: "The contrast between these two sets of findings is instructive. The key difference lies in..."
**Azione:** Fondere in un periodo unico e più semplice. Es: "The difference between these results comes down to how each study treats provision heterogeneity: [resto]."

### 1.10 Riga ~124: "may obscure"
**Azione:** Sostituire. Usare "hide", "mask", "conflate" o simile.

### 1.11 Riga ~126: Baghdadi, Martinez-Zarzoso, and Zitouna (2013)
**Azione:** TOGLIERE l'intera frase. Non aggiunge abbastanza alla narrazione.

### 1.12 Riga ~128-131: Paragrafo su Copeland & Taylor + Shapiro
**Azione:** Accorciare. Tenere solo la prima frase su Copeland & Taylor. TOGLIERE da "Shapiro (2021)" fino alla fine del paragrafo (la menzione dei suoi dati CO2 può restare in una nota a piè di pagina nella sezione 5 dove il robustness check è effettivamente usato).

### 1.13 Riga ~133-137: "These studies provide suggestive evidence..."
**Azione:** O TOGLIERE del tutto, oppure riscrivere in modo molto più cauto. Se si tiene, proposta: "Earlier work on Chinese EP effects uses specifications that may not fully separate the effect of environmental provisions from other agreement features." Niente più dettagli sui FE mancanti — è rischioso dire che gli altri paper sono sbagliati.

### 1.14 Riga ~138-141: "The present paper addresses this concern..."
**Azione:** Se si tiene 1.13, riscrivere in modo più cauto e generico: "This paper uses a triple-difference design with high-dimensional fixed effects to isolate the differential response of green and dirty products within firm-destination-year cells." Se si toglie 1.13, togliere anche questo.

### 1.15 Riga ~143-155: Confronto con Zhu & Sun (2026)
**Azione:** Riesaminare alla luce delle decisioni su 1.13-1.14. Se il tono è cautela, anche qui essere cauti. Aggiungere una nota che il confronto è "according to the WB and TREND codings used in this study". Il fattore 2.7 è verificato (−0.0119/−0.0044 = 2.70).

### 1.16 Riga ~157: "mirror position"
**Azione:** Sostituire con "symmetric result", "analogous finding", o simile.

---

## 2. Sezione 3: Data

### 2.1 Riga ~158: "two types of data"
**Azione:** Sostituire con "two data structures".

### 2.2 Spostare Table 2 (variable descriptions)
**Azione:** Spostare Table 2 (attualmente dopo la descrizione dei dati) all'INIZIO della sezione 3, subito dopo il primo paragrafo introduttivo. Aggiungere frase: "Table 2 summarizes the notation used throughout; all variable references below follow this table." Questo permette di usare la notazione $EP^{WB}_{dt}$ ecc. nel resto della sezione.

### 2.3 Riga ~161: Numerosità dataset
**Azione:** Riformulare. Prima dare il totale INCLUSI HK e Macao, poi dire che il campione principale esclude HK e Macao. Attualmente mescola le due cose.

### 2.4 Riga ~163: "4,450 and 4,766 HS6 product codes..."
**Azione:** Togliere "(the count shifts with the 2007 HS revision)". Se il paper quality (Caselli et al.) non menziona questo dettaglio, non lo menzioniamo nemmeno noi.

### 2.5 Riga ~166: "and the triple-difference design exploits..."
**Azione:** TOGLIERE. È già stato detto nell'introduzione e nella strategy.

### 2.6 Riga ~168-175: Conteggio destinazioni — RISCRIVERE
**Azione:** Riscrivere completamente questo passaggio. Struttura proposta:
1. "China signed PTAs covering 25 destination economies (Table 1). Excluding Hong Kong and Macao, 23 enter the main sample."
2. "Ten of these are parties to the ASEAN-China agreement. Since nine of them share the same EP profile (Laos also entered through the Bangkok Agreement, and Singapore signed a separate bilateral), the 23 destinations yield 13 distinct EP profiles."
3. TOGLIERE la frase "these eleven destinations provide a single source of treatment variation".
NO "eleven" vs "nine" confusione.

### 2.7 Riga ~176-178: "There is no treatment reversal..."
**Azione:** Accorciare a: "There is no treatment reversal in the sample." TOGLIERE il resto ("estimators designed for treatments that switch on and off...").

### 2.8 Riga ~179-180: "Entry into the treated group is concentrated..."
**Azione:** Riscrivere in modo meno LLM. Es: "Most treated destinations enter between 2002 and 2005 (the ASEAN accession); the remainder join one at a time in later years."

### 2.9 Riga ~183: "EPWB" prima della Table 2
**Azione:** Risolto da 2.2 (spostamento Table 2 all'inizio). Se Table 2 è già stata introdotta, la notazione è legittima.

### 2.10 Riga ~185: TREND database
**Azione:** La prima volta che si nomina TREND, scrivere il nome completo: "TREND (TRade and ENvironment Database)" — in nota a piè di pagina se preferito.

### 2.11 Figura 1: colori e legenda
**Azione:**
- Sostituire colori con scala b/n-friendly (grigio scuro, grigio chiaro, nero, tratteggio)
- Spostare legenda FUORI dal grafico (sotto o a lato)
- Rimuovere griglia di sfondo

### 2.12 Figura 2
**Azione:** TOGLIERE dal paper.

### 2.13 Figura 3: colori
**Azione:** Colori b/n-friendly. Ingrandire la figura (`width=\textwidth` o simile).

### 2.14 Riga ~191: "from the General Administration of Customs of China"
**Azione:** Uniformare col paper quality. Usare "Chinese Customs Office" o "General Administration of Customs of China" — entrambi accettabili, ma scegliere uno e usarlo ovunque.

### 2.15 Riga ~193: Dettaglio HS revision
**Azione:** Togliere "Export-value continuity checks around the 2007 vintage revision find no suspicious breaks on any translated code." — se il paper quality non ne parla, nemmeno noi.

### 2.16 Riga ~195: Nota a piè di pagina sulla concordanza ISIC-HS
**Azione:** Rendere più lineare. Proposta: "Products are mapped from ISIC Rev.2 to HS1996 using the official WITS/UNSD concordance, yielding 1,139 dirty HS6 codes. Seventeen codes that appear in both lists are assigned to the green category; the two lists are mutually exclusive."

### 2.17 Riga ~199: "These shares are consistent with..."
**Azione:** O arricchire con un grafico (pie chart o bar chart delle shares) oppure TOGLIERE il passaggio.

### 2.18 Riga ~203: "The remaining neutral products..." + trattamento
**Azione:** TOGLIERE la frase "Of all observations in the main panel, 20.3% involve a destination..." — è fuori contesto qui.

### 2.19 Riga ~206: "as compiled in Sauvage (2014)"
**Azione:** Sostituire "compiled" con "classified", "listed", "catalogued", o simile.

### 2.20 Riga ~207: "concorded"
**Azione:** Sostituire con "harmonized", "mapped", o "converted".

### 2.21 Riga ~210: Nota a piè di pagina HS2012
**Azione:** Aggiungere la % di osservazioni affected dalla perdita di distinzione HS2012. Verificare nel codice.

### 2.22 Riga ~215: "Of these, export value is the primary outcome..."
**Azione:** O togliere del tutto o accorciare a: "Export value is the primary outcome; quantity and unit value serve as decomposition checks."

### 2.23 Figura 4: commento e formato
**Azione:**
- Rivedere il commento nel testo (~riga 220). Controllare che sia coerente con i dati della figura.
- Colori b/n-friendly
- Rimuovere griglia di sfondo

### 2.24 Riga ~224: "The analysis rests on four estimation objects..."
**Azione:** Rivedere il posizionamento della citazione a Table 6. Se Table 6 descrive i pannelli, ha senso qui. Ma il testo deve collegarla meglio.

### 2.25 Riga ~227: FE sets nella sezione Data
**Azione:** I FE set sono menzionati qui per confronto collapsed vs full panel. È accettabile, ma aggiungere una frase di transizione: "The fixed-effects structure is detailed in Section 4; here we note the key difference between panels:" — poi collapsed ha pd+dt+pt, full panel ha fpd+fdt+pt.

### 2.26 Riga ~230: Nota computazionale OLS
**Azione:** Rendere più generica: "a single OLS estimation on the full panel is computationally intensive, and the bootstrap requires several hours per specification." Togliere "30 minutes" e "9,999 replications".

### 2.27 Riga ~232: Zero-fill PPML
**Azione:** Spostare in nota a piè di pagina. Sostituire "exceed available memory" con "making estimation computationally infeasible".

### 2.28 Riga ~235-241: Within-firm share panel
**Azione:** TOGLIERE del tutto. Se si decide di tenerlo, ridurre a una sola frase in nota a piè di pagina.

### 2.29 "C-qualcosa" nei nomi dei subsample
**Azione:** Aggiungere una frase che spiega la convenzione: "The prefix C- denotes the control-group variant" — oppure rinominare in modo più intuitivo (es. "HS4-restricted", "Overlap", "CEM-matched", "Deep-only").

### 2.30 Riga ~245: C-overlap spiegazione Eckel & Neary
**Azione:** Riscrivere in modo più chiaro. Proposta: "C-overlap retains only products exported to both treated and untreated destinations, ensuring that the identifying variation does not rely on products traded exclusively with one group. This guards against the possibility that multi-product firms reallocate across products within the same HS4 family when trade costs fall (Eckel and Neary, 2010)."

### 2.31 Riga ~248: Restrizione C-overlap binding
**Azione:** TOGLIERE dal testo principale. Se si vuole tenere, spostare in nota a piè di pagina.

### 2.32 Riga ~251: CEM "19 treated and 40 control"
**Azione:** VERIFICARE nel codice CEM che 19 treated e 40 control sia corretto. Se sì, tenere. Se no, correggere.

### 2.33 Riga ~253: "The few-cluster inference battery..."
**Azione:** TOGLIERE. Anticipazione non necessaria qui.

### 2.34 Riga ~255: "The reading rule that applies throughout..."
**Azione:** Riscrivere più semplicemente: "A genuine composition effect should survive every change to the comparison group; an artifact will move when the comparison changes."

---

## 3. Sezione 4: Empirical Strategy

### 3.1 Riga ~260-263: Paragrafo iniziale 3 challenges
**Azione:** Riscrivere più scorrevole. Proposta: "Three challenges shape the identification strategy: countries select into agreements, environmental and overall agreement depth correlate strongly, and the number of independent treatment units is small."

### 3.2 Riga ~265: "A natural first step would be to estimate..."
**Azione:** Sostituire con "A starting point is to estimate..." o "One could first estimate...".

### 3.3 Righe ~268-280: Saturation ladder exercise
**Azione:** TOGLIERE entrambi i paragrafi (riga 168-180 delle istruzioni, che corrispondono a righe ~700-730 del tex circa). Togliere sia la discussione della saturation ladder sia il caveat su di essa. In appendice B la tabella può restare, ma il testo nella sezione principale va eliminato.

### 3.4 Righe ~282-285: "One consequence is that this design cannot supply evidence..."
**Azione:** Riscrivere più semplice e scorrevole. Proposta: "This design cannot show that EP-heavy agreements affected aggregate trade levels — that effect is confounded with the agreement itself. The evidence speaks only to the composition margin."

### 3.5 Riga ~287: "OLS gives equal weight to each observation..."
**Azione:** La frase è corretta ma va spiegata meglio in nota a piè di pagina. Aggiungere nota: "Each observation in the panel is a firm-product-destination-year record. A firm exporting 100 products contributes 100 observations and receives correspondingly more weight in the OLS average than a single-product firm."

### 3.6 Riga ~289: Rajan & Zingales (1998) reference
**Azione:** TOGLIERE il confronto con Rajan & Zingales. Compare non è chiaro e potrebbe confondere il lettore.

### 3.7 Riga ~291: Esempio ipotetico Korea 2012
**Azione:** Sostituire con spiegazione diretta dei FE. Proposta (ispirata al paper quality): "Firm-destination-year fixed effects $\theta_{fdt}$ absorb all time-varying bilateral factors — entry into new markets, aggregate demand shifts, exchange rate movements — that affect a firm's exports to a destination uniformly across products."

### 3.8 Riga ~293: Secondo riferimento a Rajan & Zingales
**Azione:** TOGLIERE.

### 3.9 Riga ~295: θ_pt e non θ_fpt
**Azione:** Aggiungere nota a piè di pagina: "Firm-product-year fixed effects would be computationally infeasible given the panel size. Product-year effects are sufficient because firm-product shocks that do not vary across destinations are already absorbed by the combination of $\theta_{fpd}$ and $\theta_{fdt}$."

### 3.10 Righe ~297-303: "Selection into agreements does not disappear..."
**Azione:** Accorciare e semplificare. Proposta: "Selection into agreements is not eliminated but substantially narrowed: $\theta_{fdt}$ absorbs the level effect, so selection contaminates $\beta_1$ only through differential pre-trends in green versus neutral exports across destinations. Section 5 tests this directly with destination-specific trends and finds no change in the coefficient."

### 3.11 Riga ~306: Correlazione TREND-WB
**Azione:** Mettere in nota a piè di pagina: "The WB and TREND EP indices correlate at $\rho = 0.91$ (pre-singleton removal)."

### 3.12 Riga ~308: Nota a piè di pagina omitted variable bias
**Azione:** Riscrivere in modo comprensibile. Proposta: "If the non-environmental depth control imperfectly measures the true confounding depth, the bias in $\hat{\beta}_1$ depends on (i) how much EP depth correlates with the measurement error in the depth control, and (ii) the effect of the omitted component on composition. Since EP and total depth are positively correlated ($\rho = 0.91$), any bias pushes $\hat{\beta}_1$ toward the effect of overall agreement depth. With a null result on the green margin, this means the true effect is zero or negative — the null is conservative."

### 3.13 Riga ~310: VIF
**Azione:** Menzionare il VIF nel testo principale solo come concetto: "Collinearity between EP depth and non-environmental depth is substantial (VIF = 5.8)." Dettagli (confronto con Brandi et al., definizione VIF) restano in nota a piè di pagina.

### 3.14 Righe ~313-325: Clustering discussion
**Azione:** Accorciare. Proposta: "EP depth varies across destinations and years but changes in only three destinations during the sample period, making treatment effectively destination-level. Standard errors are therefore clustered by destination (Abadie et al., 2023), treating all observations from the same country as a single cluster."

### 3.15 Riga ~327: "would bias β1 toward a spurious positive"
**Azione:** Sostituire "a spurious positive" con "positively".

### 3.16 Riga ~329: "Because β1 is an imprecise zero..."
**Azione:** TOGLIERE o riscrivere diplomaticamente. Proposta se si tiene: "With a null coefficient, any upward bias from ASEAN dominance would imply the true effect is zero or negative."

### 3.17 Riga ~331: "With a null result, this distinction is immaterial..."
**Azione:** Riscrivere. Proposta: "Since the coefficient is near zero across specifications, the choice of weighting scheme does not affect the conclusion."

### 3.18 Righe ~333-336: Singleton removal paragraph
**Azione:** TOGLIERE o ridurre a una frase: "The full panel is estimated after iterative singleton removal (Correia, 2017), reducing the sample from 45.8 to 21.5 million observations."

### 3.19 Righe ~337-340: Risultati collapsed vs full panel
**Azione:** SPOSTARE alla sezione 5. Questi sono risultati, non metodologia.

### 3.20 Righe ~339-340: "Approximately three-fifths..."
**Azione:** SPOSTARE alla sezione 5. Aggiungere il calcolo esplicito: "(−0.0119 − (−0.0044)) / −0.0119 = 0.63".

### 3.21 Righe ~342-345: Wild cluster bootstrap — dettaglio B=9,999
**Azione:** Accorciare. Togliere il dettaglio su ±1 signs e B = 9,999. Proposta: "The wild cluster bootstrap (Cameron, Gelbach, and Miller, 2008; Roodman et al., 2019) provides inference that does not rely on asymptotic critical values and is appropriate for settings with few clusters."

### 3.22 Righe ~346-351: Frisch-Waugh nel collapsed panel
**Azione:** Riscrivere in modo più chiaro. Proposta: "In the full panel, the bootstrap runs directly after estimation. In the collapsed panel, fixed effects are first partialled out via the Frisch-Waugh theorem, reducing the bootstrap to a low-dimensional regression. Point estimates are identical; only computational cost changes."

### 3.23 Nota a piè di pagina: approssimazioni bootstrap collapsed
**Azione:** Riscrivere più chiaramente. Aggiungere: "These approximations do not affect the substantive conclusions."

### 3.24 Righe ~357-361: Permutation test — dettaglio da accorciare
**Azione:** Spostare in nota a piè di pagina la spiegazione di PERCHÉ solo treated destinations. Tenere nel testo solo: "The permutation test reshuffles EP profiles across the 23 treated destinations, testing whether the specific EP content — not the agreement itself — matters for composition."

### 3.25 Righe ~362-371: Permutation distribution — ASEAN duplicati
**Azione:** TOGLIERE tutto questo paragrafo e la nota a piè di pagina 10. È dettaglio tecnico eccessivo per la sezione strategy.

---

## 4. Sezione 5: Results — RIORGANIZZAZIONE MAGGIORE

### 4.0 Struttura proposta
**Azione:** Riorganizzare la sezione 5 secondo questa struttura:
1. **5.1 Baseline results** (collapsed + full panel, WB e TREND) — con tabelle
2. **5.2 Robustness: control groups** (C-prod-HS4, C-overlap, CEM, Deep-only)
3. **5.3 Robustness: full panel with controls** (tariff, HHI, AD)
4. **5.4 Dynamics and parallel trends** (event study + Sun-Abraham)
5. **5.5 The dirty margin** (inferenza + permutazione + destination fragility)
6. **5.6 Provision bundling** (sub-indici, collinearità, limiti)
7. **5.7 Extensive margin** (PPML)
8. **5.8 Destination-specific trends**
9. **5.9 Alternative outcomes** (quantity, unit value) — dare più spazio
10. **5.10 Continuous pollution intensity** (CO2)
11. (TOGLIERE: trimming, TREND vs WB switch, within-firm share)

### 4.1 Pulizia generale sezione 5
**Azione:** In TUTTA la sezione:
- Ogni risultato DEVE avere un riferimento a una tabella o figura
- Ogni tabella citata DEVE esistere
- Riscrivere in stile narrativo (prendere ispirazione dal paper quality per il flow)
- Togliere linguaggio LLM
- Ridurre elenchi di numeri: citare i più importanti, mandare il resto in appendice

### 4.2 Riga ~920: "The observed green coefficient sits comfortably..."
**Azione:** Riscrivere e aggiungere il p-value del permutation test. Proposta: "The permutation test yields p = 0.60 for the green coefficient, meaning 60% of placebo permutations produce a coefficient at least as large in absolute value."

### 4.3 Righe ~925-930: Brandi et al. benchmark
**Azione:** Spiegare meglio o TOGLIERE. Se si tiene: "Brandi et al. (2020) estimate that one liberal environmental provision raises the green share of developing-country exports by about 0.4 percentage points. In our metric (log export value), this corresponds to approximately +0.16 log points per provision, assuming a baseline green share of about 2.5%. Our 95% bootstrap confidence interval excludes effects of this magnitude."

### 4.4 Righe ~932-940: Bootstrap bound explanation
**Azione:** Riscrivere in modo comprensibile. L'attuale testo è incomprensibile. Proposta: "The bootstrap 95% confidence interval for the green coefficient spans [−X, +Y] log points. This means the design can rule out effects larger than [Y] in either direction, but is uninformative about smaller effects."

### 4.5 Righe ~943: "Across nine designs..."
**Azione:** Esplicitare. Aggiungere tra parentesi: "(collapsed panel with WB, collapsed panel with TREND, full panel with WB, full panel with controls, full panel excluding ASEAN, full panel including HK/Macao, C-prod-HS4, C-overlap, CEM)."

### 4.6 Sezione 5.3 Dynamics
**Azione:** Spiegare con chiarezza:
- La Figura 5 mostra coefficienti lead/lag dalla regressione con indicatori di tempo relativo (t−k, ..., t−1, t+1, ..., t+K) interagiti con green/dirty. Il riferimento è t = −1.
- Se i coefficienti pre-trattamento sono vicini a zero → parallel trends supportati.
- Citare la tabella corrispondente. Se manca, segnalarlo.

### 4.7 Figura 5
**Azione:** Aggiungere nel testo la spiegazione di come si produce/interpreta. Aggiungere equazione dell'event study o riferimento all'appendice.

### 4.8 Sun-Abraham estimator
**Azione:** Spiegare in modo semplice. Proposta: "The Sun and Abraham (2021) estimator addresses potential bias in staggered event studies by computing cohort-specific treatment effects and aggregating them with appropriate weights. The aggregated ATT (Average Treatment effect on the Treated) is −0.042 (p = 0.27) for the green gap and +0.073 (p = 0.28) for the dirty gap — both null, consistent with no treatment effect."

Aggiungere formula o riferimento ad appendice. Spiegare che ATT è calcolato sia per green che per dirty.

### 4.9 Riga ~1010: "the ATT is null across all three specification variants..."
**Azione:** Spiegare cosa sono le 3 varianti: (1) baseline, (2) finestra [−6,+5], (3) escludendo coorti 2014-2015.

### 4.10 Tabella 9
**Azione:** Verificare se esiste e se è citata. Se non è citata: o citarla dove serve, o toglierla.

### 4.11 Tabella 10 — full panel con controlli
**Azione:** Spiegare perché i controlli aggiungono informazione nonostante i FE. In nota a piè di pagina: "The controls (tariff, HHI, antidumping exposure) vary at the product-destination-year level, a dimension not absorbed by $\theta_{fdt}$ (which lacks the product dimension) or $\theta_{pt}$ (which lacks the destination dimension)."

### 4.12 Sezione 5.4: Dirty margin
**Azione:** Riscrivere completamente. Punti chiave:
- Il coefficiente dirty è significativo sotto asymptotics ma crolla sotto WCB e permutazione.
- La significatività dipende da 1-2 destinazioni (Australia con TotalDepth, Korea con DESTA).
- Conclusione: il dirty margin è un pattern descrittivo, non un effetto causale robusto.
Togliere linguaggio LLM. Citare tabelle.

### 4.13 Riga ~1035: "falls apart under scrutiny"
**Azione:** Sostituire con "does not survive robust inference".

### 4.14 Riga ~1040: Riassunto permutazione dirty
**Azione:** Mantenere ma accorciare a 2 frasi. Aggiungere riferimento tabella per "coarser permutation".

### 4.15 Riga ~1045: "They supply the variation..."
**Azione:** Riscrivere: "These destinations provide the identifying variation — removing them leaves too little information for a precise estimate, rather than revealing a different effect."

### 4.16 DESTA non re-introdotto
**Azione:** Aggiungere una frase prima dell'uso in sezione 5: "As an alternative depth control, we also use the DESTA index (Dür, Baccini, and Elsig, 2014), introduced in Section 2."

### 4.17 Riga ~1050: Spiegazione DESTA exercise
**Azione:** Riscrivere in modo più chiaro. Proposta: "The choice of depth control (WB non-environmental depth or DESTA) changes which destination's removal destabilizes the coefficient — with WB it is Australia, with DESTA it is South Korea. That the pivotal destination depends on a modeling choice unrelated to environmental content reinforces the fragility diagnosis."

### 4.18 Riga ~1060: "The honest reading is that the dirty margin is a false positive..."
**Azione:** Riscrivere più diplomaticamente. Proposta: "The dirty coefficient does not survive the same inference battery applied to the green margin. It is best interpreted as a descriptive pattern that cannot be distinguished from chance under robust inference."

### 4.19 Sezione 5.5 Provision bundling — semplificazione
**Azione:**
- Chiarire che i sub-indici sono COSTRUITI DAGLI AUTORI, non variabili native WB/TREND. Dichiararlo esplicitamente.
- Aggiungere tabella in appendice che mostra la composizione dei sub-indici.
- Verificare il numero "150 provisions" (25 dest × 6 provisions WB? Verificare).
- Tabella 11: semplificare o TOGLIERE se incomprensibile. Se si tiene, aggiungere note esplicative.
- Riscrivere tutto il passaggio sulla collinearità perfetta (ρ = 1.000) in modo comprensibile. Proposta: "Two WB sub-indices with a direct trade mechanism are non-zero in only three destination-years (Korea from 2015, Switzerland from 2014), always in the same proportion. They are therefore perfectly collinear in the regression and cannot be separately identified."

### 4.20 Riga ~1080: "This is the only feasible approach, but..."
**Azione:** Specificare quale tabella. Aggiungere il riferimento.

### 4.21 Righe ~1085-1095: Placebo sub-indices
**Azione:** Specificare QUALI sono i placebo sub-indices e DOVE sono riportati (tabella?). Se manca una tabella, segnalarlo e crearne una o metterla in appendice.

### 4.22 Riga ~1100: "The defensible conclusion is the weaker one..."
**Azione:** Riscrivere in modo meno esposto. Proposta: "With only 13 distinct EP profiles, the design cannot separate the effect of specific provision types from overall agreement depth. The sub-index results are informative about bundling patterns but not about individual mechanisms."

### 4.23 "mechanism-bearing"
**Azione:** Cercare e ELIMINARE ovunque appaia. Sostituire con "provisions with a direct trade mechanism" o "trade-relevant provisions".

### 4.24 Sezione 5.6 Extensive margin
**Azione:** Verificare che ci sia una tabella. Verificare che l'extensive margin sia stimato per tutte e 3 le variabili dipendenti (value, quantity, unit value). Aggiungere riferimento tabella.

### 4.25 Riga ~1120: "PPML estimates confirm the null"
**Azione:** Sostituire con "PPML estimates are consistent with the baseline findings" o "PPML estimates confirm previous findings".

### 4.26 Riga ~1125: "No green trade creation..."
**Azione:** Riscrivere. Proposta: "No statistically significant green trade creation is detected at the extensive margin."

### 4.27 Riga ~1130: "One limitation bears noting..."
**Azione:** Sostituire "bears noting" con "should be noted" o semplicemente "Note that". Spiegare PERCHÉ il fill-in è sul collassato (computazionale).

### 4.28 Sezione 5.7 (TREND vs WB switch)
**Azione:** TOGLIERE la sezione intera come subsection dedicata. Il confronto TREND vs WB è già implicito in ogni tabella che riporta entrambe le colonne.

### 4.29 Sezione 5.8 (Depth control sensitivity)
**Azione:** TOGLIERE come subsection dedicata. I contenuti rilevanti (correlazione EP/depth, effetto di togliere il depth control) vanno incorporati nella sezione dirty margin (4.12) o nella sezione strategy dove si discute il depth control. Non serve una subsection intera per questo.

### 4.30 Correlazione 0.91 vs 0.96 — ERRORE
**Azione:** Cercare TUTTE le occorrenze di "0.96" nel tex. Sostituire con "0.91". La correlazione corretta (pre-singleton) è 0.91.

### 4.31 Riga ~1145: "Dropping the depth control altogether moves the coefficient away from zero..."
**Azione:** Spiegare PERCHÉ è rilevante: "This is expected: without the depth control, EP depth also captures the effect of overall agreement depth, which may push exports in either direction."

### 4.32 Sezione 5.9 (Full panel with controls)
**Azione:** Se manca una tabella propria, segnalare. Verificare che antidumping exposure sia giustificato come controllo (varia a livello p,d,t).

### 4.33 Riga ~1155: Spiegazione tariff
**Azione:** RISCRIVERE completamente. L'attuale testo è incomprensibile. Proposta per nota a piè di pagina: "The tariff variable is the applied MFN duty from customs records, not a constructed variable. It varies across destinations within the same HS6-year cell. Its mean does not fall after PTA entry for partner destinations, confirming it captures the MFN rate rather than the preferential rate (which was not obtainable)."

### 4.34 Tabella 13: within-firm green share
**Azione:** TOGLIERE la riga "within-firm green share" dalla tabella (coerente con la rimozione del within-firm panel).

### 4.35 Sezione 5.11 Destination-specific trends
**Azione:** Verificare che ci siano tabelle. Se mancano, segnalare. Riscrivere il testo:
- "Two variants bracket the answer" → spiegare quali sono le 2 varianti e citare tabelle
- Togliere linguaggio LLM (riga ~1165: "Taken at face value..." → riscrivere)
- "the late negative drift that these trends pick up is precisely the early-cohort artifact..." → spiegare chiaramente che l'event study già mostra che le coorti early hanno un drift negativo nel post-period, e i destination trends lo catturano artificialmente.
Proposta: "Destination-specific trends estimated over the full sample absorb both pre- and post-treatment variation. When trends are instead estimated on pre-treatment years only and projected forward, all coefficients return to imprecise zeros, confirming that the one significant coefficient in the full-period specification was an artifact of the trend absorbing post-treatment dynamics (Wolfers, 2006)."

### 4.36 Sezione 5.12
**Azione:** Verificare tabella. Se manca, segnalare.

### 4.37 Trimming (sezione 5.13 nel tex attuale)
**Azione:** TOGLIERE completamente il paragrafo sul trimming.

### 4.38 Sezione CO2 intensity
**Azione:** Tenere ma aggiungere nota a piè di pagina che spiega perché non è stimato sul full panel: "The continuous intensity measure requires re-estimating the interaction structure on the full 45-million-observation panel; the collapsed-panel test is sufficient to establish that the continuous measure behaves like the binary indicator."

### 4.39 Sezione 5.9 (nel tex attuale) — Full panel with controls
**Azione:** Tenere se c'è una tabella di supporto. Se manca la tabella, segnalare. Se c'è, tenere la sezione ma eventualmente toglierla in un secondo momento se non aggiunge abbastanza.

### 4.40 Alternative outcomes
**Azione:** Dare più spazio. Verificare se esistono stime full panel per quantity e unit value. Se sì, riportarle. Se no, segnalare.

---

## 5. Sezione 6: Conclusion

### 5.1 Revisione generale
**Azione:** Rivedere alla luce di tutte le modifiche fatte alle sezioni precedenti. Verificare coerenza con la nuova struttura della sezione 5. Nessuna modifica specifica richiesta.

---

## 6. Verifiche da fare nel codice

Queste verifiche richiedono accesso ai file di codice/dati. Sonnet deve segnalarle come "DA VERIFICARE" se non può accedervi.

1. **CEM sample**: 19 treated + 40 control — verificare nello script CEM
2. **"150 provisions"**: sommare gli EP WB dalla Table 1 e verificare il totale
3. **HS2012 loss %**: calcolare la % di osservazioni affected dalla perdita di distinzione HS2012
4. **Tabelle mancanti sezione 5**: elencare quali sezioni non hanno una tabella di riferimento
5. **Alternative outcomes full panel**: verificare se esistono output per quantity/unit value sul full panel
6. **Coarser permutation (dirty, +0.005)**: trovare la tabella/output corrispondente
7. **Figure 4 commento**: verificare che il testo corrisponda ai dati

---

## 7. Note operative per Sonnet

- Procedere sezione per sezione, nell'ordine di questo documento
- Prima di riscrivere un passaggio: leggere il contesto circostante nel tex
- Quando si toglie un passaggio: controllare che non ci siano riferimenti incrociati altrove nel paper (forward/backward references, \ref, \label)
- Quando si sposta una tabella: aggiornare tutti i \ref corrispondenti
- Dopo tutte le modifiche: fare un passaggio finale di coerenza per verificare che la numerazione di tabelle/figure sia corretta
- La correlazione corretta è **0.91** ovunque, mai 0.96
- I sotto-indici EP sono **costruiti dagli autori**, non variabili native
- Il paper quality (Caselli et al.) è il modello di stile per l'esposizione dei risultati
