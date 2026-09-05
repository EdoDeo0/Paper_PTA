# Report — Risposte ai dubbi aperti

Documento di supporto alla roadmap di revisione del paper.
Ogni punto corrisponde a un dubbio espresso nelle istruzioni.

---

## Sezione 2: Related Literature

### "a finding substantially larger than earlier cross-sectional estimates"
**Verifica:** Baier & Bergstrand (2007) dicono esattamente questo nel paper originale. L'affermazione è corretta e attribuita agli autori. Si può tenere, magari esplicitando "as the authors themselves note" o "according to their estimates".

### Mattoo, Mulabdic, and Ruta (2022) usano sia WB DTA che DESTA?
**Verifica:** Nel testo attuale (riga 121 del tex), la frase segue la menzione di entrambi i database (WB DTA e DESTA). La struttura del paragrafo potrebbe suggerire che Mattoo et al. usino entrambi, ma in realtà usano il WB DTA database. Il DESTA è citato come framework indipendente che conferma la stessa eterogeneità. **Va riformulato** per rendere chiaro che Mattoo et al. lavorano sul WB DTA, non su DESTA.

### "consistent with the pollution haven prediction once one addresses the econometric difficulties"
Levinson & Taylor (2008) dicono esattamente questo. La frase è corretta ma poco chiara per il lettore. **Suggerimento:** spiegare brevemente che le "econometric difficulties" sono l'endogeneità dei costi di abbattimento dell'inquinamento e l'omitted variable bias da regolamentazione ambientale non osservata.

### "this tradition" — a cosa si riferisce?
Si riferisce alla tradizione di classificazione dei settori pollution-intensive (Mani & Wheeler 1998, Low & Yeats 1992). Il riferimento è corretto ma il pronome è vago. **Suggerimento:** sostituire "this tradition" con "the pollution-intensive sector taxonomy".

### Zhu & Sun (2026) — siamo MEGA certi del confronto collapsed vs full panel?
**Verifica dal codice:** Il fattore 2.7 è documentato nel tex (riga 832): dirty coefficient collapsed = −0.0119, full panel = −0.0044. Il rapporto è 0.0119/0.0044 = 2.70. Il calcolo è corretto. Tuttavia l'affermazione che Zhu & Sun identificano "from variation across firms and destinations" va verificata leggendo il loro paper. **Suggerimento:** aggiungere una nota che dica "according to the WB and TREND codings used in this study" per cautelarsi.

### "25 treated destinations gives 150 provisions"
**Verifica dalla Table 1 del tex:** Sommando i valori WB EP dalla tabella (inclusi HK e Macao):
Bangkok (1×5=5) + ASEAN (6×10=60) + Chile(5) + Pakistan(3) + NZ(7) + Singapore(7) + Peru(12) + CostaRica(4) + Iceland(6) + Switzerland(14) + Australia(3) + SKorea(17) + HK(4) + Macao(5) = 152.
Ma Singapore è contata una volta sola (max EP=7, dalla bilaterale), e Laos e S.Korea hanno i valori dal loro accordo più profondo. Il numero "150" nella tabella 11 è per "25 treated destinations" (inclusi HK/MO). **Il testo dice 25 ma dovrebbe dire 25 (inclusi HK/MO) oppure 23 (esclusi)**. Va verificato nel codice sorgente la somma esatta.

---

## Sezione 3: Data

### "between 4,450 and 4,766 HS6 product codes depending on the year"
**Verifica dal paper quality:** Il paper di Caselli et al. menziona l'armonizzazione HS e la concordanza alla HS1996, ma non riporta il numero esatto di codici per anno. Il paper quality usa 4,999 HS6 codes (pag. 8: "4,999 distinct HS6 products"). Il nostro paper cita un range che varia con la revisione HS 2007. **Suggerimento:** togliere il dettaglio "(the count shifts with the 2007 HS revision)" come suggerito nelle istruzioni.

### "from the General Administration of Customs of China"
**Verifica dal paper quality (pag. 8):** Il paper quality dice "collected by the Chinese Customs Office" — non "General Administration of Customs of China". Il nome ufficiale dell'ente è 中华人民共和国海关总署, che in inglese è "General Administration of Customs of the People's Republic of China" (GAC). Entrambe le forme sono accettabili, ma per uniformità col paper quality usiamo "Chinese Customs Office" oppure "General Administration of Customs of China".

### Conteggio destinazioni e EP profiles — chiarimento definitivo
Dalla Table 1 del tex:

| Conteggio | Valore | Spiegazione |
|---|---|---|
| Destinazioni totali coperte da PTA | 25 | incluse HK e Macao |
| Destinazioni nel campione principale | 23 | escluse HK e Macao |
| Destinazioni ASEAN-China | 10 | nella tabella sotto "ASEAN-China" |
| Destinazioni ASEAN-China + Laos | 11 | Laos è anche parte ASEAN dal 2005 |
| Destinazioni con ASEAN come unica fonte EP | 9 | esclude Laos (anche Bangkok) e Singapore (anche bilaterale) |
| EP profiles distinti | 13 | vedi dettaglio sotto |

**Dettaglio 13 profili:**
1. Bangkok-only (Bangladesh, India, Sri Lanka) → 1 profilo condiviso
2. ASEAN-only (Brunei, Cambodia, Indonesia, Malaysia, Myanmar, Philippines, Thailand, Timor-Leste, Vietnam) → 1 profilo condiviso (9 dest.)
3. Laos (Bangkok + ASEAN) → 1 profilo unico
4. Singapore (ASEAN + bilaterale) → 1 profilo unico
5. S. Korea (Bangkok + bilaterale) → 1 profilo unico
6–13. Chile, Pakistan, NZ, Peru, Costa Rica, Iceland, Switzerland, Australia → 8 profili unici

**Totale: 3 + 9 + 1 + 1 + 1 + 8 = 23 destinazioni, 13 profili.** ✓

Il testo è corretto ma confusionario. Va riscritto in modo lineare. Il problema è che dice "eleven destinations" (ASEAN parties), poi "nine" (ASEAN-only), e il lettore non capisce la differenza.

### Tabella 5 — "cell count" sotto mean e sd
Nella Table `tab:sumstats_collapsed` (riga 421): Cell count (n_pdt) ha Mean=12.13 e SD=43.64. Significa che in media ogni cella HS6-destination-year contiene 12.13 osservazioni firm-level, con SD=43.64. È la distribuzione del numero di imprese per cella.

### "C-qualcosa" come nomi dei subsample
I nomi C-prod-HS4, C-overlap ecc. usano "C-" come abbreviazione di "Control group". È una convenzione interna. **Suggerimento:** o si spiega al lettore o si usa un nome più intuitivo.

### C-overlap e la riallocazione Eckel & Neary
L'idea è: quando i costi commerciali scendono, le imprese multi-prodotto possono concentrarsi sui prodotti "core" (quelli in cui sono più competitive) e abbandonare quelli periferici (Eckel & Neary, 2010). Se un prodotto neutral è nella stessa famiglia HS4 di un green, e l'impresa lo abbandona per concentrarsi sul green, il confronto green-vs-neutral dentro la stessa famiglia HS4 potrebbe essere distorto. C-overlap serve come check complementare: se il prodotto è esportato sia verso destinazioni trattate che non trattate, la variazione identificante non dipende da prodotti scambiati solo con un gruppo.

### CEM sample: "19 treated and 40 control destinations"
**Da verificare nel codice CEM.** Il numero 19 (vs 23 totali) indica che il matching CEM non trova un match per 4 destinazioni trattate. Va verificato esplicitamente.

---

## Sezione 4: Empirical Strategy

### "OLS gives equal weight to each observation"
Significa: il coefficiente OLS dà lo stesso peso a ogni riga del dataset. Un'impresa che esporta 100 prodotti pesa 100 volte più di un'impresa che ne esporta 1. Non è una media pesata per impresa (dove ogni impresa conterebbe uguale) né per destinazione. Il coefficiente riflette la composizione del campione in termini di osservazioni.

### Perché θ_pt e non θ_fpt?
Ragione computazionale: firm-product-year FE avrebbe un numero di parametri dell'ordine di centinaia di milioni, rendendo la stima impossibile sulla memoria disponibile. Teoricamente, θ_pt è sufficiente perché cattura gli shock globali a livello di prodotto-anno. Gli shock firm-product-year sarebbero assorbiti solo se variassero per destinazione, ma quelli sono già catturati da θ_fpd e θ_fdt congiuntamente.

### Frisch-Waugh nel collapsed panel bootstrap — perché la complicazione?
Il collapsed panel usa meno FE (pd + dt + pt), ma il wild cluster bootstrap in Stata (boottest dopo reghdfe) per il full panel gira nativamente. Per il collapsed panel, si usa un approccio Frisch-Waugh per ridurre il carico computazionale del bootstrap. I punti stimati sono identici perché il Frisch-Waugh theorem garantisce equivalenza algebrica. Le approssimazioni nella nota a piè di pagina (product-year FE non nested nel cluster destination) non cambiano i risultati in modo materiale perché il null è ampiamente non significativo.

### "Approximately three-fifths of the collapsed dirty coefficient reflects between-firm composition"
Calcolo: (0.0119 − 0.0044) / 0.0119 = 0.63 ≈ 3/5. La differenza tra collapsed (−0.0119) e full panel (−0.0044) è 0.0075, che è il 63% del coefficiente collapsed. Questo 63% riflette lo spostamento di *quali* imprese esportano dirty goods (composizione between-firm), non la riallocazione *dentro* la singola impresa.

---

## Sezione 5: Results

### "The observed green coefficient sits comfortably in the middle of the placebo distribution"
Significa: nel test di permutazione, il coefficiente green osservato non è estremo rispetto ai coefficienti ottenuti con EP profiles permutati casualmente. Il 60% dei placebo produce un coefficiente uguale o più grande in valore assoluto (p = 0.60). L'affermazione manca di un riferimento numerico esplicito. **Va aggiunto il p-value del permutation test.**

### Brandi et al. benchmark — "roughly +0.16 log points"
Brandi et al. (2020) trovano che una liberal EP alza la green share di ~0.4pp. Convertendo in log points: se la share base è circa 2.5%, +0.4pp = da 2.5% a 2.9%, cioè +0.16 in log(share). Tuttavia il nostro outcome è log(export value), non log(share). Il confronto è approssimativo e forse fuorviante. **Va spiegato meglio o tolto.**

### "Across nine designs — the four control-group subsamples, the two baseline specifications, and three full-panel variations"
- 4 control-group subsamples: C-prod-HS4, C-overlap, CEM, Deep vs Shallow
- 2 baseline: collapsed panel e full panel (con WB)
- 3 full-panel variations: con controlli, senza ASEAN, con HK/MO
Totale = 9. **Va esplicitato nel testo.**

### Figura 5 (event study)
La figura mostra i coefficienti lead/lag della composizione green e dirty vs neutral intorno all'entrata in vigore del PTA. Si stima su collapsed panel con FE pd+dt+pt, periodo di riferimento t=−1. Se i coefficienti pre-trattamento sono vicini a zero, supporta l'assunzione di parallel trends.

### Sun-Abraham estimator — spiegazione semplice
Sun & Abraham (2021) è un estimatore per event studies con trattamento staggered. A differenza dell'event study classico (TWFE), pesa correttamente i coefficienti di ogni coorte per evitare "contaminazione" tra coorti. ATT = Average Treatment effect on the Treated. Qui è applicato su un outcome aggregato a livello destination-year (il gap di composizione green-vs-neutral), con trattamento binario (EP presente/assente). I p-values riportati si riferiscono all'ATT aggregato su tutte le coorti.

### Tabella 9 — mai citata
**Verifica:** nel tex non esiste una Table 9 indipendente nel senso tradizionale. Le tabelle sono numerate automaticamente da LaTeX. La "tabella 9" a cui l'utente si riferisce potrebbe corrispondere a `tab_09_sunab.tex` in appendice (Sun-Abraham). Va verificato il numbering finale nel PDF compilato.

### Tabella 10 — full panel con controlli, perché aggiungere controlli se i FE assorbono tutto?
I FE firm-destination-year (θ_fdt) assorbono tutto ciò che varia a livello (f,d,t). Ma i controlli aggiunti (tariff, HHI, AD exposure) variano a livello (p,d,t) — cioè per prodotto-destinazione-anno. Questa variazione *non* è assorbita da θ_fdt (che non ha la dimensione prodotto), né da θ_pt (che non ha la dimensione destinazione). Quindi i controlli product-destination-year aggiungono informazione.

### DESTA — era stato introdotto prima?
**Sì**, nella lit review (riga 121 del tex): "The DESTA depth index (Dür, Baccini, and Elsig, 2014) provides an independent coding across seven policy areas." Ma nella sezione dirty margin viene usato come controllo alternativo senza re-introdurlo formalmente. **Va aggiunta una frase che lo ri-presenta come depth control alternativo.**

### Correlazione 0.91 vs 0.96
**Errore nel testo.** Riga 771: ρ = 0.91 (sezione Strategy). Riga 1209: "correlate at 0.96" (sezione Results). L'utente conferma che 0.91 è il valore corretto (pre-singleton). **Va corretto a 0.91 ovunque.**

### Antidumping exposure nei controlli
AD_pdt è una variabile pre-esistente nei dati doganali (verificato nel session-log: "AD_pdt nello script 18_robustness_fullpanel.do è variabile pre-esistente nei dati doganali, non costruita da noi"). Si include come controllo perché le misure antidumping variano a livello product-destination-year e potrebbero essere correlate sia con EP depth che con la composizione green/dirty.

### Perché non stimiamo CO2 intensity sul full panel?
Ragione computazionale: la misura continua sostituisce l'indicatore binario dirty, il che cambia la struttura delle interazioni e richiede una nuova stima sul full panel (45M osservazioni). Nel collapsed panel il test è sufficiente per mostrare che la misura continua si comporta come quella binaria.

### Alternative outcomes — anche per il full panel?
La Table `tab:outcomes` nel tex riporta solo il collapsed panel. Le stime full panel per quantity e unit value non sono riportate nel testo attuale. **Va verificato se esistono nei file di output e se includerle.**

---

## Note generali

### Within-firm share panel — serve?
L'utente è tentato di toglierlo. Il panel è usato solo come "descriptive complement" (riga 1189-1206 del tex) e l'autore stesso ammette che non è identifying evidence. **Suggerimento:** toglierlo dal corpo principale. Se proprio si vuole tenerlo, ridurlo a una frase e una riga in una tabella di robustness.

### Sotto-indici EP — disclosure
Come documentato nel session-log (2026-09-03), tutti i sotto-indici (GreenLiberalization, StandardsNonRegression, ecc.) sono costruzioni nostre, non variabili native WB/TREND. **Il paper deve dichiararlo esplicitamente**, idealmente con una tabella in appendice che mostri la composizione di ogni sotto-indice.
