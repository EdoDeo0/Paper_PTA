# Audit Report — Paper_PTA (versione /New, paper_v3)

**Data:** 2026-09-05  
**Scope:** Intero progetto /New — paper_v3.tex, tutti i codici R e Stata, tabelle, figure, dati, bibliografia  
**Lingue disponibili:** R, Stata (Python non usato nel progetto)

---

## Giudizio complessivo

**CONDITIONAL PASS** — Il paper è metodologicamente solido e il codice è eccezionalmente ben verificato (53 repliche Stata-R, guard Frisch-Waugh su ogni stima, cross-check hardcoded). La struttura logica regge. I problemi principali sono di presentazione, non di sostanza: 5 tabelle in italiano nel paper inglese, numeri non riconciliati tra testo e tabelle, gap di osservazioni mai spiegato. Questi problemi sono tutti risolvibili senza toccare la sostanza del lavoro.

Il risultato principale — null per i prodotti verdi, marginalmente significativo (asintotico) per i prodotti sporchi — è presentato onestamente. L'analisi MDE fornisce bound informativi. L'event study non mostra trend pre-trattamento (bene per parallel trends) né effetti dinamici post-trattamento, coerente con il null. L'inferenza bootstrap (p=0.072) e permutazione (p=0.278) ridimensiona la significatività asintotica del coefficiente dirty: il paper lo riconosce apertamente.

**Stato del paper:** Pronto per la circolazione interna / working paper. Per la sottomissione a rivista, servono le correzioni elencate sotto — nessuna richiede nuove stime o nuovi dati, solo editing del LaTeX e traduzione delle tabelle.

---

## 1. Problemi critici

### C1. Cinque tabelle in italiano nel paper inglese

**Gravità: CRITICO**

Le seguenti tabelle sono incluse nel PDF compilato tramite `\input{}` e sono interamente in italiano (didascalia, intestazioni colonne, note a piè di tabella):

| Tabella | File | Posizione nel paper |
|---------|------|---------------------|
| Destinazioni trattate | `Tabelle/tab_01_trattamento.tex` | Appendice D (riga 1321) |
| Matrice coefficienti | `Tabelle/tab_07_matrice.tex` | Appendice (riga 1327) |
| Robustezza full panel | `Tabelle/tab_11_robustness_full.tex` | Appendice (riga 1333) |
| APEC vs OCSE | `Tabelle/tab_18_apec.tex` | Appendice (riga 1339) |
| Effetto minimo rilevabile | `Tabelle/tab_19_mde.tex` | Appendice (riga 1345) |

Altre 5 tabelle italiane nella cartella (`tab_03`, `tab_04`, `tab_08`, `tab_10`, `tab_17`) **non** sono incluse nel paper compilato — sono state sostituite dalle tabelle-frammento (`ptab_*`). Non servono interventi su queste.

### C2. Riga "alta_dose" in tab_16 (leave-one-out)

**Gravità: CRITICO**

In `Tabelle/tab_16_leaveoneout.tex`, riga 14: compare "alta\_dose" — etichetta italiana, non spiegata nelle note, con coefficiente -0.0271 (molto lontano dal range LOO dichiarato nel testo: -0.0097 a -0.0133). Non è chiaro cosa rappresenti. Se è un test di sottocampione (high-dose destinations?), va spiegato in inglese e spostato altrove; se è un residuo di editing, va rimosso.

### C3. Gap osservazioni mai spiegato (45.8M → 21.5M)

**Gravità: CRITICO (presentazione)**

La Tabella 3 riporta 45,781,211 osservazioni. Le tabelle di regressione mostrano 21,519,511. La differenza (~53%) è causata dalla rimozione iterativa dei singleton imposta dai tre FE ad alta dimensione. Questo è standard, ma **il paper non lo dichiara mai esplicitamente per il full panel**. Per il collapsed panel sì ("post-singleton 3,681,023"). Un referee chiederà dove siano spariti 24 milioni di osservazioni.

### C4. Incoerenza pronomi (we / I)

**Gravità: CRITICO (stile)**

Il paper usa "we" nella maggior parte del testo, ma passa a "I" nella sezione CO₂ e in almeno un altro punto. Per un paper a singolo autore, scegliere uno stile e mantenerlo ovunque. "We" è la convenzione accademica standard anche per singoli autori.

---

## 2. Problemi di warning

### W1. Discrepanza VIF: 5.8 vs 5.7

- **Testo** (riga ~663): "VIF for EP depth in the main specification is 5.8"
- **ptab_depthbounds.tex** (nota): "reducing the VIF from 5.7 to 1.9"

Uno dei due è sbagliato. Verificare il valore reale dal codice e uniformare.

### W2. Percentuale permutazione: 27.7% vs 27.8%

- **Testo** (riga ~849): "27.7% of placebo draws"
- **tab_06**: p = 0.278 → 27.8%

Correggere a "27.8%".

### W3. Bootstrap p-value destination trends: 0.012 vs 0.015

- **Testo**: il coefficiente TREND green "survives the wild cluster bootstrap (p = 0.012)"
- **tab_12_desttrends.tex**, Panel A', colonna 1: [0.015]

Probabile errore da seed diverso tra run successivi. Il testo deve corrispondere alla tabella pubblicata.

### W4. Conteggio deep/shallow: 7 vs 9

- **Testo**: "16 deep, 7 shallow"
- **ptab_stability.tex** (nota): "16 deep vs. 9 shallow"

La differenza è HK/MO: esclusi → 7 shallow (23 - 16), inclusi → 9 shallow (25 - 16). Va chiarito esplicitamente a quale campione si riferisce ciascun conteggio.

### W5. ptab_main riporta lo stesso N per tutte e 4 le colonne

`ptab_main.tex` mostra 21,519,511 osservazioni per tutte e 4 le colonne. Ma WB e TREND differiscono di 1,845 osservazioni (tab_03 mostra 21,519,511 WB vs 21,517,666 TREND). Mostrare conteggi separati o specificare che si tratta di un arrotondamento.

### W6. SE Australia non verificabile dalla tabella

Il testo dice che escludendo l'Australia l'SE del coefficiente dirty sale da 0.0030 a 0.0087. Tab_16 mostra i coefficienti ma **non** gli errori standard. Il lettore non può verificare. Aggiungere una colonna SE alla tabella LOO o citare la fonte esplicitamente.

### W7. CEM sample: 14.0M vs 13.7M

- **Testo** (righe 538, 563): "14.0 million observations"
- **tab_10, ptab_stability**: 13,728,510 (13.7M)

La differenza è singleton removal. Chiarire nel testo che 14.0M è pre-singleton, oppure aggiornare il numero a 13.7M.

### W8. "Roughly nine distinct EP profiles" vs 13

Tab_06 nota dice "roughly nine distinct EP profiles." Il testo dice 13. Il contesto è diverso (profili indipendentemente permutabili vs totali), ma la formulazione è ambigua. Precisare.

---

## 3. Note

### N1. 16 voci non citate nel .bib

`baccini2017`, `baghdadi2013`, `bertrand2004`, `brunnermeier2004`, `callaway2021`, `conley2011`, `copelandtaylor2004`, `dean2009`, `dechezlepretre2017`, `fisher1935`, `headmayer2014`, `jaffe1997`, `kellenberg2014`, `medvedev2010`, `neri2023`, `rajan1998`. Rimuoverle.

### N2. Chiavi bib disallineate

- `morin2018` → year = 2017 (corretto nel PDF, chiave fuorviante)
- `gutsch2024` → year = 2025 (corretto nel PDF, chiave fuorviante)

### N3. "Timor-Leste" vs "East Timor"

Usato in modo incoerente. Raccomandazione: "Timor-Leste" ovunque (nome ufficiale ONU).

### N4. 3 file figura inutilizzati in figures/

`fig_ep_distribution_wb.pdf`, `fig_ep_distribution_trend.pdf`, `fig_ep_timeline_twopanel.pdf`/`.png` — non referenziati. Rimuovere o spostare.

### N5. Data "August 2026" nel paper

Verificare se aggiornare alla data di sottomissione.

### N6. run_pipeline.R solo Windows

Riga 40: `Rscript.exe`. Non funziona su macOS. I singoli script funzionano ovunque, ma l'orchestratore è Windows-only.

### N7. Nessun lockfile versioni R

Manca `renv.lock`. `fwildclusterboot >= 0.13` ha cambiato RNG (documentato nel codice), quindi la sensibilità alle versioni è un rischio reale per la replicabilità.

### N8. FE/clustering hardcoded nel generatore tabelle

`44_make_tables_tex.R` ha FE structure e clustering level hardcoded, non estratti dalle stime. Se la specifica cambia, le note delle tabelle non si aggiornano.

---

## 4. Codice

### 4.1 R Pipeline — 0 problemi critici

Il codice è eccezionalmente ben difeso:

- **Guard Frisch-Waugh** su ogni stima `feols`: se la cross-verifica FWL diverge oltre 1e-6, lo script si blocca
- **53 repliche Stata** confermano i risultati R a 15 cifre significative
- **callr subprocess** per gestire crash di fixest su stime grandi (~50% delle volte)
- **Seed doppio** (`set.seed` + `dqrng::dqset.seed`) per la riproducibilità WCB con `fwildclusterboot >= 0.13`
- **Classificazioni coerenti** — green, dirty, HK/MO definiti identicamente in tutti gli script
- **stopifnot guard** sulla sequenza WBID nel merge WB
- **Continuity check** al confine HS 2006/2007 per la concordanza green goods

1 warning (run_pipeline.R Windows-only), 16 note di best practice — nessuna influisce sui risultati.

### 4.2 Classificazioni prodotto

- **Green**: 248 codici CLEG (HS2012 → HS1996 via concordanza univoca). 246/248 concordati, 2 fallback. Continuity check superato.
- **Dirty**: Mani-Wheeler ISIC Rev.2 → HS6 via WITS. 5 settori core + cemento esteso. 17 overlap con green risolti (precedenza green). Scelta petroleum vs cement documentata per difesa referee.

### 4.3 Collapsed panel

Costruzione corretta: `y = mean(ln_export)` per cella (hs6, country_code, year), non `ln(sum)`. Evita Jensen's inequality bias. Variabili EP prese via `first()` (corrette: sono a livello destination-year, identiche per tutte le firm nella cella).

---

## 5. Econometria

### 5.1 Design

Triple differenza: green/dirty/neutral × EP depth × FE — solido. I tre FE (firm-product-destination, firm-destination-year, product-year) assorbono il livello, i trend firm-destination, e gli shock settoriali-anno. La FWL cross-check conferma che i coefficienti sono identificati dalla variazione residua corretta.

### 5.2 Inferenza — la sfida principale

23 destinazioni trattate, 13 profili EP distinti, ~9 indipendenti (ASEAN condivide un accordo). Il paper affronta il problema con tre metodi complementari:

| Metodo | p dirty | p green | Affidabilità |
|--------|---------|---------|-------------|
| Asintotico | <0.001 | 0.649 | Bassa (pochi cluster) |
| Wild cluster bootstrap | 0.072 | 0.649 | Media-alta |
| Permutazione | 0.278 | 0.597 | Alta (non parametrico) |

Il paper riporta tutti e tre onestamente. Il bootstrap è il bound più credibile; la permutazione è il test più conservativo. La conclusione è che il dirty effect è al massimo suggestivo, non definitivamente significativo.

### 5.3 Collinearità EP depth / agreement depth

ρ = 0.91, VIF = 5.8. Il paper risponde con:
1. Quattro varianti del controllo profondità (ptab_depthbounds) — estimato si muove di 0.0024
2. Variante DESTA (ρ = 0.35, VIF = 1.9) — conferma il risultato

### 5.4 Event study

Pre-treatment flat per green e dirty (tutti i coefficienti pre-trattamento non significativi) — supporto per parallel trends. Post-treatment: nessun effetto dinamico significativo per entrambi — coerente con il null. Sun-Abraham (tab_09) con SE di eventstudyinteract (corretti per incertezza quote coorte) conferma.

### 5.5 Singleton removal

45.8M → 21.5M osservazioni (-53%). Percentuale alta ma attesa con tre FE ad alta dimensione. Non è un problema, ma va dichiarato esplicitamente (C3).

### 5.6 CEM

Matching coarsened exact a livello destinazione. Riduce il campione a 13.7M osservazioni. I risultati sono stabili. Il matching è conservativo (grana grossa) e documentato.

---

## 6. Coerenza esterna

### 6.1 Letteratura

Posizionamento corretto rispetto a:
- **Brandi et al. (2020)** — confronto quantitativo diretto (tab_20). Stesso ordine di grandezza.
- **Mattoo, Rocha, Ruta (2020)** — deep agreements e commercio.
- **Copeland & Taylor** — framework teorico regolazione ambientale e commercio.
- **Manova & Zhang (2012)** — selezione prodotti quality upgrading.
- **Zhu & Sun (2026)** — reconciliazione aggiunta in v3 (citata nell'header).

### 6.2 Dati

- Dogane cinesi HS6: standard nella letteratura. 45.8M osservazioni plausibili.
- WB + TREND paralleli: scelta robusta.
- CLEG e Mani-Wheeler: classificazioni standard.

### 6.3 Risultato null

Il null è credibile e informativo. L'MDE table (tab_19) mostra che il design può escludere effetti superiori a ~3% per unità di EP depth al 95% di confidenza. Questo è un contributo: non solo "non troviamo un effetto," ma "possiamo escludere effetti superiori a questa soglia." L'interpretazione è onesta e ben calibrata.

---

## 7. Figure

Tutte e 5 le figure nel paper sono in inglese e visivamente corrette:

1. **fig_ep_timeline.pdf** — timeline EP depth per WB e TREND
2. **fig_map_treated.pdf** — mappa destinazioni trattate
3. **fig_composition_shares.pdf** — quote composizione green/dirty
4. **eventstudy_collapsed_v3.png** — event study due pannelli (green, dirty), CI 90% e 95%, pre-treatment flat
5. **eventstudy_sunab_v3.png** — Sun-Abraham, CI da eventstudyinteract, coerente con tab_09

---

## 8. Tabella riassuntiva

| # | Problema | Gravità | File | Stato |
|---|----------|---------|------|-------|
| C1 | 5 tabelle in italiano nel paper | CRITICO | tab_01, 07, 11, 18, 19 | Aperto |
| C2 | Riga "alta_dose" non spiegata | CRITICO | tab_16_leaveoneout.tex | Aperto |
| C3 | Gap 45.8M → 21.5M mai spiegato | CRITICO | paper_v3.tex | Aperto |
| C4 | Pronomi we/I incoerenti | CRITICO | paper_v3.tex | Aperto |
| W1 | VIF 5.8 vs 5.7 | WARNING | paper_v3.tex / ptab_depthbounds | Aperto |
| W2 | 27.7% vs 27.8% | WARNING | paper_v3.tex riga ~849 | Aperto |
| W3 | Bootstrap p = 0.012 vs 0.015 | WARNING | paper_v3.tex vs tab_12 | Aperto |
| W4 | Deep/shallow 7 vs 9 | WARNING | paper_v3.tex vs ptab_stability | Aperto |
| W5 | ptab_main N uguale per 4 colonne | WARNING | ptab_main.tex | Aperto |
| W6 | SE Australia non verificabile | WARNING | paper_v3.tex vs tab_16 | Aperto |
| W7 | CEM 14.0M vs 13.7M | WARNING | paper_v3.tex vs tab_10 | Aperto |
| W8 | "~9 EP profiles" vs 13 | WARNING | tab_06 vs testo | Aperto |
| N1 | 16 bib entries non citate | NOTA | references.bib | Aperto |
| N2 | Chiavi bib disallineate | NOTA | references.bib | Aperto |
| N3 | Timor-Leste vs East Timor | NOTA | vari .tex | Aperto |
| N4 | 3 figure inutilizzate | NOTA | figures/ | Aperto |
| N5 | Data "August 2026" | NOTA | paper_v3.tex | Aperto |
| N6 | run_pipeline.R solo Windows | NOTA | run_pipeline.R | Aperto |
| N7 | Nessun lockfile versioni R | NOTA | progetto | Aperto |
| N8 | FE/clustering hardcoded in table gen | NOTA | 44_make_tables_tex.R | Aperto |

---

## 9. Verdetto

- [ ] PASS — nessun problema critico
- [x] **CONDITIONAL PASS** — problemi critici tutti di presentazione, risolvibili senza nuove stime
- [ ] FAIL — problemi critici di sostanza

**Il codice è tra i più puliti che abbia mai auditato in un progetto accademico.** La doppia verifica R/Stata su 53 specifiche, i guard Frisch-Waugh automatici, e la documentazione interna sono esemplari. I problemi sono tutti nel LaTeX: traduzione tabelle, riconciliazione numeri, qualche frase. La sostanza econometrica è solida.
