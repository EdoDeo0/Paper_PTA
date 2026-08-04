# Audit Report — New/ (focus econometrico)

**Data:** 2026-08-02
**Scope:** `New/Code/*.R` (28 script) + `New/Code/stata/*.do` (5 script), con verifica incrociata
sugli output gia' prodotti in `New/Output/` e sui dati ausiliari in `New/Data/`.
**Linguaggi disponibili in questo ambiente:** nessuno eseguibile (R e Stata non sono sul PATH di
questa shell). Nessuno script e' stato rilanciato: l'audit e' statico sul codice + lettura degli
output gia' scritti su disco. **Step 2 (replicazione cross-language) saltato** — anche su richiesta
esplicita ("concentrati sulla parte econometrica").
**Nessun file dell'autore e' stato modificato.**

---

## 1. Code Audit

### 31_robustness_leaveoneout.R — il verdetto automatico e' vuoto per costruzione

`dirty_leaveoneout.csv` **non contiene la riga `baseline`** (solo `lista_estesa` + 20 righe
`senza_*`). Il blocco di verdetto fa:

```r
b0  <- loo[spec == "baseline", coef]          # -> numeric(0)
sub[sign(coef) != sign(b0), .N]               # -> logical(0) -> .N = 0, SEMPRE
```

Con `b0` vuoto il confronto di segno restituisce `logical(0)` e il conteggio e' 0 qualunque cosa
succeda: lo script stampa "Cambi di segno: 0/20" anche se tutti i coefficienti si fossero invertiti.
Il controllo su `pval > 0.10` invece funziona (e infatti isola Australia).

Inoltre **mancano 3 dei 23 paesi trattati**: `112` Indonesia, `322` Iceland, `415` Costa Rica. Sono
i sottoprocessi falliti dopo 4 tentativi: lo script stampa `[FALLITO ...]` e prosegue, scrive il CSV
e produce il verdetto come se la tabella fosse completa. Non c'e' `stop()` ne' una riga di stato nel
file di output che segnali l'incompletezza. Chi legge il CSV (o il paper) non ha modo di accorgersene.

`[CRITICAL]` verdetto leave-one-out non informativo + tabella LOO incompleta al 87% senza alcun flag.

### 25_heterogeneity_subindices.R — due dei sotto-indici sono lo stesso regressore

Verificato su `Data/Merged/Merged_TREND_WB_Indices_Only.csv`: le coppie distinte
(`WB_GreenLiberalization`, `WB_StandardsNonRegression`) sul panel destinazione-anno sono
**solo `(0,0)` e `(1,3)`**. I due sotto-indici sono perfettamente proporzionali
(`Standards = 3 x GreenLib`), quindi lo stesso identico regressore riscalato.

Lo si vede nell'output: in `subindices_collapsed.csv` i p-value delle due specifiche coincidono alla
nona cifra (0.898101730077538 vs 0.898101730074438; 0.0712518349539275 vs 0.0712518344918517) e i
coefficienti stanno in rapporto esattamente 3.000.

Conseguenze:
- la tabella dei sotto-indici presenta come **due test distinti di meccanismo** ("green
  liberalization" vs "standards / non-regression") quello che e' una sola variabile. Un referee che
  se ne accorge legge doppio conteggio dell'evidenza.
- `WB_GreenLiberalization` e' **binaria** su tutto il campione: la versione WB dell'analisi di
  meccanismo non e' un'intensita', e' una dummy "il PTA ha o non ha quel tipo di clausola". Va detto
  esplicitamente, altrimenti il coefficiente sembra un effetto marginale per clausola.

`[CRITICAL]` due sotto-indici WB collineari al 100% riportati come meccanismi separati.

### Collinearita' EP vs TotalDepth — il "null" sul green e' fragile

`New/Output/Diagnostics/14_descriptives_collinearity.md`, prodotto dalla pipeline stessa:

| | WB_EP_Depth | TREND_EP_Count |
|---|---:|---:|
| corr within (FE paese+anno) con TotalDepth | **0.959** | 0.848 |
| VIF | 5.76 | 1.33 |

La spec principale (16) include contemporaneamente `EP:green` e `TotalDepth:green`, e la variazione
che li identifica e' la stessa al 96%. Il coefficiente WB green (-0.00456, se 0.00696) non e' quindi
"uno zero preciso": e' uno zero **la cui imprecisione e' in buona parte prodotta dal controllo**.
L'IC WCB va da -0.018 a +0.032 — non e' possibile escludere effetti economicamente rilevanti.

Il paper non puo' presentare questo come "null informativo" senza uno dei due:
1. una dichiarazione esplicita di MDE / test di equivalenza (quale effetto e' escluso al 95%?);
2. la spec **senza** le interazioni TotalDepth affiancata, per mostrare quanto il controllo pesa.

`[WARNING]` null riportato come informativo in presenza di corr. within 0.96 tra trattamento e controllo.

### 22_permutation_inference.R — lo schema di randomizzazione non e' quello del null testato

Due punti distinti.

**(a) TotalDepth resta al posto giusto mentre EP viene permutato.** In `stima_perm()` si ripermuta
solo `ep_green`/`ep_dirty`; `td_green`/`td_dirty` restano **ai valori veri delle destinazioni vere**
e sono gia' demeanati fuori dal loop. Poiche' EP e TD sono correlati 0.96 within, nella regressione
osservata i due si spartiscono la variazione, mentre in ogni regressione permutata TD e' l'unico ad
avere la struttura vera e assorbe tutto. Statistica osservata e statistica permutata **non escono
dallo stesso stimatore**, quindi la distribuzione di riferimento non e' la distribuzione nulla del
coefficiente riportato. Il segno del bias non e' ovvio a priori, ma la conclusione operativa lo e':
la permutazione andrebbe rifatta ripermutando **il profilo PTA completo** (EP *e* TotalDepth
insieme, sono attributi dello stesso accordo).

Non e' un dettaglio accademico: e' proprio sul dirty che permutazione (p=0.023) e WCB (p=0.072) danno
risposte diverse, e il paper usa il contrasto tra i due nella sezione "anatomy of a false positive".

**(b) Si permuta solo tra i 23 trattati.** `treated <- unique(cell[EP > 0, country_code])`, i
never-treated restano a 0 in ogni draw. Il test risponde a "il *quale* profilo e' andato a *quale*
partner e' casuale?", non a "il trattamento e' casuale". E' un test legittimo ma va etichettato per
quello che e': **non e' un placebo sul trattamento**, e non e' l'esercizio Bertrand-Duflo-Mullainathan
(che riassegna leggi placebo anche ai non trattati).

**(c) p-value senza correzione:** `mean(abs(b_perm) >= abs(b_obs))`. La forma corretta e'
`(1 + #{...}) / (1 + B)` (Davison-Hinkley); con B=1000 sposta 0.023 -> 0.024, irrilevante nel merito,
ma va sistemato perche' com'e' scritto puo' restituire p=0 esatto.

`[WARNING]` schema di permutazione non allineato allo stimatore osservato; p-value non corretto.

### 23_eventstudy_sunab.R — l'ATT aggrega su una finestra fortemente sbilanciata

`summary(m_sa, agg = "ATT")` media **tutti** i periodi relativi post, che in `sunab_gap.csv`
arrivano fino a `year::13`. La tabella delle coorti (prodotta dallo script stesso) dice che:

- `t = +13` e' identificato **solo dalla coorte 2002** (5 destinazioni);
- `t = -15` **solo dalla coorte 2015** (1 destinazione), `-14`/`-13`/`-12` da 3 destinazioni;
- la finestra realmente bilanciata su tutte e 23 le destinazioni e' `t ∈ {-2, -1}`.

L'ATT riportato (green -0.0440, dirty +0.0727) e' quindi una media su orizzonti tra 0 e 13 anni con
composizione di coorte che cambia a ogni orizzonte. Sun-Abraham protegge dall'eterogeneita' *tra
coorti a parita' di orizzonte*, non dal fatto che orizzonti diversi siano popolati da coorti diverse.
Va riportato un ATT su **finestra bilanciata** (es. `[-4, +5]`, dove ci sono >=17 destinazioni), e la
finestra piena va tenuta solo nel grafico. Il grafico (che gia' taglia a `[-6, 5]`) e' corretto; e'
il numero aggregato che non lo e'.

`[WARNING]` ATT Sun-Abraham su finestra non bilanciata.

### 23 sezione B — standard di inferenza incoerente col resto della pipeline

Il pre-trend `gap_dirty` a `t = -6` (+0.0465, p=0.0013) e' preso sul serio e riceve un'intera sezione
diagnostica. Ma il panel `gap` ha ~28 cluster e **qui si usano solo p-value asintotici cluster-robust**,
esattamente la situazione per cui tutto il resto della pipeline (20, 21, 27, 29) applica il wild
cluster bootstrap. Il pattern e' anche sospetto: p=0.0013 con se=0.0142, contro se 0.02-0.10 su tutti
gli altri lead — un se anomalmente piccolo su un lead identificato da 8 destinazioni e' proprio la
firma della over-rejection few-clusters. Prima di scrivere nel paper che c'e' un pre-trend dirty,
serve il WCB su quel coefficiente. Idem per l'ATT.

`[WARNING]` inferenza asintotica su 28 cluster dove altrove si usa WCB; il "pre-trend a t=-6" potrebbe
essere l'artefatto che il paper corregge ovunque tranne che qui.

### 28 / r79c_pretrends.csv — il detrending non conferma il null, semplicemente non puo' rifiutare

Nella variante con trend stimati solo sul pre-periodo, i se esplodono: WB green se_asy 0.0133
(vs 0.0070 nella baseline), IC WCB `[-0.172, +0.139]`. Il coefficiente e' pure diventato **positivo**
(+0.0168). Questa spec non e' evidenza a favore del null: e' una spec che ha perso quasi tutta la
variazione identificante e non distingue -0.17 da +0.14. Va presentata come "non informativa", non
come conferma.

`[WARNING]` robustezza presentata come conferma di un null quando in realta' non ha potenza.

### 26 vs 28 — i due controlli per trend danno risposte opposte

- `r79_desttrends.csv` (trend su tutto il periodo): `TREND_EP_Count:env_good` = -0.00216, p=0.0015,
  e **significativo anche sotto WCB** (`r79b_wcb_trends.csv`, p=0.0122, IC che esclude lo zero).
- `28` (trend solo pre-periodo): stesso coefficiente +0.0074, p_wcb 0.18.

L'header di 28 spiega bene perche' (contaminazione alla Wolfers 2006), e l'argomento e' corretto.
Ma il paper deve dire in chiaro che **esiste una specifica che rompe il null e sopravvive al WCB**,
e perche' la si scarta — non nasconderla dietro la variante preferita. E' il primo punto che un
referee andra' a cercare.

`[WARNING]` risultato WCB-significativo in una spec di robustezza, da dichiarare esplicitamente.

### 29_robustness_co2intensity.R — attenuazione meccanica sul margine continuo

```r
mu <- mean(cell$co2_total, na.rm = TRUE)
cell[is.na(co2_total), co2_total := mu]     # ~9.5% degli HS6
cell[, co2_z := (co2_total - mu) / sdv]     # sdv calcolato PRIMA dell'imputazione
```

Imputare la media al 9.5% dei prodotti e poi standardizzare comprime la dispersione del regressore:
classico errore di misura -> attenuazione di `ep_co2` verso lo zero. Il null sull'intensita' continua
e' quindi in parte meccanico. Va affiancata la stima sul solo sottocampione con concordanza
disponibile (dove il regressore e' misurato). Nota: `mu` e' anche una media **non pesata sulle celle**,
mentre la regressione e' pesata per `n` — l'imputazione non e' neutra rispetto ai pesi.

`[WARNING]` imputazione alla media su 9.5% del regressore continuo, non affiancata da sottocampione coperto.

### Merge senza diagnostica — pattern ripetuto in 9 script

In 16, 20, 22, 24, 25, 26, 28, 29, 30 e nel `.do` 17 ogni merge finisce con
`X[is.na(X), X := 0]` senza alcun controllo sul tasso di match. Il caso peggiore e' **25**:

```r
cell[idx, on = c("country_code","year"), SUB := i.SUB]
cell[is.na(SUB), SUB := 0]
```

una destinazione-anno trattata che per qualunque motivo non matchasse verrebbe ricodificata come
*non trattata su quel sotto-indice*, in silenzio e senza cambiare `nobs`. Oggi il merge funziona
(verificato: `wb_totaldepth_country_year.csv` ha 249 righe, 25 paesi, 0 `country_code` mancanti,
anni 2002-2015, nessun paese non mappato), quindi **non c'e' un errore in essere** — ma non c'e'
nemmeno niente che lo garantisca al prossimo rerun o su un altro campione.

Fix minimo, una riga per merge:
```r
stopifnot(cell[get(tr) > 0 & is.na(TotalDepth_nonEnv), .N] == 0)
```

`[WARNING]` nessuna asserzione di copertura sui merge; NA -> 0 e' silenzioso e semanticamente "non trattato".

### 24_stability_controlgroups.R — usa il CEM vecchio, non quello nuovo

`groups$cem_v1` legge `Output/CEM/matched_countries.csv` (root, pipeline vecchia, quella di cui
l'audit precedente diceva "CEM balance weak"). Nel frattempo `New/Output/CEM_v2/matched_countries_v2.csv`
esiste (prodotto il 2026-07-16 da `12_cem_matching.R`) e **non e' usato da nessuno script a valle**;
`New/Data/Matching_v2/` e' vuota. La tabella di stabilita' mescola quindi vintage di matching.

`[WARNING]` CEM v2 prodotto ma mai usato; la stability table gira sul matching v1.

### Collassato vs full panel — differenza di fattore 2.7 sul coefficiente chiave, non discussa

| | `WB x dirty` | p |
|---|---:|---:|
| collassato (16, FE pd+dt+pt, N=3.68M celle) | **-0.01187** | 7.8e-05 |
| full panel (17 Stata, FE fpd+fdt+pt, N=21.5M) | **-0.00435** | 0.052 |

Il full panel da' meno della meta' dell'effetto e a malapena il 10%. Le due stime sono su estimandi
diversi (il full panel ha il margine within-firm, il collassato pesa per `n`) e questo e' documentato
negli header, ma **la differenza quantitativa non e' spiegata da nessuna parte**. Dato che il paper
usa il dirty come esempio di falso positivo, questo e' un fatto a suo favore — va usato, non lasciato
implicito.

`[NOTE]` divergenza collassato/full panel sul coefficiente dirty non commentata.

### Altro

- `[NOTE]` `10_collapsed_panel.R` usa `first(WB_EP_Depth)` per cella senza asserire che l'indice sia
  costante entro `(hs6, country_code, year)`. Lo e' per costruzione, ma un `uniqueN(...) == 1` costa
  nulla e blinda il collasso.
- `[NOTE]` Multiplicita': 8 sotto-indici x 2 interazioni = 16 test in 25, piu' i 4 principali e le
  robustezze, senza alcuna correzione. Gli unici "significativi" nei sotto-indici
  (`TREND_GreenMarketAccess x dirty` p=0.045, `WB_GreenLib x dirty` p=0.071) non sopravvivono a un
  Holm sui 16. Se la tabella serve a mostrare che nessun canale morde, va detto che e' un esercizio
  esplorativo e vanno riportati i p aggiustati.
- `[NOTE]` `19_saturation_ladder.R` mette le stelle sulla ladder table con p-value asintotici a 23
  cluster. Il WCB della ladder e' in 21, separato: le due cose vanno unite, o la ladder va
  pubblicata senza stelle.
- `[NOTE]` WCB su `lm` FWL-demeanato (20, 28, 29): il bootstrap non ri-partialla gli FE a ogni
  replica e la correzione di gradi di liberta' usa k=4 invece di k = 4 + #FE assorbite. Nel rapporto
  t i fattori costanti si semplificano, quindi i p-value sono sostanzialmente corretti; gli **IC**
  scritti in `wcb_collapsed.csv` ereditano pero' la scala non corretta e sono leggermente stretti.
- `[NOTE]` `set.seed(42)` prima di `boottest()` non governa `dqrng`: i p_wcb non sono esattamente
  riproducibili (gia' noto, ~1pp di oscillazione). Se un p_wcb finisce vicino a 0.05 (oggi il piu'
  vicino e' 0.072) va riportato con questa avvertenza.
- `[NOTE]` `sd()`/`mean()` in 29 calcolati sulle celle e non pesati per `n`, mentre la stima e' pesata.
- `[NOTE]` East Timor (`144`) codificato erroneamente come membro ASEAN: gia' diagnosticato in
  `Diagnostics/timor_check.md`, impatto verificato nullo alla sesta cifra. Nessuna azione.
- `[NOTE]` Path `$ROOT` hardcoded per OS negli script Stata: convenzione dichiarata del progetto,
  non un difetto di replicabilita' entro il progetto.

**Cose che invece tornano e vanno segnalate come tali:**
- La verifica Frisch-Waugh interna in 16 (`stop()` se `feols` e FWL divergono a 1e-6) e il check
  identita' in 22 sono la risposta corretta al problema di corruzione silenziosa dei sottoprocessi.
  E' una pratica migliore dello standard del campo.
- `overlap_dirty_green_CHECK.csv`: i 17 codici in sovrapposizione **sono** rimossi dal dirty in
  `06_dirty_goods.R` (precedenza alla lista green). Green e dirty sono mutuamente esclusivi, la
  categoria omessa "neutri" e' ben definita. Nessun problema.
- Struttura triple-diff corretta: `dt` assorbe l'accordo e tutto cio' che varia a destinazione-anno,
  l'identificazione e' il differenziale green/dirty vs neutri entro destinazione-anno. Coerente tra
  R e Stata (stessa formula, stesso cluster, stesso filtro HK/MO).
- Parametrizzazione HK/MO via `_sample_config.R` con suffisso applicato **anche alle cache**: e'
  esattamente il rischio giusto da aver coperto.
- Nota su Mani-Wheeler core vs Tabella 1 originale (06): autocritica documentata correttamente.

---

## 2. Cross-Language Replication

**Saltata.** R e Stata non sono invocabili da questa shell e l'audit era richiesto sulla parte
econometrica. `New/verification/equivalence_log.md` e `compare_final_dataset.do` coprono gia'
l'equivalenza dataset R/Stata a monte.

---

## 3. Econometria — sintesi per dimensione

**Standard errors.** Cluster a `country_code` ovunque, giustificato (trattamento persistente per
destinazione, Bertrand-Duflo-Mullainathan). WCB applicato alle stime principali. **Buco:** il modulo
Sun-Abraham (23) e la ladder (19) restano su p asintotici a 23-28 cluster.

**Fixed effects.** `pd + dt + pt` (collassato) / `fpd + fdt + pt` (full panel) assorbono correttamente
il PTA e gli shock prodotto-anno; nessuna collinearita' tra FE. Il problema di collinearita' non e' tra
FE ma **tra regressori** (EP vs TotalDepth, within 0.96).

**Identificazione.** Il contrasto e' composizione green/dirty vs neutri entro destinazione-anno —
coerente col design dichiarato e con Abman-Lundberg-Ruta. Il punto debole non e' la struttura ma la
**potenza**: 23 destinazioni trattate, 2 coorti che ne contengono 15, 7 coorti singleton; le celle
`fdt` identificanti sono il 26% (green) e il 12% (dirty) delle trattate.

**Restrizioni di campione.** Coerenti e centralizzate (`_sample_config.R`, analogo `$HKMOEXPR` in
Stata). Verificato: nessun filtro HK/MO residuo inline fuori dai sottoprocessi callr, dove e'
correttamente duplicato con commento.

**Parallel trends.** Event study TWFE + Sun-Abraham presenti. Il pre-trend `dirty` a `t=-6` e'
l'unico segnale, ed e' proprio quello che manca di WCB (vedi sopra).

**Estensivo.** PPML con zeri (30) coerente col resto; nessun problema di specifica rilevato.

---

## 4. Summary & Required Actions

| # | Issue | Sev | File | Azione |
|---|---|---|---|---|
| 1 | Verdetto LOO vacuo (`b0` vuoto) + 3/23 paesi mancanti senza flag | CRITICAL | `31_robustness_leaveoneout.R` | rilanciare i 3 falliti + `baseline`; `stop()` se la tabella e' incompleta |
| 2 | `WB_GreenLiberalization` = `WB_StandardsNonRegression`/3 (stesso regressore) | CRITICAL | `25_heterogeneity_subindices.R` | tenerne uno solo; dichiarare che la versione WB e' binaria |
| 3 | Null green con corr. within 0.96 tra EP e TotalDepth | WARNING | `16`, tutta la pipeline | aggiungere MDE/equivalenza + spec senza interazioni TD |
| 4 | Permutazione: TD non permutato con EP; p senza `(1+·)/(1+B)` | WARNING | `22_permutation_inference.R` | permutare il profilo PTA completo; correggere il p |
| 5 | Permutazione solo tra trattati — non e' un placebo sul trattamento | WARNING | `22` + paper | rietichettare nel testo |
| 6 | ATT Sun-Abraham su finestra non bilanciata (fino a t=+13) | WARNING | `23_eventstudy_sunab.R` | ATT su finestra bilanciata `[-4,+5]` |
| 7 | Nessun WCB sul modulo sunab (28 cluster) | WARNING | `23` | WCB su ATT e su `t=-6` |
| 8 | Spec con trend (26) WCB-significativa e non dichiarata nel testo | WARNING | `26`/`27` + paper | dichiararla esplicitamente |
| 9 | Detrending (28) senza potenza, presentato come conferma | WARNING | `28` + paper | rietichettare "non informativa" |
| 10 | CO2: 9.5% imputato alla media -> attenuazione | WARNING | `29` | affiancare sottocampione coperto |
| 11 | Merge senza asserzione di copertura (`NA -> 0` silenzioso) | WARNING | 9 script + `17.do` | `stopifnot` sul match rate dei trattati |
| 12 | Stability usa CEM v1; CEM v2 prodotto e mai usato | WARNING | `24` / `12` | decidere quale vintage e allinearlo |
| 13 | Divergenza 2.7x collassato vs full panel sul dirty non discussa | NOTE | `16` vs `17.do` | paragrafo nel paper |
| 14 | Multiplicita' non corretta (16 test sui sotto-indici) | NOTE | `25` | p aggiustati o etichetta "esplorativo" |
| 15 | Stelle asintotiche sulla ladder a 23 cluster | NOTE | `19` | unire il WCB di `21` o togliere le stelle |
| 16 | IC del WCB su scala non corretta per DoF FE | NOTE | `20`, `28`, `29` | segnalare o correggere |
| 17 | `first(WB_EP_Depth)` senza asserzione di costanza entro cella | NOTE | `10` | `stopifnot(uniqueN(...) == 1)` |

**Totale: 2 critical, 10 warning, 5 note.**

---

## 5. Verdetto

**CONDITIONAL PASS.**

L'impianto econometrico e' corretto: la triple-diff e' specificata bene, gli FE assorbono quello che
devono, il clustering e' giustificato, il WCB c'e' dove serve di piu', e i controlli interni
Frisch-Waugh contro la corruzione silenziosa dei sottoprocessi sono sopra lo standard del campo.
Nessuno dei risultati principali risulta sbagliato.

Le due criticita' sono di **affidabilita' del reporting**, non di stima: un verdetto di robustezza
che non puo' fallire (#1) e due colonne di una tabella di meccanismo che sono la stessa variabile
(#2). Vanno sistemate prima di sottomettere, entrambe con interventi contenuti.

Il rischio piu' serio in sede di referaggio non e' nessuna di queste due, ma il **#3**: il paper
vende un null, e il null e' misurato con un controllo correlato al 96% con il trattamento. Senza un
MDE o un test di equivalenza, "non troviamo effetto" e "non riusciamo a misurarlo" restano
indistinguibili — ed e' esattamente l'obiezione che arriva per prima.
