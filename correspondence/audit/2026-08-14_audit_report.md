# Audit Report — `New/`, con focus sul draft inglese

**Data:** 2026-08-14
**Scope:** tutto `New/` — 41 script R, 4 `.do` Stata, gli output in `New/Output/`, e soprattutto
`New/Paper/draft_paper.tex` (1.237 righe, modificato oggi). Enfasi richiesta: **scelte
econometriche** — specificazione, identificazione, stima, interpretazione.
**Linguaggi disponibili:** R 4.x ✅ · Python 3.12 ✅ · Stata solo come app GUI (`StataNow`), non
da riga di comando.
**Replica cross-language: NON eseguibile.** `New/Data/` è in `.gitignore` **ed è fisicamente
vuota** su questa macchina: mancano `green_codes_hs1996.csv`, `dirty_goods_hs6.csv`, il panel
collassato, i flag dei sotto-campioni. Niente a valle dello script 10 è rieseguibile qui. Vedi
[N1].

**Rapporto con gli audit precedenti.** L'audit del 2026-08-12 esaminava
`Paper/Tabelle/Tabelle_Stime.tex`. Questo esamina il **draft inglese**, che è un documento
diverso e più recente. Diverse cose segnalate allora risultano **chiuse** (§6). Una **non lo è, e
nel draft è peggiorata** — è il punto 1 qui sotto.

---

## 0. In dieci righe

I numeri del draft si riproducono: ho verificato una per una tutte le cifre della prosa contro i
CSV e coincidono, tranne quelle elencate in §1 e §5. Nessun errore di calcolo nella pipeline.
La pipeline è, anzi, **insolitamente ben protetta**: guardie Frisch–Waugh con `stop()` che
bloccano i risultati corrotti dai crash dell'allocatore, cache suffissate per variante, e la
diagnostica C6 che riproduce il collassato dal full panel a 7 cifre. Questa parte è fatta bene.

I problemi sono tre, e sono tutti di **come i risultati vengono raccontati**, non di come sono
calcolati:

1. Una frase del draft afferma l'**opposto** di quello che dice l'output del suo stesso placebo.
2. Tutte le affermazioni di **precisione** — il cuore del paper — usano intervalli di confidenza
   asintotici, mentre gli intervalli bootstrap, che il paper stesso dichiara essere quelli
   giusti, esistono già su disco e sono **7 volte più larghi**.
3. Un modulo del paper (la quota verde within-firm) usa esattamente il disegno che la §3.1 del
   paper dichiara non identificabile.

---

## 1. CRITICO — Il placebo dice il contrario di quello che il paper scrive

**Dove.** `draft_paper.tex`, §5.1 «Provision bundling», righe 762–766:

> «The placebo components with no trade mechanism (TREND soft provisions, regulatory-space
> clauses) show nothing on either margin ($p \geq 0.27$), which is reassuring: **a selection
> story would light up precisely there**.»

**Cosa dice davvero l'output** (`Output/TripleDiff/Tables/subindices_collapsed.csv`):

| Sotto-indice placebo | × verde | $p$ | × sporco | $p$ |
|---|---:|---:|---:|---:|
| TREND soft provisions | +0.0132 | 0.275 | +0.0019 | 0.858 |
| **TREND regulatory space** | **+0.0242** | **0.015** | **+0.0225** | **0.0095** |

Il «$p \geq 0.27$» vale **solo** per il primo dei due placebo. Il secondo — che il paper nomina
esplicitamente — ha i due $p$-value **più bassi di tutta la tabella dei sotto-indici**, più bassi
di qualunque coefficiente della specifica principale.

**Perché è grave, e non è un refuso.** Il paper non si limita a riportare male un numero: ci
costruisce sopra un argomento («a selection story would light up precisely there»). Con la logica
del paper stesso, quel placebo che si accende è un segnale che **resta variazione confondente a
livello destinazione × anno × tipo-di-prodotto che gli effetti fissi non assorbono** — cioè
attacca l'identificazione, non solo quella tabella. È il primo punto su cui si fermerà un
referee, e la frase così com'è è indifendibile in replica.

Tutte le **altre** citazioni di quella sezione le ho verificate e sono corrette
(GreenLib × verde 0.90 ✓; TREND GMA × verde 0.37→«0.38» ✓; sporco 0.044–0.071→«0.04–0.07» ✓;
i quattro Enforcement 0.91/0.90/0.78/0.71 ✓). L'errore è isolato in una frase — ma è la frase che
chiude l'argomento.

**Da fare, nell'ordine:**
1. Correggere la frase. Il placebo va **dichiarato**, non nascosto.
2. Sottoporlo al wild cluster bootstrap. Con 3 country-year non nulli e 23 cluster trattati, un
   $p$ asintotico di 0.0095 verosimilmente evapora — ma va **mostrato**, non presunto. È lo
   stesso standard che il paper applica al margine sporco.
3. Se sopravvive, va usato per qualificare l'affermazione centrale.

*(Questo punto era già [C1] nell'audit del 2026-08-12. Nel draft attuale, due giorni dopo, la
frase non è solo rimasta: è diventata più assertiva.)*

---

## 2. CRITICO — «Null preciso»: la precisione è misurata con lo strumento sbagliato

Questa è la mossa retorica centrale del paper, e non regge nella forma attuale.

**L'argomento del paper.** §3.3 spiega, correttamente e a lungo, che con 23 cluster trattati
l'inferenza asintotica cluster-robust **non è affidabile**, e che per questo ogni stima
principale viene sottoposta a wild cluster bootstrap e permutazione. Poi §4.1 costruisce l'intera
affermazione di precisione sugli intervalli **asintotici**:

| Affermazione nel draft | Da dove viene |
|---|---|
| «full-panel 95% CI $[-0.0100, +0.0055]$» | IC **asintotico** (`tripledd_full_reghdfe.csv`) |
| «the upper bound is about **one twenty-ninth** of the Brandi-equivalent effect» | $0.16 / 0.0055$ |
| «a one-SD increase moves green rel. exports by **at most $-2.7\%$ to $+1.5\%$**» | IC asintotico × 2.7 |
| «it **firmly rejects** economically meaningful composition effects» | idem |

**Il numero giusto esiste già.** `New/Output/OLS/Bootstrap/wcb_fullpanel.csv`, prodotto da
`stata/17b_wcb_fullpanel.do` sulla **stessa** specifica principale, con `boottest` nativo dopo
`reghdfe`, B=9.999:

```
WB_green   coef -.0022564   p_wcb .686   CI [-.0353376, +.0355059]   N 21.519.511   G 225
WB_dirty   coef -.0043521   p_wcb .185   CI [-.0432458, +.0114816]
```

**Il confronto:**

| | IC asintotico | IC wild bootstrap | rapporto |
|---|---|---|---:|
| WB × verde, full panel | $[-0.0100, +0.0055]$ | $[-0.0354, +0.0355]$ | **6,5×** |

Riscrivendo le affermazioni del paper con l'IC che il paper stesso dichiara corretto:

- «un ventinovesimo di Brandi» → $0.16/0.0355 \approx$ **un quarto e mezzo**.
- «da $-2.7\%$ a $+1.5\%$ per 1 SD» → **circa $\pm 9.6\%$**.
- «rifiuta qualunque cosa più grande di qualche punto percentuale di Brandi» → **falso**. Regge
  invece la versione più debole che il paper usa altrove («metà delle magnitudini di Brandi»,
  cioè 0.08: $0.0355 < 0.08$, quindi quella sì).

**Non è un dettaglio di presentazione.** Un paper il cui contributo dichiarato è «un modello di
inferenza onesta quando i cluster trattati sono pochi» non può misurare la propria precisione con
lo strumento che dichiara inaffidabile. È il tipo di incoerenza che un referee metodologico usa
per rifiutare.

**Da fare.** Portare la riga «wild cluster bootstrap $p$» e l'IC bootstrap anche nella riga «Full
panel» della Tabella 3 (oggi esistono solo per il collassato), e ricalibrare tutte le
affermazioni di magnitudine di §4.1 sull'IC bootstrap. Effetto collaterale positivo: il $p_{wcb}$
= 0.185 sul margine sporco full-panel **rafforza** la tesi del falso positivo di §4.4, e oggi non
è nel paper.

**Nota collegata.** La SD usata nel paper («$\approx$ 2.7 provisions … across treated
destination–years») non è quella dello script: `33_mde_equivalence.R` calcola 2.3827, e la calcola
**su tutte le celle, incluse le mai-trattate con EP = 0** — non «across treated destination–years»
come dice il testo. Due discrepanze da chiudere: il valore e la sua definizione.

---

## 3. CRITICO — La quota verde within-firm usa il disegno che il paper dichiara non identificato

**Cosa dice il paper.** §3.1 («Why a level effect is not identifiable») argomenta — bene — che
regredire l'export sul *livello* di EP depth non identifica nulla, perché EP depth accende
insieme all'accordo e dopo non si muove più: «"environmental depth" and "having a (deep)
agreement" cannot be told apart». Tutta l'evidenza del paper è per questo costruita sul margine di
**composizione**.

**Cosa fa il modulo di §5.3.** `stata/18_robustness_fullpanel.do`, blocco `G_*_withinfirm`:

```
outcome:  quota verde del paniere impresa × destinazione × anno
regressori: WB_EP_Depth  (livello), totaldepth_nonenv
FE:       firm × destination, year
```

Non c'è nessuna interazione con `green`/`dirty`, e **non c'è la FE `fdt`**. È esattamente una
regressione di livello di EP depth: l'accordo non è assorbito da niente. Il confondente che §3.1
dichiara fatale è qui al suo posto, intatto.

Il paper però lo presenta come evidenza sostantiva («Chinese firms did not rebalance their
baskets») e ci appende un confronto con Cherniwchan (2017).

**Perché il null non salva la situazione.** Un null da un disegno non identificato non è
informativo: non si sa se il coefficiente sia zero perché l'effetto è zero o perché il
confondente lo cancella. E il coefficiente TREND è nominalmente significativo ($p=0.043$), il che
rende l'incoerenza visibile: il paper lo liquida con il caveat «few clusters», mentre il problema
vero è la specificazione.

**Due uscite oneste.**
- (a) Rifare il modulo come triple-diff coerente col resto (interagire EP con green/dirty dentro
  una struttura di FE che assorba l'accordo), oppure
- (b) tenerlo, ma **declassarlo esplicitamente a descrittivo**, con una riga che dica che è
  soggetto al confondente di §3.1 — e togliere il paragone con Cherniwchan, che implica
  un'interpretazione causale.

---

## 4. Econometria: gli altri punti, in ordine di importanza

### [E1] Trattamento continuo + adozione scaglionata: $\beta_1$ non è l'ATT che il paper definisce

§3.2 scrive una definizione formale in termini di potential outcomes
($\beta_1 = E[\dots \mid d,t \text{ treated}]$) e la chiama «the average treatment effect on the
treated». Con **dose continua** (EP depth da 1 a 17) e **adozione scaglionata** su 9 coorti, la
TWFE non restituisce quell'oggetto: restituisce una media ponderata di effetti dose-specifici con
pesi che possono essere negativi, e mescola l'effetto di «avere una dose» con quello di «avere una
dose più alta» (Callaway, Goodman-Bacon & Sant'Anna 2024, NBER WP 32117 — riferimento assente
dalla bibliografia).

**Quanto pesa davvero:** poco sulla sostanza — un null preciso resta un null sotto qualunque
schema di pesi. Ma pesa **direttamente sull'interpretazione MDE/equivalenza** di §4.1, che è
l'affermazione forte del paper: «escludiamo effetti sopra X» presuppone di sapere di quale media
di effetti X sia il limite. Va aggiunto il riferimento e una frase che circoscriva l'estimando.

**Fatto strutturale che rende il punto concreto:** la variazione within-destination è
praticamente inesistente (3 switcher: Laos 1→6 nel 2005, Singapore 6→7 nel 2009, Corea 1→17 nel
2015 — un solo anno post). E la variazione cross-destination è grossolana: 11 delle 23 trattate
(tutta l'ASEAN) hanno **lo stesso identico valore** (WB = 6). Il regressore «continuo» ha in
pratica ~9 valori distinti su 23 unità.

### [E2] «Il collassato replica l'identificazione one-for-one» — non è così

§3.2: «A collapsed panel … replicates the identification argument one-for-one — $dt$ absorbs the
agreement as $fdt$ does in the full panel — … and the two give nearly identical answers, an
internal cross-validation of both pipelines.»

**Verificato numericamente**, ed è un'ottima notizia sul codice: `tripledd_full_pddt.csv`
(reghdfe sul full panel con `absorb(pd dt pt)`) riproduce il collassato a 7 cifre —
$-0.0045685$ e $-0.0118734$ contro $-0.00456850$ e $-0.01187339$. Le due pipeline sono
algebricamente la stessa cosa. La diagnostica C6 funziona: complimenti, è la verifica giusta.

**Ma proprio per questo la frase del paper è sbagliata.** Il collassato **non** è il full panel
aggregato: è il full panel **senza le FE d'impresa**. `dt` non fa quello che fa `fdt` — `fdt`
confronta prodotti *dentro* la stessa impresa-destinazione-anno, `dt` confronta anche imprese
diverse. E le risposte non sono «nearly identical»:

| | full panel (`fpd+fdt+pt`) | collassato (`pd+dt+pt`) |
|---|---:|---:|
| WB × verde | −0.0023 | −0.0046 |
| **WB × sporco** | **−0.0044** ($p$ 0.052) | **−0.0119** ($p<$0.001) |

Un fattore **2,7×** sul margine sporco, e il salto di significatività su cui è costruita l'intera
§4.4. La lettura naturale del divario è **selezione di quali imprese esportano prodotti sporchi**,
non riallocazione within-firm — e questa è un'informazione interessante, non un problema. Vale la
pena scriverla: il collassato non valida il full panel, lo **completa**, e la differenza fra i due
misura il contributo della composizione fra imprese.

Nota operativa collegata: tutta la batteria di inferenza robusta (WCB, permutazione,
leave-one-out, trend di destinazione, CO₂) gira sul **collassato**, cioè sul disegno **senza** FE
d'impresa — mentre la tabella principale è il full panel. Il paper non lo dice.

### [E3] «Common support» è una riga vuota

Tabella 5 riporta «Common support (C-overlap) — 21.5M — −0.0022 — 0.57», e la Tabella 4 la
presenta come il test contro l'«extrapolation outside common support».

`Output/Subsamples/overlap_diagnostics.txt`:
```
Righe: 49.245.304 totali -> 49.244.934 con overlap_loose (100.0%)
```
La restrizione elimina **370 righe su 49,2 milioni**. Nella stima è ancora meno: 21.519.197
contro 21.519.511, cioè **314 osservazioni su 21,5 milioni (0,0015%)**. La riga non è inventata —
la stima esiste davvero (`D_WB_overlap`) — ma non prova nulla: è la baseline con un altro nome.

Da fare: o si dichiara nella nota che il supporto comune è di fatto totale (il che è un
**risultato**: non c'è problema di supporto), oppure si toglie la riga. Presentarla come una
delle quattro «tightenings» che il disegno supera è ciò che non regge.

### [E4] Il PPML non vede il margine estensivo che il paper dichiara

§5.2: «If EPs created green trade at the extensive margin — **new firm–product–destination
combinations** — the intensive estimates would miss it.»

`30_robustness_extensive_ppml.R` gira su `ppml_agg_pdt_zerofill.fst`, griglia **HS6 ×
destinazione × anno**. È il margine estensivo *prodotto-destinazione*, non *impresa*-prodotto-
destinazione. Non può, per costruzione, vedere una nuova impresa che inizia a esportare un
prodotto già esportato da altre — che è il caso di gran lunga più frequente e quello che la frase
promette. Basta correggere la frase.

### [E5] «La correlazione residua attenuerebbe, non gonfierebbe» — non è giustificato

§3.2, sul controllo `TotalDepth`: «because TotalDepth counts all non-environmental provisions
rather than isolating green-relevant ones, it is an imperfect proxy … so residual correlation
between $EP_{dt}$ and $TD_{dt}$ would **attenuate, not inflate**, the composition estimates.»

Un controllo mal misurato non produce attenuazione della variabile *di interesse*: produce
sotto-controllo, e $\beta_1$ ne raccoglie una parte dell'effetto della profondità generale, **con
segno indeterminato**. Non c'è nessun risultato che garantisca la direzione. Con corr. within 0,96
e VIF 5,76 (`14_descriptives_collinearity.md` ✓, coerente col «0.91 / 0.96 / 5.8» del testo) il
problema non è trascurabile.

La cosa da dire è quella vera e già dimostrata nello script 42: **il ventaglio dei controlli di
depth non muove il risultato** (da −0.0057 senza controlli a −0.0033 col controllo mirato, tutti
non significativi). Questo è un argomento solido; «attenuerebbe» non lo è. Sostituire.

### [E6] Il test di permutazione: cosa testa davvero

`22_permutation_inference.R` §B. Da segnalare tre cose, nessuna fatale ma tutte da dichiarare:

1. **Riassegna i profili solo fra le 23 trattate.** È un test valido, ma l'ipotesi nulla è «quale
   paese trattato ha quale profilo», non «trattato vs non trattato». Il paper lo descrive
   correttamente; va solo detto che è un nullo più ristretto.
2. **Il supporto effettivo è molto più piccolo di 1.000.** Le 11 destinazioni ASEAN hanno profili
   **identici**: permutarle fra loro non cambia nulla. I profili distinti sono ~9, non 23. La
   distribuzione di permutazione è quindi molto più granulare di quanto «1.000 draws» suggerisca,
   e i $p$-value hanno una risoluzione limitata da questo, non da $B$.
3. **Permuta contenuto *e* timing insieme** (`prof` contiene tutti gli anni, zeri pre-accordo
   compresi). Il draft lo descrive correttamente («entire EP profiles (depth and timing)»), ma
   allora la frase «is it really the environmental content» va attenuata: è un test congiunto
   contenuto+timing.

Da riconoscere: la correzione di continuità $(1+\#)/(1+B)$ e la permutazione **congiunta di EP e
TD** (che preserva la collinearità sotto il nullo) sono entrambe implementate correttamente. Erano
i due difetti seri segnalati nell'audit precedente: **chiusi**.

### [E7] Il WCB del collassato è un'approssimazione più grossolana di quello Stata

`20_wcb_collapsed.R` demeana con `fixest::demean()` e passa un `lm()` a `boottest()`. I gradi di
libertà e le correzioni small-sample di `boottest` vedono $k=4$ regressori, non $4 + \#\text{FE}$:
sono sbagliate. Il paper dichiara onestamente l'approssimazione FWL e il fatto che `pt` non è
nested nel cluster — ma non questo.

`stata/17b` fa invece la cosa giusta (`boottest` nativo dopo `reghdfe`). È **un'altra ragione** per
promuovere i risultati full-panel di 17b nel paper (vedi §2).

### [E8] Sun–Abraham: è diagnostica di timing, non conferma della specifica principale

`23_eventstudy_sunab.R` cambia tre cose rispetto alla specifica principale: trattamento
**binarizzato** (EP > 0 anziché continuo), **nessun controllo di depth**, e l'outcome è un gap
dest × anno costruito collassando via i prodotti — quindi **nessuna FE prodotto × anno**. È un
disegno diverso che risponde a una domanda diversa (l'entrata in un PTA con contenuto ambientale
ha spostato il gap di composizione?).

§4.3 lo usa come se convalidasse la statica. Va riqualificato come diagnostica di
timing/eterogeneità di coorte. *(Già indicato in `PIANO_RIPRESA_2026-08-09.md` §6 — non ancora
recepito nel draft.)*

L'appendice sul lead $t=-6$ è invece **molto ben fatta**: quattro argomenti indipendenti, con la
decomposizione per coorte a supporto. Nessuna obiezione.

### [E9] I sotto-indici non sono una decomposizione

`25_heterogeneity_subindices.R` stima **un sotto-indice per volta**, ciascuno da solo (più i
controlli TD). I coefficienti non sono quindi effetti parziali «a parità degli altri tipi di
clausola»: ognuno assorbe anche la variazione degli altri sotto-indici correlati con esso.
Scelta ragionevole visto il vincolo di collinearità che il paper documenta, ma §5.1 li legge come
una decomposizione dell'indice aggregato. Una riga di nota basta.

### [E10] Il detrending pre-trattamento ha regressori generati

`28_robustness_desttrends_pre.R`: stima le pendenze per destinazione in un primo stadio, le
sottrae dall'outcome, poi ristima e bootstrappa il secondo stadio. L'errore di stima del primo
stadio non entra nell'inferenza del secondo. Con SE già enormi ($+0.0168$ per WB × verde,
bootstrap $p=0.71$) il punto è più formale che sostanziale, e il paper è onesto nel definire il
risultato «an imprecise zero». Va solo dichiarato.

---

## 5. Numeri del draft che non tornano

| Riga del draft | Dice | Fonte dice | Gravità |
|---|---|---|---|
| §5.1, placebo | «$p \geq 0.27$» su entrambi i placebo | RegulatorySpace: 0.015 / 0.0095 | **CRITICO** — §1 |
| §4.1 | IC $[-0.0100,+0.0055]$ come misura di precisione | IC WCB $[-0.0354,+0.0355]$ | **CRITICO** — §2 |
| §4.1 | SD ≈ 2.7 «across treated dest–years» | script: 2.3827, su **tutte** le celle | WARNING |
| §2.2 | «a unique 1:1 concordance exists for all 247 codes» | 246/248 univoci; **2 non concordati**, restano codici HS2012 applicati a un panel HS1996 | WARNING |
| Tab. 3 | TREND × verde, WCB $p$ = 0.38 | 0.391 | NOTE |
| §4.1 | «one twenty-ninth of the Brandi-equivalent» | conversione 0.4 pp → 0.16 log points: **nessuno script la produce** | WARNING |
| §3.2 / Tab. 3 | test F congiunto $p$ = 0.31 / 0.71 | **nessuno script lo produce**, esiste solo nel `.tex` | WARNING |

Le ultime due meritano una riga in più: sono i due numeri che sorreggono rispettivamente
l'affermazione di magnitudine e l'affermazione di null congiunto, e **nessuno dei due ha un
generatore**. Il test F è un `test wb_green wb_dirty td_green td_dirty` dopo la `reghdfe` in 17 —
cinque minuti. La conversione Brandi è un calcolo a mano che andrebbe messo in uno script o in una
nota a piè di pagina esplicita, perché confronta anche due estimandi diversi (quota
sull'export aggregato di un paese vs. rapporto log within-firm).

---

## 6. Punti dell'audit 2026-08-12: cosa è chiuso

| | Esito |
|---|---|
| C6 — l'ipotesi «è la ponderazione» è falsa; fare `absorb(pd dt pt)` | ✅ **chiuso** — diagnostica C6 in `17_...do`, match a 7 cifre |
| C7.1 — la permutazione non permutava TD | ✅ **chiuso** — `prof` ora trasporta EP **e** TD |
| C7.3 — correzione di continuità | ✅ **chiuso** — $(1+\#)/(1+n)$ |
| C8 — `33_mde` mescolava le varianti | ✅ **chiuso** — `out_path()` su tutti e quattro i path |
| C9 — «MDE bootstrap» non era un MDE | ✅ **chiuso** — rinominato «semi-ampiezza», con nota esplicita |
| C7.2 — permuta anche il timing | ⚠️ **aperto** — vedi [E6.3] (ora è solo una questione di come lo si descrive) |
| C1 — placebo che fallisce, non dichiarato | ❌ **aperto e peggiorato** — vedi §1 |
| «il WCB full-panel non ha un posto nel paper» (ROADMAP §10) | ❌ **aperto**, ed è il punto §2 |

---

## 7. Struttura, riproducibilità, automazione

### [N1] `New/Data/` è vuota e in `.gitignore` — CRITICO per la replica

Sono ignorati per intero, e assenti su questa macchina:
`green_codes_hs1996.csv` (248 righe), `dirty_goods_hs6.csv` (~1.100 righe), il panel collassato,
i flag dei sotto-campioni, i file TotalDepth/DESTA.

Le prime due sono **la classificazione prodotto su cui poggia tutto il paper**, pesano pochi KB, e
non sono versionate. Oggi nessuno — incluso l'autore su un'altra macchina — può rieseguire o
verificare niente a valle dello script 10.

**Da fare:** togliere dal `.gitignore` almeno `New/Data/Classifications/`,
`New/Data/TotalDepth/`, `New/Data/Subsamples/`. Il divieto sui `.fst`/`.dta` grandi resta
sensato.

### [N2] Nessuno script master

I 41 script hanno un ordine implicito (numerazione + note «Input:»/«Sostituisce:» — che sono
scritte bene), ma non esiste un `run_all`. Con 4 varianti × ~25 script, la sequenza vive solo nei
`PIANO_*.md`. Un `00_run_all.R` che sorgi gli script nell'ordine giusto per la variante corrente
costa mezz'ora e chiude il punto.

### [N3] Path assoluti hardcoded negli `.do`

`$ROOT` è impostato con un `if c(os)` a tre rami. Funziona per l'autore, non per un replicatore.
Gli script R usano `here()` correttamente.

### [N4] Il paper ha zero `\input{}`

`44_make_tables_tex.R` genera 19 tabelle in `New/Paper/Tabelle/` — e `draft_paper.tex` non ne
usa **nessuna**: tutte le tabelle del draft sono battute a mano, in inglese, mentre le generate
sono in italiano. Sono due artefatti paralleli che possono divergere in silenzio, ed è esattamente
il rischio che il generatore era stato scritto per eliminare. Nota onesta: i numeri compaiono
anche nella **prosa**, e quelli nessun generatore li protegge (§1 e §5 ne sono la dimostrazione).

### [N5] `39_epshare_treatedonly.R` sovrascrive fra le varianti

Riga 34: `DEPTH_FILE` viene riassegnato dopo aver sorgiato il config (scelta probabilmente
intenzionale — `EP_share` ha bisogno di TotalDepth per definizione), ma poi la riga 145 scrive
`OUT_MD` **senza** `out_path()`. Ogni variante sovrascrive lo stesso `.md`, e il CSV esce con
suffisso `_desta` pur essendo calcolato su TotalDepth. Stesso pattern da ricontrollare in 37, 40,
41 (`out_path` = 0 chiamate).

### [N6] Struttura FE e livello di clustering non sono in nessun CSV

Nessun output registra se viene da `pd+dt+pt` o `fpd+fdt+pt`. È implicito nello script. Fra sei
mesi, o per un replicatore, i CSV non sono auto-descrittivi. Due colonne costanti in più.

### Cosa è fatto bene, e va detto

- **Le guardie Frisch–Waugh con `stop()`** in 16 e 22: dato che l'allocatore R su questa macchina
  può restituire un risultato silenziosamente corrotto dopo un retry, verificare ogni `feols`
  contro un demean+`qr.solve` indipendente e fermarsi a $10^{-6}$ è la risposta giusta. Non l'ho
  vista fare quasi mai.
- **La matrice 2×2 di varianti** con cache suffissate e l'avvertimento esplicito in
  `_sample_config.R` sul rischio di leggere la cache sbagliata.
- **La documentazione negli header degli script**: ogni file dichiara cosa sostituisce, cosa
  legge, cosa scrive, quanto ci mette. La nota in `06_dirty_goods.R` che ammette che il «core
  Mani–Wheeler» usato è la convenzione della letteratura e **non** la Tabella 1 dell'originale è
  il tipo di onestà che salva da un referee.
- **La diagnostica C6** e l'appendice sul lead $t=-6$.

---

## 8. Riepilogo e azioni

| # | Problema | Grav. | Dove | Costo |
|---|---|---|---|---|
| 1 | Il placebo regulatory-space è significativo; il draft afferma il contrario | **CRITICO** | `draft_paper.tex` §5.1 | 1 frase + 1 WCB |
| 2 | Precisione misurata con IC asintotici; l'IC WCB (7× più largo) esiste su disco | **CRITICO** | §4.1, Tab. 3 | riscrivere §4.1 |
| 3 | Quota verde within-firm: regressione di livello, disegno che §3.1 dichiara non identificato | **CRITICO** | §5.3, `18_...do` | ristimare o declassare |
| 4 | $\beta_1$ non è l'ATT definito (dose continua + staggered) | WARNING | §3.2 | ref. + 1 paragrafo |
| 5 | «il collassato replica one-for-one»: è il full panel senza FE d'impresa, 2,7× sul dirty | WARNING | §3.2 | riscrivere, e usare il divario come risultato |
| 6 | «Common support» droppa 314 obs su 21,5M | WARNING | Tab. 4-5 | togliere o dichiarare |
| 7 | PPML: griglia HS6×dest×anno, non impresa | WARNING | §5.2 | 1 frase |
| 8 | «attenuate, not inflate» non giustificato | WARNING | §3.2 | sostituire con l'argomento di 42 |
| 9 | Test F congiunto e conversione Brandi senza script | WARNING | §3.2, §4.1 | 1 riga Stata + 1 nota |
| 10 | SD 2.7 vs 2.383, e definizione diversa | WARNING | §4.1 | allineare |
| 11 | Concordanza green: 246/248, non «all 247» | WARNING | §2.2 | correggere + dire cosa si fa dei 2 |
| 12 | Permutazione: supporto effettivo ~9 profili, non 23; testa contenuto+timing | NOTE | §3.3 | 2 frasi |
| 13 | WCB collassato: df non contano le FE assorbite | NOTE | `20_...R` | dichiarare, o usare 17b |
| 14 | Sun–Abraham presentato come conferma della spec principale | NOTE | §4.3 | riqualificare |
| 15 | Sotto-indici letti come decomposizione | NOTE | §5.1 | nota di tabella |
| 16 | `New/Data/` vuota e ignorata: nulla è rieseguibile | **CRITICO (replica)** | `.gitignore` | 3 righe |
| 17 | Nessun master script; path assoluti Stata; 0 `\input{}`; 39 sovrascrive | NOTE | vari | — |

### Ordine d'attacco consigliato

1. **§1** — la frase del placebo. È l'unica cosa nel progetto che è *fattualmente sbagliata* in un
   modo che favorisce la conclusione del paper. Prima di tutto il resto.
2. **§2** — promuovere il WCB full-panel e ricalibrare le magnitudini. È il punto che decide se il
   paper è metodologicamente coerente con la sua stessa tesi.
3. **§3** — il modulo within-firm.
4. **[N1]** — versionare le due liste di classificazione. Tre righe, e il progetto torna
   verificabile.
5. Il resto, che sono correzioni di frase.

---

## 9. Verdetto

- [ ] PASS
- [ ] CONDITIONAL PASS
- [x] **FAIL** — tre problemi critici da risolvere prima che i risultati possano essere
      presentati così come sono.

**Precisazione importante, perché il verdetto non sia frainteso.** «FAIL» qui **non** significa che
i numeri siano sbagliati. Li ho verificati e sono corretti; la pipeline è solida e in alcuni punti
(guardie FW, diagnostica C6, matrice di varianti) sopra lo standard di quello che si vede di
solito. Il FAIL riguarda **tre affermazioni del draft** che l'output non sostiene: una che dice
l'opposto del proprio placebo (§1), una che misura la precisione con lo strumento che il paper
stesso dichiara inaffidabile (§2), e un modulo che usa il disegno che il paper stesso dichiara non
identificato (§3). Tutte e tre si chiudono **senza rifare nessuna stima pesante** — la §1 richiede
un solo WCB, la §2 usa un file che è già su disco, la §3 è una scelta fra ristimare e declassare.
Il lavoro empirico c'è; è il racconto che in tre punti ha preso una scorciatoia.
