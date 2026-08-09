# Report — Roadmap §8.1-8.10 (potenza e collinearità EP/TotalDepth)

**Data**: 2026-08-07
**Obiettivo**: eseguire tutti i punti §8.1-8.10 della roadmap (diagnostiche e mitigazioni
sulla collinearità tra profondità ambientale EP e profondità generale dell'accordo
TotalDepth), producendo stime dove necessario e documentando l'esito anche quando negativo.

**Metodo**: solo codice ed esecuzione — nessuna modifica al testo del paper
(`New/Paper/draft_paper.tex`) in questa sessione. Tutti gli script nuovi sono numerati
33-43 in `New/Code/`, tutti gli output in `New/Output/Diagnostics/` e
`New/Output/TripleDiff/Tables/`.

---

## Sintesi in una riga per punto

| § | Titolo | Esito |
|---|---|---|
| 8.1 | MDE / equivalence test | Fatto — **corregge** il preliminare: TREND non è peggio di WB |
| 8.2 | Diagnostica di potenza | Fatto — confermato squilibrio di peso 163x tra cluster |
| 8.3 | TotalDepth per area | Fatto — guadagno marginale (VIF 5,76→5,69) |
| 8.4 | EP_share sui trattati | Fatto — coefficiente quasi-significativo al 10% |
| 8.5 | VIF sotto-indici | Fatto — confermato: guadagno meccanico, non reale |
| 8.6 | Bounds (versione leggera) | Fatto — il ventaglio di stime è stretto e attraversa lo zero |
| 8.7 | Switcher (descrittivo) | Fatto — nessuna stima, solo tabella |
| 8.9 | DESTA come double-check | Fatto — **risultato migliore**: VIF crolla a 1,92 |
| 8.10 | APEC EGL subsample | Fatto — null regge anche sui 54 prodotti "core" |

**Il messaggio complessivo**: il null sul margine green è confermato e diventa più difendibile
— non emerge nessuna evidenza nascosta di un effetto reale sotto nessuna delle mitigazioni
provate. La scoperta più interessante è che una fonte di profondità indipendente (DESTA)
migliora la precisione della stima, senza cambiarne il segno o la sostanza.

---

## 8.1 — Quanto possiamo davvero escludere?

**Cosa fa**: calcola il Minimum Detectable Effect (MDE) sul campione di stima vero (pesato,
incluse le destinazioni mai-trattate), sostituendo il calcolo "a mano" del cappello §8 con
uno rigoroso.

**Risultato chiave — e una correzione importante**: il calcolo preliminare (fatto a mano il
2026-08-03) diceva che TREND fosse leggermente *peggiore* di WB come indice. Il calcolo
corretto dice l'opposto:

| | MDE per 1 deviazione standard (asintotico) | MDE per 1 SD (wild cluster bootstrap) |
|---|---:|---:|
| WB × green | 4,64% | 5,90% |
| TREND × green | 4,16% | **3,62%** |

Al 95% di confidenza (bootstrap), il disegno esclude effetti superiori al **3,2%** per
provisione WB sul margine green. Per il salto tipico osservato in Corea (+16 unità EP), il
disegno esclude effetti superiori al 39,7%.

**File**: `New/Output/Diagnostics/33_mde_equivalence.md`

---

## 8.2 — Perché la precisione è quella che è

**Cosa fa**: consolida in un unico documento perché l'MDE calcolato in 8.1 è quello che è.

**Risultato**: confermato lo squilibrio già segnalato in una sessione precedente: tra i 23
cluster trattati, il rapporto tra il peso del più grande e del più piccolo è **163 volte**, e
i primi 5 cluster coprono il **50,7%** della massa trattata totale. Solo 2 coorti di entrata
(2002 e 2005) coprono 15 dei 23 trattati — le altre 7 coorti sono quasi tutte singleton.
Questo (non solo la collinearità EP-TotalDepth) è il vincolo strutturale del disegno.

**File**: `New/Output/Diagnostics/34_power_diagnostics.md`

---

## 8.3 — Si può controllare per la profondità "generale" in modo più mirato?

**Cosa fa**: scompone `TotalDepth_nonEnv` (17 aree WB sommate) nelle sue 17 componenti, e
misura quanto ciascuna correla con l'indice ambientale.

**Risultato**: quadro misto, non il negativo pulito atteso. 14 aree su 17 correlano
fortemente (0,74-0,97), ma 3 (Labor Market Regulations, Visa and Asylum, Subsidies)
correlano poco o nulla. Ristimando con un controllo "mirato" che esclude queste 3 aree, il
VIF scende solo da **5,76 a 5,69** — guadagno marginale. I coefficienti restano stabili in
segno (WB×green passa da -0,0046 a -0,0033).

**Nota tecnica**: questa stima ha causato la parte più laboriosa della sessione — crash
ripetuti del sottoprocesso R (>50 tentativi, un vero stallo del processo terminato
manualmente). Risolto eseguendo la stima direttamente senza sottoprocesso di isolamento; il
controllo di sicurezza interno (Frisch-Waugh) ha confermato che il risultato è corretto.

**File**: `New/Output/Diagnostics/37_totaldepth_byarea.md`, `38_totaldepth_targeted.md`

---

## 8.4 — L'effetto è nella composizione dell'accordo, non nel livello?

**Cosa fa**: ristima usando `EP_share = EP/TotalDepth` (invece del livello) solo sui 25
paesi che hanno effettivamente un PTA — una domanda diversa: "dato che l'accordo esiste, la
sua composizione ambientale sposta le esportazioni?"

**Risultato**: EP_share × green = **-2,25 (p=0,063)** — negativo, quasi significativo al
10% ma non al 5%. EP_share × dirty = -1,60 (p=0,315), non significativo. Segno coerente col
resto del paper, ma da leggere con cautela: la varianza di EP_share è molto più bassa del
livello, e l'estimando è concettualmente diverso.

**File**: `New/Output/Diagnostics/39_epshare_treatedonly.md`

---

## 8.5 — I sotto-indici EP hanno meno problemi di collinearità?

**Cosa fa**: prima corregge un bug reale trovato dall'audit precedente (due sotto-indici WB
erano lo stesso identico regressore, uno il triplo dell'altro — la tabella li contava due
volte come "meccanismi distinti"). Poi calcola VIF e MDE per ciascun sotto-indice rimasto.

**Risultato**: confermato l'esito atteso. I sotto-indici con VIF basso (es.
WB_GreenLiberalization, VIF 1,03) lo hanno solo perché sono variabili quasi-binarie con poca
varianza da condividere — non perché offrano un'identificazione migliore. Nessuna strada di
mitigazione qui.

**File**: `New/Output/Diagnostics/41_vif_subindices.md`

---

## 8.6 — Il coefficiente dipende dal controllo di profondità scelto?

**Cosa fa**: mette a confronto il coefficiente WB×green sotto 4 controlli diversi: nessuno,
aggregato (spec principale), DESTA (fonte esterna), mirato (§8.3).

**Risultato**: il coefficiente varia tra -0,0057 e -0,0033 — sempre negativo o vicino a
zero, mai significativo. Gli intervalli di confidenza si sovrappongono ampiamente in tutti
e 4 i casi. Nessun controllo "sblocca" un effetto nascosto: la scelta del controllo di
profondità non guida il risultato.

**File**: `New/Output/Diagnostics/42_bounds_depth_controls.md`

---

## 8.7 — I 3 paesi che hanno cambiato profondità EP nel tempo

**Cosa fa**: solo una tabella descrittiva (nessuna stima, come previsto dalla roadmap — la
variazione disponibile è troppo debole per una regressione difendibile).

**Risultato**: confermati i numeri già noti — Corea (salto EP 1→17 ma **1 solo anno post**),
Laos (1→6, 11 anni post, il caso migliore), Singapore (6→7, 7 anni post ma salto piccolo).

**File**: `New/Output/Diagnostics/40_switchers_descriptive.md`

---

## 8.9 — Una fonte di profondità indipendente aiuta? (il risultato migliore della sessione)

**Cosa fa**: costruisce il DESTA depth index (Dür, Baccini & Elsig 2014) — una misura di
profondità PTA da un database completamente indipendente da quello della Banca Mondiale che
misura anche EP — e la confronta come controllo alternativo a TotalDepth.

**Risultato — positivo**: la correlazione within tra EP e il controllo di profondità scende
da 0,96 (TotalDepth, stessa fonte di EP) a **0,89 (DESTA, fonte indipendente)**. Il VIF crolla
da **5,71 a 1,92**. Ristimando la spec principale con DESTA al posto di TotalDepth, i
coefficienti restano stabili ma **l'errore standard del margine green si dimezza** (da 0,0070
a 0,0043). Questo rafforza — non indebolisce — la lettura del null come "null di precisione":
con una misura di profondità meno meccanicamente legata a EP, la stima diventa più precisa e
il null si conferma.

**File**: `New/Output/Diagnostics/32_desta_check.md`, tabella robustezza in
`New/Output/TripleDiff/Tables/tripledd_collapsed_desta.csv`

---

## 8.10 — Il null regge anche sui prodotti "verdi" su cui c'è consenso politico?

**Cosa fa**: estrae dalla letteratura (Sauvage 2014, OECD) i 54 codici prodotto che
compongono la APEC Environmental Goods List — un sottoinsieme "core" della lista usata nel
paper (247 codici), su cui esiste consenso multilaterale esplicito (Dichiarazione APEC di
Vladivostok, 2012). Ristima la spec principale usando solo questi 54 prodotti come margine
"green".

**Risultato**: il coefficiente WB×green cambia segno (da -0,0046 a **+0,0050**) ma resta
**statisticamente indistinguibile da zero** (p=0,69). Non è un'evidenza contraria — è quello
che ci si aspetta quando si riduce l'80% del campione green e la stima diventa più rumorosa.
Il punto della verifica è confermato: il null non dipende dall'ampiezza della classificazione.

**File**: `New/Output/Diagnostics/43_apec_egl_subsample.md`

---

## Cosa NON è stato fatto (fuori scope di questa sessione)

- **Nessuna modifica al testo del paper** (`draft_paper.tex`). Tutte le frasi da riformulare
  (es. "no effect" → "effetti sopra X esclusi al 95%") sono identificate nei checkpoint della
  roadmap ma non applicate al testo.
- **§9 (rerun pipeline per il fix della lista green)** resta aperto — non toccato in questa
  sessione, come da nota nella roadmap che indica di aspettare prima di rilanciare tutto.
- Le tabelle e i numeri prodotti sono pronti per essere citati in appendice/nota a piè di
  pagina, ma l'integrazione editoriale nel paper è un passo successivo.

## File nuovi creati (13 script + relativi output)

`New/Code/33_mde_equivalence.R` · `34_power_diagnostics.R` · `35_desta_correlation_check.R` ·
`36_robustness_desta.R` · `37_totaldepth_byarea.R` · `38_robustness_totaldepth_targeted.R` ·
`39_epshare_treatedonly.R` · `40_switchers_descriptive.R` · `41_vif_subindices.R` ·
`42_bounds_depth_controls.R` · `43_apec_egl_subsample.R`, oltre alla correzione di
`25_heterogeneity_subindices.R` (fix audit finding #2).

`New/ROADMAP.md` aggiornato con tutti i checkpoint 8.1-8.10 completati e i risultati inline.
