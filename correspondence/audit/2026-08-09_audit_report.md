# Audit Report — Paper_PTA, cartella `New/`

**Data:** 2026-08-09
**Scope:** `New/Code/` (43 script R + 5 `.do`), `New/Paper/draft_paper.tex`, output in `New/Output/`
**Linguaggi disponibili:** R 4.5.2, Stata SE (17/18/19). Python presente.
**Modalità:** audit in sessione indipendente (nessuno script è stato scritto da questa sessione).
Nessuna stima è stata eseguita, nessun file dell'autore è stato modificato.
Cross-language replication (Step 2 dello skill) **non eseguita**: già coperta da
`New/replication/` e da `New/verification/equivalence_log.md`; l'utente ha chiesto
esplicitamente un audit di codice/econometria.

---

## Sintesi

L'impianto econometrico è solido e, in diversi punti, più onesto della media: la
triple-diff è specificata correttamente (i main effect sono assorbiti dalle FE giuste,
non omessi per sbaglio), l'inferenza è triangolata su tre metodi, e il paper **dichiara
esplicitamente** l'approssimazione Frisch-Waugh del wild bootstrap e il fatto che `pt`
non è nested nel cluster (§4, righe 519–525). Il guard Frisch-Waugh dentro
`16_main_tripledd_collapsed.R` (stop() se feols e FW divergono oltre 1e-6) è una difesa
seria contro la corruzione silenziosa da crash callr.

I problemi sono concentrati in due punti, entrambi introdotti nelle ultime due sessioni:

1. la **riscrittura dello script 21 del 2026-08-09 è una regressione, non un fix**;
2. l'**asse DEPTH della parametrizzazione (Fase A, 2026-08-07) non è arrivato nei suffissi
   di cache e output di 5 script R** — Run 3 e Run 4 produrranno numeri sbagliati senza
   sollevare alcun errore.

Il punto 2 è urgente: la Run 3 (excl + DESTA) è in coda per partire automaticamente.

**Verdetto: FAIL** — 3 problemi critici da risolvere prima che i risultati delle Run 3/4
possano essere considerati validi, e prima che lo script 21 venga rieseguito.

---

## 1. Econometria

### [CRITICAL] C1 — `21_wcb_ladder_fullpanel.R`: il "bug fix" del 2026-08-09 ha rotto uno script corretto — **RISOLTO 2026-08-09**

> **Aggiornamento (decisione utente):** il WCB sulla ladder è stato giudicato non portante
> e **rimosso**. Azioni: (a) tolta la frase di conferma WCB dal paper (`draft_paper.tex`,
> ex righe 422-423); (b) script 21 ripristinato alla versione corretta da HEAD e **ritirato**
> in `New/_legacy/code/` — la Run 3 lo salta automaticamente (glob `21_*.R` non trova nulla).
> Al suo posto, il WCB full-panel che serve davvero (sull'equazione principale, comparabile
> al panel collassato) è ora in **`New/Code/stata/17b_wcb_fullpanel.do`**: `reghdfe` +
> `boottest` nativo su `fpd+fdt+pt`, più rigoroso dell'approssimazione FW-una-volta del
> collassato. Sotto, la diagnosi originale.


Il session-log del 2026-08-09 riporta: *"stimava la spec della ladder (senza depth
control) invece della spec principale del paper. Bug preesistente"*. **Non era un bug.**

- Lo script 21 serve la **saturation ladder** (script 19), che è una specifica in
  **livelli** — `WB_EP_Depth` da sola, blocchi `*_NI` = *No Interaction*
  ([19_saturation_ladder.R:168-169](New/Code/19_saturation_ladder.R:168)). La riga più
  satura della ladder è `fpt + fpd`.
- Il paper cita esattamente quei numeri: *"il null nella struttura più satura è
  confermato da un wild cluster bootstrap (B=9.999; p=0.97 per WB, 0.64 per TREND, e
  0.97/0.61 con controlli)"* ([draft_paper.tex:422-423](New/Paper/draft_paper.tex:422)).
  Corrispondono riga per riga a `bootstrap_summary.csv` (spec `wb_baseline`,
  `wb_controls`, `trend_baseline`, `trend_controls`) e al log del 2026-07-22.

La riscrittura introduce tre problemi distinti:

**(a) Struttura FE sbagliata per l'estimand dichiarato.** Lo script nuovo mette i
regressori della triple-diff dentro le FE della ladder:

```
ln_export ~ EP:env_good + EP:dirty_p + depth:env_good + depth:dirty_p | fpt + fpd
```

La triple-diff del paper è `fpd + fdt + pt` — eq. (1)
([draft_paper.tex:451-453](New/Paper/draft_paper.tex:451)) e
[stata/17_main_tripledd_fullpanel.do:157](New/Code/stata/17_main_tripledd_fullpanel.do:157)
(`absorb(fpd fdt pt)`). Con `fpt + fpd` **nessuna FE assorbe la dimensione
destinazione×anno**, e `EP_dt` non è né incluso come main effect né assorbito. Per un
prodotto green, `ep_green = EP_dt`; per un neutro, `ep_green = 0`. Il coefficiente non è
quindi una tripla differenza: è l'effetto di livello di EP sui soli prodotti green,
confuso con qualunque shock destinazione-anno. È esattamente il confondimento che tutto
il §5.1 del paper è costruito per evitare.

**(b) Lo script non può girare.** Punta a due file inesistenti:

```
New/Data/GreenGoods/green_goods_hs1996.csv     <- non esiste
New/Data/DirtyGoods/dirty_goods_hs6.csv        <- non esiste
```

I path canonici, usati da tutti gli altri 15 script, sono
`New/Data/Classifications/green_codes_hs1996.csv` e
`New/Data/Classifications/dirty_goods_hs6.csv`. `fread()` fallisce dentro il sottoprocesso
callr → il `tryCatch` lo classifica come `[CRASH]` → 4 retry falliscono → `[SPEC FALLITA]`.
Riscontro empirico: **nessun `fw_boot_*.rds` esiste** in `New/Output/OLS/Bootstrap/` né in
`New/Output/OLS_inclHKMO/Bootstrap/`, e i due `bootstrap_summary.csv` contengono ancora i
nomi-spec vecchi. La riga di session-log *"Script 21 rieseguito per Run 1 e Run 2 con spec
corretta"* non è supportata dagli artefatti su disco.

**(c) Rischio di distruzione dei numeri del paper.** `fwrite(out, .../bootstrap_summary.csv)`
([21:145](New/Code/21_wcb_ladder_fullpanel.R:145)) è incondizionato. Al primo run che
riesce dopo la correzione dei path, i valori 0.97/0.64/0.97/0.61 citati a riga 422–423
vengono sovrascritti e **nessuno script nel repo li riproduce più**.

**Azione:** ripristinare la versione precedente
(`git show HEAD:New/Code/21_wcb_ladder_fullpanel.R`), che è corretta. Se serve davvero un
WCB full-panel sulla *triple-diff*, va scritto come script separato con FE `fpd+fdt+pt` e
file di output distinto — e realisticamente in Stata (`boottest` dopo `reghdfe`), visto
che è proprio la combinazione di FE che fa crashare R.

### [CRITICAL] C2 — L'asse DEPTH non è nei suffissi di cache e output (5 script R) — **RISOLTO 2026-08-09**

> **Aggiornamento:** corretto. `SAMPLE_SUFFIX` → `OUT_SUFFIX` nelle 5 righe sotto (22:221,
> 24:122, 25:95, 26:77, 30:76). Parse-check pulito su tutti e 5. Nessuna cache cancellata:
> il fix redirige le run desta su nomi `..._desta.rds` inesistenti (ricalcolo), lasciando
> intatte le `.rds` totaldepth di Run 1/2. Sotto, la diagnosi originale.
>
> Distinzione confermata sul disco: **24, 25, 26, 30** avrebbero riusato la cache `.rds`
> totaldepth → numeri sbagliati; **22** ha la cache dei batch già suffissata via `out_path`
> (numeri corretti) ma il CSV finale collideva col nome della run totaldepth → sovrascrittura.


`_sample_config.R` avverte esplicitamente:
> *"ATTENZIONE - out_path() va su path di CACHE oltre che sugli output finali: senza
> suffisso, una run 'incl' o 'desta' legge la cache della run precedente e restituisce
> numeri sbagliati SENZA errore."*

Cinque script usano `SAMPLE_SUFFIX` (solo asse HK/Macao) dove serve `OUT_SUFFIX`
(campione + depth):

| File | Riga | Cosa rompe |
|---|---|---|
| [22_permutation_inference.R:221](New/Code/22_permutation_inference.R:221) | `suff <- paste0(..., SAMPLE_SUFFIX)` | **nome dell'output finale** |
| [24_stability_controlgroups.R:122](New/Code/24_stability_controlgroups.R:122) | `suffix = SAMPLE_SUFFIX` → `STAB_*.rds` | cache modelli |
| [25_heterogeneity_subindices.R:95](New/Code/25_heterogeneity_subindices.R:95) | `SUBIDX_%s%s.rds` | cache modelli |
| [26_robustness_desttrends.R:77](New/Code/26_robustness_desttrends.R:77) | `r79_desttrends_%s%s.rds` | cache modelli |
| [30_robustness_extensive_ppml.R:76](New/Code/30_robustness_extensive_ppml.R:76) | `PPML_ext_%s%s.rds` | cache modelli |

Conseguenze concrete per la Run 3 (`excl` + `desta`), già in coda:

- **24, 25, 26, 30**: `OUT_SUFFIX = "_desta"` ma la cache cercata è `..._.rds` → il file
  della run totaldepth esiste già → viene letto. Il CSV finale (che *sì* passa da
  `out_path`) si chiamerà `..._desta.csv` ma conterrà i coefficienti totaldepth. È la
  modalità di fallimento peggiore possibile: nessun errore, nome del file che mente.
- **22**: il suffisso governa il *nome dell'output*, non solo la cache. La Run 3 scriverà
  su `r710_permutation_draws.csv` e `r710_permutation_summary.csv` **senza suffisso**,
  sovrascrivendo i risultati della spec principale — cioè il p=0.02 sul margine dirty
  citato in Tabella 5 e nel §6.

Gli script Stata 17 e 18 usano correttamente `$OUTSFX`: il problema è solo lato R.

**Azione, prima di far partire la Run 3:** sostituire `SAMPLE_SUFFIX` con `OUT_SUFFIX` nelle
5 righe sopra, e cancellare `New/Output/TripleDiff/Models*/*.rds` e
`New/Output/TripleDiff/Models/r710_batches/`. Verificare anche che i CSV di Run 1/Run 2 già
prodotti non siano stati contaminati (Run 1 e Run 2 differiscono solo per HK/MO, quindi
sono a posto — il danno è potenziale, non ancora avvenuto).

### [NOTA] C4 — Run 2 halted (crash allocatore su script 29), NON un bug di cwd — rettifica

Diagnosi iniziale (poi rettificata): sembrava un bug di working directory nell'orchestratore
che bloccava Run 2. **Non è così.** Ricostruzione dal log `run2_chain.log`:

- Script 24-28 completati con `here()` = radice progetto (cwd corretto).
- Script 29 (`co2intensity`) abortito dal crash **noto** `recursive gc invocation` (×4
  tentativi callr) — instabilità dell'allocatore, non cwd.
- Un *singolo* tentativo di resume (22:19:07) è partito da `C:/Users/edodr/Documents`
  (cwd errato) → fallito. Ma è stato **già corretto** da chi conduceva le Run: la versione
  attiva di `run2_resume29.ps1` contiene `Set-Location $repo`, rilanciata alle 22:19:40.
  Verificato empiricamente che quella riga fa partire Rscript dalla radice corretta.

Stato reale al momento dell'audit: nessun processo R attivo, entrambi i chain-log congelati
(run2 alle 22:19:40, run3 alle 22:20:20) → **pipeline ferma, non in loop**. Il vero ostacolo
a completare Run 2 è il crash stocastico dell'allocatore su script 29 (già in memoria di
progetto), non l'orchestratore. Voci residue: (a) l'orchestratore scrive l'output catturato
solo a fine processo, quindi gli script lunghi non mostrano progresso nel log — fastidio di
monitoraggio, non bug; (b) `run3_orch_v5.ps1` include lo **script 21 rotto** nella lista Run 3
(`@(14,16,19,20,21,...)`) → Run 3 abortirebbe sul 21 finché C1 non è risolto.

### [CRITICAL] C3 — `33_mde_equivalence.R` incrocia varianti diverse

[33:24-27](New/Code/33_mde_equivalence.R:24): `CACHE_FST` passa da `out_path()`, ma
`TRIPLEDD`, `WCB` e `OUT_MD` no. Nella run "incl" lo script calcola la SD dei regressori
sul panel *inclHKMO* e la combina con i SE e gli IC bootstrap letti dalle tabelle *excl*,
scrivendo su un `.md` con lo stesso nome della run precedente. Il numero che ne esce —
*"il disegno esclude effetti superiori a X%"*, riformulazione centrale suggerita per
l'abstract — può quindi essere un ibrido tra due campioni senza che nulla lo segnali.

### [WARNING] W1 — L'event study Sun-Abraham non stima l'estimand del paper

[23_eventstudy_sunab.R:91-92](New/Code/23_eventstudy_sunab.R:91):

```r
feols(gap_green ~ sunab(entry_year, year) | country_code + year, weights = ~n_tot, cluster = ~country_code)
```

Due scarti rispetto alla spec principale:

- **Nessun controllo di profondità.** L'intero argomento identificativo del paper è che
  `EP` va separato da `TotalDepth_nonEnv` (con cui correla 0,96 within). Qui il controllo
  non c'è. Il trattamento è "essere entrati in un PTA con EP>0", quindi il coefficiente
  misura *l'effetto dell'entrata in un PTA sulla composizione*, non l'effetto delle EP
  a profondità data.
- **Trattamento binarizzato** contro una spec principale a dose continua.

Come diagnostica di timing/eterogeneità di coorte è legittima e ben fatta (la sezione B
sul lead t=−6 è un lavoro accurato). Non va però descritta come conferma della spec
principale. Da segnalare anche che la coorte 2002 (5 destinazioni, incluso il blocco
ASEAN) contribuisce solo 2 periodi pre-trattamento, e che a t=−6 identificano solo 8
destinazioni su 23 ([r71_sunab_diag.md](New/Output/TripleDiff/Diagnostics/r71_sunab_diag.md)) —
il che spiega da solo la fragilità di quel lead, come il paper già riconosce.

### [WARNING] W2 — Trattamento continuo in un DiD scaglionato: riferimento e diagnostica mancanti

`WB_EP_Depth` (1–17) e `TREND_EP_Count` sono **dosi**, non trattamenti binari. Le tre
citazioni sugli stimatori robusti — Callaway & Sant'Anna (2021), Sun & Abraham (2021),
de Chaisemartin & D'Haultfœuille (2020) — riguardano tutte il caso binario, e `sunab()`
viene applicato a un trattamento binarizzato apposta. Il risultato pertinente è
**Callaway, Goodman-Bacon & Sant'Anna, "Difference-in-Differences with a Continuous
Treatment"** (NBER WP 32117 / arXiv 2107.02637; companion in AEA P&P 2024, "Event Studies
with a Continuous Treatment"): con dose continua il TWFE mescola *level effect* e *slope
effect* e richiede un parallel-trends **forte**, che vale tra livelli di dose diversi e
non solo tra trattati e controlli.

Questa è la lacuna metodologica più citabile che ho trovato: un referee di un journal di
commercio internazionale la solleverà. Nel caso specifico è probabilmente un'obiezione
gestibile (il paper riporta un null, e il bias da eterogeneità di dose non crea un null
spurio), ma va detto esplicitamente invece che lasciato implicito.

### [WARNING] W3 — I report `.md` non sono suffissati per variante

33, 34, 38, 39, 42, 43 (e 35, 41) scrivono `New/Output/Diagnostics/*.md` con `here()`
invece di `out_path()`, pur leggendo input suffissati. Ognuna delle 4 run sovrascrive il
report della precedente. I CSV sono corretti; sono i report leggibili — quelli da cui si
copiano i numeri nel testo — a essere ambigui.

### [WARNING] W4 — Convenzione del p-value di permutazione

[22:91](New/Code/22_permutation_inference.R:91) e
[22:233-234](New/Code/22_permutation_inference.R:233):
`mean(abs(b_perm) >= abs(b_obs))` può restituire esattamente 0 ed è lievemente
anti-conservativo. La convenzione esatta di un test di randomizzazione include il valore
osservato: `(1 + #{|b_perm| >= |b_obs|}) / (1 + B)`. Con B=1.000 il p dirty passa da
0,023 a 0,0240 — irrilevante per le conclusioni, ma è il genere di dettaglio che un
referee attento nota.

*Verificata e scartata una preoccupazione collegata*: permutare `EP` tenendo fisso il
vero `TotalDepth` potrebbe rompere la collinearità 0,96 e stringere artificialmente la
distribuzione nulla. Non succede: la sd dei 1.000 draw WB-dirty è 0,00302 contro un SE
cluster-robusto analitico di 0,00295. La permutazione è ben calibrata.

### [WARNING] W5 — Risultati parziali scritti come se fossero completi

Pattern ricorrente in 21, 22, 24, 30: quando una stima fallisce dopo i retry lo script
stampa un `cat()` e prosegue, poi aggrega e scrive comunque. In
[22:213-238](New/Code/22_permutation_inference.R:213) un batch fallito riduce
silenziosamente il numero di draw: `n_perm = nrow(dd)` viene registrato nel CSV (buono),
ma il paper cita "1.000 riassegnazioni" e nulla verifica che siano effettivamente 1.000.
Suggerimento minimo: `stopifnot(nrow(dd) == N_PERM)` prima di scrivere il summary, e in
21/24/30 non scrivere il CSV finale se `length(res) < length(SPECS)`.

### Cose che ho verificato e che sono a posto

- **Struttura FE della triple-diff collassata**: `pd + dt + pt` con solo le quattro
  interazioni. `EP_dt` è assorbito da `dt`, `green_p`/`dirty_p` da `pd`/`pt`. I main
  effect non sono omessi per errore: sono algebricamente assorbiti. Corretto.
- **WLS sul panel collassato**: `y` = media di `ln_export` per cella, pesi `= n`. Poiché
  regressori e FE sono costanti dentro la cella, i punti-stima coincidono con l'OLS
  micro sotto le stesse FE. La scelta della media invece di `ln(somma)` (evita Jensen) è
  giusta e documentata.
- **`boottest()` rispetta i pesi di `lm`**: verificato empiricamente (WLS e OLS
  restituiscono `point_estimate` e `t_stat` diversi). La via `demean pesato → lm(weights=n)
  → boottest` di [20_wcb_collapsed.R](New/Code/20_wcb_collapsed.R) è quindi valida.
- **L'approssimazione FWL del WCB è dichiarata nel paper**, incluso il fatto che `pt` non
  è nested nel cluster destinazione mentre `pd` e `dt` lo sono
  ([draft_paper.tex:519-525](New/Paper/draft_paper.tex:519)). La dichiarazione è accurata.
  Buona pratica, raramente vista.
- **Guard anti-corruzione in [16:102-121](New/Code/16_main_tripledd_collapsed.R:102)**: la
  verifica Frisch-Waugh con `stop()` a 1e-6 è la risposta corretta al problema noto
  "callr retry può restituire un coefficiente sbagliato senza errore".
- **Numeri della Tabella 5 verso i file sorgente**: full panel `wb_green = −0,00226
  (0,00393)`, `wb_dirty = −0,00435 p=0,0519`, collassato `−0,004569` e `−0,011873` —
  tutti coincidono con `tripledd_full_reghdfe.csv` e `tripledd_collapsed.csv`. L'IC
  `[−0,0100, +0,0055]` è aritmeticamente corretto.
- **Multiple testing sui sub-indici (25)**: 7 stime, nessuna dichiarata significativa nel
  paper. Non serve correzione: non c'è cherry-picking da correggere.
- **Seed**: presenti dove servono (12, 20, 21, 22, 27, 28, 29). La non-riproducibilità
  esatta di `boottest` (RNG `dqrng`) è nota e documentata.

---

## 2. Cross-Language Replication

Saltata (vedi *Scope*). Coperture esistenti: `New/replication/21_collapsed_replication.do`,
`New/replication/r79_desttrends_replication.do`, `New/verification/compare_final_dataset.do`
e `New/verification/equivalence_log.md`.

Nota: la spec principale full-panel gira **solo** in Stata (`reghdfe`, `fpd+fdt+pt`) perché
R crasha su quella combinazione di FE. Non esiste quindi una verifica indipendente in un
secondo linguaggio del risultato full-panel di Tabella 5. Data la centralità di quel
numero, una replica R su un sottocampione (es. le sole destinazioni ASEAN) che riproduca
il coefficiente Stata sarebbe un controllo a basso costo.

---

## 3. Directory & Replication Package

- Path relativi via `here()` ovunque in R: **buono**. Gli `.do` usano `global ROOT`
  condizionale su `c(os)` con tre rami (Win/Mac/Unix): portabile, ma il ramo Windows è
  hardcoded su `C:\Work\projects\Paper_PTA` — da documentare nel README di replica.
- Numerazione 01–43 coerente e sequenziale, `_sample_config.R` come unico punto di
  configurazione: **buono**. Il commento di intestazione di ogni script dichiara input,
  output e tempo di run — sopra la media.
- Separazione raw/derivati rispettata: il dataset originale in `Data/` non viene mai
  scritto, tutto l'output va in `New/`.
- **Manca uno script master** che esegua 01→43 in ordine. Esiste solo l'orchestratore ad
  hoc per le Run. Per un replication package serve un `run_all` esplicito.
- I numeri di script citati nelle intestazioni sono **sfasati**: `16_main_tripledd_collapsed.R`
  si intitola "12 —", `20_wcb_collapsed.R` si intitola "16 —", `23_eventstudy_sunab.R`
  "19 —", ecc. Residuo della rinumerazione +4 del 2026-07-20. Confonde chi legge (e i
  riferimenti incrociati dentro i commenti: "come in 12", "da 06").

---

## 4. Output Automation

- Tutte le tabelle e figure principali sono generate da script. Nessun segno di editing
  manuale.
- **[NOTE] Il test F congiunto non ha uno script generatore.** Il paper riporta *"joint F
  on the four interactions: p=0.31 (WB), p=0.71 (TREND)"*
  ([draft_paper.tex:598](New/Paper/draft_paper.tex:598) e §5.1): non esiste alcun `test`
  o `testparm` in `17_main_tripledd_fullpanel.do` / `18_robustness_fullpanel.do`, né
  `wald()` in nessuno script R. Numero non riproducibile dalla pipeline. Va aggiunto un
  `test wb_green wb_dirty td_green td_dirty` dopo la `reghdfe` in 17, con export.
- I `.md` diagnostici sovrascritti tra varianti (W3) sono un rischio di automazione: il
  file esiste ma non si sa a quale run appartiene.

---

## 5. Bibliografia

Verificati tutti i 36 `\bibitem` contro i record delle fonti. **Nessun errore trovato**:
volumi, numeri, pagine e titoli sono corretti, incluso il caso che avevo motivo di
sospettare — Abman, Lundberg & Ruta (2024), *JEEA* 22(6), 2507–2548, DOI
`10.1093/jeea/jvae023`: esatto. Tutte le chiavi citate hanno un `bibitem` e tutti i
`bibitem` sono citati.

Due osservazioni:

- **[NOTE]** Riferimento mancante, non sbagliato: Callaway, Goodman-Bacon & Sant'Anna
  sul trattamento continuo (vedi W2). È l'unica lacuna bibliografica sostanziale.
- **[NOTE]** Resta aperto l'item segnalato il 2026-07-30: la frase tra virgolette
  *"content conditional on agreement"* attribuita a `\citet{abman2024}`
  ([draft_paper.tex:105-106](New/Paper/draft_paper.tex:105)) è una parafrasi presentata
  come citazione diretta. L'utente aveva detto che l'avrebbe sistemata a mano; non è
  ancora sistemata.

---

## 6. Note minori

- **[NOTE] 17 codici HS6 sono sia green sia dirty** (`overlap_dirty_green_CHECK.csv`).
  `env_good` e `dirty_p` non sono mutuamente esclusivi, quindi quei codici caricano su
  entrambe le interazioni e la categoria di riferimento è "né green né dirty". È coerente
  e il file di check mostra che la cosa è stata guardata, ma il paper dovrebbe dire
  esplicitamente che il gruppo di confronto è "neutri" nel senso di *nessuno dei due*,
  non "non green".
- **[NOTE] `30_robustness_extensive_ppml.R`** si intitola "margine estensivo" ma PPML sui
  livelli identifica intensivo + estensivo insieme. Il testo del paper è più preciso
  dell'intestazione dello script; è solo il commento a essere impreciso.

---

## 7. Azioni richieste

| # | Problema | Severità | File | Urgenza |
|---|---|---|---|---|
| 1 | Script 21: FE sbagliate + input inesistenti | CRITICAL | `21_wcb_ladder_fullpanel.R` | **RISOLTO** — ladder WCB rimosso; sostituito da `stata/17b_wcb_fullpanel.do` |
| 2 | `SAMPLE_SUFFIX` invece di `OUT_SUFFIX` su cache e output | CRITICAL | 22, 24, 25, 26, 30 | **RISOLTO 2026-08-09** |
| 3 | MDE incrocia varianti (input non suffissati) | CRITICAL | `33_mde_equivalence.R` | differibile (nessuna dipendenza) |
| 3b | ~~Orchestratore: resume da cwd sbagliata~~ → rettificato: Run 2 halted per crash allocatore su 29; cwd già patchato (`Set-Location`) | NOTA | orchestratore Run | non un bug aperto |
| 4 | Sun-Abraham senza depth control: estimand diverso | WARNING | `23_eventstudy_sunab.R` | in fase di scrittura |
| 5 | Trattamento continuo: riferimento e discussione mancanti | WARNING | `draft_paper.tex` §4 | prima del submit |
| 6 | Report `.md` non suffissati per variante | WARNING | 33, 34, 35, 38, 39, 41, 42, 43 | prima della Run 3 |
| 7 | p-value di permutazione: convenzione `(1+r)/(1+B)` | WARNING | `22_permutation_inference.R` | basso |
| 8 | Risultati parziali scritti come completi | WARNING | 21, 22, 24, 30 | basso |
| 9 | Test F congiunto senza script generatore | NOTE | `stata/17_*.do` | prima del submit |
| 10 | Citazione Abman non verbatim (pending dal 2026-07-30) | NOTE | `draft_paper.tex:105` | prima del submit |
| 11 | Numeri di script nelle intestazioni sfasati di −4 | NOTE | quasi tutti | basso |
| 12 | Manca uno script master `run_all` | NOTE | `New/Code/` | replication package |

## 8. Verdetto

- [ ] PASS
- [ ] CONDITIONAL PASS
- [x] **FAIL** — tre problemi critici. Nessuno invalida i risultati **già prodotti** per
  Run 1 e Run 2 (che ho verificato coincidere con quanto riportato nel paper); tutti e tre
  invalidano ciò che sta per essere prodotto, o distruggono ciò che è già stato prodotto.
  Il #1 e il #2 vanno risolti prima di far girare qualunque altra cosa.

---

# Audit mirato — `New/Code/stata/17b_wcb_fullpanel.do` (2026-08-09, sera)

**Nota di indipendenza:** questo script è stato scritto in questa stessa sessione (l'auditor
è anche l'autore dell'impianto). Il blocco di residualizzazione FWL è stato modificato dopo
la stesura (utente/linter) e non è dell'auditor. Audit svolto su richiesta esplicita; per
piena indipendenza servirebbe una sessione terza. Nessuna modifica applicata al file.

**Scope:** solo `17b_wcb_fullpanel.do`. Cross-language replication: **non fattibile** (vedi sotto).

## Cosa esegue ora il codice
FWL esplicito: `reghdfe var, absorb(fpd fdt pt) residuals()` su ognuna delle 5 variabili, poi
`regress ... , nocons vce(cluster country_code)` + `boottest` sul regressore residualizzato.
I coefficienti coincidono col point-estimate `reghdfe` per il teorema FWL.

### [WARNING] A1 — L'header descrive un metodo diverso da quello eseguito (claim di rigore falso)
Header righe 15-21: «native boottest… più rigoroso del collassato perché non finge che `pt`
sia nested nel cluster». Il codice fa invece FWL residualize-once → **esattamente** quella
approssimazione, identica a `20_wcb_collapsed.R`. Il valore residuo di 17b è reale ma diverso
(WCB sul campione **pieno**, within-firm/`fdt`, non sul collassato), NON un maggior rigore su
pt-nesting. Correggere l'header e non rivendicare quel rigore nel paper. La riga 158 afferma
che `boottest` nativo «non funziona dopo reghdfe con più FE assorbite»: non verificabile senza
Stata — **se il nativo funziona in questa versione, usarlo** (dà davvero la versione rigorosa
e più leggera in RAM); altrimenti l'header va riscritto per descrivere onestamente l'approssimazione.

### [WARNING] A2 — Guard di equivalenza dichiarato ma non implementato
Riga 163 dice che la reghdfe di point-estimate serve «come check che il FWL concordi», ma
nessuna riga confronta i due. Nel CSV `coef = b_wbg` (reghdfe) mentre `p_wcb`/CI vengono dal
`regress` FWL: se le 5 residualizzazioni e la reghdfe scartano singleton in modo diverso, si
accoppia un coef di un campione con un p-value di un altro, senza errore. Aggiungere dopo il
`regress`: `assert reldif(_b[\`ewbg'], \`b_wbg') < 1e-6` (stop altrimenti), come i guard FW in R 16/22.

### [NOTE] A3 — Memoria
5 variabili residualizzate `double` su 21,5M righe (~0,9 GB) + overhead reghdfe, su macchina
con instabilità note dell'allocatore. Il nativo (se funziona) sarebbe più leggero.

### [NOTE] A4 — IC potenzialmente disgiunto
`r(CI)` può avere >1 riga (WCB con intervalli disgiunti); il codice prende solo il primo intervallo.

### [NOTE] A5 — Seed condiviso
green e dirty usano entrambi `seed(42)`: riproducibile ma non sono draw indipendenti. Innocuo.

### [OK] Coerenza
Cluster `country_code`, FE `fpd fdt pt`, filtri HK/MO e depth-drop rispecchiano lo script 17.
Suffissi `$OUTSFX` e cache corretti.

## Cross-language replication — non fattibile
R crasha su `fpd+fdt+pt` full panel (motivo per cui 17b è in Stata): replica R a piena scala
infattibile; su sotto-campione verificherebbe solo il point-estimate, non il WCB. Python
(`pyfixest`+`wildboottest`) urterebbe lo stesso muro di RAM, non verificato qui. Saltata.

## Verdetto 17b
**CONDITIONAL PASS** — nessun errore che produce numeri sbagliati *di per sé*; lo script gira e
dà un WCB full-panel valido sotto l'approssimazione demean-once. Ma prima che i numeri entrino
nel paper: (A1) correggere l'header / provare il nativo, (A2) aggiungere il guard di equivalenza.
