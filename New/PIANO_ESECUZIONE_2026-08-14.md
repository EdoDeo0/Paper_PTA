# PIANO DI ESECUZIONE — 2026-08-14 (handoff per Sonnet, su Windows)

Handoff **self-contained**: contiene stato verificato sui file, comandi esatti, e i
checkpoint di verifica. Eseguire **in ordine**. Non rifare le modifiche al codice: il
codice è già aggiornato e committato (commit `72496f0`), **manca solo far girare gli
script** perché gli output CSV prendano le colonne nuove.

Macchina: Windows, `C:\Work\projects\Paper_PTA`. Qui vivono i dati **canonici** (i `.fst`).

---

## 0. REGOLE D'ORO (non negoziabili — nascono dai crash di agosto)

1. **Un solo processo per script.** Mai catene che lanciano più Rscript in parallelo sullo
   stesso log. Lancia, aspetta, verifica, poi il prossimo.
2. **Verifica sempre gli ARTEFATTI su disco, mai l'exit code.** Su questa pipeline il
   fallimento tipico è "exit 0 su lavoro incompleto". Dopo ogni run: apri il CSV, conta le
   righe attese, controlla che le colonne nuove ci siano, controlla che i numeri siano
   diversi dalla variante precedente (se hai cambiato variante).
2b. **Sorveglia la CRESCITA di un file** (il CSV/`.md` di output), non l'uscita del processo:
   un processo appeso non esce mai.
3. **Non editare mai `_sample_config.R` (o qualunque `.R`) mentre un run è attivo.** R
   sorcia gli script in modo incrementale: un edit a metà corsa corrompe il parse. Cambia la
   config **solo** quando non gira nulla.
4. **fixest: in-process, non `callr`.** 16b/20/31 sono già in-process — lasciali così. `callr`
   PEGGIORA i crash dell'allocatore, non li protegge.
5. Se uno script ha un `stop()` di guardia e scatta, **fermati e leggi il messaggio** — non
   aggirarlo. È lì apposta (dataset stantio, schema colonne vecchio, ecc.).

---

## 1. AMBIENTE — come si lancia

- **R** (v4.5.2):
  `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" New\Code\<script>.R`
  Working dir = root del repo (`here()` si ancora da solo). Redirigi stdout/stderr su un log
  nello scratchpad, es. `... 1> log.out 2> log.err`.
- **Stata** (batch): `"C:\Program Files\Stata18\StataSE-64.exe" /e do <wrapper>.do`
  (adatta la cartella Stata alla versione presente). File `.do` in **UTF-8 senza BOM**. Il
  log `.log` esce nella root del progetto. Lancialo in background e sorveglia la coda del log.

---

## 2. LE 4 VARIANTI — come si cambia campione/depth

Si tocca **solo** `New/Code/_sample_config.R`, righe **18-19**:

| Variante | riga 18 `SAMPLE` | riga 19 `DEPTH` | suffisso output |
|---|---|---|---|
| **baseline** (spec principale) | `"excl"` | `"totaldepth"` | *(nessuno)* |
| robustezza campione | `"incl"` | `"totaldepth"` | `_inclHKMO` |
| robustezza depth | `"excl"` | `"desta"` | `_desta` |
| robustezza doppia | `"incl"` | `"desta"` | `_inclHKMO_desta` |

I 4 pannelli `.fst` esistono già in `New/Data/Collapsed/` (verificato). Gli output di ogni
variante escono con il suffisso giusto via `out_path()` — non si sovrascrivono a vicenda.

⚠️ Ordine di lavoro per gli script multi-variante: cambi config → run → verifichi → **poi**
cambi config per la variante dopo. Mai due varianti insieme. **Lascia la config su
`excl`/`totaldepth` (baseline) a fine sessione.**

---

## 3. TASK A — i 4 script da rigirare

### A1 · Script **16b** (dose-bins) — SOLO baseline · **priorità: decide la mossa Callaway**

Perché prima di tutto: il suo risultato decide se serve uno stimatore a dose continua
(Callaway–Goodman-Bacon–Sant'Anna). Testa se l'effetto è **lineare nella dose** invece di
assumerlo.

- Config: `SAMPLE="excl"`, `DEPTH="totaldepth"` (baseline).
- Comando: `Rscript.exe New\Code\16b_dose_bins.R`
- Durata attesa: ~5-10 min.
- **Guardia**: 16b ha un `stop()` anti-dataset-stantio. Se scatta → il `.fst` è pre-fix
  env-laws, **fermati e segnala** (andrebbe rigenerato con lo script 10, ma NON farlo senza
  conferma dell'utente).
- Output attesi:
  - `New/Output/TripleDiff/Tables/dose_bins_collapsed.csv`
  - `New/Output/Diagnostics/16b_dose_bins.md`
- **Verifica**: il CSV ha 3 coefficienti di fascia (basso/medio/alto × green) + SE asintotici.
  Leggi il `.md`: dice esplicitamente se la fascia alta è muta (SE enorme) e se
  `bL/dose_L` ≈ `bH/dose_H` (linearità) oppure no (concavità).
- **Consegna all'utente**: riporta i 3 coefficienti, i loro rapporti dose-normalizzati, e la
  frase del `.md` sulla forma. Serve per la decisione B.

### A2 · Script **20** (WCB collassato) — **4 varianti** · aggiunge `nobs`/`nclust`/`fe`

Oggi `wcb_collapsed.csv` ha solo `treat,term,coef,p_wcb,conf_low,conf_high,B` (verificato).
Il codice nuovo esporta anche `nobs`, `nclust`, `fe` (i "236 clusters" citati nel paper oggi
vivono solo nel log).

- In-process, veloce (~1 min per variante).
- Per **ognuna delle 4 varianti** (vedi §2): setta config → `Rscript.exe New\Code\20_wcb_collapsed.R`
- Output per variante: `New/Output/TripleDiff/Tables/wcb_collapsed<suffisso>.csv`
- **Verifica per ogni file**: l'header ora contiene `nobs`, `nclust`, `fe`. `nclust` baseline
  deve essere ~236 (excl); `_inclHKMO` deve salire (+HK+Macao); `_desta` deve calare di ~2
  (Timor Est fuori). p_wcb devono restare **identici** a prima (il fix aggiunge colonne, non
  cambia le stime — se cambiano, qualcosa è andato storto).

### A3 · Script **31** (leave-one-out) — **4 varianti** · aggiunge `se`/`nobs`/`nclust`/`fe`

Oggi `dirty_leaveoneout.csv` ha solo coef+pval. Il codice nuovo esporta anche `se`, `nobs`,
`nclust`, `fe`, **e** ha un controllo di schema che **scarta** una cache a colonne vecchie
invece di mescolarla (righe 126-127).

- ~10-15 min per variante (nessuna cache: ~27 stime per variante).
- ⚠️ **Prima di ogni variante**, se esiste un `dirty_leaveoneout<suffisso>.csv` vecchio (a
  colonne vecchie), il controllo di schema lo scarterà da solo — ma verifica che lo faccia
  (il log lo dice). Non mescolare a mano.
- Per **ognuna delle 4 varianti**: setta config → `Rscript.exe New\Code\31_robustness_leaveoneout.R`
- Output: `New/Output/TripleDiff/Tables/dirty_leaveoneout<suffisso>.csv`
- **Verifica**: header con `se,nobs,nclust,fe`; numero righe = paesi trattati della variante
  (~23 excl, ~25 incl); nessun cambio di segno atteso; i coef devono coincidere con i valori
  storici (baseline riga vera ≈ −0,0057).

### A4 · Script **Stata 17** (triple-diff full panel) — F congiunto · baseline **obbligatorio**, 3 varianti opzionali

Il codice nuovo esporta il **test F congiunto** in `joint_F_fullpanel<suffisso>.csv` (oggi
**non esiste**, verificato) e mette `fe`/`nclust` via `addlabel` sui `regsave`.

- **Nota sulla cache**: 17 ha cache per modello + marcatore F (`_F_WB`, `_F_TREND`). Se i
  `.dta` dei modelli esistono ma i marcatori F no, 17 **ri-stima** (condizione
  `!wb_cached | !wb_fdone`). Quindi metti in conto ~25 min per la baseline (2 modelli).
- Baseline: config `excl`/`totaldepth` → lancia Stata 17 in batch.
- Output: `New/Output/TripleDiff/Tables/joint_F_fullpanel.csv` (+ i marcatori `_F_WB.txt`,
  `_F_TREND.txt`).
- **Verifica**: il CSV esiste, contiene la statistica F e il p-value per WB e TREND; i
  `regsave` `_full_WB.dta`/`_full_TREND.dta` ora hanno le etichette `fe`/`nclust`.
- **3 varianti restanti**: opzionali ma raccomandate **per allineare i CSV fra loro**
  (~25 min × 3 ≈ 75 min di Stata). Se il tempo/temperature non lo permettono, fai **almeno la
  baseline** e segnala che le altre 3 restano disallineate.

> ⚠️ **Temperature**: i crash di agosto avevano una componente termica (PC a ~90°C). Se la
> macchina scotta, spezza il lavoro e non incatenare le 4 varianti Stata di fila.

---

## 4. TASK B — decisione Callaway (dopo A1)

Non implementare niente prima di aver letto l'output di **16b**. Riporta all'utente e proponi:

- Se **16b mostra linearità plausibile** (i coef di fascia crescono ~in proporzione alla dose,
  fascia alta muta ma coerente): il β₁ lineare è già un buon riassunto → **Callaway aggiunge
  poco**, si può chiudere citando 16b come evidenza che la forma regge. Raccomandazione:
  **non** implementare lo stimatore continuo, documentare il limite.
- Se **16b mostra non-linearità** (concavità marcata, `bL/dose_L` ≫ `bH/dose_H`): allora vale
  la pena stimare a **dose continua** (Callaway–Goodman-Bacon–Sant'Anna 2024, NBER WP 32117)
  per mostrare la forma. In tal caso **fermati e chiedi conferma all'utente** prima di
  scrivere lo stimatore: i dati sono grumosi (11/23 paesi a dose 6, solo Perù/Svizzera/Corea
  sopra 7, Corea con 1 solo anno post) → probabile che documenti un limite più che superarlo.

**In entrambi i casi la decisione finale è dell'utente. Non implementare lo stimatore continuo
senza suo via libera esplicito.**

---

## 5. TASK C — pendenze aperte (dopo A, non calcolo pesante)

Da fare **solo dopo** aver chiuso il Task A e discusso B con l'utente. Ognuna può richiedere
una decisione: **non decidere da solo, proponi.**

1. **SD 2,7 vs 2,383 da allineare.** Nel paper compare una SD 2,7 che non combacia con 2,383
   calcolata altrove. Trova le due fonti (probabile: `33_mde_equivalence.R` / testo), stabilisci
   quale è quella giusta per il campione di stima vero, allinea. Verifica sul `.fst`, non a
   memoria.
2. **Conversione Brandi senza script generatore.** Il confronto con Brandi et al. (2020) nel
   paper è fatto a mano. Serve uno script che produca la conversione riproducibile (come per le
   tabelle). Proponi dove metterlo prima di scriverlo.
3. **`.gitignore` su `./New/Data/`.** Decidere cosa versionare e cosa no in `New/Data/` (i
   `.fst` grandi restano fuori). Proponi la regola, non applicarla senza conferma.
4. **Master script** che orchestra la pipeline in ordine (0→3 + analisi). Oggi manca. Proponi
   struttura (uno `.R`/`.ps1` che documenta l'ordine e lancia un-processo-per-script con le
   verifiche di §0), poi scrivilo.

---

## 6. CHECKLIST FINALE (spunta a fine sessione)

- [ ] A1: `dose_bins_collapsed.csv` + `.md` prodotti; forma della risposta riportata all'utente.
- [ ] A2: `wcb_collapsed*.csv` per le 4 varianti con colonne `nobs/nclust/fe`; p_wcb invariati.
- [ ] A3: `dirty_leaveoneout*.csv` per le 4 varianti con `se/nobs/nclust/fe`; nessun cambio di segno.
- [ ] A4: `joint_F_fullpanel.csv` (almeno baseline) esiste con F+p per WB e TREND.
- [ ] B: decisione Callaway presentata all'utente con raccomandazione basata su 16b.
- [ ] Config `_sample_config.R` riportata a `excl`/`totaldepth` (baseline).
- [ ] **Nessun commit** senza richiesta esplicita dell'utente in quel turno.
- [ ] `session-log.md` aggiornato con cosa è stato fatto e lo stato.

---

## 7. NOTE DI SICUREZZA

- Non committare e non pushare senza che l'utente lo chieda in quel turno.
- Se un run scrive numeri diversi da quelli storici attesi (§3 verifiche), **fermati**: è più
  probabile un dataset/cache stantio che una scoperta.
- Se ti serve rigenerare un `.fst` (script 10), **chiedi prima**: è l'operazione più rischiosa
  della pipeline.
