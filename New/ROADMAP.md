# ROADMAP — Revisione empirica Paper_PTA

> **Scopo di questo file.** Istruzioni dettagliate e autosufficienti per una sessione futura
> (eseguibile anche da un modello più piccolo, es. Sonnet) per ripulire e irrobustire
> l'analisi empirica del paper sulle Environmental Provisions (EP) nei PTA cinesi.
> Tutto il contesto necessario è qui dentro: **non serve ri-leggere tutto il repo**.
>
> Autore della diagnosi: sessione Opus del 2026-06-08.
> Stato dati/codice fotografato in questo file: vedi sezioni §2–§3.
>
> ⚠️ **AGGIORNAMENTO 2026-06-09 — RIDISEGNO (§7).** Una revisione complessiva del progetto
> ha prodotto un ridisegno dell'identificazione che **supera le Fasi 2–5 sottostanti**:
> il nuovo piano operativo è in **§7**. La Fase 1 (§4) resta valida ed è in esecuzione.
> Le vecchie Fasi 2–5 restano come riferimento ma vanno lette attraverso §7.
>
> ⚠️ **AGGIORNAMENTO 2026-06-24 — FASE R-CONTROL (§7.4).** Aggiunti gruppi di controllo /
> sub-campioni — sia per **feasibility** (rendere eseguibili PPML aggregato e stimatori DiD
> moderni, oggi impossibili a 49,2M righe) sia per **robustezza** della triple-diff §7.1 — con
> valutazione econometrica completa. Chiude il pending "Fase R-control" aperto nei log dal
> 2026-06-18. Vedi **§7.4**.
>
> ⚠️ **AGGIORNAMENTO 2026-07-12 — REVISIONE PRE-SUBMISSION (§7-R7).** Referee report
> simulato sulla bozza → 11 task in 4 priorità. Bloccante: il pre-trend Sun-Abraham dirty
> a t=−6 (R7.1, ex pending B4). Poi riframing a costo zero (R7.2-R7.5), diagnostiche
> leggere (R7.6-R7.8), stime nuove (R7.9-R7.11). Vedi **§7-R7** in fondo al file.
>
> ⚠️ **AGGIORNAMENTO 2026-07-15 — RIORDINO E RISCRITTURA DI `New/`.** Campagna §7-R7
> completata. Prossimo cantiere: pulizia, riorganizzazione e riscrittura leggibile di
> tutti gli script in `New/Code/` con verifica di equivalenza degli output (condizione
> non negoziabile). Piano dettagliato e autosufficiente per un modello più economico:
> **`New/PIANO_RIORDINO_2026-07-15.md`**.
>
> ✅ **AGGIORNAMENTO 2026-07-16 — RIORDINO COMPLETATO.** Le 3 fasi del piano
> (pulizia, riorganizzazione, riscrittura) sono state eseguite ed OGNI script
> riscritto è stato verificato contro un riferimento congelato pre-riordino.
> Dettaglio completo per ogni script in **`New/verification/equivalence_log.md`**.
> Sintesi:
> - **Pulizia**: 44 file superati spostati in `New/_legacy/` (script deprecati/
>   duplicati/one-off, piani e report datati, log di esecuzione). Nessuna
>   cancellazione.
> - **Riorganizzazione**: `Data/Concordance`+`Data/Dirty` uniti in
>   `Data/Classifications/`; `Data/Dirty/shapiro2021` spostato in
>   `Data/External/`; nuova cartella `Code/stata/` per i 2 script `.do`;
>   nuova cartella `verification/` col protocollo di equivalenza.
> - **Riscrittura**: 27 script (25 R numerati 01-27 + 2 Stata) sostituiscono
>   ~42 script legacy sovrapposti/duplicati, in ordine logico del paper
>   (dataset → descrittive → stime → inferenza → robustezze). Ogni script ha
>   header con input/output/tempo di run, sezioni commentate, stile uniforme
>   (`rm(list=ls())` in alto, una `library()` per riga).
> - **Verifica**: TUTTI gli output numerici (CSV/tex) sono risultati
>   identici o entro tolleranza nota (vedi sotto) rispetto al riferimento
>   pre-riordino — nessuna regressione nei risultati del paper.
> - **Scoperte fatte durante il riordino** (non note prima):
>   1. **Bug di corruzione silenziosa**: un sottoprocesso `callr` crashato e
>      ritentato può, raramente, restituire un coefficiente completamente
>      sbagliato SENZA sollevare errore (osservato 2 volte, su script 16 e
>      31 — numerazione aggiornata al 2026-07-20, vedi sotto). Mitigato con
>      una verifica Frisch-Waugh interna (`stop()` se il risultato non è
>      auto-consistente). Salvato come memoria di progetto permanente
>      (`fixest-callr-crash-can-silently-corrupt-results`).
>   2. **`fwildclusterboot` p_wcb non esattamente riproducibile**: il
>      generatore `dqrng` interno non è seedato da `set.seed()` di R base;
>      i p-value del wild cluster bootstrap oscillano di ~1pp anche tra run
>      identiche (coefficienti restano deterministici). Non è un bug del
>      nostro codice. Salvato come memoria di progetto permanente.
>   3. **Il riferimento pre-riordino stesso era incompleto in due punti**,
>      mai notato prima: `deepshallow×TREND` (script 24/13) non aveva mai
>      una cache valida, e il leave-one-out dirty (script 31/15b) aveva solo
>      11/27 righe. Il riordino li ha completati per la prima volta — i
>      nuovi numeri coincidono esattamente con quelli già citati nel paper
>      (deep-vs-shallow TREND -0.0004/p=0.72; leave-one-out Corea -0.0059/p=0.21).
> - **Non eseguito** (deliberatamente): `09_wits_tariffs.R` (API WITS-TRAINS
>   nota come irraggiungibile, mai completata con successo, nessun output
>   da riprodurre).
> - Nessun commit fatto da Claude (regola del progetto): tutte le modifiche
>   sono nel working tree, pronte per la review dell'utente.
>
> ⚠️ **AGGIORNAMENTO 2026-08-03 — POTENZA E COLLINEARITÀ EP/TotalDepth (§8).** Discussione
> Q&A (non un audit) partita dal finding #3 dell'audit `2026-08-02_audit_report.md`: la
> collinearità within destinazione-anno tra `WB_EP_Depth` e `TotalDepth_nonEnv` (corr. 0,96,
> VIF 5,76). Confrontato con Brandi et al. (2020, wiki), che ha lo stesso problema più diluito
> (0,67, VIF 4,62) per scala campionaria, non per un trucco econometrico diverso.
> **Scoperta che ha riordinato le priorità**: convertendo in MDE per 1 SD del regressore,
> **TREND non è meno affetto di WB — è leggermente peggio** (7,4% vs 5,2%). Il VIF basso di
> TREND (1,33) era fuorviante perché ignora la scala del regressore. Quindi il vincolo che
> morde è la **potenza del disegno** (23 cluster trattati), non solo la collinearità di WB:
> §8 è ordinata di conseguenza (prima quantificare la potenza, poi eventualmente mitigare).
> **Due delle idee della prima stesura sono state corrette dopo verifica sui dati** e sono
> conservate con la correzione a vista (§8.4 e §8.7): l'`EP_share` su tutto il campione
> collassava a una dummy di trattamento (reintroduceva il confound C1), e i 3 "switcher" non
> hanno la variazione che sembrava (Corea: salto grande ma **1 solo anno post**; Singapore:
> salto di 1 unità). Piano operativo in **§8**, ordine di esecuzione in **§8.8**. Non ancora
> eseguito nulla.

> ✅ **AGGIORNAMENTO 2026-07-20 — INCLUSA LA COSTRUZIONE DEL DATASET DI BASE
> (script 01-04), RINUMERAZIONE COMPLETA.** Il riordino del 07-16 partiva da
> `final_dataset_pta_env_indices_compressed.fst` come input già dato: la
> pipeline che lo COSTRUISCE (`Code/WB/WB_Dataset_Conversion.do` +
> `Code/Dataset_Creation/1-3_Build_Final_PTA_EP_Dataset.*`, alla radice del
> progetto, mai portata dentro `New/`) non era coperta. Su richiesta esplicita
> dell'utente, integrata come nuovi **01-04**, e tutti i 27 script già
> verificati **rinumerati di +4** (i vecchi 01-27 sono ora 05-31; mappatura
> completa in `New/verification/equivalence_log.md`, ogni riferimento
> incrociato nei commenti aggiornato di conseguenza).
> - **01** (`stata/01_wb_dataset_conversion.do`): path Mac hardcoded
>   sostituiti con macro `local` condizionali sul sistema operativo
>   (Windows/MacOSX/Unix) - su richiesta esplicita dell'utente, che ha
>   fornito un esempio di pattern da un altro suo progetto. Verificato
>   IDENTICO (403 variabili confrontate una per una).
> - **02** (`02_build_dataset_wb_trend_merge.R`): **bug trovato e corretto**
>   durante la riscrittura (una riga di rimozione di 7 righe-intestazione
>   di capitolo omessa per errore, causava 7 colonne spurie quasi-vuote
>   `WB_51..WB_57`) - non alterava alcun numero del paper (le colonne spurie
>   erano NA/0, ininfluenti su `WB_EP_Depth`), ma corretto per fedeltà
>   completa. Dopo il fix: IDENTICO su tutti i valori dei 2 file consumati
>   a valle.
> - **03** (`stata/03_build_dataset_customs_merge.do`, lo step più pesante:
>   merge sui 49,2M righe di dati doganali grezzi, 13,4 GB): path assoluti
>   sostituiti con `global`. Verificato IDENTICO via `cf _all using` di
>   Stata (nessuna differenza su 120 variabili x 49.245.304 righe).
> - **04** (`04_build_dataset_convert_fst.R`, conversione a `.fst`, 18 GB
>   in input): IDENTICO **MD5 byte-per-byte** al riferimento.
> - Nessuna regressione: il dataset finale (`.fst` da cui partono tutti gli
>   script 05+) è verificato bit-per-bit identico a quello già in uso.
>
> ✅ **AGGIORNAMENTO 2026-07-21/23 — FIX CONCETTUALE `WB_EP_Depth` (esclusione
> `Env_Laws_AC`/`Env_Laws_LE`) + RIESECUZIONE COMPLETA DELLA CAMPAGNA DI STIME.**
> Durante la review collaborativa di `08_total_depth.R`, la sua validazione
> interna ha rivelato che `WB_EP_Depth` (calcolato in `02_build_dataset_wb_
> trend_merge.R`) sommava per errore concettuale anche due indicatori
> **"horizontal content"** (`Env_Laws_AC`, `Env_Laws_LE` — giudizio aggregato
> a livello di intera area tematica, da `DTA 1.0 - Horizontal Content.xlsx`)
> dentro il conteggio delle 48 disposizioni **"vertical content"** granulari
> (da `WB_DTA.dta`). Verificato in letteratura (Hofmann-Osnago-Ruta 2017;
> Abman-Lundberg-Ruta 2024, che usa la stessa fonte WB) che le due misure
> vanno tenute separate. **Deciso con l'utente: sostituzione completa**, non
> solo una robustezza aggiuntiva.
> - **Fix**: `02_build_dataset_wb_trend_merge.R` — `Env_Laws_AC`/`Env_Laws_LE`
>   esclusi dalla mappatura `WB_*` (restano nel dataset col nome originale,
>   per un uso futuro separato). `WB_EP_Depth` passa da range 1-19 a 1-17
>   (29/249 country-year toccati di 1-2 punti, correlazione 0,98 con la
>   versione precedente).
> - **Propagato Step 1→2→3**: dataset finale rigenerato (49.245.304 righe
>   invariate). `08_total_depth.R` ora valida **249/249** (era 220/249).
>   `ppml_agg_pdt_zerofill.fst` (input orfano di `30`, nessuno script lo
>   ricostruiva) patchato via merge mirato su country_code+year (colonna
>   passeggera, nessuna ricostruzione della griglia necessaria) — backup in
>   `New/verification/reference/ppml_agg_pdt_zerofill_BACKUP_preEnvLawsFix.fst`.
> - **Intera campagna di stime rieseguita** (script 10-31 + Stata 17-18,
>   cache invalidata): risultati quasi tutti stabili (margine green
>   confermato null ovunque), **eccetto il margine dirty**, che si è
>   leggermente rafforzato: WCB collassato 0,18→0,072; permutation esatta
>   0,079→0,023 (ora sotto 0,05); leave-one-out — il paese pivotale non è
>   più la Corea (che ora resta marginale, p=0,095, se esclusa) ma
>   l'Australia (p=0,236 se esclusa). Verdetto aggiornato nel paper
>   (`New/Paper/draft_paper.tex`): dirty margin da "smontato" a **"fragile,
>   non un falso positivo pulito"** — lettura più sfumata, non più un null
>   netto su entrambi i margini.
> - Script 19 (saturation ladder) ha richiesto 8 retry per il crash noto
>   `recursive gc invocation`; una nuova struttura FE (`fpt_pd`) si è
>   aggiunta empiricamente a `HIGH_CARDINALITY_FE` (nthreads=1). Script 22
>   (permutation) si è bloccato ~13h per sospensione della macchina durante
>   la notte (non un crash) — resuscitato con `taskkill` + rilancio.
> - Bozza di uno script Stata equivalente per la ladder (`19b_saturation_
>   ladder_fullpanel.do`, **non eseguito**, solo scritto) come possibile
>   alternativa più stabile in futuro — reghdfe non soffre del conflitto
>   R/OpenMP col garbage collector che causa i crash ricorrenti.

---

## 0. REGOLA OPERATIVA FONDAMENTALE — cartella di lavoro isolata

**Non modificare MAI nulla fuori da `C:\Work\projects\Paper_PTA\New\`.**

- Tutti gli script nuovi/modificati, gli output e le **copie** dei dataset vanno in `New/`.
- Gli originali in `Paper_PTA/Data`, `Paper_PTA/Code`, `Paper_PTA/Output` sono **read-only**:
  si possono leggere solo per confronto, mai sovrascrivere.
- Se uno script originale serve come base, **copialo** in `New/Code/...` e modifica la copia.

### Struttura da creare dentro `New/`

```
New/
├── ROADMAP.md                  ← questo file
├── Code/
│   ├── pta_functions.R         ← copia (eventualmente patchata) della libreria condivisa
│   ├── 01_inference_fix.R      ← Fase 1
│   ├── 02_preferential_tariff.R← Fase 2
│   ├── 03_identification.R     ← Fase 3
│   ├── 04_alt_margins.R        ← Fase 4 (opzionale)
│   └── 05_robustness.R         ← Fase 5
├── Data/
│   └── final_dataset_pta_env_indices_compressed.fst   ← COPIA dell'originale
└── Output/
    ├── OLS/  PPML/  CEM/  Diagnostics/
```

### Comandi di setup (PowerShell) — eseguire all'inizio della sessione

```powershell
$base = "C:\Work\projects\Paper_PTA"
$new  = "$base\New"
New-Item -ItemType Directory -Force "$new\Code","$new\Data","$new\Output\OLS","$new\Output\PPML","$new\Output\CEM","$new\Output\Diagnostics" | Out-Null

# Copia del dataset principale (file grande: la copia è VOLUTA, da istruzione utente)
Copy-Item "$base\Data\Final Dataset\final_dataset_pta_env_indices_compressed.fst" "$new\Data\" -Force

# Copia della libreria condivisa come base di partenza
Copy-Item "$base\Code\Analysis\pta_functions.R" "$new\Code\pta_functions.R" -Force
```

> Nota: il `.fst` è grande. Copiarlo una volta sola. Tutti gli script in `New/Code/` devono
> puntare a `New/Data/final_dataset_pta_env_indices_compressed.fst`, **non** all'originale.

### Come eseguire R su questa macchina

- `Rscript` **non è nel PATH**. Usare il percorso assoluto:
  `& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" script.R`
- Su Windows, con dataset grandi, `fixest` + `fst` multi-thread possono crashare.
  Mettere in cima a ogni script:
  ```r
  library(fst); library(fixest); library(data.table)
  threads_fst(1); setFixest_nthreads(1)
  ```
- PowerShell: evitare `$` non quotati e `2>&1` su eseguibili nativi (vedi note ambiente).

---

## 1. CONTESTO E DIAGNOSI (perché facciamo tutto questo)

Paper: effetto delle EP nei PTA firmati dalla Cina (2000–2015) sui flussi di export cinesi.
Unità: impresa × HS6 × destinazione × anno. Dati doganali cinesi (non nel repo, ma il `.fst`
finale processato **sì**, in `Data/Final Dataset/`).

**Diagnosi della sessione Opus (da non rifare):**

1. **Nessun effetto EP robusto.** Coefficiente di EP depth su export/quantità/valore unitario
   cambia segno e significatività a seconda della struttura di FE.
2. **I risultati "significativi" sono artefatti.** Sono concentrati nella specifica `fpd + year`,
   che è (a) la meno satura di FE e (b) **l'unica clusterizzata a `pdt`** (~2,9 milioni di cluster)
   invece che a `dt` (~3.500). Il clustering troppo fine gonfia le stelle.
3. **Scala dell'effetto trascurabile** (0.0005–0.005 su outcome in log per unità di depth),
   non significativo nelle specifiche sature nonostante 30+ milioni di osservazioni → è un
   **null stimato con precisione**, non rumore.
4. **L'unico coefficiente stabile** in tutte le 64 tabelle è la tariffa (`tariffs`, sempre
   negativa ***). Ma è la variabile **mal specificata**: è la tariffa **MFN**, non quella
   bilaterale preferenziale (vedi §Fase 2). Quindi l'unica cosa "che funziona" è anche da rifare.
5. **PPML internamente incoerente**: segni opposti tra margini (valore su, quantità giù nella
   stessa specifica) e tra strutture di FE. La specifica PPML `fpt`-only è un outlier
   (coefficienti enormi, segni invertiti) → **da eliminare**.
6. **CEM debole**: L1 imbalance 0.788 → 0.652 (poco), e il bilanciamento di `log_gdppc_2000`
   **peggiora** (SMD −0.108, oltre soglia 0.10). Solo 25 paesi trattati × 3 covariate.

**Pattern chiave da sfruttare nella scrittura:** salendo la "scala" di saturazione delle FE
(`fpd+year` → `fpt+pd` → `fpd+pt` → `fpt+fpd`) l'effetto si **azzera monotonicamente**.
Quando si assorbe l'eterogeneità di livello della relazione commerciale (FE `fpd`), l'effetto
sparisce → firma di **selezione** (le destinazioni che firmano PTA sono mercati più grandi/in
crescita), non di un effetto causale delle EP.

---

## 2. MAPPA DEI DATI (colonne già verificate nel `.fst`)

File: `final_dataset_pta_env_indices_compressed.fst` (~49,2 milioni di righe).

> **Copia CANONICA (A3, chiuso 2026-07-06):** quella su **Windows**
> (`C:\Work\projects\Paper_PTA\Data\Final Dataset\`), dove gira la pipeline.
> Righe: **49.245.304** | MD5: `2045C2610AA2217D50C2637A585D8338`.
> La copia Mac ha 9 righe in meno (49.245.295) → da riallineare a questa quando comodo;
> fino ad allora i risultati si producono e confrontano solo su Windows.
Caricare **solo le colonne necessarie** con `read_fst(path, columns = c(...))`.

**Outcome (già in log nel dataset):**
- `ln_export` = ln(export) — valore export (outcome principale)
- `ln_export_qua` = ln(exp_qua) — quantità
- `ln_export_value` = ln(uv_exp) — valore unitario (proxy qualità/prezzo)

**Trattamento (depth aggregata):**
- `WB_EP_Depth` (World Bank), `TREND_EP_Count` (TREND) — = 0 se nessun PTA attivo

**Sotto-indici EP già costruiti (per Fase 4, eterogeneità per tipo di provision):**
- TREND: `TREND_GreenMarketAccess`, `TREND_EnforcementDSM`, `TREND_RegulatorySpace`,
  `TREND_ClimateEnergy`, `TREND_BiodivForestsFisheries`, `TREND_Soft`, `TREND_Hard`,
  `TREND_Hardness_Share`
- WB: `WB_GreenLiberalization`, `WB_EnforcementDSM`, `WB_StandardsNonRegression`,
  `WB_RegulatorySpaceExceptions`, `WB_Assistance`, `WB_Hardness_Share`

**Interazione green:**
- `env_good` (binaria, 1 = bene ambientale OECD; = 0 se mancante)

**Controlli attuali:**
- `tariffs` = ln(1 + `duty`) — **ATTENZIONE: `duty` è MFN, non preferenziale** (vedi Fase 2)
- `ln_hhi_baci` — concentrazione di mercato

**ID per Fixed Effects (già pre-calcolati come gruppi interi):**
- `fpd` (firm×product×destination), `fpt` (firm×product×time), `pd`, `pt`, `dt`, `pdt`,
  `fdt`, `ft`, `p4d`, `p4dt`, `p2dt`, `fp2dt`
- `year` (= time), `hs6`, `hs4`, `hs2`, `companyID`
- **`country_code` = DESTINAZIONE** (variabile chiave per il clustering a destinazione, vedi Fase 1)

**Modulo anti-dumping (per Fase 5, confondente potenziale):**
- `AD_pdt`, `treat_yr`, `revoke_yr`, `invest_yr`, `L1_ad`…`L8_ad`, `F0_ad`…`F8_ad`

**Macro controlli disponibili ma inutilizzati (eventuale robustezza):**
- `RER`, `ln_RER`, `gdp_growth`, `ln_demand`, `ln_aggexp_l`, `bec`, `prodclass`, `sector`

**Colonna spuria da ignorare/eliminare:** `_merge` (residuo del merge Stata).

---

## 3. COSA FA IL CODICE ORIGINALE (sintesi, per non rileggerlo)

- `Code/Analysis/pta_functions.R` — libreria condivisa. Funzioni:
  - `estimate_model()` stima un modello (`feols` o `fepois`), carica solo le colonne servono,
    salva `.rds`. Parametro `vcov`: formula tipo `~pdt` per cluster, o stringa `"HC1"`.
  - `run_block()` esegue una lista di formule, cache con skip-se-esiste.
  - `make_table()` genera tabelle LaTeX a mano.
  - **BUG/limiti noti** (da correggere nelle copie in `New/`):
    - `make_table` riporta R² anche per PPML (via `sq.cor`) → non sensato per Poisson.
    - In `CEM.R`: `library(wdi)` minuscolo (il pacchetto è `WDI`) → errore; e `fwrite` di
      `matched_countries.csv` è commentato (CEM.R righe ~481–484) ma documentato come output.
- `Code/Analysis/OLS_HDFE.R` — 4 strutture FE × {WB, TREND} × {No-Int, Int} × {baseline, controlli}.
  - `fpd + year` → cluster `~pdt` ⚠️ (incoerente con le altre)
  - `fpt + pd`, `fpt + fpd`, `fpd + pt` → cluster `~dt`
- `Code/Analysis/PPML.R` — parallelo, con in più una specifica `fpt`-only (da eliminare).
- `Code/Analysis/CEM.R`, `OLS_CEM.R`, `PPML_CEM.R` — matching e stime sul campione matchato.

---

> 🗄️ **STORICO — SUPERATO (2026-07-12).** Le §4–§7.4 qui sotto sono la checklist *pre-bozza*:
> tutti i loro checkpoint sono stati assorbiti nella campagna di stima e nella bozza
> `New/Paper/draft_paper.tex` (esiti in **§7-R6**). Restano come memoria del percorso, **non
> sono lavoro aperto**. L'unico checkpoint pre-bozza mai chiuso — le tariffe preferenziali
> WITS (§4 Fase R2) — è ora un caveat dichiarato nel paper e un task vivo in **§7-R7** (R7.11
> non lo copre; vedi housekeeping). Le cose da fare vere sono **solo in §7-R7** (in fondo).

## 4. ROADMAP OPERATIVA

> Ordine consigliato: **Fase 1 → 2 → 3**, poi bivio di framing. Fase 4 solo se si tenta il recupero
> di un risultato positivo. Ogni fase ha un **checkpoint di verifica** (stile goal-driven).

### FASE 1 — Inferenza e igiene delle specifiche  ⭐ priorità massima, basso costo

È anche un **test diagnostico decisivo**: dice se sopravvive qualche risultato.

**Azioni:**

1. **Clustering a livello DESTINAZIONE.** La destinazione è `country_code`.
   - Specifica principale: `cluster = ~country_code`.
   - Robustezza: two-way `cluster = ~country_code + year`.
   - Dato che le destinazioni *trattate* sono ~25, riportare **wild cluster bootstrap**
     per il coefficiente di interesse:
     ```r
     # fixest >= 0.11
     m <- feols(ln_export ~ WB_EP_Depth | fpt + fpd, data = d, cluster = ~country_code)
     wb <- boot::boot # NO: usare fixest::wald o il pacchetto `fwildclusterboot`
     # Preferito: fwildclusterboot::boottest(m, param = "WB_EP_Depth",
     #            clustid = "country_code", B = 9999)
     ```
     Se `fwildclusterboot` non è installato: `install.packages("fwildclusterboot")`.
2. **Uniformare il clustering** su TUTTE le specifiche (eliminare l'eccezione `pdt` di `fpd+year`).
   Tutte → `~country_code` (e/o `~dt` come confronto, ma coerente ovunque).
3. **Stesso campione baseline vs. controlli.** Prima di stimare, definire il sample sui
   non-missing dei controlli e stimare *entrambe* le colonne su quel sample:
   ```r
   d <- read_fst(path, columns = c("ln_export","WB_EP_Depth","tariffs","ln_hhi_baci",
                                   "fpt","fpd","country_code","year"), as.data.table = TRUE)
   d <- d[!is.na(tariffs) & !is.na(ln_hhi_baci)]   # sample comune
   ```
4. **Eleggere `fpt + fpd` come specifica PRINCIPALE** (assorbe il livello di relazione).
   Le FE lasche (`fpd+year`, `fpt+pd`) vanno in appendice come "ladder" diagnostica.
5. **PPML:** eliminare la specifica `fpt`-only. Investigare l'anomalia di convergenza in
   `PPML_TREND_fpd_year` (col. 5, segno tariffa positivo): ricontrollare con `glm`/`fepois`
   e `nthreads=1`, verificare separazione perfetta.

**Checkpoint Fase 1:**
- [ ] Tutte le tabelle clusterizzate allo stesso livello (`country_code`).
- [ ] Le stelle positive di `fpd+year` sono sparite o fortemente ridotte (atteso).
- [ ] Baseline e "con controlli" hanno lo **stesso** numero di osservazioni.
- [ ] PPML senza `fpt`-only; nessun segno tariffa positivo residuo.
- [ ] Tabella "ladder": effetto che si azzera salendo in saturazione di FE → documentato.

→ **DECISIONE DI FRAMING** (Fase 0): se dopo la Fase 1 nulla sopravvive → **paper "null di
precisione"** (procedi Fasi 3 e 5). Se vuoi un tentativo di segnale → aggiungi Fase 4.

---

### FASE 2 — Tariffa preferenziale (correggere l'unica variabile robusta ma sbagliata)

`duty` è la tariffa **MFN** della destinazione, non quella **bilaterale preferenziale**
applicata alla Cina sotto il PTA. Prova diagnostica già fatta: per i partner PTA la `duty`
media **non scende** dopo l'entrata in vigore (15–66%), mentre dovrebbe se fosse preferenziale.
→ Oggi l'effetto EP potrebbe assorbire la liberalizzazione tariffaria del PTA stesso.

**Azioni:**
1. Scaricare le **tariffe preferenziali/effettivamente applicate (AHS)** da **WITS TRAINS**:
   reporter = le ~25 destinazioni PTA, partner = Cina (CHN), livello HS6, annuale 2000–2015.
2. Costruire `pref_tariff_pdt` e `tariffs_pref = ln(1 + pref_tariff)`.
3. Mergiare nel dataset di lavoro in `New/Data/` (chiave: HS6 × destinazione × anno).
4. Rieseguire le specifiche principali con `tariffs_pref` al posto di `tariffs`.

**Checkpoint Fase 2:**
- [ ] Per i partner PTA la tariffa preferenziale scende dopo l'entrata in vigore.
- [ ] Risultati EP stabili anche con il controllo tariffario corretto.

> Se reperire WITS richiede troppo tempo: documentare il limite e usare almeno la specifica
> `fpt+pd`/`pt` che assorbe shock prodotto×tempo (ma **non** la tariffa preferenziale
> destinazione-specifica → resta un limite).

---

### FASE 3 — Identificazione (rende il null credibile o fa emergere segnale)

1. **Controllo per depth NON-ambientale del PTA.** Separa "EP" da "accordo profondo in generale".
   Serve un indice di profondità complessiva dell'accordo (non solo ambientale). Se non presente
   nel `.fst`, verificare in `Data/Merged/` o ricostruirlo dai dati WB/TREND. Aggiungerlo come
   regressore accanto a `WB_EP_Depth`/`TREND_EP_Count`.
2. **Event study / pre-trends** attorno all'entrata in vigore del PTA (timing scaglionato).
   - Costruire il tempo relativo all'entrata in vigore per ogni destinazione.
   - Stimare leads/lags; graficare. Assenza di pre-trend + nessun salto = rafforza il null.
3. (Opzionale) **Callaway & Sant'Anna (2021)** via pacchetto `did` — era previsto e commentato
   nel README originale. Robustezza al TWFE con timing scaglionato.

**Checkpoint Fase 3:**
- [ ] Coefficiente EP con controllo depth non-ambientale: documentato.
- [ ] Grafico event-study prodotto in `New/Output/Diagnostics/`.
- [ ] Pre-trend assenti (o, se presenti, discussi).

---

### FASE 4 — Margini alternativi ed eterogeneità (OPZIONALE — tentativo di recupero)

Costo quasi zero: i dati e i sotto-indici sono **già pronti**.

1. **Sotto-componenti EP** (vedi §2): rieseguire le specifiche principali usando una alla volta
   `TREND_GreenMarketAccess`, `WB_GreenLiberalization`, `*_EnforcementDSM`, `*_Hard` vs `*_Soft`.
   Il depth aggregato nullo non esclude che una dimensione specifica morda.
2. **Margine estensivo / composizione:** quota di green goods sull'export totale per
   destinazione×anno; entrata di imprese nei green goods. Richiede aggregazione dal dataset.
3. **Eterogeneità per dimensione d'impresa** (usare `companyID` per costruire classi di size,
   es. export totale dell'impresa) — interazione EP × size.

**Checkpoint Fase 4:**
- [ ] Tabella per sotto-componente prodotta; identificato (se esiste) il canale che morde.
- [ ] Almeno un'analisi su margine estensivo/composizione o eterogeneità di size.

---

### FASE 5 — Robustezza e pulizia finale

1. **Controllo anti-dumping:** aggiungere `AD_pdt` (ed eventuali leads/lags) come controllo;
   verificare l'overlap tra timing PTA e attività AD (potenziale confondente trovato nei dati).
2. **Pulizia output/codice nelle copie in `New/`:**
   - Togliere R² dalle tabelle PPML (o etichettarlo "squared correlation").
   - Correggere `library(WDI)` (maiuscolo) nella copia di CEM.
   - Scrivere davvero `matched_countries.csv` (o togliere la voce dalla doc).
   - Eliminare/ignorare la colonna `_merge`.
3. **Tabelle finali** in `New/Output/` + decisione di framing definitiva.

**Checkpoint Fase 5:**
- [ ] `AD_pdt` testato come controllo.
- [ ] Tabelle PPML senza R² fuorviante.
- [ ] Set finale di tabelle coerente e clusterizzato a destinazione.

---

## 5. PRINCÌPI DA RISPETTARE (dal CLAUDE.md del progetto)

- **Pensa prima di scrivere codice**: esplicita assunzioni, segnala ambiguità.
- **Semplicità**: codice minimo che risolve il problema, niente astrazioni speculative.
- **Modifiche chirurgiche**: ogni riga modificata deve tracciare a un punto di questa roadmap.
- **Goal-driven**: spuntare i checkpoint di ogni fase prima di passare alla successiva.
- **Solo dentro `New/`**: originali intoccabili.

## 6. ~~PRIMO PASSO CONSIGLIATO~~ (SUPERATO — vedi §11.3)

> **STATO AL 2026-08-11**: questa sezione è di giugno e il suo "primo passo" (clustering a
> `country_code` + wild bootstrap) è stato fatto da tempo. **Per sapere da dove ripartire oggi
> andare a §11.3.** Testo originale conservato sotto per memoria.

Eseguire il **setup di §0**, poi partire dalla **Fase 1, punto 1** (clustering a `country_code`
+ wild bootstrap su `fpt+fpd`): è l'intervento singolo che decide se il paper è "null" o "con
risultato". Tutto il resto discende da quell'esito.

---

## 7. RIDISEGNO 2026-06-09 — piano per la pubblicabilità in un top journal

> Esito della revisione complessiva (sessione Opus 2026-06-09, piano approvato dall'utente).
> Questo paragrafo **supera le Fasi 2–5 di §4**: la Fase 2 (tariffe WITS) viene assorbita
> nella nuova Fase R2; le Fasi 3–5 sono riformulate attorno a un nuovo design identificativo.
> Documento completo del piano: `C:\Users\edodr\.claude\plans\distributed-cuddling-crane.md`.

### 7.0 Diagnosi aggiuntiva (oltre a §1)

- **C1 — L'effetto-livello di EP depth non è identificabile.** Varia a livello dest×anno ed è
  collineare con l'entrata in vigore del PTA stesso. Variazione effettiva: ~14 accordi (ASEAN
  = un solo accordo per 10 destinazioni). Il livello va declassato a diagnostica (ladder).
- **C2 — Mancano i dirty goods.** Solo `env_good` (lista OECD green). L'ipotesi pollution-haven
  (EP ↓ export inquinanti) non è testabile senza intensità emissiva per HS6.
- **C3 — Concordanza HS6 nel tempo probabilmente assente.** Il panel 2000–2015 attraversa le
  revisioni HS 2002/2007/2012: se non concordato, gli FE `fpd`/`fpt` spezzano le serie e
  `env_good` è mal assegnato. **Da verificare prima di tutto: può invalidare il pregresso.**
- **C4 — Hong Kong e Macao (CEPA)** contaminano il trattato (entrepôt + accordo sui generis).
  Escludere dalla specifica principale, robustezza con inclusione.
- **C5 — EP depth correlata con la profondità totale dell'accordo.** Serve `TotalDepth_dt`
  (costruibile dai file WB DTA in `Data/WB/`).
- **C6 — PPML**: firm-level su flussi positivi resta (correzione eteroschedasticità), ma va
  affiancato da PPML aggregato `pd×t` su griglia con zeri; PPML su unit value va eliminato.
- **C7 — CEM** → appendice o fuori; sostituire con not-yet-treated + synthetic DiD.
- **C8 — Inferenza**: oltre a WCB, **permutation inference** (riassegnare EP depth tra i ~14
  accordi a timing PTA fisso → testa il *contenuto* ambientale, non l'accordo).

### 7.1 Specifica principale (triple-difference sulla composizione)

```r
ln_export ~ EP_depth:green_p + EP_depth:dirty_p
          + TotalDepth:green_p + TotalDepth:dirty_p
          + tariffs_pref + AD_pdt
          | fpd + fdt + pt,  cluster = ~country_code
```

- `fdt` (già nel `.fst`) assorbe **tutto** ciò che varia a impresa-dest-anno, incluso il PTA
  stesso → il confound C1 sparisce per costruzione.
- `pt` assorbe gli shock globali di prodotto; `fpd` il livello della relazione.
- Identificazione: entro impresa-destinazione-anno, tra prodotti green/dirty vs neutri.
- Event study differenziale: leads/lags entrata PTA × `green_p`/`dirty_p` (+ `sunab()`).
- Inferenza a 3 livelli: cluster `~country_code`, WCB B=9999, permutation 1.000 draws.

### 7.2 Fasi operative (sostituiscono Fasi 2–5 di §4)

- **R0 — Chiudere Fase 1** (in corso): completare `01_inference_fix.R`, bootstrap, ladder.
  Archiviati gli script superseded in `New/Code/_archive/`.
- **R1 — Igiene dati**: audit concordanza HS (decisivo); tabella trattamento (14 accordi,
  switch effettivi); peso HK+MO; trimming UV 1/99 within HS2-anno; consistenza `companyID`
  (attenzione al 2004, liberalizzazione trading rights). Output in `New/Output/Diagnostics/`.
- **R2 — Nuovi dati**: tariffe preferenziali AHS da WITS TRAINS (ex Fase 2); `TotalDepth`
  non-ambientale dai file WB; `dirty_p` da intensità emissive (Shapiro 2021 / IPPS,
  concordanza ISIC→HS6, top quartile; robustezza Mani-Wheeler). Opzionale: ownership e
  regime processing/ordinary dal raw customs.
- **R3 — Stime principali**: triple-diff (§7.1) su 3 outcome × {WB, TREND}; event study;
  inferenza a 3 livelli; ladder come diagnostica; PPML doppio (firm-level positivi +
  aggregato `pd×t` con zeri, FE `pd + pt + dt`). I sub-campioni di **§7.4** rendono eseguibili
  il PPML aggregato e i DiD moderni (oggi impraticabili sul panel pieno).
- **R4 — Margini e meccanismi**: margine estensivo (n. imprese/prodotti green per `d×t`,
  entrata nuove imprese nei green); **riallocazione within-firm** (quota green nel paniere
  delle multiprodotto verso `d`, FE `fdt`) ← potenziale risultato da top journal;
  eterogeneità per sub-indice (`GreenMarketAccess`, `EnforcementDSM`, `Hard`/`Soft`) e
  per size d'impresa.
- **R5 — Robustezza (set chiuso)**: escl. HK+MO / incl.; escl. ASEAN; leave-one-out per
  accordo; controllo `AD_pdt`; solo not-yet-treated; synthetic DiD su quota green a livello
  destinazione; Callaway-Sant'Anna/dCDH su trattamento binario; UV trimmed vs non;
  **stabilità dell'interazione lungo i gruppi di controllo di §7.4** (è il vero stress test).
- **R6 — Framing e scrittura**: descrittiva "gli EP cinesi nella distribuzione mondiale
  TREND"; bivio di framing DOPO R3 (interazione sopravvive → JIE/JEEM, headline triple-diff
  + within-firm; nulla sopravvive → precision null vs Brandi 2020 e Abman-Lundberg-Ruta
  JEEA 2024 → World Development/JEEM). Wiki: aggiungere ALR 2024, Shapiro 2021,
  Cherniwchan 2017, Copeland-Shapiro-Taylor 2022. Ridurre a 6–8 tabelle main.

### 7.3 Cosa si abbandona

- Effetto-livello EP come headline (→ diagnostica ladder).
- PPML su unit value.
- CEM come strategia identificativa principale.
- Le 4 strutture FE come robustezza simmetrica (→ una principale + ladder).

---

### 7.4 FASE R-CONTROL — gruppi di controllo e sub-campioni (feasibility + robustezza)

> **Origine.** Discussione 2026-06-18 (control group à la Caselli, Huang, Tomasi & Zhu,
> *Anti-dumping and Product Quality*) + approfondimento econometrico 2026-06-23/24. Chiude il
> pending "Fase R-control" segnato nei log. **Non sostituisce la triple-diff §7.1**: la raffina
> (control group più credibili) e la rende **eseguibile** (PPML aggregato + DiD moderni).

#### 7.4.0 Motivazione doppia

1. **Computazionale.** A 49,2M righe, PPML aggregato con zeri e gli stimatori DiD moderni
   (Sun-Abraham, Callaway-Sant'Anna, dCDH) crashano / non terminano. Serve un sub-campione.
2. **Identificazione.** Il control group di prodotto attuale (247 green vs **tutti** i 4.752
   non-green) è l'analogo del loro *Full sample* — il più lasco possibile sul margine-prodotto.

#### 7.4.1 Decomposizione in due margini (chiave concettuale)

Il disegno è un triplo-differenza; il problema del control group si scompone in due margini:

- **Margine destinazione** → già gestito dal **CEM-paese** (≈ loro *Control 2*). **Ma** il CEM
  tiene le destinazioni più grandi per volume (KOR, IND, IDN, THA, SGP, AUS tra i trattati; USA,
  DEU, GBR, FRA, ITA, BRA, MEX tra i controlli) → **taglia poche righe** (54/238 paesi ≠ 54/238
  osservazioni) e **non scioglie i pochi cluster trattati**.
- **Margine prodotto** → **leva ancora aperta**. green (247 HS6) vs non-green.

> **Riconciliazione col log 2026-06-18** ("i gruppi di controllo aiutano selezione e taglia ma
> NON l'identificazione"): vero per l'**effetto-livello** (EP varia solo a `d`). Per
> l'**interazione** triple-diff (§7.1) i controlli di prodotto **affinano i trend comuni del
> differenziale** green/dirty → qui sono econometricamente rilevanti, non solo cosmetici.

#### 7.4.2 Numeri ancora (verificati 2026-06-24, letture leggere)

- **green**: 247 HS6 distinti, su **23 capitoli HS2** (38,39,40,44,45,53,54,56,63,68,69,70,73,
  76,83,84,85,86,87,89,90,94,95); ~11% delle righe (5,31M su 49,2M). File `Data/Env_Codes_HS.dta`.
- **CEM** (`Output/CEM/matched_countries.csv`): ~19 trattati + ~35 controlli con `country_code`
  valido; alcuni controlli (BHR, ISR, IRN…) senza `country_code` → non entrano nel `.fst`.

#### 7.4.3 Strategie (mappate sui control group del paper), ordinate per credibilità/taglia

- **C-prod-HS4 (≈ loro *Control 3*).** Tieni i non-green **entro la stessa HS4** dei green. Da
  4.999 → poche centinaia di prodotti = **leva di taglia massima**. *Econometria:* affina i
  common-trends dell'interazione e **non è ridondante coi FE** (i FE tolgono il livello, non il
  trend differenziale). *Difetto reale:* **spillover within-firm cross-prodotto** (Eckel et al.
  2023) — il non-green nello stesso HS4, esportato dalla stessa impresa, può essere contaminato
  dalla riallocazione indotta dal PTA → **riportare insieme a un controllo più pulito, mai da
  solo**. È la stessa ragione per cui il paper declassa Control 3/4.
- **C-prod-match (≈ loro *Control 4*).** Dentro HS4, **bilancia** i non-green sui green su
  covariate **pre-periodo** (dimensione del flusso, crescita, unit value, penetrazione import
  cinese). *Nota:* trattarlo come **covariate balancing**, NON come propensity-to-be-green
  (`env_good` è lista fissa OECD, non trattamento stocastico → il framing logit è artificioso).
- **C-overlap (≈ loro *Control 1*, il più pulito).** Tieni solo gli HS6 esportati **sia** verso
  partner-PTA **sia** verso controlli (common support prodotto×destinazione). Evita
  l'estrapolazione, rinforza il CEM, immune allo spillover di C3/C4. **Leva di taglia minore.**
- **C-deepshallow (à la Abman-Lundberg-Ruta 2024) — la più adatta al nostro vincolo.** Campione
  **solo partner-PTA**, confronto **deep-EP vs shallow-EP**. Sidestep totale della selezione
  trattati-vs-mai-trattati (il confound C1 sul margine within-treated). Da combinare con
  `TotalDepth` (C5/R2) per separare il contenuto **ambientale** dalla profondità generale.
- **(scaffolding, NON headline) C-aggr.** Collasso a `pd×anno` solo per **prototipare la pipeline
  / girare veloce**. Reintroduce selezione d'impresa (Melitz) + bias di Jensen
  (`ln Σexport ≠ Σ ln export`) → **mai** specifica principale.

#### 7.4.4 Verdetto econometrico (cosa ci aspettiamo)

- Sub-campioni su **covariate pre-trattamento** = ATT **condizionato valido** (nessun selection
  bias indotto dal restringimento).
- **Il risultato interessante È la stabilità** di `EP×green_p` (e `EP×dirty_p`) lungo
  Full → CEM → C-overlap → C-prod-HS4 / C-deepshallow. Stabile = robusto (storia green market
  access, da JIE/JEEM). Muore sotto i controlli puliti = il full-sample era artefatto (risultato
  negativo ma vero). **La stabilità tra control group è il contributo**, non un dettaglio.
- **Vincolo che i sub-campioni NON sciolgono:** **pochi cluster trattati** (~19-25 paesi;
  ASEAN = 1 accordo). Ridurre le righe **non aumenta** i cluster trattati → la precisione cala
  **meno** di quanto suggerisca il crollo di N (gli SE dipendono dai cluster, non dalle righe),
  ma il **pavimento dell'inferenza** resta. Inferenza **sempre** con **wild cluster bootstrap su
  `~country_code`** (+ permutation à la §7.0-C8), **mai** SE cluster asintotici.
- **Guadagno vero della taglia ridotta:** rende eseguibili (i) **PPML aggregato con zeri** →
  margine **estensivo** = *green trade creation*, dove plausibilmente vive il risultato nuovo
  (l'OLS-su-log cattura solo l'intensivo, flussi positivi); (ii) **Sun-Abraham / Callaway-
  Sant'Anna / dCDH** su timing scaglionato → robustezza ai pesi negativi TWFE.
- **Focalizzare sull'interazione, non sul livello:** l'effetto-livello è ostaggio dei ~19 paesi
  trattati; l'interazione è identificata *within-destinazione* tra prodotti (molti più cluster
  effettivi) → più potente e più solida. P1/P3 agiscono proprio sul margine-prodotto.

#### 7.4.5 Checkpoint Fase R-control

- [x] **Eseguiti 2026-06-25** gli script `New/Code/08-12_*.R` (uno per sub-campione + CEM v2).
      Numeri ancorati: **C-prod-HS4** 106/103→ poi visto su 09 servono 351 non-green entro HS4
      verdi, 10,09M righe (20,5%) sopravvivono. **C-prod-match**: match esatto HS4 troppo sottile
      (69% famiglie senza candidati validi) → **rilassato a HS2** (22 capitoli, 97% dei verdi
      matchati, 1.376/4.817 HS6); L1 di `imbalance()` non comparabile pre/post per il match esatto
      carattere (bin ricalcolati su campioni diversi) — usare il **love plot**, non l'L1, come
      diagnostica di riferimento. ⚠️ **Aggiornamento post-fix C1 (audit 2026-07-03, ri-run
      2026-07-06):** le covariate della run originale erano sbagliate (unit value al posto del
      valore). Con le covariate corrette: `pre_lnvalue` ben bilanciata (SMD ~0,02) e
      `pre_unitvalue` bilanciata (SMD ~0,085), ma **`pre_hhi` resta sopra soglia (SMD ~0,18
      post-match vs ~0,21 pre)** → 2 covariate su 3 ok; la concentrazione di mercato è un limite
      esplicito di C-prod-match da riportare nel paper. **C-overlap**: 98,5%
      HS6 / ~100% righe in overlap → tagli quasi nulla (atteso, è la leva debole sul fronte
      taglia/forte sul fronte identificazione). **C-deepshallow**: split 17 deep/8 shallow (mediana
      con pareggi), 30,9% righe sopravvivono; **shallow ha solo 8 cluster** → WCB ancora più
      fragile dei 19-25 generali, da riportare come limite esplicito. **CEM v2** (baseline
      commerciale pre-PTA come 4ª covariata): **testato e scartato**. La prima run (2026-06-25:
      16→11 trattati, SMD ~0,55) usava una covariata costruita male (bug C2, audit 2026-07-03);
      la ri-esecuzione col fix (2026-07-06) **conferma e peggiora**: 8 trattati matchati (vs 16
      del v1), covariata nuova ancora squilibrata (SMD ~0,37, soglia 0,1), `gdp_growth_2000`
      sopra soglia (~0,16). **Verdetto DEFINITIVO: mantenere il CEM originale**
      (`Output/CEM/matched_countries.csv`, 16 trattati + 40 controlli), non sostituirlo con v2.
- [x] **Chiuso 2026-06-25 — vintage HS6 dei green goods.** Indagine approfondita (script
      `02_data_hygiene_audit.R`, `02b_hs_vintage_check.R`, `03_hs_concordance.R`, ad-hoc in `/tmp/`)
      ha confermato un'anomalia reale al confine 2006→2007 (6,03% del valore export su codici
      "morti", soglia di concordanza superata) e che la lista green (`Data/Env_Codes_HS.dta`) è
      nativa **HS2012** (fingerprint 100%), mentre il pannello è dichiarato HS1996 dallo script
      grezzo originale (`1_create_panel_export.do`, Step B — mai eseguito sul file consegnato,
      verificato per confronto diretto). Tentata una concordanza completa del pannello a vintage
      unica (`03_hs_concordance.R`) ma `concord()` non risolve i casi-prova (NA su 854213/854230).
      **Decisione presa**: fidarsi della vintage HS1996 dichiarata dal fornitore del dataset e
      tradurre **solo la lista green** a HS1996 una volta, applicata uniformemente a tutti gli anni
      (`New/Code/03b_green_codes_to_hs1996.R` → `New/Data/Concordance/Env_Codes_HS1996.csv`).
      Risultato: **247/247 codici verdi con match univoco 1:1** HS2012→HS1996, nessuno split/non
      concordato, nessun crollo di valore sospetto 2006→2007 sui match univoci — traduzione pulita,
      nessuna perdita. Gli script `08_subsample_prodHS4.R`, `09_subsample_prodmatch.R`,
      `10_subsample_overlap.R` sono stati aggiornati per ricalcolare `env_good` da questa lista
      (anziché fidarsi della colonna `env_good` del `.fst`, che viene da un merge diretto
      HS2012-vs-HS1996 senza concordanza) e rieseguiti — numeri aggiornati: C-prod-HS4 invariato
      (106 famiglie HS4, 20,5% righe); C-prod-match leggermente diverso (236 verdi candidati vs 229,
      1.438/1.953 matchati a HS2, 228 verdi matchati); C-overlap invariato (98,5%/96,8%). Script 11/12
      non toccano `env_good` direttamente, nessun aggiornamento necessario.
- [ ] Quantificati (lettura leggera: solo colonne `hs6`/`hs4`/`country_code` dal `.fst`) gli
      **switchers effettivi** e le **righe sopravvissute** a C-prod-HS4 (cutoff HS4 vs HS2) e
      C-overlap → decidere sui numeri reali prima di stimare.
- [x] **Fatto 2026-07-06** — Triple-diff §7.1 su 3 control group + panel collassato; tabella di
      stabilità in `New/Output/TripleDiff/Tables/tripledd_stability.csv` (+ `tripledd_collapsed.csv`).
      **Esito: EP×green è un null STABILE** (WB: −0,0009/−0,0022/−0,0021/−0,0023 su
      prodHS4/CEM/deepshallow/collassato, mai p<0,4). EP×dirty (WB) negativo ma fragile:
      −0,0089 p=0,006 sul collassato, −0,0040 p=0,056 su CEM, ma permutation aggregata p=0,50
      con segno invertito e TREND non conferma → pista, non risultato. Caveat: (i) il **full
      panel** (07/07b) crasha l'allocatore R con 3 FE alte-dim in ogni configurazione provata
      (callr, diretta, 4-12 thread, 61GB RAM) → **RISOLTO 2026-07-06 sera via Stata/reghdfe**
      (`16_tripledd_full.do`): 24,3M singleton rimossi, 21,5M oss., **WB×green −0,0021 p=0,55,
      WB×dirty −0,0040 p=0,038 asint., F congiunto p=0,26; TREND×green −0,0001 p=0,91,
      TREND×dirty −0,0009 p=0,15, F congiunto p=0,71** → precision null confermato al livello
      impresa (`Tables/tripledd_full_reghdfe.csv`); (ii) deepshallow TREND e C-overlap mancanti
      (stesso limite RAM — ora aggirabile con reghdfe se serve); (iii) inferenza: cluster
      asintotici + permutation sul collassato (green p=0,45; dirty p=0,50) — WCB fatto (v. sotto).
- [x] **Fatto 2026-07-07/08 — PPML aggregato con zeri** (`20_ppml_extensive.R` su
      `ppml_agg_pdt_zerofill.fst`, 8,3M celle): nessuna green trade creation al margine
      estensivo — EP×green +0,0014 (p=0,73) WB / +0,0001 (p=0,95) TREND; dirty n.s.
      Stessa notte chiusi anche (run `17_remaining_models.do` + `18`/`19`): **C-overlap**
      (WB −0,0021 p=0,55; TREND −0,0001 p=0,91), **deepshallow TREND** (−0,0004 p=0,72),
      **robustezze full-panel** (controlli p=0,93; no-ASEAN p=0,42; incl-HKMO p=0,73),
      **within-firm** (quota green: WB p=0,37, TREND −0,00006 p=0,044 — trascurabile),
      **sotto-indici** (bundling: WB GreenLib ⊥ Standards con ρ=1,00 → eterogeneità per
      clausola NON identificabile; placebo Soft/RegSpace correttamente nulli),
      **Sun-Abraham sul gap** (ATT green p=0,24, dirty p=0,28 → la deriva a +5 era
      eterogeneità di coorte). Output in `New/Output/TripleDiff/Tables/`.
      **PRIMA BOZZA DEL PAPER**: `New/Paper/draft_paper.tex` (Overleaf-ready, figure incluse).
- [x] **Fatto 2026-07-06 (sera) — WCB sul collassato + chiusura pista dirty.**
      `15_wcb_collapsed.R` (WCB B=9999 via Frisch-Waugh: demean + lm, perché feols non-lean
      crasha l'allocatore): **tutte e 4 le interazioni NON significative** — WB×green p=0,88,
      **WB×dirty p=0,18** (il p asintotico 0,006 era illusione da pochi cluster), TREND×green
      p=0,39, TREND×dirty p=0,85. `15b_dirty_leaveoneout.R`: coefficiente dirty stabile (~−0,009)
      ma **togliendo la Corea (133) muore** (−0,0059, p=0,21) — uno dei 3 soli switcher within.
      **VERDETTO: pista dirty CHIUSA, non robusta** (WCB + permutation aggregata segno opposto +
      TREND nullo + dipendenza da un paese). Il paper è un **precision null su entrambi i margini
      della composizione**, salvo conferma full-panel su macchina capiente. WCB sui sub-campioni
      firm-level: non fattibile su questa macchina (richiede modelli non-lean), rimandato al server.
- [ ] **C-deepshallow** (solo-PTA, deep vs shallow EP) stimata come identificazione alternativa,
      con controllo `TotalDepth`.
- [ ] C-prod-HS4 riportata **accanto** a C-overlap (mai da sola) per esporre l'eventuale
      spillover Eckel.

#### 7-R6 Audit post-bozza (Fable 5, 2026-07-08) e implementazione (Sonnet 5, 2026-07-08)

- [x] **Audit completo** della campagna di stima 13–20/16.do/17.do e della bozza
      (`New/Audit/2026-07-08_audit_report.md`): **nessun errore nelle stime**, tutti i numeri
      del paper tracciano agli output; **3 CRITICAL** (tutti nel testo del paper): SD di
      WB\_EP\_Depth sbagliata nel claim di magnitudine §4.1 (era "6 provisioni/1,4%", vero
      3,09/2,7%); "249 country-year" includeva erroneamente HK-MO (giusto: 223, con il fatto
      che GreenLib/Standards sono non-zero solo in 3 country-year, Corea 2015 e Svizzera
      2014-15); citazione fantasma "Caselli et al." senza bibitem. Piano di correzione:
      `New/PIANO_SONNET_2026-07-08.md`.
- [x] **Correzioni A1-A9 applicate** a `draft_paper.tex`: i 3 CRITICAL sopra, nota permutation
      (b\_obs=−0,0052 sul gap aggregato, non −0,0023 del collassato), split deep/shallow
      corretto a 17 vs 6 (escl. HK-MO), arrotondamenti (no-ASEAN dirty −0,0041 non −0,0042),
      `\label{sec:dirty}` al posto di "Section~4.4" hardcoded, riconciliazione celle
      collassato (3,68M post-singleton / 3,77M pre), abstract "45,8M" (era "46M"),
      `headmayer2014`/`larch2025` citati nel corpo, footnote metodologica sul WCB
      Frisch-Waugh (pt non annidata nel cluster). Check statico A9: begin/end bilanciati
      (25/25), nessuna cite/ref orfana, nessun pending.
- [x] **Sotto-indici enforcement completati** (B1): rieseguito `18_subindices_collapsed.R`
      (2 tentativi per il crash noto dell'allocatore su TREND\_EnforcementDSM) →
      `subindices_collapsed.csv` ora 8/8 sotto-indici (32 righe). Entrambi nulli su
      entrambi i margini (WB EnforcementDSM p=0,91/0,90; TREND EnforcementDSM p=0,78/0,71),
      aggiunto al §5.1 del paper.
- [x] **Replica cross-language esatta del collassato** (B2): nuovo `21_collapsed_replication.do`
      + export R→Stata (`New/Data/Collapsed/panel_pdt_for_stata.dta`). Stata reghdfe sullo
      stesso panel collassato: coefficienti identici a fixest entro 1e-9 (ben oltre 6
      decimali), stesso N finale 3.681.023 (92.475 singleton, stesso insieme). Dettagli in
      `New/Audit/comparison_collapsed.md`. Chiude lo Step 2 (cross-language) dell'audit per
      la spec collassata; il full panel resta validato solo per coerenza di segno/ordine
      di grandezza (allocatore R non regge il full panel 3-HDFE su questa macchina).
- [x] **Diagnosi East Timor** (B3, `New/Code/22_check_timor.R`): l'origine è
      `Code/Dataset_Creation/1_Build_Final_PTA_EP_Dataset.R` righe 244/316 — "East Timor"
      elencato per errore nella lista ASEAN originale (mai stato membro). Non modificato
      (file originale, fuori da `/New`). Impatto sulla stima collassata: differenza <1e-6 su
      tutti e 4 i coefficienti WB (praticamente nulla) — 144 pesa 9.069/45,8M righe (0,02%).
      Documentato in `New/Output/Diagnostics/timor_check.md` e nella nota di tab:treatment
      (A6 del piano).
- [x] **B4 (opzionale) — non aggiunta appendice Sun-Abraham.** La figura
      `eventstudy_sunab.png` non era mai inclusa nel paper (file orfano, rimosso da
      `figures/`). Nota per l'autore: il gap SA dirty a t=−6 è **+0,047 (p=0,001)**, un
      pre-trend significativo appena fuori dal periodo di riferimento — se in futuro si
      vuole includere la figura in appendice, va commentato esplicitamente (non è banale:
      è in tensione con l'affermazione di pre-trend piatti del TWFE event study §4.2, che
      usa un `rel_time` troncato diverso).
- [x] **Igiene codice (C1-C2, C4)**: rimosso in `19_sunab_gap.R` il filtro no-op
      `gap[entry_year != 10000L | TRUE]` e il modello `m_tw` mai usato/salvato. Corretto in
      `17_remaining_models.do` il bug di quoting nel loop di append finale (r(601): i nomi
      restituiti da `dir ... files` sono già tra virgolette compound e il primo giro deve
      essere `use`, non `append`, su un dataset vuoto) — testato in isolamento sui `.dta`
      di cache già esistenti, output identico a `tripledd_robustness_reghdfe.csv`. `/bibcheck`
      manuale su `neri2023` e `larch2025` (formato non-.bib, verifica diretta via ricerca
      web): entrambi i paper esistono ma con **titolo sbagliato** in entrambe le voci
      (mancava "Heterogeneous" in neri2023; larch2025 aveva un titolo completamente diverso
      da quello vero e "forthcoming" invece dei dati di pubblicazione reali, vol. 33(5),
      1066–1092) — corretti.
- [ ] **C3 — commit non ancora proposto/eseguito**: tutta la campagna 2026-07-06/08 e la
      bozza restano non committate (regola di sicurezza: mai commit senza conferma esplicita
      dell'utente).

#### 7-R7 Revisione pre-submission da peer review simulata (Fable 5, 2026-07-12)

Origine: referee report simulato "da top journal" sulla bozza `New/Paper/draft_paper.tex`
(sessione 2026-07-12). Verdetto simulato: **major revision** — esecuzione giudicata solida
(replica R↔Stata, LOO, joint F, battery inferenziale già oltre lo standard), le richieste
vere sono di framing + 4 analisi aggiuntive. Bilancio completo nella conversazione;
qui i task ordinati per priorità di attacco.

**Priorità 1 — il fatto avverso noto (bloccante per la submission)**

- [x] **R7.1 (Major 6) — Dinamiche Sun-Abraham complete.** Il gap SA dirty a t=−6 è
      **+0,047 (p=0,001)** (pending B4 di §7-R6, mai risolto). Indagare PRIMA di ogni
      altra cosa: scomposizione per coorte (il sospetto è che venga dalle coorti early
      ASEAN/Cile con pochi anni di pre-periodo), sensibilità alla scelta dei bin/troncamento
      di `rel_time`, confronto col TWFE §4.2. Esito atteso: o si spiega e si mostra in
      appendice con discussione, o si capisce che invalida "pre-trend piatti" per il dirty.
      Script di partenza: `19_sunab_gap.R`. Il paper NON va sottomesso senza questa risposta.
      **RISOLTO 2026-07-14** (`23_r71_sunab_diag.R` → `r71_sunab_diag.{csv,md}`): il t=−6
      NON è un pre-trend. (i) È identificato da sole 8 destinazioni (coorti 2006-15); (ii) i
      coefficienti coorte-specifici sono contraddittori (Cile −0,49, Perù −0,33, NZ +0,14,
      Australia +0,10 — segni opposti); (iii) leave-one-cohort-out: senza la sola Australia
      (coorte 2015) il coefficiente INVERTE segno (−0,051, p=0,11); senza le coorti 2014-15
      diventa −0,109 — il "+0,047 significativo" è interamente sospeso all'Australia via
      pesi n_tot; (iv) la VCOV clusterizzata è non-PSD (23 cluster vs ~28 coefficienti):
      i p-value sui singoli lead sono comunque inaffidabili; (v) l'ATT resta nullo in ogni
      variante (p 0,27/0,11/0,29) e i lead vicini [−5,−2] sono tutti piatti, come nel TWFE.
      Testo FATTO (2026-07-14): Appendix A (`app:sunab`) in `draft_paper.tex` con figura
      `figures/eventstudy_sunab.png` + i 4 argomenti; rimando aggiunto in §4 Dynamics.

**Priorità 2 — riscrittura a costo zero (un pomeriggio, nessun calcolo)**

- [x] **R7.2 (Major 1) — Riframing centrale**: da "null generale sulle EP" a "i capitoli
      dominati da cooperazione (il contenuto modale dei PTA cinesi) non hanno morso
      commerciale; le clausole con meccanismo esistono in soli 3 country-year e non sono
      testabili". L'evidenza (ρ=1,00, 3 country-year) è GIÀ nel §5.1: va portata in
      abstract e introduzione come tesi, non come caveat.
      **FATTO 2026-07-14**: abstract riscritto (il "central reading" è ora il bundling,
      non il null generico); paragrafo finale dell'intro riorganizzato con la tesi in
      apertura invece che come terzo di tre motivi.
- [x] **R7.3 (Major 2) — Brandi come conferma, non contrasto**: l'abstract dice "contrast
      with brandi2020"; il null è invece ciò che l'eterogeneità di Brandi predice per
      accordi senza clausole trade-restrictive/liberalizing. Allineare abstract+intro
      alla conclusione (che già lo dice).
      **FATTO 2026-07-14**: abstract e intro ora dicono esplicitamente "consistent with,
      not a contrast to, Brandi et al." e agganciano anche Abman et al. sulla stessa logica.
- [x] **R7.4 (Major 3, parte testo) — argomento del bias conservativo**: se le EP profonde
      correlano con preferenze verdi crescenti nella destinazione, il bias su EP×green è
      POSITIVO → il null osservato è un limite superiore. Aggiungere al §3.2.
      **FATTO 2026-07-14**: nuovo paragrafo in §3.2 dopo la definizione delle FE.
- [x] **R7.5 (Minori 1-8)**: switcher in tab:treatment (leggere `B_treatment_entry.csv`);
      nota dagger → testo §4.4; armonizzare "five designs" vs 9 righe tab:stability; frase
      re-export HK verso terzi (Feenstra-Hanson); caveat TD×green come proxy imperfetto
      delle concessioni green-specific; frase di qualificazione sull'analogia RZ (green/dirty
      è policy-defined, non tecnologica); 4 bibitem nuovi: Baghdadi-MartinezZarzoso-Zitouna
      2013 (JEEM), MacKinnon-Webb 2017 (JAE), Conley-Taber 2011 (REStat), Roodman et al.
      2019 (Stata Journal).
      **FATTO 2026-07-14** (tutti gli 8 punti + 5 bibitem, non 4 — aggiunto anche
      Feenstra-Hanson 2004 per il punto re-export HK, mancante nell'elenco originale).
      Switcher identificati via query diretta sul `.fst` (non c'era uno script dedicato):
      Laos (2002→2005), Singapore (2005→2009), Corea (2002→2015) — nomi aggiunti alla nota
      di `tab:treatment`. Bibliografia verificata via WebSearch prima di scrivere (non a
      memoria): **Baghdadi et al. 2013 è in Journal of International Economics, non JEEM
      come indicato nel piano originale** — corretto alla fonte reale. Conteggio "five
      designs" era sbagliato: i sub-campioni di controllo enumerati nel testo sono 4
      (C-prod-HS4, CEM, deep/shallow, common support), non 3 — abstract/intro ora dicono
      "six designs (... four control-group subsamples)"; §4.2 chiarisce che la tabella ha
      9 righe totali (6 design + 3 varianti di campione). Rimosso durante la scrittura un
      rimando in avanti a un risultato non ancora esistente (R7.6, corr(EP,TotalDepth)) che
      avevo scritto per errore nel paragrafo del proxy TD — corretto prima del check statico.
      Check statico pulito: begin/end bilanciati, nessun ref/cite orfano, nessun bibitem
      inutilizzato (script `check_refs2.py`, scratchpad).

**Priorità 3 — diagnostiche leggere (Windows, letture singole)**

- [x] **R7.6 (Major 4) — corr(EP_dt, TotalDepth_dt)** sui 223 country-year trattati
      in-sample + VIF/partialling-out dell'interazione. Un minuto di calcolo; riportare nel
      §3.2. Se ρ alto, qualificare "precise null".
      **FATTO 2026-07-14** (`24_r76_collinearity.R` → `r76_collinearity.md`): WB corr
      grezza 0,88, within (FE paese+anno) **0,95**, VIF 4,4 (≈ il max 4,6 di Brandi);
      TREND molto meno collineare (0,50/0,85). Dichiarato apertamente in §3.2 con la
      qualificazione: il coefficiente è identificato dalla quota modesta di variazione EP
      ortogonale a TD, già prezzata negli SE (ed è il motivo per cui i CI WB sono più
      larghi dei TREND); che due codings con collinearità così diversa diano lo stesso
      null è di per sé rassicurante. Disambiguato anche il VIF di §5.1 (sub-indici).
- [x] **R7.7 (Major 4) — benchmarking formale vs Brandi**: convertire il +17%-della-media
      green di Brandi nelle nostre unità (per provision o per SD) e mostrarlo contro i CI
      di full panel e collapsed. Sostituisce l'attuale frase singola sul 2,7%.
      **FATTO 2026-07-14**: numeri verificati dalla card wiki (Brandi: +0,4pp/provision
      liberale = +17% della quota green media ⇒ ≈ +0,16 log points nella nostra metrica;
      dirty: −5% della media ⇒ ≈ −0,05). Nuovo paragrafo in §4.1: il CI full panel
      [−0,0088, +0,0046] per provision ha upper bound ≈ 1/35 dell'effetto Brandi-equivalente;
      collapsed [−0,0147, +0,0100]; dirty: anche prendendo il point estimate asintotico
      (−0,0040) è un ordine di grandezza sotto. Caveat esplicito: le clausole di Brandi
      sono liberal/restrictive, quasi assenti negli accordi cinesi — che è la tesi stessa.
      La frase per-SD (−2,7%/+1,4%) conservata in coda.
- [x] **R7.8 (Major 8) — caratterizzazione campione post-singleton**: quota di valore
      export che sopravvive nei 21,5M, quote green/dirty pre/post rimozione, N prodotti
      green/dirty nelle celle identificanti (Minor 8). Un solo script diagnostico.
      **FATTO 2026-07-14** (`25_r78_sample_character.R` → `r78_sample_character.md`,
      replica della rimozione singleton in data.table puro, 8 iterazioni, converge a
      21.519.537 vs 21.519.511 di reghdfe — differenza di 26 oss., verosimilmente NA su
      ln_export scartati da reghdfe a monte): sopravvive il **47% delle oss. ma il 70,4%
      del valore export**; quote green 11,5→11,9% e dirty 7,0→6,4% (composizione stabile);
      celle fdt trattate identificanti: 26% green / 12% dirty, mediana 1 prodotto
      green/dirty per cella, 3 prodotti per cella trattata. Testo aggiunto al §3.2
      (paragrafo implementazione). Check statico finale pulito.

**Priorità 4 — stime nuove (Windows, run pesanti)**

- [x] **R7.9 (Major 3, parte stima) — robustezza con trend lineari destinazione×green**
      sul collapsed panel (fattibile; sul full panel solo se regge la RAM).
      **FATTO 2026-07-14, con colpo di scena gestito.** Tre script:
      `26_r79_desttrends.R` (trend full-sample via varying slopes fixest),
      `27_r79b_wcb_trends.R` (WCB con Frisch-Waugh esteso agli slopes — demean() con
      slope.vars/slope.flag; equivalenza FW↔feols verificata a 1e-9),
      `28_r79c_pretrend_variant.R` (trend stimati SOLO sul pre-periodo e proiettati).
      Esito: (1) trend full-sample — WB invariato (green p_wcb 0,28, dirty 0,28), ma
      **TREND×green inverte segno (+0,0018 n.s. → −0,0022) e sopravvive al WCB (p=0,013)**,
      unico coefficiente del paper a farlo; (2) firma Wolfers 2006 (unit trends che
      assorbono dinamica post) + coding gemello che non conferma + coerenza con la deriva
      di coorte già smontata da Sun-Abraham; (3) variante pre-period trends: TUTTO torna
      nullo (TREND green +0,0074 p_wcb 0,18; WB green +0,0176 p_wcb 0,46; dirty 0,30-0,61)
      — il segno dipende interamente da come si stimano i trend ⇒ artefatto di specifica.
      Nuova sottosezione "Destination-specific trends in the composition gap" in §5 del
      paper con entrambe le varianti e la lettura completa; bibitem `wolfers2006` aggiunto
      (verificato via WebSearch: AER 96(5), 1802-1820). CSV: `r79_desttrends.csv`,
      `r79b_wcb_trends.csv`, `r79c_pretrends.csv`. Check statico pulito.
- [x] **R7.10 (Major 5) — permutation sulla specifica collapsed vera** (non sull'aggregato):
      1.000 riassegnazioni × feols su 3,68M celle ≈ lancio notturno. Se infattibile,
      documentare perché e difendere l'aggregato nel testo. In ogni caso: citare e discutere
      MacKinnon-Webb (pochi cluster TRATTATI) e Conley-Taber nel §3.3.
      **IN ESECUZIONE 2026-07-14 ~16:47** (`29_r710_permutation_true.R`, processo detached
      via PowerShell, log in `New/Output/r710_run.log`). Molto più veloce del previsto
      (~1-2s/permutazione grazie al Frisch-Waugh incrementale: y e TD demeanati una volta
      per batch, a ogni draw solo le 2 colonne EP + QR solve): stima ~1,5-2h per 2×1.000
      draws, non serve la notte. Smoke test superato (identità FW = coefficienti di 14
      esatti per WB e TREND). Batch da 50 con cache RDS in `Models/r710_batches/`:
      rilanciabile, riparte dai batch fatti. Schema permutazione identico a 14 (remap dei
      profili EP tra le 23 trattate, seed 1000+batch). Citazioni MacKinnon-Webb/Conley-Taber
      in §3.3: GIÀ FATTE in R7.5.
      **COMPLETATA 2026-07-14 18:24** (1h37m, 40/40 batch senza crash). Risultati
      (`r710_permutation_summary.csv`, 1.000 draws per indice): WB×green p_perm=**0,897**;
      WB×dirty p_perm=**0,079** (marginale, coerente con WCB 0,18); TREND×green 0,170;
      TREND×dirty 0,852. Paper aggiornato: riga permutation di tab:main ora sulla spec
      esatta (0,90/0,079/0,17/0,85), nota tabella riscritta (l'aggregato con inversione di
      segno resta come variante), §4.1, §4.4 (tolto il riferimento al dagger, non più in
      tabella) e intro allineati. Check statico pulito. Draws completi in
      `r710_permutation_draws.csv`.
- [x] **R7.11 (Major 7) — dirty continuo alla Shapiro (2021)**: scaricare il replication
      package (link già in `05_dirty_goods.R`, che ha lo stub `shapiro2021_intensity.csv`),
      concordanza industria→HS6, interazione EP×intensità come robustezza. Rinviabile a
      dopo la circolazione come working paper, ma un referee vero la chiederà.
      **FATTA 2026-07-15** (`31_r711_shapiro_intensity.R`). Package scaricato da Harvard
      Dataverse (doi:10.7910/DVN/CTUS2E, 4,2MB) in `New/Data/Dirty/shapiro2021/`.
      **Verifica chiave del crosswalk** (non assunta): il file `combined_exiobase_s.dta`
      etichetta un sottoinsieme di p-code con nomi leggibili — confermato che il prefisso
      a 2 cifre = divisione ISIC3/NACE 1.1 (Pulp=p21, Coke oven=p23.1, fertilizzanti=
      p24.c/d, mattoni/cemento=p26.c/d, preziosi/alluminio/ferro-acciaio=p27.41-45+a),
      esattamente le divisioni dei 6 settori Mani-Wheeler già usati per `dirty_p` binario.
      **Bug trovato e risolto in corsa**: il primo sanity check (media CO2 per `dirty_p`
      da `dirty_goods_hs6.csv`) dava un segno NEGATIVO/contro-intuitivo — causa: quel file
      ha come popolazione base SOLO l'insieme esteso Mani-Wheeler (include il cemento),
      quindi confrontava "dirty core" contro "dirty esteso ma non core" (= cemento, il
      settore più intenso in assoluto), non contro i veri neutri. Rifatto il check contro
      la tricotomia vera del pannello: dirty 0,00115 > neutral 0,00078 > green 0,00061 —
      gradiente corretto, copertura crosswalk 90,5% degli HS6. Imputazione HS6 non
      concordati alla MEDIA campionaria (non zero, che avrebbe distorto sotto il "verde").
      **Stima** (collapsed panel, EP×intensità standardizzata al posto del binario,
      Frisch-Waugh+WCB come 27/29/30): WB null (−0,0033, p_wcb 0,52); TREND marginale
      che si affievolisce sotto WCB (asintotico p=0,012 → p_wcb 0,06) — stesso pattern
      "significatività asintotica fragile" di tutto il §4.4, non un risultato nuovo o più
      forte. Nuova sottosezione nel paper ("A continuous dirty margin: CO2 intensity") in
      §5, dopo i trend destinazione. Output: `New/Data/Dirty/co2_intensity_hs6.csv`,
      `New/Output/TripleDiff/Tables/r711_shapiro_intensity.csv`.

**Housekeeping (pre-esistenti, restano aperti)**

- [ ] C3 di §7-R6: commit dell'intera campagna 2026-07-06/12 (in attesa di conferma
      esplicita dell'utente).
- [ ] PDF del draft: install tectonic su Windows interrotto a metà (2026-07-11), da
      completare o compilare su Overleaf.
- [x] WCB sulla ladder full-panel: timeout a ~426s (2026-07-11) — decidere se timeout
      più lungo, collassato, o documentare come limitazione.
      **RISOLTO 2026-07-15** (`30_r7h_wcb_ladder.R`, Frisch-Waugh come 15/27/29):
      `bootstrap_summary.csv` finalmente prodotto (pending dal 2026-06-11). Le 4 spec
      di 01d (fpt+fpd, WB/TREND × baseline/controlli, B=9999): p_wcb 0,910/0,885/
      0,644/0,617 — il null della riga più saturata regge anche sotto WCB; frase
      aggiunta al §3.2. Bug trovato: boottest crasha sui design a 1 colonna con 49M
      righe → fix con intercetta (innocua su dati demeanati). Dettagli in
      `correspondence/audit/2026-07-15_audit_report.md`.
- [x] **Provenienza della lista green (247 HS6) VERIFICATA 2026-07-15**, su richiesta
      esplicita dell'utente ("da dove viene questa lista?"). La nota del
      2026-06-25/26 ("l'OCSE ha pubblicato la sua Combined List nel 2014 in HS2012")
      era un'ipotesi plausibile ma MAI verificata con una fonte primaria. Trovata e
      confermata: **Sauvage, J. (2014), "The Stringency of Environmental Regulations
      and Trade in Environmental Goods", OECD Trade and Environment Working Papers
      No. 2014/03** (DOI 10.1787/5jxrjn7xsnmq-en), Tabella A.1 (Annex 1, pag. 51-56):
      248 codici HS6 della CLEG. Primo controllo (contro un documento OECD 2005 con
      lista più vecchia, 132 codici) aveva dato un falso allarme — confrontava con la
      vintage sbagliata. Estratti programmaticamente tutti e 248 i codici della
      Tabella A.1 e confrontati uno per uno con `Data/Env_Codes_HS.dta`: **246/248 in
      comune (99,2%)**, unica differenza `871410` (progetto, aggregato) vs
      `871411`+`871419` (CLEG 2014, split più granulare) — stessa voce doganale,
      differenza di granularità, non un errore di classificazione. Citazione
      `sauvage2014` aggiunta alla bibliografia del paper (§2.2, prima non citata).

**Opzionale (non prioritario, valutare in futuro)**

- [ ] **Aggregazione country-year alla Brandi (2020)**: collassare il pannello a
      livello destinazione-anno (DIRTSHARE/GREENSHARE come in Brandi) invece di
      firm-product-destinazione-anno, come ulteriore robustness check di livello di
      aggregazione (discusso in paper a §5.1, "A second, compounding difference is
      the level of aggregation..."). Valutato il 2026-07-15: **non è una vera
      replica di Brandi**, perché Brandi sfrutta variazione cross-country
      (476.152 flussi bilaterali, molti esportatori), mentre qui c'è un solo
      esportatore (Cina) — collassare produce solo ~368 country-year (23
      destinazioni × ~16 anni), meno potenza del disegno attuale, non di più.
      Nessuna vera potenza aggiuntiva attesa; da fare solo se un referee lo chiede
      esplicitamente come robustness check aggiuntivo, non come priorità.

---

## 8. POTENZA E COLLINEARITÀ EP/TotalDepth (2026-08-03)

> **Origine.** Finding #3 dell'audit `correspondence/audit/2026-08-02_audit_report.md`:
> `WB_EP_Depth` e `TotalDepth_nonEnv` hanno corr. within (FE destinazione+anno) **0,96**, VIF
> **5,76** (`New/Output/Diagnostics/14_descriptives_collinearity.md`). L'IC WCB del
> coefficiente principale (WB×green, `wcb_collapsed.csv`) va da −0,018 a +0,032 — troppo largo
> per distinguere "nessun effetto" da "effetto non misurabile con questo disegno".
>
> ⚠️ **RISULTATO PRELIMINARE CHE RIORDINA LE PRIORITÀ (calcolato 2026-08-03, vedi 8.1).**
> L'ipotesi di partenza era "il problema è la collinearità di WB; TREND (VIF 1,33) è meno
> affetto". **È falsa.** Convertiti in unità comparabili (MDE per 1 SD del regressore), i due
> indici stanno così:
>
> | | SE | MDE/unità | SD regressore | **MDE per 1 SD** |
> |---|---:|---:|---:|---:|
> | WB × green | 0,00696 | 0,0195 | 2,65 | **5,2 %** |
> | TREND × green | 0,00182 | 0,0051 | 14,59 | **7,4 %** |
>
> (MDE = 2,80 × SE, potenza 80% al 5% bilaterale; SE da `tripledd_collapsed.csv`; SD dei
> regressori da `Data/Merged/Merged_TREND_WB_Indices_Only.csv`, 249 country-year trattati.)
>
> **TREND non sta meglio: sta leggermente peggio.** Il VIF basso di TREND era fuorviante perché
> non tiene conto della scala del regressore. Conseguenza per la roadmap: il vincolo che morde
> **non è (solo) la collinearità WB/TotalDepth, è la potenza del disegno** — 23 destinazioni
> trattate, di cui 2 coorti che ne contengono 15. Le mitigazioni 8.3–8.6 attaccano la
> collinearità, che è una *parte* del problema; **nessuna di esse aumenta il numero di cluster
> trattati**, che è il pavimento vero (già scritto in §7.4.4). Da qui l'ordine sotto: prima si
> quantifica la potenza (8.1–8.2, costo ~zero, decidono il framing), poi eventualmente si
> lavora sulla collinearità (8.3–8.6), che ha senso solo se 8.1 mostra che c'è margine.
>
> ⚠️ I numeri della tabella sopra sono un **back-of-envelope da rifare in R** (vedi 8.1): sono
> calcolati a livello country-year sui soli trattati e non pesati, mentre il campione di stima è
> a livello cella pesato per `n` e include le destinazioni mai-trattate con EP=0. L'ordine di
> grandezza e il *confronto* WB-vs-TREND reggono (stesso metodo per entrambi), il livello no.
>
> **Confronto con la letteratura (Brandi et al. 2020, wiki `Brandi2020_EPsGreenExports.md`):**
> stesso problema (corr. ENVPROVS-DEPTH 0,67, VIF 4,62), ma diluito da scala campionaria (680
> PTA, 476.152 flussi, molti esportatori), **non risolto con uno strumento diverso** — il loro
> presidio è lo stesso nostro (controllo per depth DESTA + VIF check). Con un solo esportatore e
> 14 accordi ci si aspetta collinearità strutturalmente più alta: confermato (0,96 vs 0,67).
> Utile in sede di scrittura: il limite è **intrinseco ai disegni single-exporter**, non un
> difetto di esecuzione di questo paper.
>
> **Nessuna delle voci sotto sostituisce la spec principale (§7.1)**: sono diagnostiche o
> robustezze d'appendice.

### 8.1 MDE / test di equivalenza sul campione di stima vero  ⭐ priorità massima, costo ~zero

Il deliverable minimo, e quello che l'audit indica come rischio n.1 in referaggio: sostituire
ovunque "non troviamo effetto" con "escludiamo effetti sopra X". Non richiede nuove stime, solo
gli SE già in `New/Output/TripleDiff/Tables/tripledd_collapsed.csv` più la SD dei regressori
**nel campione di stima** (pesata per `n`, con gli zeri delle mai-trattate).

- Nuovo script leggero `New/Code/33_mde_equivalence.R`: legge il panel collassato
  (`New/Data/Collapsed/panel_pdt_collapsed.fst`) per le SD pesate di `WB_EP_Depth` e
  `TREND_EP_Count`, legge SE e IC da `tripledd_collapsed.csv` + `wcb_collapsed.csv`, produce
  una tabella con: SE, MDE/unità, SD del regressore, MDE per 1 SD, MDE per il salto tipico
  osservato (es. Laos 1→6, Corea 1→17), e l'estremo superiore dell'IC WCB in % .
- Riportare **sia** l'MDE basato sull'SE asintotico **sia** quello basato sull'IC WCB (più
  onesto: il WCB è l'inferenza che il paper dichiara di usare).
- Opzionale ma consigliato: **TOST / equivalence test** formale contro una soglia sostantiva
  dichiarata a priori (es. "un effetto inferiore all'1% dell'export per unità di EP non è
  economicamente rilevante" — la soglia va argomentata, non scelta per far tornare il test).

**Checkpoint 8.1:**
- [x] `33_mde_equivalence.R` scritto ed eseguito → `New/Output/Diagnostics/33_mde_equivalence.md`
      (2026-08-07).
- [x] Numeri della tabella nel cappello di §8 sostituiti con quelli veri — vedi tabella
      corretta sotto.
- [x] **SMENTITO**: sul campione di stima vero (pesato, incl. mai-trattate), TREND non è
      peggio di WB — è leggermente **migliore** sul margine green (MDE/1SD: WB 4,64%
      asint./5,90% WCB; TREND 4,16% asint./3,62% WCB). Il back-of-envelope preliminare del
      cappello (SD non pesata sui soli trattati) portava alla conclusione opposta. Vedi nota
      di correzione sotto.
- [ ] Frase del paper riscritta: da "no effect" a "effetti sopra X esclusi al 95%; sotto X il
      disegno non discrimina". Tutte le occorrenze, non solo l'abstract. (Non fatto in questa
      sessione — richiede editing del testo del paper, fuori scope di questa passata di
      esecuzione script.)

> ⚠️ **Correzione al back-of-envelope del cappello di §8 (eseguito 2026-08-07).** La tabella
> preliminare usava la SD non pesata dei regressori sui soli 249 country-year trattati. Sul
> campione di stima vero (panel collassato, pesato per `n`, incluse le mai-trattate con
> EP=0, 3.773.498 celle) i risultati sono:
>
> | | SE (asint.) | MDE/unità (asint.) | SD regressore (pesata, campione vero) | **MDE per 1 SD (asint.)** | **MDE per 1 SD (WCB)** |
> |---|---:|---:|---:|---:|---:|
> | WB × green | 0,0070 | 0,0195 | 2,383 | **4,64 %** | **5,90 %** |
> | TREND × green | 0,0018 | 0,0051 | 8,165 | **4,16 %** | **3,62 %** |
>
> L'IC WCB al 95% esclude effetti superiori al **3,2%** per provisione WB sul margine green
> (estremo superiore). Per il salto tipico osservato: Laos (+5 unità) → MDE 12,4% (WCB); Corea
> (+16 unità) → MDE 39,7% (WCB). Dettaglio completo, incluso il margine dirty, in
> `New/Output/Diagnostics/33_mde_equivalence.md`.

### 8.2 Diagnostica di potenza per coorte/cluster (costo ~zero, chiude il quadro di 8.1)

Documentare *perché* l'MDE è quello che è, così il limite è attribuito alla fonte giusta (pochi
cluster trattati) e non letto come sciatteria. Materiale già quasi tutto prodotto:
`Diagnostics/13_descriptives_treatment.md` (25 trattati, 3 switcher),
`Diagnostics/r71_sunab_diag.md` (tabella coorti: 2002→5, 2005→10, poi 7 coorti singleton),
`Diagnostics/15_descriptives_sample.md` (celle `fdt` identificanti: 26% green, 12% dirty).

- Consolidare in un unico paragrafo/tabella d'appendice: n. cluster trattati, distribuzione
  per coorte, quota di celle che identificano davvero l'interazione, sbilanciamento dei pesi
  tra cluster (già noto: rapporto ~163× per peso osservazioni, top-5 = 51% della massa
  trattata — vedi session-log 2026-07-31 e la memoria di progetto su MacKinnon-Webb).
- Nessuna stima nuova.

**Checkpoint 8.2:**
- [x] Tabella/paragrafo "power and identifying variation" pronto per l'appendice — nuovo
      script `New/Code/34_power_diagnostics.R`, output
      `New/Output/Diagnostics/34_power_diagnostics.md` (2026-08-07). Ricalcolato e confermato
      lo squilibrio di peso citato in session-log 2026-07-31: rapporto max/min **163,2x**,
      top-5 cluster = **50,7%** della massa trattata.
- [x] Collegato esplicitamente all'MDE di 8.1 (sezione finale del file).

### 8.3 Disaggregazione di TotalDepth per area (la mitigazione più promettente)

Il controllo `TotalDepth_nonEnv` è uno scalare che assorbe **tutta** la profondità non
ambientale. Se l'ambiente correla fortemente solo con *alcune* aree (es. standard tecnici,
SPS) e debolmente con altre (es. investimenti, visti), controllare per l'aggregato drena
variazione di EP senza necessità: un controllo mirato ne lascia di più.

**Fattibilità verificata 2026-08-03**: `Data/WB/WB_DTA.dta` contiene le ~18-20 aree standard
DTA 2.0 (confermata la presenza di Antidumping, Competition, Countervailing, Environmental,
Investment, Intellectual Property, Labor, Movement of Capital, Public Procurement, Sanitary,
Services, State-Owned Enterprises, Subsidies, Technical Barriers, Trade Facilitation, Visa,
Customs — centinaia di provision ciascuna). `08_total_depth.R` legge già `wb$Area` e la usa
per separare l'ambientale (`grepl("Environmental Laws", wb$Area)`): la stessa `build_depth()`
applicata a sottoinsiemi di `Area` produce i sotto-totali senza riscrivere la logica.

- Nuovo script `New/Code/33_totaldepth_byarea.R`: stessa logica di `08_total_depth.R`, ma
  `build_depth()` chiamata una volta per gruppo di aree → `wb_totaldepth_byarea_country_year.csv`.
- Calcolare corr. within (FE paese+anno, stesso metodo di `14_descriptives_collinearity.md`)
  tra `WB_EP_Depth` e ciascun sotto-totale. **Attesa realistica**: le aree probabilmente
  correlano *tutte* fortemente tra loro (un accordo profondo è profondo ovunque) — se è così
  il guadagno è nullo e va documentato come tale, non forzato.
- Se e solo se emergono aree a bassa correlazione: ristimare la spec principale con controllo
  mirato (solo aree ad alta corr.) e confrontare SE/VIF con l'aggregato.

**Checkpoint 8.3:**
- [x] Raggruppamento delle `Area` deciso e documentato — 17 aree WB non-ambientali, ciascuna
      trattata individualmente (nuovo script `New/Code/37_totaldepth_byarea.R`,
      `New/Data/TotalDepth/wb_totaldepth_byarea_country_year.csv`, 2026-08-07).
- [x] Matrice di corr. within EP vs ogni area prodotta →
      `New/Output/Diagnostics/37_totaldepth_byarea.md`. **Bug trovato e corretto** durante lo
      sviluppo: un pattern di demeaning "in place" con `.SDcols` in data.table collassava
      erroneamente a SD=0 (verificato con un caso a 2 variabili, confrontato col pattern a 3
      righe già verificato in `14_descriptives_collinearity.R`).
- [x] **Esito misto, non il negativo pulito atteso**: 14/17 aree correlano fortemente
      (0,74–0,97) con WB_EP_Depth, ma **3 aree correlano poco**: Labor Market Regulations
      (varianza zero nel campione — nessun accordo cinese la tocca), Visa and Asylum (0,51),
      Subsidies (0,67).
- [x] **Ristima con controllo mirato prodotta** (nuovo script
      `New/Code/38_robustness_totaldepth_targeted.R`,
      `New/Output/Diagnostics/38_totaldepth_targeted.md`): VIF passa da **5,71** (aggregato,
      17 aree) a **hemos** — vedi tabella:

      | Controllo | VIF |
      |---|---:|
      | TotalDepth_nonEnv (17 aree) | 5,76 |
      | TotalDepth_targeted (14 aree) | 5,69 |

      | | Spec principale (aggregato) | Robustezza (mirato) |
      |---|---:|---:|
      | WB × green | −0,0046 (se 0,0070) | **−0,0033 (se 0,0079)** |
      | WB × dirty | −0,0119 (se 0,0030) | −0,0121 (se 0,0035) |
      | TREND × green | +0,0018 (se 0,0018) | +0,0021 (se 0,0018) |
      | TREND × dirty | +0,0004 (se 0,0016) | +0,0005 (se 0,0016) |

      Coefficienti stabili in segno; SE leggermente più ampi col controllo mirato (non più
      stretti) — il guadagno è marginale, coerente con l'esito misto della matrice di
      correlazione: le 3 aree escluse pesano poco sulla collinearità complessiva.

> **Nota tecnica sui crash**: la stima TREND del controllo mirato ha richiesto oltre 50
> tentativi in sottoprocesso `callr` (crash "recursive gc invocation" + fallimenti del
> controllo Frisch-Waugh), fino a uno stallo genuino del processo (terminato manualmente
> dopo 15+ minuti senza progresso). Risolta eseguendo la stima **direttamente in-process**
> (bypassando `callr`): il controllo Frisch-Waugh ha confermato il risultato con margine
> ampio (differenza ~5e-16, tolleranza 1e-6). Stesso pattern già osservato in §8.4 e nella
> riga WB dello stesso modello (quella è invece riuscita in `callr` al 5° tentativo). La
> causa resta l'instabilità nota dell'allocatore R su questa macchina (vedi memoria di
> progetto `fixest-callr-crash-can-silently-corrupt-results`) — qui aggravata su modelli con
> 4 interazioni su un regressore a bassa varianza (`TotalDepth_targeted`).

### 8.4 Contrasto deep-vs-shallow sui soli trattati (già parzialmente in casa)

> ⚠️ **Correzione rispetto alla prima stesura di questa sezione (2026-08-03, pre-verifica).**
> L'idea originale era usare `EP_share = EP/TotalDepth` come regressore alternativo su tutto il
> campione. **Non funziona così**: per le mai-trattate `TotalDepth = 0`, quindi `EP_share` è
> indefinita e porla a 0 rende la variabile ≈ una **dummy di trattamento riscalata** (0 per le
> non trattate, ~0,03 per le trattate) → reintroduce esattamente il confound **C1 di §7.0**
> (livello EP collineare con l'entrata in vigore del PTA) che tutto il disegno §7.1 esiste per
> evitare. Il rapporto ha senso **solo condizionatamente all'avere un accordo**.

Formulazione corretta: stimare **sul solo campione dei partner PTA** (nessuna mai-trattata),
con `EP_share` — cioè "a parità di *avere* un accordo, un accordo più ambientale sposta la
composizione?". È il contrasto *content conditional on agreement* di Abman-Lundberg-Ruta (2024)
già citato nel paper, ed è **il sotto-campione `C-deepshallow` che esiste già**
(`New/Data/Subsamples/flag_deepshallow.csv`, usato da `24_stability_controlgroups.R`).

- Variazione disponibile, verificata 2026-08-03 sui 223 country-year trattati (excl. HK+MO):
  `EP_share` va da 0,0119 a 0,0680, 12 valori distinti, **CV 0,184 contro 0,579 del livello EP**.
  Varia, ma molto meno del livello → **attendersi SE ampi**: è un regressore a bassa varianza,
  non una scorciatoia gratuita. Corr(EP, TotalDepth) *sui soli trattati* = 0,909 (contro 0,96
  within complessivo): il restringimento ai trattati da solo non scioglie la collinearità.
- Implementazione: estendere `24_stability_controlgroups.R` (che già filtra `deepshallow`)
  con una riga di regressione aggiuntiva su `EP_share`, oppure nuovo
  `New/Code/34_epshare_treatedonly.R` se si preferisce non toccare la cache di 24.
- **Attenzione al cambio di estimando**: da "effetto marginale di una clausola EP in più" a
  "effetto della composizione ambientale dell'accordo". Va dichiarato nel testo, non usato come
  robustezza silenziosa.

**Checkpoint 8.4:**
- [x] `EP_share` costruito sui soli trattati (25 paesi deep+shallow, panel collassato,
      534.846 celle); distribuzione documentata: 12 valori distinti, range [0,0119, 0,0680],
      CV 0,188 (nuovo script `New/Code/39_epshare_treatedonly.R`,
      `New/Output/Diagnostics/39_epshare_treatedonly.md`, 2026-08-07).
- [x] Stima prodotta: **EP_share × green = −2,25 (se 1,15, p=0,063)** — negativo, marginale
      al 10% ma non al 5%; **EP_share × dirty = −1,60 (se 1,55, p=0,315)**, non significativo.
      Segno coerente con la spec principale (entrambi negativi sul margine green), ma qui
      il coefficiente è quasi significativo al 10% — da leggere con cautela: varianza di
      EP_share molto più bassa del livello (come atteso), SE ampi, e l'estimando è diverso
      (composizione ambientale dell'accordo dato che l'accordo esiste, non effetto marginale
      di una clausola in più).
- [ ] Se entra nel paper: paragrafo che dichiara il cambio di estimando + il legame con ALR
      2024. (Non fatto in questa sessione — editing del testo del paper fuori scope.)

> **Nota tecnica**: la stima di questo checkpoint ha richiesto di bypassare il sottoprocesso
> `callr` standard (usato per isolare i crash noti di `feols` su questa macchina) — con
> `EP_share` (scala 0,01-0,07) il sottoprocesso falliva il controllo Frisch-Waugh interno
> molto più spesso del solito (>20 tentativi). Verificato con debug diretto: **eseguita
> in-process, la stima supera il controllo con ampio margine** (differenza ~1,5e-11,
> tolleranza 1e-6) — il problema era instabilità del sottoprocesso, non del risultato.
> Stesso pattern osservato anche in §8.3 (vedi nota lì).

### 8.5 VIF dei singoli sotto-indici (priorità bassa — probabile vicolo cieco)

Verificare se un sotto-indice specifico correla con `TotalDepth_nonEnv` meno dell'aggregato.

**Perché la priorità è bassa (verificato 2026-08-03).** I sotto-indici WB hanno range minuscolo:
`WB_GreenLiberalization` ∈ {0,1}, `WB_StandardsNonRegression` ∈ {0,3},
`WB_EnforcementDSM` ∈ {0,1,2,3}. Le uniche combinazioni osservate di
(GreenLib, StandardsNonRegr, EnfDSM) sono (0,0,0), (0,0,1), (0,0,2), (1,3,2), (1,3,3). Un VIF
basso su una variabile quasi-binaria sarebbe **meccanico** (poca varianza da condividere), non
un segno di identificazione migliore — e l'MDE per 1 SD peggiorerebbe, non migliorerebbe. Vale
la pena calcolarlo solo per **documentare** che questa strada è chiusa.

- Prerequisito non negoziabile: correggere prima il **finding #2 dell'audit**
  (`WB_GreenLiberalization` e `WB_StandardsNonRegression` sono lo stesso regressore, rapporto
  esatto 3×) — altrimenti la tabella VIF conterrebbe due righe identiche.
- Estendere `14_descriptives_collinearity.md` (o nuovo `14b_…_subindices.md`) con corr. within e
  VIF per i sotto-indici di `25_heterogeneity_subindices.R`, **escludendo**
  `WB_StandardsNonRegression`.

**Checkpoint 8.5:**
- [x] Finding #2 dell'audit corretto in `25_heterogeneity_subindices.R` (2026-08-07):
      `WB_StandardsNonRegression` rimosso (era lo stesso regressore di
      `WB_GreenLiberalization`, riscalato 3x, corr=1,000). Script rieseguito, tutte le
      stime rimanenti erano già cachate.
- [x] Tabella VIF + **MDE per 1 SD** per sotto-indice — nuovo script
      `New/Code/41_vif_subindices.R`, `New/Output/Diagnostics/41_vif_subindices.md`.

      | Sotto-indice | corr within con TD | VIF | MDE/1SD |
      |---|---:|---:|---:|
      | WB_EnforcementDSM | 0,973 | 3,93 | 7,2% |
      | TREND_RegulatorySpace | 0,891 | 1,94 | 8,4% |
      | TREND_Hard | 0,826 | 1,32 | 5,6% |
      | TREND_Soft | 0,820 | 1,14 | 6,0% |
      | TREND_EnforcementDSM | 0,766 | 1,11 | 5,4% |
      | TREND_GreenMarketAccess | 0,766 | 1,04 | 1,6% |
      | WB_GreenLiberalization | 0,766 | 1,03 | 2,8% |

- [x] **Esito confermato: negativo, come atteso.** Il VIF basso di `WB_GreenLiberalization`
      e `TREND_GreenMarketAccess` è meccanico — sono variabili quasi-binarie con poca
      varianza da condividere con TotalDepth, non un segno di identificazione migliore. Il
      valore dei sotto-indici resta nel test di meccanismo (quale canale specifico "morde"),
      non nella riduzione della collinearità.

### 8.6 Bounds / partial identification (ultima risorsa di framing)

Se 8.3–8.5 non stringono l'IC in modo sostanziale — esito **probabile**, viste 8.1 e la natura
strutturale del vincolo — riportare un intervallo di stime plausibili sotto assunzioni
alternative su come allocare la covarianza condivisa tra EP e TotalDepth, invece di un unico
coefficiente puntuale che il disegno non separa con precisione.

- Nessuno script per ora. Da valutare **solo dopo** 8.1 e 8.3. Se si procede, serve prima una
  nota metodologica (letteratura partial identification, Manski) — non implementare a scatola
  chiusa.
- Alternativa più leggera e forse sufficiente: presentare in una sola tabella il coefficiente
  EP×green sotto tre controlli (nessun controllo di depth / TotalDepth aggregato / controllo
  mirato di 8.3) e lasciare che l'ampiezza del ventaglio parli da sola, senza formalismo
  Manski. Spesso è ciò che un referee vuole davvero vedere.

**Checkpoint 8.6:**
- [ ] Deciso dopo 8.1/8.3 se serve.
- [ ] Se sì: nota metodologica scritta prima del codice.

### 8.7 Switcher within-country (declassato a check descrittivo — NON una stima)

> ⚠️ **Correzione rispetto alla prima stesura (2026-08-03, pre-verifica).** Era indicata come
> "identificazione più pulita disponibile, priorità alta". **La variazione non c'è.** Serie
> complete verificate su `New/Data/TotalDepth/wb_totaldepth_country_year.csv`:
>
> | Paese | cc | Salto EP | Anno | Anni post nel campione |
> |---|---|---|---|---|
> | Corea | 133 | 1 → 17 | **2015** | **1** (ultimo anno del panel) |
> | Laos | 119 | 1 → 6 | 2005 | 11 |
> | Singapore | 132 | 6 → 7 | 2009 | 7 |
>
> Cioè: la Corea ha il salto grande ma **un solo anno post**; Singapore ha 7 anni post ma un
> salto di **1 unità**; solo il **Laos** ha salto e finestra decenti. Una stima su 3 cluster di
> cui uno con un anno di post-periodo non è "l'identificazione più pulita": è una stima che non
> si può difendere.

Cosa resta di utile: usare i tre switcher come **evidenza descrittiva** che la proporzionalità
EP/TotalDepth non è perfetta (Corea: EP ×17 contro TotalDepth ×9,5 — quindi la variazione
residua che identifica il coefficiente esiste, non è zero), a supporto del paragrafo di 8.2 su
dove sta la variazione identificante. Un grafico o una riga di tabella, non una regressione.

**Checkpoint 8.7:**
- [x] Tabella dei 3 switcher (salto EP, salto TotalDepth, anni post) in appendice — nuovo
      script `New/Code/40_switchers_descriptive.R`,
      `New/Output/Diagnostics/40_switchers_descriptive.md` (2026-08-07). Numeri confermati:
      Corea 1→17 (2015, **1 anno post**), Laos 1→6 (2005, 11 anni post), Singapore 6→7
      (2009, 7 anni post). Rapporto EP supera sempre il rapporto TotalDepth (Corea 17x vs
      9,5x; Laos 6x vs 5,2x; Singapore 1,17x vs 1,39x — unico caso invertito).
- [x] **Non** stimata la spec sui soli switcher, come da istruzione.

### 8.9 DESTA depth index come double-check del controllo TotalDepth (fonte esterna)

> **Origine.** Discussione 2026-08-05: usare il DESTA depth index (Dür, Baccini & Elsig
> 2014) come misura alternativa di profondità PTA per ridurre la correlazione meccanica
> EP–TotalDepth. `TotalDepth_nonEnv` viene dalla stessa fonte WB che misura EP: la
> collinearità 0,96 è in parte strutturale (accordi profondi coprono anche le EP) ma in
> parte artefatto di database (stesso codificatore, stesso questionario). Il DESTA
> depth_index proviene da un dataset indipendente → la corr. EP-DESTA riflette solo la
> realtà, non l'architettura dello stesso dataset.
>
> **Cosa copre il DESTA depth_index**: 7 aree tematiche (beni, servizi, investimenti,
> standard, appalti, concorrenza, IPR). Le environmental provisions **non rientrano** nel
> conteggio per costruzione → nessuna correlazione meccanica con EP.

**Cosa è già pronto:**

- `New/Code/32_desta_depth.R` — **già scritto**, costruisce
  `New/Data/TotalDepth/desta_depth_country_year.csv` da DESTA v2.3
  (`New/Data/External/DESTA/`). Copre tutti i 14 accordi del campione con espansione
  country-year 2000–2015. East Timor escluso (non in DESTA). Non ancora eseguito.

**Da fare:**

1. Eseguire `32_desta_depth.R` → `desta_depth_country_year.csv` (< 1 min).
2. Eseguire il check correlazione (script `corr_ep_desta.R` già scritto in scratchpad,
   da spostare in `New/Code/` se riutilizzabile):
   - confronta corr(EP, TotalDepth_nonEnv) vs corr(EP, DESTA_depth_index) sui 223
     country-year trattati (excl. HK+MO) e sul campione di stima completo;
   - verifica anche la correlazione TotalDepth_nonEnv ~ DESTA_depth_index (quanto si
     somigliano come misure).
   - **Se la riduzione di corr. è < 0,05 in valore assoluto**: documentare e chiudere
     qui — il DESTA non risolve la collinearità strutturale, solo la parte meccanica.
3. Se riduzione sostanziale (> 0,05): sostituire `TotalDepth_nonEnv` con
   `DESTA_depth_index` in `16_main_tripledd_collapsed.R` come robustezza e confrontare
   SE / coefficienti EP con la spec principale.

**Note:**
- Copertura: DESTA 2.3 arriva al 2019 ma il depth_index è più sparsamente codificato
  sulle versioni tardive — già gestito in `32_desta_depth.R` con fallback al base_treaty.
- La correlazione EP-DESTA attesa è alta (stessa realtà economica) ma probabilmente
  inferiore a 0,96: beni/servizi/investimenti correlano con EP meno degli standard
  tecnici/SPS, che sono le aree WB più vicine all'ambientale.

**Checkpoint 8.9:**
- [x] `32_desta_depth.R` eseguito → `desta_depth_country_year.csv` prodotto (2026-08-07,
      239 country-year, 25 paesi, East Timor escluso).
- [x] Matrice di correlazione EP vs TotalDepth_nonEnv vs DESTA documentata (nuovo script
      `New/Code/35_desta_correlation_check.R`) → `New/Output/Diagnostics/32_desta_check.md`.
- [x] **Esito POSITIVO** — la riduzione supera la soglia di rilevanza (0,05):
      - corr within (FE paese+anno) WB_EP_Depth: TotalDepth 0,959 → **DESTA 0,891**
        (riduzione 0,068); VIF: 5,71 → **1,92**.
      - corr grezza: TotalDepth 0,908 → DESTA 0,691 (riduzione ancora più marcata, 0,217).
      - corr TotalDepth~DESTA = 0,710: le due misure di profondità generale si somigliano
        ma non sono la stessa cosa — c'è margine reale, non solo rumore.
      - Copertura: 212/223 country-year trattati (95,1%; East Timor mancante).
- [x] Stima di robustezza prodotta (nuovo script `New/Code/36_robustness_desta.R`) →
      `New/Output/TripleDiff/Tables/tripledd_collapsed_desta.csv`. Confronto con la spec
      principale (16, TotalDepth):

      | | Spec principale (TotalDepth) | Robustezza (DESTA) |
      |---|---:|---:|
      | WB × green | −0,0046 (se 0,0070) | −0,0043 (se **0,0043**) |
      | WB × dirty | −0,0119 (se 0,0030) | −0,0113 (se 0,0022) |
      | TREND × green | +0,0018 (se 0,0018) | +0,0016 (se 0,0020) |
      | TREND × dirty | +0,0004 (se 0,0016) | −0,0002 (se 0,0014) |

      Coefficienti stabili in segno e ordine di grandezza; **il SE del margine green WB si
      dimezza** (0,0070→0,0043), coerente col VIF molto più basso. Il null regge, e la
      precisione migliora usando una misura di profondità da fonte indipendente — rafforza
      la lettura "null di precisione", non un artefatto della collinearità WB-TotalDepth.

### 8.10 APEC EGL subsample come check di classificazione (priorità bassa — nota a piè di pagina)

> **Origine.** Discussione 2026-08-06: verifica dell'overlap tra la nostra lista green
> (247 codici HS1996, OECD Combined List) e la APEC Environmental Goods List (2012
> Vladivostok Declaration, Annex C), come possibile subsample di "alta confidenza".

**Overlap verificato** su Sauvage (2014) Tabella A.1 (DOI 10.1787/5jxrjn7xsnmq-en,
disponibile in Zotero, item `RGDJCIA8`): la colonna APEC della tabella marca 53 codici
HS 2007. Tutti e 53 compaiono nella nostra lista (441872/HS2007 → 441830/HS1996 già
presente nel file di concordanza). **La APEC EGL è un sottoinsieme completo della nostra
OECD Combined List**: nessuna conversione extra necessaria.

**Razionale del subsample**: restringe i prodotti "green" ai 53 su cui esiste consenso
politico multilaterale esplicito. Se il null regge anche lì, la classificazione OECD
(più ampia) non può essere accusata di rumore. La minaccia specifica è: "forse il null
esiste perché la lista a 247 codici include prodotti borderline". Risposta: non cambia.

**Perché è bassa priorità:**
- Da 247 codici green a 53 → riduzione dell'~79% del campione green.
- Le celle identificanti (firm×destination×year con almeno un prodotto APEC-green +
  uno neutral) crollano; i cluster trattati non aumentano.
- La minaccia di classificazione è meglio coperta da C-prod-HS4 (già in casa), che
  restringe i neutral (non i green) ai prodotti tecnologicamente simili.
- Interesse informativo basso aggiuntivo rispetto ai subsample già presenti.

**Implementazione (se si decide di includere):**

1. Aggiungere al file `New/Data/Classifications/green_codes_hs1996.csv` una colonna
   `apec_egl` (0/1) marcando i 53 codici che compaiono nella APEC EGL (elenco da
   Sauvage 2014, Tabella A.1, colonna APEC = X).
2. Aggiungere un flag `apec_green` al panel collassato (`10_collapsed_panel.R` o
   separatamente in `11_subsamples.R`).
3. Riestimare la spec principale su `apec_green + dirty + neutral` invece di
   `green + dirty + neutral` — una riga aggiuntiva nella tabella di stabilità, oppure
   una nota a piè di pagina nella sezione robustezza.

**Nessuno script nuovo necessario**: basta una lista di 53 codici e un join.

**Checkpoint 8.10:**
- [x] Lista APEC EGL codificata come colonna `apec_egl` in `green_codes_hs1996.csv`
      (2026-08-07, nuovo script `New/Code/43_apec_egl_subsample.R`) — **54 codici** (non 53:
      l'estrazione automatica dalla Tabella A.1 di Sauvage 2014 ha trovato un codice in più
      del conteggio visivo iniziale). Tutti e 54 confermati presenti nella lista OECD
      completa (247 codici) — nessuna concordanza extra necessaria.
- [x] Stima prodotta (collapsed panel, spec principale, solo prodotti APEC-green, 2,0% delle
      celle contro 8,4% della lista completa) →
      `New/Output/TripleDiff/Tables/tripledd_collapsed_apecgreen.csv`,
      `New/Output/Diagnostics/43_apec_egl_subsample.md`.

      | | Lista completa (247 codici) | APEC EGL (54 codici) |
      |---|---:|---:|
      | WB × green | −0,0046 (se 0,0070, p=0,513) | **+0,0050** (se 0,0127, p=0,693) |
      | TREND × green | +0,0018 (se 0,0018, p=0,319) | +0,0032 (se 0,0021, p=0,126) |

      Il segno del coefficiente WB si inverte (da negativo a leggermente positivo) ma resta
      **statisticamente indistinguibile da zero** (p=0,69) — coerente con il null, non
      un'evidenza contraria. SE quasi raddoppiati, come atteso dalla riduzione dell'80% del
      campione green.
- [ ] Riportata come nota a piè di pagina nella sezione §6 del paper (robustezza
      classificazione), con citazione Sauvage (2014) + APEC (2012). (Non fatto in questa
      sessione — editing del testo del paper fuori scope.)

---

### 8.8 Ordine di esecuzione consigliato

1. **8.1 + 8.2** (mezza giornata, nessuna stima pesante) → decidono il framing dell'intero
   capitolo. Se l'MDE risulta accettabile per il tipo di effetto che la letteratura si aspetta,
   il paper è a posto e 8.3–8.7 diventano appendice opzionale.
2. **8.9** (< 1 ora, check veloce) → verifica se una fonte esterna riduce la collinearità;
   conviene farlo prima di 8.3 perché i risultati cambiano il valore atteso di 8.3.
3. **8.3** (la mitigazione con un meccanismo plausibile) → se dà un guadagno reale, si
   ristima; se no, si documenta il vicolo cieco.
4. **8.4** se si vuole il contrasto ALR-style esplicito (ha valore di framing anche a SE ampi).
5. **8.5 / 8.6 / 8.7** solo su richiesta di un referee o se avanza tempo.
6. **8.10** (< 1 ora se si decide di includerlo) — solo se si vuole la nota a piè di
   pagina sulla classificazione. Basta aggiungere la colonna `apec_egl` al CSV e una
   riga di regressione: nessuna dipendenza dagli altri punti.

> **Aspettativa onesta da tenere presente:** nessuna di queste voci trasforma il null in un
> risultato. Servono a rendere il null **difendibile e quantificato** invece che ambiguo. La
> decisione di framing (capitolo "null di precisione / limiti dei disegni single-exporter" vs
> "effetto delle EP") va presa dopo 8.1, non prima.

---

## 9. ~~⚠️ RERUN DOVUTO~~ ✅ FATTO — lista green goods corretta (2026-08-04)

> **STATO AL 2026-08-11: CHIUSO.** `green_codes_hs1996.csv` è stato rigenerato il 2026-08-07
> (contiene 871411 e 871419, il fantasma 871410 non c'è più) e **tutte e quattro le run della
> matrice 2×2 usano la lista corretta** — ogni script punta a
> `New/Data/Classifications/green_codes_hs1996.csv`. Il testo sotto è la diagnosi originale,
> conservata per memoria: **ignorare le sue istruzioni "da fare", sono già state eseguite.**
> Resta aperto solo il punto 4 (aggiornare il conteggio green in `draft_paper.tex`), che è
> lavoro di scrittura ed è ripreso in §11.3.

**Cosa è cambiato**: `Data/Env_Codes_HS.dta` (fonte dei prodotti "verdi", creata a mano
tempo fa) aveva 247 codici invece dei 248 del CLEG originale (Sauvage 2014, Table A.1).
Il codice mancante non era un'omissione ma un errore di trascrizione: il file aveva
871410, un codice che **non esiste** in nessuna revisione HS verificata; l'originale ha
invece due codici distinti, 871411 e 871419. **Corretto** il file sorgente (ora 248
righe, 871410 → 871411 + 871419) — verificato via estrazione diretta della Table A.1 dal
PDF ufficiale OECD (vedi `New/Output/Diagnostics/CLEG_Sauvage2014_extraction.md` e
`New/Data/Classifications/CLEG_Sauvage2014_TableA1.csv`/`.dta`).

**Perché serve un rerun (non ancora fatto)**: tradotto a HS1996 col metodo corretto, il
fix aggiunge **1 codice HS1996 nuovo** (871411) che prima non veniva mai considerato
"verde" nel matching contro il pannello doganale — i prodotti verdi finali passano da
245 a 246 codici distinti. Impatto atteso piccolo (1 codice su ~246, stessa scala dei
robustness leave-one-out già in appendice), ma non verificato: **nessuna stima è stata
rifatta**. Finché non si rilancia la pipeline, tutti i numeri attuali nel paper restano
basati sulla lista vecchia (245 codici, con 871410 fantasma).

**Da fare, quando si decide di rilanciare (non ora — prima si cercano altri errori simili)**:
1. Rilanciare `New/Code/05_green_goods_hs1996.R` (rigenera `green_codes_hs1996.csv` da
   `Data/Env_Codes_HS.dta` corretto — leggero, ~2 min).
2. Verificare se 871411 compare davvero nel pannello doganale con flussi non-zero (se
   non compare, l'impatto pratico è zero anche sulla lista finale).
3. Se compare, rilanciare l'intera pipeline `New/` (stime principali + robustezze) con
   il conteggio verde aggiornato.
4. Aggiornare `draft_paper.tex`: conteggio green (245→246, o quello che risulta), e
   il passaggio confuso 247/248/246 di §2.2 (vedi conversazione 2026-08-04).

**Nota collegata**: lo stesso controllo ha verificato che l'assunzione di vintage HS del
CLEG nello script 05 (`origin = "HS4"`, cioè HS2012) è **sbagliata** — il paper dichiara
HS2007 (`origin` dovrebbe essere `"HS3"`) — ma sui 247 codici originali questo non
cambiava nulla a parte il codice 871410 stesso (245 distinti in entrambi i casi). Non
richiede azione separata, il fix sopra la assorbe.

---

## 10. LACUNE DEGLI EXPORT — informazioni non ricostruibili dai CSV (2026-08-11)

Rilevate mentre si chiudeva Run 2, verificando se i CSV bastino a ricostruire le tabelle del
paper in un secondo momento. **Nessuna azione presa: si è deciso di finire prima tutte le run.**

> **STATO AL 2026-08-11**: le 4 run sono chiuse, quindi la condizione che rinviava questi fix
> è caduta — **le 4 lacune qui sotto sono ora lavorate**, e sono riprese in §11.3 punto 4.
> Attenzione al vincolo di coerenza già segnalato al punto 1: aggiungere colonne a un export
> significa **rilanciare quello script per tutte e 4 le varianti**, non solo per l'ultima.

Risposta breve: **sì, in larga parte**. Per la tabella principale (coefficiente, SE, p
asintotico, p bootstrap, p permutazione, N) i CSV bastano — `tripledd_full_reghdfe*`,
`tripledd_collapsed*`, `tripledd_robustness_reghdfe*`, `wcb_fullpanel*`,
`r711_shapiro_intensity*`, `ppml_extensive*`, `r710_permutation_summary*` hanno tutti
coef/se/pval/N. Le lacune sono quattro, precise:

1. **`wcb_collapsed.csv` non esporta `nobs` né `nclust`** (schema: treat, term, coef, p_wcb,
   conf_low, conf_high, B). La nota della tabella principale cita "236 clusters" per il
   collassato: quel numero oggi vive solo nel log. Fix: due colonne in più nell'export di
   `20_wcb_collapsed.R`. Attenzione alla coerenza: se si aggiunge ora, Run 3/4 lo avranno e
   Run 1/2 no → va rilanciato anche il 20 per le prime due (è veloce, non è 17b).
2. **`dirty_leaveoneout*.csv` ha solo `coef` e `pval`**, niente SE né N. Sufficiente per
   l'affermazione che il paper fa davvero (stabilità del coefficiente), insufficiente per una
   tabella completa dei 25 leave-one-out in appendice.
3. **La specificazione delle FE non è registrata in nessun CSV** (`pd+dt+pt` vs `fpd+fdt+pt`):
   è implicita nello script. In tabella si scrive a mano, ma il CSV da solo non è
   auto-descrittivo a distanza di mesi.
4. **Test F congiunto (`p=0.31` WB, `p=0.71` TREND)**: già segnalato dall'audit, **non ha
   alcuno script che lo generi** — esiste solo nel `.tex`. Non è una lacuna di export ma di
   riproducibilità: va aggiunto `test wb_green wb_dirty td_green td_dirty` dopo la `reghdfe`
   in 17, con export.

**Contesto più ampio (deciso: rimandato a run completate).** Il paper ha **32 tabelle e zero
`\input{}`**: tutti i numeri sono battuti a mano nel `.tex`, i CSV non li legge nessuno.
Verificato che i numeri attuali sono corretti (tabella principale = Run 1: -0.0022/-0.0044,
21.519.511 obs, WCB collassato 0.65/0.07 — tutti coincidono). Con 4 run × ~10 output ciascuna,
la trascrizione manuale diventa però il punto di rischio principale, e nessun controllo
automatico intercetterebbe un refuso. Precedente utile nel progetto: `make_table()` in
`Code/Analysis/pta_functions.R` (vecchia pipeline) generava frammenti LaTeX dai risultati.
Limite onesto: i numeri compaiono anche nella **prosa** ($p=0.07$, $\approx -0.012$), e quelli
nessun generatore li sistema.

**Lacuna paper-facing collegata**: il WCB **full-panel** prodotto da 17b (Run 1: p=0.686 green,
0.185 dirty) **non ha ancora un posto nel paper** — nella tabella principale la riga "wild
cluster bootstrap p" esiste solo per il collassato, la riga "Full panel" si ferma al p
asintotico. È un risultato nuovo da collocare.

---

## 11. MATRICE 2×2 COMPLETA — esito e cosa resta (2026-08-11, sera)

Tutte e quattro le run sono chiuse su R e Stata. Nessun buco: 13 script R + 3 Stata per
variante, sotto-indici 7/7 ovunque, event study in tutte e 4, leave-one-out completo.
`_sample_config.R` va lasciato sulla variante che si intende rieseguire (asse 1 SAMPLE,
asse 2 DEPTH); le tre `.do` hanno le globals gemelle `PTA_SAMPLE` / `PTA_DEPTH`.

### 11.1 Esito — `WB × dirty` (l'unico coefficiente che si muove)

| unità | excl+TD | incl+TD | excl+DESTA | incl+DESTA |
|---|---|---|---|---|
| collassato | −0.0119 (0.0001) | −0.0189 (0.0022) | −0.0113 (WCB 0.047, perm 0.036) | **−0.0082 (WCB 0.198, perm 0.489)** |
| full panel | ✓ | ✓ | −0.0056 (WCB 0.049) | −0.0057 (WCB **0.035**) |

**Sui green: nulla, in tutte e 4 le run, con ogni metodo di inferenza.** Risultato più solido.

### 11.2 L'unica cella fragile, e cosa farne

`incl+DESTA` sul **collassato** è l'unica combinazione che non regge: il coefficiente si dimezza
e svanisce man mano che l'inferenza diventa conservativa (asy 0.055 → WCB 0.198 → perm 0.489),
e il leave-one-out perde significatività togliendo 4 paesi su 25 (127, 133, 412, 601) contro
1 su 23 in Run 3.

**Ma la stessa specifica sul full panel tiene** (−0.0057, WCB 0.035), e coincide con quella di
Run 3 (−0.0056). Quindi la fragilità è **specifica dell'unità collassata**, non generale.

**Ipotesi coerente coi dati, DA VERIFICARE e non da assumere**: è un effetto di **ponderazione**.
Il collassato pesa le celle per numero di transazioni; HK e Macao sono entrepot ad altissimo
volume e lì pesano moltissimo, mentre sul full panel ogni osservazione conta 1.
**Test decisivo (non ancora fatto)**: ristimare il collassato **senza pesi** (o con pesi
alternativi) su incl+DESTA. Se il coefficiente torna in linea con il full panel, l'ipotesi è
confermata; altrimenti va cercata un'altra spiegazione prima di scriverla nel paper.

### 11.3 Cosa resta (scrittura, non calcolo)

1. Generare le tabelle LaTeX dai CSV — oggi ci sono ~32 tabelle e zero `\input{}` nel .tex.
2. Collocare nel paper il WCB **full panel** di 17b (vedi §10, lacuna paper-facing).
3. Scrivere §11.2 come robustezza, **dopo** aver fatto il test sui pesi.
4. Le 4 lacune degli export di §10 (nobs/nclust nel 20, SE/N nel leave-one-out, spec FE, test F).
5. Punti paper-facing già noti: `33_mde_equivalence.R` (C3), framing Sun-Abraham, riferimento
   Callaway–Goodman-Bacon–Sant'Anna mancante, citazione Abman non verbatim.
6. ~~`.gitignore`: il WCB full-panel della run BASELINE non è versionato~~ ✅ **FATTO (11/08,
   Windows)**. Il file `New/Output/OLS/Bootstrap/wcb_fullpanel.csv` **esiste** su Windows
   (10/08 01:12, coerente con la run 17b di Run 4) — non era mai andato perso, solo escluso
   dalla riga 10 del `.gitignore` (regola di giugno, quando lì stavano i `.rds` della vecchia
   pipeline). Regola ristretta a `New/Output/OLS/Bootstrap/*.rds`; sia `wcb_fullpanel.csv` che
   `bootstrap_summary.csv` sono ora visibili a git (non ancora committati — restano nel working
   tree per review, come da vincolo di sessione).

### 11.4 Debito tecnico — la causa dei crash resta ignota

Sette bug in due giorni, **tutti con la stessa firma: exit code 0 su lavoro incompleto o mancato.**
Le mitigazioni funzionano (cache per unità di lavoro, `stop()` sull'incompletezza, sorveglianza
sulla CRESCITA DI UN FILE e mai sull'uscita del processo, `$p.Handle` per leggere l'ExitCode),
ma sono rimedi, non diagnosi. `*** recursive gc invocation` si presenta con 432 MB occupati su
61 GB, quindi **non è memoria**. Da fare, in ordine: (a) temperature a freddo — il PC arriva a
~90° anche con 2 thread su 24 core; (b) memtest; (c) test controllato 1/2/4/8 thread sugli
stessi batch del 22, che direbbe anche quanto si guadagna davvero parallelizzando.
