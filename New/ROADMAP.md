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

## 6. PRIMO PASSO CONSIGLIATO per la sessione futura

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
