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
  aggregato `pd×t` con zeri, FE `pd + pt + dt`).
- **R4 — Margini e meccanismi**: margine estensivo (n. imprese/prodotti green per `d×t`,
  entrata nuove imprese nei green); **riallocazione within-firm** (quota green nel paniere
  delle multiprodotto verso `d`, FE `fdt`) ← potenziale risultato da top journal;
  eterogeneità per sub-indice (`GreenMarketAccess`, `EnforcementDSM`, `Hard`/`Soft`) e
  per size d'impresa.
- **R5 — Robustezza (set chiuso)**: escl. HK+MO / incl.; escl. ASEAN; leave-one-out per
  accordo; controllo `AD_pdt`; solo not-yet-treated; synthetic DiD su quota green a livello
  destinazione; Callaway-Sant'Anna/dCDH su trattamento binario; UV trimmed vs non.
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
