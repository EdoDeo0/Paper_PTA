# PIANO DI RIORDINO E RISCRITTURA — `New/` (2026-07-15)

> **Scopo.** Istruzioni autosufficienti per una sessione futura (eseguibile da un modello
> più economico, es. Sonnet/Haiku) per: (1) ripulire `New/` dal materiale superato,
> (2) riorganizzare cartelle e nomi, (3) riscrivere gli script in forma leggibile
> mantenendo **risultati identici** a quelli già prodotti.
>
> Autore del piano: sessione del 2026-07-15. Inventario file fotografato a quella data.

---

## 0. REGOLE NON NEGOZIABILI

1. **Non toccare MAI nulla fuori da `C:\Work\projects\Paper_PTA\New\`.**
   Né `Data/`, né `Code/`, né `Output/` a livello root del repo, né `.gitignore`.
2. **I commit li fa l'utente.** Non committare mai. PRIMA di iniziare la Fase 1,
   chiedere all'utente di fare un commit di checkpoint: senza checkpoint, ogni
   spostamento/cancellazione va fatto spostando in `New/_legacy/`, mai cancellando.
3. **Equivalenza dei risultati = condizione non negoziabile.** Ogni script riscritto
   deve riprodurre gli output di riferimento (vedi §4, protocollo di verifica).
   Se un valore non torna, fermarsi, non "aggiustare" la tolleranza, e riportarlo.
4. **Vincoli hardware di questo PC** (violazioni = crash dell'allocatore R o BSOD):
   - `fixest`: sempre `lean = TRUE`, mai più di una stima full-panel per processo R;
   - i job pesanti girano **uno alla volta**, detached via PowerShell
     (`Start-Process Rscript -WindowStyle Hidden -RedirectStandardOutput/-Error`);
   - **mai modificare un file `.R`/`.do` mentre un processo lo sta eseguendo**
     (R sourcia incrementalmente: l'edit corrompe il parse a metà run);
   - R sempre via file script temporaneo/salvato, mai `Rscript -e` inline con codice lungo;
   - Stata solo in batch da PowerShell (`"C:\Program Files\Stata19\StataMP-64.exe" /e do ...`).
5. **Non modificare l'algoritmo dei percorsi "strani".** Se uno script usa
   Frisch-Waugh + `fixest::demean` + `qr.solve` invece di un semplice `feols`, è
   perché `feols` non-lean crasha su 49M righe su questa macchina. La riscrittura
   migliora leggibilità (nomi, commenti, struttura), NON sostituisce l'algoritmo.
6. **Seed RNG**: gli script con bootstrap/permutazioni devono conservare **lo stesso
   seed e lo stesso ordine di estrazione** del vecchio script, altrimenti i p-value
   cambiano e la verifica di equivalenza fallisce. Prima di riscrivere, individuare
   ogni `set.seed(...)`/`dqset.seed(...)` nel vecchio script e replicarlo.

---

## 1. FASE 1 — PULIZIA (spostare in `New/_legacy/`, niente cancellazioni dirette)

Creare `New/_legacy/` con sottocartelle `code/`, `docs/`, `logs/`, `output/`.

### 1.1 Script superati → `_legacy/code/`

| File | Motivo |
|---|---|
| `Code/_archive/` (intera cartella) | già archiviata a suo tempo |
| `Code/03_hs_concordance.R` | marcato DEPRECATO nel header (concordanza abbandonata) |
| `Code/07_triple_diff.R` | full-panel R: crashava; sostituito da `16_tripledd_full.do` (Stata) |
| `Code/07b_tripledd_full_direct.R` | retry del precedente, stesso destino |
| `Code/09_subsample_prodmatch.R` | subsample C-prod-match superato da CEM v2 (non è in tab:samples del paper) |
| `Code/14b_permutation_dirty.R` | permutation su aggregato, superata da `29_r710_permutation_true.R` |
| `Code/01c_fpd_pt_diag.R` | diagnostica one-off di un crash risolto |
| `Code/02b_hs_vintage_check.R` | verifica one-off completata (output conservato in Diagnostics) |
| `Code/03c_check_A2_continuity_fix.R` | fix one-off completato (integrare la logica nel nuovo 01, vedi §3) |
| `Code/22_check_timor.R` | diagnosi one-off completata (Timor: errore documentato, output conservato) |
| `Code/_boot_api.R`, `_boot_test.R`, `_ladder_diag.R`, `_gen_ladder_tex.R` | utility one-off di debug (la logica di `_gen_ladder_tex.R` va integrata nel nuovo script ladder) |

**Tutti gli altri script attivi restano dove sono finché il loro sostituto riscritto
non ha superato la verifica di equivalenza** (§4). Solo allora si spostano in `_legacy/code/`.

### 1.2 Documenti superati → `_legacy/docs/`

| File | Motivo |
|---|---|
| `AUDIT_PIANO_2026-07-03.md` | piano di audit completato |
| `PIANO_SONNET_2026-07-08.md` | piano operativo completato |
| `REPORT_Ripartire_Da_Zero.md` + `.pdf` | report storico, superato dal ROADMAP |
| `Audit/` (intera cartella: report 2026-07-08 e comparison) | audit chiusi; il report corrente è in `correspondence/audit/` (fuori da New, NON toccare) |
| `status_report_build.py`, `working_paper_build.py` | builder one-off di PDF di status |
| `Output/Status_Report_2026-07.pdf`, `Output/WorkingPaper_PTA_Status.pdf` | deliverable one-off datati |

**Restano al loro posto:** `ROADMAP.md` (storia del progetto, sempre attivo), `Paper/`.

### 1.3 Log di esecuzione → `_legacy/logs/`

Tutti i `.log`/`.err` sparsi:
- root di `New/`: `boot_test_*.log`, `bootstrap_*.log`, `fpd_pt_*`, `inference_fix_*`, `ladder_gen_*`
- root di `Output/`: `TripleDiff_*.log` (9 file), `r710_run.{log,err}`, `r7h_wcb_ladder*.{log,err}`,
  `16_tripledd_full.log`, `17_remaining_models.log`, `21_collapsed_replication.log`
- `Data/WITS_download.log`

### 1.4 Cosa NON toccare in Fase 1

- `Data/` (tutto): sono input/derivati costosi da rigenerare (il solo
  `Data/Concordance/export_fpdt_2000_2015_HS1996.fst` è 1,3 GB e richiede ore).
  Riorganizzazione minima in Fase 2, nessuna cancellazione.
- `Output/**/Tables/*.csv|.tex`, `Output/**/Diagnostics/*`, `Output/CEM_v2/`,
  `Output/Subsamples/`: sono i **risultati di riferimento** per la verifica (§4).
- Le cache `.rds` (`Output/OLS/Models_Output/` 48 modelli, `Output/OLS/Bootstrap/`,
  `Output/TripleDiff/Models*/`, `r710_batches/`): pochi MB, evitano ri-run di ore.
- `replication/` (replica Stata di r79): parte della documentazione di audit.

---

## 2. FASE 2 — NUOVA STRUTTURA E CONVENZIONI

### 2.1 Struttura target

```
New/
├── Code/                      # SOLO gli script riscritti, numerati 01..NN
│   └── stata/                 # gli script .do (full panel via reghdfe)
├── Data/                      # invariata nei contenuti; rinomina cartelle sotto
│   ├── Classifications/       # ex Concordance (green) + Dirty (senza shapiro)
│   ├── External/shapiro2021/  # ex Dirty/shapiro2021 (dati replica Shapiro QJE)
│   ├── Collapsed/             # invariata
│   ├── Subsamples/            # invariata
│   └── TotalDepth/            # invariata
├── Output/
│   ├── Tables/                # tutte le tabelle finali (csv/tex)
│   ├── Figures/               # png (event study ecc.)
│   ├── Diagnostics/           # report md/csv diagnostici
│   └── Cache/                 # .rds intermedi (modelli, batch permutation, bootstrap)
├── Paper/                     # invariata (draft_paper.tex + figures/)
├── replication/               # invariata (repliche cross-language)
├── verification/              # NUOVA — protocollo di equivalenza (§4)
│   ├── reference/             # copia congelata degli output pre-riordino
│   └── equivalence_log.md     # esito verifica per ogni script
├── _legacy/                   # tutto il materiale superato (Fase 1 + vecchi script)
├── ROADMAP.md
└── PIANO_RIORDINO_2026-07-15.md   # questo file
```

Nota: `New/Data/` e `*.rds` sono in `.gitignore` — gli spostamenti dentro `Data/` non
sporcano git. **Aggiornare i path dentro gli script riscritti**, non serve altro.

### 2.2 Convenzioni di nome

- Script: `NN_verbo_oggetto.R` in snake_case, dove `NN` segue l'ordine logico del paper
  (dataset → descrittive → stime → robustezza). Vedi mappa §3.
- Output: nome che richiama lo script che lo produce (`12_main_collapsed.csv` prodotto
  da `12_main_tripledd_collapsed.R`), senza i vecchi prefissi criptici (`r710_`, `r79b_`).
- Ogni script riscritto dichiara nel header: cosa fa (2-3 righe), input, output,
  tempo di run indicativo, e lo script legacy che sostituisce.

### 2.3 Template di stile per gli script R (lo stile dell'autore)

```r
########################################################
###### NN — Titolo breve dello script               ####
########################################################
## Author: Edoardo Vitella
## Sostituisce: <vecchio script>. Run: ~<tempo>.
## Cosa fa: <2-3 righe in italiano semplice>
## Input:  <file>
## Output: <file>

## --- Setup ---------------------------------------------------------------
rm(list = ls())
library(fst)
library(fixest)
library(data.table)
library(here)

## --- Parametri e percorsi ------------------------------------------------
DATA_FILE <- here("Data/Final Dataset/final_dataset_pta_env_indices_compressed.fst")
OUT_DIR   <- here("New/Output/Tables")

## --- Caricamento dati ----------------------------------------------------
# carico solo le colonne che servono (il pannello completo non sta in RAM)
dt <- read_fst(DATA_FILE, columns = c(...), as.data.table = TRUE)

## --- Sezione 1: <cosa fa> ------------------------------------------------
# commento che spiega il perché, non solo il cosa
...
```

Regole di stile: `rm(list = ls())` in alto; una `library()` per riga; niente pipe
lunghissime (spezzare in passaggi nominati); commenti in italiano che spiegano il
*perché* dei passaggi non ovvi; sezioni separate da `## --- ... ---`; nessuna
funzione-helper astratta se usata una volta sola (codice lineare).

---

## 3. FASE 3 — RISCRITTURA: MAPPA VECCHIO → NUOVO

Ordine logico del paper: **A. dataset → B. descrittive → C. stime principali →
D. inferenza robusta → E. robustezza/estensioni**.

Per OGNI script: prima di riscrivere, leggere il vecchio per intero, scrivere una
mini-roadmap di 5-10 righe in `verification/equivalence_log.md` (input, output,
seed trovati, passaggi chiave, insidie), poi riscrivere, poi verificare (§4).

### A. Costruzione dataset (run: veloci, tranne dove indicato)

| Nuovo | Sostituisce | Note per la riscrittura |
|---|---|---|
| `01_green_goods_hs1996.R` | `03b_green_codes_to_hs1996.R` + fix di `03c` | Integrare il fix A2 (continuità) direttamente; output `Data/Classifications/green_codes_hs1996.csv` |
| `02_dirty_goods.R` | `05_dirty_goods.R` | Due misure (core Mani-Wheeler + estesa); output `dirty_goods_hs6.csv` |
| `03_co2_intensity.R` | `31_r711_shapiro_intensity.R` (SOLO parte costruzione crosswalk) | HS6→ISIC3(WITS)→EXIOBASE; imputazione non-matchati alla MEDIA campionaria (non zero!); output `co2_intensity_hs6.csv`. La parte di stima va in `25_` |
| `04_total_depth.R` | `06_total_depth.R` | TotalDepth non ambientale da WB_DTA |
| `05_wits_tariffs.R` | `04_wits_pref_tariffs.R` | Richiede rete/API WITS: di default NON rieseguire, riusare l'output esistente; riscrivere solo per documentazione |
| `06_collapsed_panel.R` | parte di `14_tripledd_collapsed.R` | **Estrarre** la costruzione di `panel_pdt_collapsed.fst` in uno script dedicato (oggi è mescolata alla stima) |
| `07_subsamples.R` | `08` + `10` + `11` | I tre flag (prodHS4, overlap, deepshallow) in un solo script con tre sezioni; output invariati in `Data/Subsamples/` |
| `08_cem_matching.R` | `12_cem_v2.R` | CEM di destinazione; conserva Love plot e balance summary |

### B. Statistiche descrittive

| Nuovo | Sostituisce | Note |
|---|---|---|
| `09_descriptives_treatment.R` | `02_data_hygiene_audit.R` | Mappa trattamento, quota HK/Macao, imprese per anno, stabilità HS6 |
| `10_descriptives_collinearity.R` | `24_r76_collinearity.R` | Correlazioni EP↔TotalDepth raw/within + VIF (223 country-year trattati) |
| `11_descriptives_sample.R` | `25_r78_sample_character.R` | Replica singleton-removal in data.table puro (NON usare fixest qui: scelta deliberata anti-crash); target 21.519.537 righe |

### C. Stime principali

| Nuovo | Sostituisce | Note |
|---|---|---|
| `12_main_tripledd_collapsed.R` | `14_tripledd_collapsed.R` (solo stima + event study TWFE) | Spec: `y ~ EP:green + EP:dirty + TD:green + TD:dirty \| pd+dt+pt`, cluster ~country_code. Output di riferimento: `tripledd_collapsed.csv` |
| `stata/13_main_tripledd_fullpanel.do` | `16_tripledd_full.do` | Full panel 45,8M righe via reghdfe (R crasha: NON portare in R) |
| `stata/14_robustness_fullpanel.do` | `17_remaining_models.do` | Robustezze full-panel (controls, noASEAN, inclHKMO, overlap, deepshallow, withinfirm) |
| `15_saturation_ladder.R` | `01_inference_fix.R` + `01c_fpd_pt.R` + `01d_bootstrap_ladder.R` + `_gen_ladder_tex.R` | **Il più delicato.** 48 modelli full-panel: run di ore. Riscrivere mantenendo la cache RDS per-modello (skip se esiste) e il run per-sottoprocesso. Per la verifica è ammesso riusare le cache esistenti (copiate in `Output/Cache/`) invece di ristimare |

### D. Inferenza robusta (few clusters)

| Nuovo | Sostituisce | Note |
|---|---|---|
| `16_wcb_collapsed.R` | `15_wcb_collapsed.R` | WCB (B=9999) sul collassato. Conservare seed |
| `17_wcb_ladder_fullpanel.R` | `30_r7h_wcb_ladder.R` | Frisch-Waugh + boottest su 49M righe. NON rimuovere l'intercetta dal `lm` (bug boottest su design a 1 colonna: fix documentato). Conservare seed. Run lungo |
| `18_permutation_inference.R` | `29_r710_permutation_true.R` | 1000 permutazioni, Frisch-Waugh incrementale, batch da 50 con cache; `qr.solve` per WLS (NON `lm.wfit`: bug NULL-weights). Conservare seed e ordine dei draw. Run ~1h40m; mantenere la modalità `TEST` per smoke test |
| `19_eventstudy_sunab.R` | `19_sunab_gap.R` + `23_r71_sunab_diag.R` + `14c_eventstudy_plot.R` | Sun-Abraham + diagnosi coorte t=-6 (Australia) + grafici. Output figure per il paper: NON cambiare i nomi file citati in `draft_paper.tex` (`Paper/figures/eventstudy_collapsed_v2.png`, `eventstudy_sunab.png`) |

### E. Robustezza ed estensioni

| Nuovo | Sostituisce | Note |
|---|---|---|
| `20_stability_controlgroups.R` | `13_tripledd_stability.R` | Triple-diff sui 4 subsample; output `tripledd_stability.csv` |
| `21_heterogeneity_subindices.R` | `18_subindices_collapsed.R` | Sotto-indici EP (GreenLib, StandardsNonRegression, EnforcementDSM, ...) |
| `22_robustness_desttrends.R` | `26_r79_desttrends.R` | Trend destinazione full-sample (varying slopes via callr, retry+cache) |
| `23_robustness_desttrends_wcb.R` | `27_r79b_wcb_trends.R` | WCB sui trend via FW esteso agli slope (`demean(..., slope.flag)`); mantenere il check di equivalenza FW-vs-feols con `stop()` |
| `24_robustness_desttrends_pre.R` | `28_r79c_pretrend_variant.R` | Trend stimati SOLO su anni pre-trattamento e proiettati |
| `25_robustness_co2intensity.R` | `31_r711_shapiro_intensity.R` (parte stima) | FW+WCB su EP×intensità CO2 standardizzata |
| `26_robustness_extensive_ppml.R` | `20_ppml_extensive.R` | PPML con zeri (margine estensivo) |
| `27_robustness_leaveoneout.R` | `15b_dirty_leaveoneout.R` | Leave-one-out sul coefficiente dirty (Corea) |

Gli script `.do` restano in Stata (motivo documentato: fixest crasha sul full panel).
`21_collapsed_replication.do` si sposta in `replication/` (è materiale di audit, non pipeline).

---

## 4. PROTOCOLLO DI VERIFICA DI EQUIVALENZA (obbligatorio)

1. **Prima di ogni riscrittura** (una volta sola, all'inizio): copiare in
   `verification/reference/` tutti i file di `Output/**/Tables/*.csv`,
   `Output/**/Diagnostics/*.csv`, `Output/Diagnostics/*.md`, `Output/OLS/Tables/*.tex`,
   `Output/OLS/Bootstrap/bootstrap_summary.csv`. Questi sono i valori "veri".
2. **Dopo ogni script riscritto**: eseguirlo (nei nuovi path), poi confrontare ogni
   output con il corrispettivo in `reference/`:
   - CSV numerici: uguaglianza a `1e-8` (idealmente identici byte-per-byte se seed
     e ordine sono conservati);
   - `.tex`/`.md`: diff testuale, differenze ammesse solo nei path/nomi file.
3. Registrare l'esito in `verification/equivalence_log.md`: script, data, output
   confrontati, esito (IDENTICO / DIFF entro tolleranza / **FALLITO**), tempo di run.
4. **Se un confronto fallisce**: non procedere oltre con quello script; conservare
   entrambe le versioni; annotare il primo valore divergente; segnalare all'utente.
5. Solo dopo esito positivo: spostare il vecchio script in `_legacy/code/`.
6. Script pesanti (15, 17, 18, e i .do full-panel): rieseguire **uno alla volta**,
   detached, mai in parallelo; per `15_saturation_ladder.R` è ammessa la verifica
   via cache RDS esistente (le stime non cambiano se non si ristima).

## 5. ORDINE DI ESECUZIONE CONSIGLIATO

1. Chiedere all'utente il commit di checkpoint.
2. Fase 1 (pulizia) + creazione struttura Fase 2 + congelamento `verification/reference/`.
3. Riscrittura A (dataset, 01-08): veloci, verificabili subito.
4. Riscrittura B (descrittive, 09-11): veloci.
5. Riscrittura C/D/E leggere (12, 16, 19, 20, 21, 25, 26, 27): collassato = minuti.
6. Riscrittura pesanti, una per notte/sessione: 22-24 (trend), 18 (permutation),
   17 (WCB ladder), 15 (ladder, verifica via cache), .do Stata (13, 14).
7. Aggiornare `ROADMAP.md` con una voce che documenta il riordino e la nuova mappa.
8. Report finale all'utente con `equivalence_log.md` completo.

## 6. COSA NON FARE (anti-pattern già visti in questo progetto)

- Non usare heredoc Bash (`cat << EOF`) per scrivere script R con regex/backslash:
  gli escape vengono mangiati. Usare il tool Write.
- Non lanciare `feols` non-lean sul full panel: crash allocatore.
- Non usare `lm.wfit` con pesi NULL (bug): `qr.solve(X*sqrt(w), y*sqrt(w))`.
- Non togliere l'intercetta nei `lm` passati a `boottest` su design a 1 regressore.
- Non modificare script mentre un run detached li sta ancora eseguendo.
- Non cambiare seed, numero di draw, o ordine delle permutazioni "per pulizia".
- Non rinominare i file figura citati in `draft_paper.tex`.
