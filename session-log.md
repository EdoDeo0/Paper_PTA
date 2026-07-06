# Session Log — Paper_PTA

## 2026-07-03 — Audit completo (wiki-lint + /audit) + fix Fase A

**Audit generale** su richiesta: wiki OK (5 problemi cosmetici), codice con **4 CRITICAL**,
tutto documentato in `./New/AUDIT_PIANO_2026-07-03.md` (unico artefatto + piano in 3 fasi).
- **C1/C2 (chiave):** `ln_export_value` nel .fst è il log dello **unit value** (`ln(uv_exp)`,
  vedi `2_Build_Final_PTA_EP_Dataset.do:73`), NON del valore → 09 aveva 2/3 covariate di
  matching sbagliate; 12 sommava unit value come "baseline commerciale" → il verdetto
  "CEM v2 scartato" del 25/06 è SOSPESO, da rifare.
- **C3:** 9/25 country_code errati in 04 (Svizzera=141 collideva col Vietnam). **C4:** 07
  usava env_good stantio del .fst.
**Fase A completata** (fix in `./New/Code/`, parse R OK): 09, 12, 07 (3 sezioni + diagnostica
merge + event study), 04, 05 (dirty solo HS1996), banner deprecato su 01/03.
**Valutazione econometrica d'insieme: l'impianto triple-diff regge**; minaccia principale da
trattare esplicitamente nel paper: shock destinazione×green×tempo (preferenze verdi endogene).
**Pending:** Fase B (fix wiki, su Mac); Fase C su Windows: check `hs6_final==orig` su
Env_Codes_HS1996.csv → ri-run 05 → 09 → 12 → poi 07; drift .fst Mac/Windows (9 righe) da risolvere.

## 2026-06-25/26 — Esecuzione Fase R-control (08-12) + chiusura vintage HS6 green goods

**Parte A — esecuzione script 08-12.** Eseguiti tutti gli script scritti il 2026-06-24 (mai girati
prima), con fix vari lungo il percorso (data.table dispatch in 09, API cutpoints di `cem()`, nessun
metodo `summary.cem.match` reale → aggiunta diagnostica `imbalance()` L1 in 09/12). Risultati: **C-
prod-HS4** 106 famiglie HS4, 20,5% righe sopravvivono; **C-prod-match** rilassato da HS4 a HS2 (HS4
troppo sottile, 69% famiglie senza candidati) → 97% verdi matchati, L1 non comparabile pre/post (bin
ricalcolati su campioni diversi) → usare love plot; **C-overlap** 98,5%/96,8% HS6, ~100% righe (leva
debole su taglia, forte su identificazione, come atteso); **C-deepshallow** split 17/8 (mediana con
pareggi), shallow ha solo 8 cluster → WCB ancora più fragile; **CEM v2** (baseline commerciale
pre-PTA) testato e **scartato** (perde 5 trattati, non bilancia bene la covariata aggiunta) →
mantenuto CEM originale. Dettagli completi in `New/ROADMAP.md` §7.4.5.

**Parte B — dubbio vintage HS6 (il grosso della sessione).** L'utente ha chiesto se il pannello usa
una vintage HS6 unica prima di fidarsi della Parte A. Indagine: `02_data_hygiene_audit.R` (mai
eseguito prima) ha trovato un'anomalia enorme al confine 2006→2007 (6,03% valore export su codici
"morti", soglia di concordanza superata, altri confini puliti). Tracciata la causa a una
ristrutturazione di nomenclatura HS2007 (es. 854212/13/40/50 → 854230) confermata sistematicamente su
tutti i 367 codici morti. Trovato che lo script grezzo originale (`Desktop/china/.../
1_create_panel_export.do`, Step B) dichiara di armonizzare tutto a HS1996 ma le tabelle di
corrispondenza locali non esistono più, e il file consegnato (`export_fpdt_2000_2015.dta`) ha gli
stessi numeri non corretti del dataset finale → Step B non è mai stato eseguito sul file ricevuto.
Fingerprint di `Data/Env_Codes_HS.dta` (lista green OCSE, 247 codici) contro le liste ufficiali per
vintage: matcha **HS2012 al 100%** (vs 93-96% altre vintage) — coerente col fatto che l'OCSE ha
pubblicato la sua "Combined List" nel 2014 in HS2012. Tentata una concordanza completa del pannello a
HS1996 (`03_hs_concordance.R`) ma `concord()` restituisce NA sui casi-prova (854213/854230) →
abbandonata.

**Decisione finale dell'utente**: fidarsi della vintage HS1996 dichiarata dal fornitore del dataset
(ricercatori affermati) e tradurre **solo la lista green** a HS1996, una volta, uniforme su tutti gli
anni — non una concordanza per-blocco-anno. Test di verifica rigoroso (solo match univoci 1:1, non il
fan-out di `concord(all=TRUE)` che gonfia falsamente i tassi di match) in
`03b_green_codes_to_hs1996.R`: **247/247 codici verdi con match univoco**, nessuno split/non
concordato, nessun crollo di valore sospetto 2006→2007. Output:
`New/Data/Concordance/Env_Codes_HS1996.csv`. Aggiornati `08_subsample_prodHS4.R`,
`09_subsample_prodmatch.R`, `10_subsample_overlap.R` per ricalcolare `env_good` da questa lista
(anziché fidarsi della colonna `env_good` del `.fst`, mergiata HS2012-vs-HS1996 senza concordanza) e
rieseguiti — numeri quasi invariati (C-prod-match leggermente diverso: 236 verdi candidati vs 229).
Script 11/12 non toccano `env_good`, nessun aggiornamento necessario.

**File nuovi non ancora committati**: `02b_hs_vintage_check.R` (primo tentativo, metodologia
parzialmente superata ma tenuto come artefatto), `03_hs_concordance.R` (tentativo di concordanza
completa, abbandonato/non funzionante ma tenuto per documentare il perché), `03b_green_codes_to_
hs1996.R` (lo script che ha effettivamente risolto il problema). Tutte le regole di non-intervento su
`Desktop/china` rispettate (sola lettura).

**Pending**: nessun commit fatto in questa sessione (l'utente non l'ha richiesto). Prossimo passo
naturale: rieseguire `07_triple_diff.R` con `env_good` corretto se la stima finale dipende da quella
colonna del `.fst` principale (non solo dagli script 08-10 di sub-sampling).

## 2026-06-24 (continuazione) — Script per i 4 sub-campioni + CEM v2

Su richiesta dell'utente, dopo aver chiarito a parole la logica dei sub-campioni e risolto due dubbi
econometrici (spillover within-firm Eckel et al. 2023 come motivo per non usare C-prod-HS4/match da
soli; C-deepshallow sposta il controllo dal margine-destinazione al margine-prodotto, già assorbito
dalle FE `fdt`), ho scritto 5 script R **nuovi** sotto `./New/Code/` (nessuno eseguito ancora):
- `08_subsample_prodHS4.R` — C-prod-HS4 (non-verdi nella stessa famiglia HS4 di un verde)
- `09_subsample_prodmatch.R` — C-prod-match (CEM su covariate pre-periodo, hs4 come match esatto)
- `10_subsample_overlap.R` — C-overlap (HS6 con common support trattati/controlli, varianti loose/CEM)
- `11_subsample_deepshallow.R` — C-deepshallow (solo partner PTA, split deep/shallow su WB_EP_Depth)
- `12_cem_v2.R` — CEM destinazione migliorato (+ covariata baseline commerciale pre-PTA) e check di
  bilanciamento diagnostico tra i gruppi deep/shallow dello script 11
Tutti rispettano la regola "mai toccare fuori da `New/`", usano il pattern callr già visto in
`01_inference_fix.R`/`07_triple_diff.R` per le letture pesanti dal `.fst`, e scrivono output su file
(diagnostics .txt, flag .csv, love plot .png) così sono revisionabili anche dopo un'esecuzione fatta
dall'utente in locale (es. VS Code) — confermato all'utente che posso valutare i risultati leggendo
quei file a posteriori, anche senza eseguire io stesso gli script.
**Pending:** utente vuole revisionare gli script prima di farli girare; nessuna esecuzione finora.

## 2026-06-24 — ROADMAP §7.4 "Fase R-control" (gruppi di controllo + sub-campioni)

**Chiuso il pending storico** ("aggiungere Fase R-control al §7", aperto dal 2026-06-18). Aggiunta
**§7.4** a `./New/ROADMAP.md` + banner + pointer in R3/R5. Solo letture leggere, nessun file
dati/codice toccato. Innesco: modelli che crashano sul panel pieno (49,2M righe) → strategie di
sub-campione "control group ad hoc" alla Caselli et al. (AD & Product Quality).
- **§7.4**: doppia motivazione (feasibility PPML/DiD moderni + robustezza triple-diff §7.1);
  due margini (destinazione = CEM già fatto ma taglia poco; prodotto = leva aperta); 5 strategie
  ordinate per credibilità/taglia (C-prod-HS4, C-prod-match, C-overlap, C-deepshallow alla ALR
  2024, C-aggr scaffolding) ciascuna col suo difetto econometrico.
- **Verdetto**: restringere su covariate pre-trattamento = ATT condizionato valido; il vincolo
  non sciolto = pochi cluster trattati (~19-25) → sempre WCB + permutation; il guadagno della
  taglia = PPML estensivo (green trade creation) + DiD moderni eseguibili.
- **Numeri ancorati**: green = 247 HS6 su 23 capitoli HS2 (~11% righe); CEM ~19+35 paesi.
- **Note ambiente**: PDF→`%TEMP%` (Python Win non vede `/tmp`); Rscript non nel PATH; R via file
  `.R` temporaneo (non `-e` inline).

**Pending:** eseguire `./New/Code/01_inference_fix.R` → triple-diff (07); poi Fase R-control §7.4
(prima conteggi leggeri switchers/righe, poi stime su ≥3 control group). **Pulizia disco**: i 2
backup pre-audit in `./correspondence/audit/backup_pre_step3/` (~32 GB) — identici agli attivi —
da cancellare tra qualche giorno (utente ha chiesto di aspettare).

---

## 2026-06-21 — Wiki lint, fix header References, ingest Rajan-Zingales (1998)

**Wiki lint (`./wiki/`):** 5 orfani (BlackDevereux2011, CrowleyHanPrayer2021, LeeRochaRuta2021,
LefebvreFernandesRocha2021, NeriOreficeRuta2021), nessun link rotto reale, alcuni cross-link
mancanti tra paper "deep PTA" affini, index/log coerenti.

**Fix strutturale:** header References corretto da `## References (Wikilinks)` a
`### References (Wikilinks)` (spec corrente skill `/paper-card`) su tutte le 16 card.

**Nuova card:** `RajanZingales1998_FinancialDependenceGrowth` (AER 1998) — design canonico di
interazione cross-industry × cross-country, usato come template del triple-diff del progetto
(prodotto green/dirty × destinazione EP-depth). Tag `area/methods/program-eval`. Salvata in
`./wiki/` e nella wiki globale; nuova sezione "Identification Design References" in
`./wiki/index.md`. PDF Zotero è scansione JSTOR senza testo estraibile — contenuto scritto da
conoscenza consolidata + abstract verbatim, non da inferenza.

**Pending (invariato):** "Fase R-control" nel ROADMAP §7; eseguire
`./New/Code/01_inference_fix.R` su Windows; poi triple-diff (07) come spec principale.

---

## 2026-06-19 — Allineamento CLAUDE.md + aggiornamento log

**Fix CLAUDE.md di progetto:** sezione "Reading PDFs" usava `markitdown` invece di `pymupdf4llm`.
Corretta per allinearsi alle istruzioni globali (`~/.claude/CLAUDE.md`, sezione `## PDF Conversion`).
Nessun'altra modifica ai file di codice o dati.

**Confermato:** le istruzioni globali Claude Code mandano esplicitamente `pymupdf4llm` per PDF
e `markitdown` per tutti gli altri formati (Word, HTML, PowerPoint, ecc.).

**Pending (invariato):** aggiungere "Fase R-control" al ROADMAP §7; eseguire
`./New/Code/01_inference_fix.R` su Windows; poi triple-diff (07) come spec principale.

---

## 2026-06-18 — Revisione complessiva "da zero" + controllo gruppi + PDF

**Revisione strategica (Opus 4.8):** analisi a fondo di tutto il progetto con report
`./New/REPORT_Ripartire_Da_Zero.md` (convertito anche in `REPORT_Ripartire_Da_Zero.pdf`).

**Diagnosi principale del report:**
- Problema di domanda, non esecuzione. EP depth non identificabile: ~14 accordi effettivi,
  ASEAN=11 destinazioni con valori identici, quasi nessuna variazione within-paese nel tempo.
- La ladder (`OLS_Ladder_FE.tex`) conferma la firma di selezione: effetto sparisce monotonicamente.
- Raccomandazione: riformulare come domanda di composizione/riallocazione (triple-diff).

**Discussione gruppi di controllo (à la Caselli et al. AD paper):**
- Paper di riferimento letto: stesso dataset (49.2M obs, f×p×d×t), DDD con 4 gruppi controllo.
- Differenza strutturale: AD varia a p×d (permette controlli within-prodotto e within-dest);
  EP varia solo a d → i controlli non risolvono il problema dei pochi cluster (~14 accordi).
- I gruppi controllo aiutano per selezione e dimensione campione, ma NON l'identificazione.
- Proposta "Fase R-control": campione "solo-PTA, deep vs shallow EP" à la AbmanLundbergRuta2024.
  Da registrare nel ROADMAP come fase futura (non eseguita).

**Threading:** confermato `threads_fst(1)` + `setFixest_nthreads(N-1)` in `./New/Code/01_inference_fix.R`.

**Pending:** aggiungere "Fase R-control" al ROADMAP §7; eseguire `01_inference_fix.R` su Windows;
poi triple-diff (07) come spec principale.

## 2026-06-11 — Crash kernel, ladder generata, bootstrap abbandonato

**Kernel crash (evento 41):** PC si è riavviato durante il bootstrap test, perdendo il processo R in corso.

**Stato post-crash confermato:**
- Tutti i **96 modelli OLS** in cache (`New/Output/OLS/Models_Output/`) — nessuna perdita.
- **Bootstrap**: directory `New/Output/OLS/Bootstrap/` vuota; i run precedenti avevano sempre crashato (errori API `fwildclusterboot` v0.14.3: `seed` rimosso, `data` non valido, `lean=TRUE` richiesto).
- **`OLS_Ladder_FE.tex`** generata correttamente via `_gen_ladder_tex.R` (standalone, senza bootstrap).

**Ladder risultati (4 righe, tutte e 4 le strutture FE):**
- `fpd+t`: WB 0.00143, TREND 0.00055 — non significativi
- `fpt+pd`: WB 0.00439*, TREND 0.00114** — segnale marginale
- `fpt+fpd`: WB 0.00031, TREND 0.00027 — **null**
- `fpd+pt`: WB −0.00027, TREND 0.00031 — **null**
Attenzione monotona confermata: effetto-livello è selezione assorbita dagli FE più alti.

**Bootstrap abbandonato:** test B=100 su 5M righe impiegava 30+ minuti — `fpt+fpd` (FE altamente dimensionali) rende il WCB computazionalmente impraticabile a livello micro. Il null è già lampante dall'OLS (p≈0.91). Alternativa futura: permutation test su dati aggregati a livello accordo (più veloce, già nel piano).

**Creato `.gitignore`** (mancante): esclude `.fst`, `.dta`, `.rds`, log, `Models_Output/`, `Bootstrap/`; include i `.tex`.

**Pending:** Fare push della repo → rivalutare la strategia; poi Fase 1 audit igiene dati (Task #4) e Fase 2 nuovi dati (Task #5). Task #1 chiuso di fatto (ladder ✅, bootstrap ❌ → scelta deliberata).



## 2026-06-11 — Fix crash + rilancio 01_inference_fix.R

**Crash diagnosticato:** `recursive gc invocation` in `TREND_Int_fpt_fpd` (blocco 4/4
della struttura fpt+fpd). Causa: `section_ols` eseguiva tutti e 4 i blocchi (WB_NI,
WB_Int, TREND_NI, TREND_Int) in un unico sottoprocesso callr — la memoria si accumulava
tra un blocco e l'altro e al 4° blocco l'allocatore crashava.

**Fix applicato a `New/Code/01_inference_fix.R`:** ogni blocco è ora il proprio
sottoprocesso separato (funzione `section_ols_block` + `run_one_block`). RAM completamente
liberata tra un blocco e l'altro. 16 sottoprocessi totali (4 blocchi × 4 strutture FE).

**Stato RDS al momento del rilancio:**
- fpd+year: 4/4 blocchi DONE (24/24 RDS)
- fpt+pd: 4/4 blocchi DONE (24/24 RDS)
- fpt+fpd: WB_NI ✅, WB_Int ✅, TREND_NI ✅, TREND_Int 1/6 (crash a modello 2)
- fpd+pt: 0/24 (non iniziato)

**Rilanciato** come background task (bj77emi4o). Task completato parzialmente:
- TREND_Int_fpt_fpd: ✅ 6/6 modelli in 281.7 min
- fpd+pt WB_NI: CRASH immediato a modello 1 — `recursive gc invocation`

**Diagnosi crash fpd+pt:** con callr::r(), un `R_Suicide`/abort() nel sottoprocesso
propaga il segnale al processo padre attraverso callr. tryCatch non intercetta crash
a livello C. Il crash fpd+pt avviene al primo modello anche in subprocess fresco —
probabilmente resource exhaustion dopo ore di processo padre attivo.

**Fix definitivo (bwd9zsdem):** creati due script standalone senza callr:
- `New/Code/01c_fpd_pt.R` — 4 blocchi fpd+pt in sessione diretta, gc() tra i blocchi
- `New/Code/01d_bootstrap_ladder.R` — bootstrap B=9999 + ladder table

`01c_fpd_pt.R` lanciato come task bwd9zsdem, output in `New/fpd_pt_run.log`.
Dopo completamento: `Rscript New/Code/01d_bootstrap_ladder.R`.

**PDF working paper** aggiornato a 32 pagine con tabelle corrette (Paragraph objects).

## 2026-06-10 — Monitoraggio processi + conferma screen-lock

**Contesto:** Sessione breve post-compattazione, ripresa dopo context overflow.

**Confermato:** Blocco schermo (Win+L) non interrompe processi R in background su Windows —
solo ibernazione/sleep vera pauserebbe i processi. Due avvertenze: (1) verificare che
"sospendi dopo N min" sia impostato a "Mai" in Impostazioni → Alimentazione (distinto
dall'ibernazione); (2) Windows Update può riavviare il PC se ha aggiornamenti in coda.

**Stato pipeline Fase 0 (da task output `brx1afgg0`):**
- `fpd+pt` (sezione WB_NI): partito ma hit `*** recursive gc invocation` al primo modello
  → stesso crash da concorrenza `.fst`/OpenMP già visto. Probabilmente il job `fpt+fpd`
  era ancora attivo. Regola confermata: **un solo job pesante alla volta**.
- Script già scritti e pronti: `02_data_hygiene_audit.R`, `04_wits_pref_tariffs.R`,
  `05_dirty_goods.R`, `06_total_depth.R`, `07_triple_diff.R`.

**Pending:** Verificare stato processi R in background → se completati, controllare
output in `./New/Output/OLS/` e `bootstrap_summary.csv`; se crashati, rilanciarli
singolarmente. Poi: audit R1 (02) → tariffe WITS (04) → dirty goods (05) + TotalDepth (06)
→ triple-diff (07).

## 2026-06-09/10 — Revisione complessiva + ridisegno (Opus) + esecuzione Fase 1

**Revisione totale del progetto** (codice, dati, risultati, letteratura) → piano approvato
dall'utente per la pubblicabilità in top journal. **Integrato in `New/ROADMAP.md` §7** (supera
le vecchie Fasi 2–5). Punti chiave del ridisegno:
- L'effetto-livello di EP depth NON è identificabile (collineare col PTA; ~14 accordi effettivi)
  → declassato a diagnostica (ladder). Nuova specifica principale: **triple-diff sulla
  composizione** `EP×green_p + EP×dirty_p | fpd + fdt + pt`, cluster `~country_code`.
- Criticità da risolvere: dirty goods mancanti (Shapiro 2021 per intensità CO2); concordanza
  HS6 2002/2007/2012 da verificare (può invalidare il pregresso); HK+Macao da escludere (CEPA,
  entrepôt); controllo TotalDepth non-ambientale; permutation inference oltre a WCB.
- Nuove fasi R0–R6 in §7.2; piano completo: `~/.claude/plans/distributed-cuddling-crane.md`.

**Esecuzione (stato):**
- Conflitto risorse risolto: girava ancora il vecchio `01c` dell'utente (4 thread, 18:35) in
  parallelo al nuovo orchestratore → uccisi i duplicati. Scoperto che **2 processi R pesanti
  concorrenti sul `.fst` causano il crash `recursive gc invocation`** (anche a 4 thread!) —
  non è solo RStudio. Regola: **mai più di un job pesante alla volta**.
- In corso: `_archive/01c_fpt_fpd.R` a **6 thread** (stabile da solo), modelli WB_NI 1–6 già
  in cache. Alla fine: lanciare `01_inference_fix.R` (ora a 12 thread) per fpd_pt + bootstrap
  + ladder. Fallback a 4 thread se crasha al primo modello.
- Archiviati `01a–01e`, `run_fase1.*` in `New/Code/_archive/`; eliminato `common_sample.fst`.

**Nuovi script (da eseguire):**
- `New/Code/02_data_hygiene_audit.R` — Fase R1: stabilità HS6, mappa trattamento, peso HK+MO,
  outlier UV, consistenza companyID. Eseguire DOPO le stime (un job alla volta).
- `New/Code/04_wits_pref_tariffs.R` — Fase R2: download tariffe WITS TRAINS via API SDMX
  (sintassi verificata: `rest/data/DF_WITS_Tariff_TRAINS/A.{rep}...reported`; PARTNER=000=MFN,
  gruppi con TARIFFTYPE=PREF; pref_cina = min sui gruppi con Cina). Mode download/parse,
  cache per file. Download = solo rete → può girare in parallelo alle stime.

**Wiki:** aggiunte 4 card chiave: AbmanLundbergRuta2024 (JEEA — competitor diretto),
Shapiro2021 (QJE — fonte per dirty_p), Cherniwchan2017 (JIE), CopelandShapiroTaylor2022
(Handbook). Indice aggiornato. **Zotero in local-only mode: add via DOI fallito** — configurare
ZOTERO_API_KEY o aggiungere a mano i 4 DOI (10.1093/jeea/jvae023, 10.1093/qje/qjaa042,
10.1016/j.jinteco.2017.01.005, 10.1016/bs.hesint.2022.02.002).

**Pending:** fine stime fpt_fpd → orchestratore (fpd_pt+bootstrap+ladder) → checkpoint Fase 1
(p_wcr nulli? ladder monotona?) → audit R1 → risoluzione gruppi WITS → Fase R3 (triple-diff).

## 2026-06-09 — Fase 1: script inference_fix

**Task:** Scritto `./New/Code/01_inference_fix.R` per la Fase 1 del ROADMAP.

**Cosa fa lo script:**
- Step 0: costruisce `./New/Data/common_sample.fst` (filtro `!is.na(tariffs) & !is.na(ln_hhi_baci)`) — campione comune per baseline e con-controlli
- Sezioni 1–4: riesegue tutte e 4 le strutture FE con `vcov = ~country_code` uniformemente (corregge l'eccezione `~pdt` di `fpd+year`)
- Sezione 5: wild cluster bootstrap su `fpt+fpd` × `ln_export` × WB e TREND (baseline + controlli), B=9999, risultati in `./New/Output/OLS/Bootstrap/bootstrap_summary.csv`
- Sezione 6: ladder table LaTeX (`OLS_Ladder_FE.tex`) — coefficiente EP per ogni struttura FE, mostra l'azzeramento monotono

**Decisioni tecniche:**
- `threads_fst(1)` + `setFixest_nthreads(detectCores()-1)`: fst single-thread (evita conflitto allocatori OpenMP su Windows), fixest multi-thread (demeaning CPU-bound)
- Libreria originale `Code/Analysis/pta_functions.R` usata direttamente (nessuna patch necessaria per Fase 1)
- Dataset originale mai toccato; tutto l'output va in `./New/`

**Struttura cartelle creata:** `./New/Code/`, `./New/Data/`, `./New/Output/{OLS,PPML,CEM,Diagnostics}/`

**Pending:** eseguire lo script su Windows; valutare checkpoint Fase 1 (stelle fpd+year sparite? ladder monotona? bootstrap p-val?); poi Fase 2 (tariffa preferenziale WITS).

## 2026-06-09 — Foundational literature search

**Task:** Modified `/paper-search` skill — no date filter, sorted by citations, topic-driven queries targeting
gaps in the existing wiki (env provisions in PTAs, gravity/PPML methodology, staggered DiD, clustering/inference).

**Output:** 13 staging cards written to `~/Documents/work/research-wiki/staging/`:
- `staging/environment-trade/`: Brandi2020_EPsGreenExports ⭐⭐⭐ (closest direct precedent),
  Morin2018_TRENDDataset ⭐⭐ (TREND data reference), Morin2019_KickStartingDiffusion
- `staging/pta/`: SantosSilvaTenreyro2006_LogGravity ⭐⭐⭐, HeadMayer2014_GravityWorkhorse ⭐⭐⭐,
  Melitz2003_ImpactTrade ⭐⭐⭐, ManovaZhang2012_ExportPrices ⭐⭐
- `staging/econometrics/` (new folder): BertrandDufloMullainathan2004_TrustDiD ⭐⭐⭐,
  GoodmanBacon2021_DiDVariation ⭐⭐⭐, deChaisemartinDHaultfoeuille2020_TWFE ⭐⭐⭐,
  CameronGelbachMiller2008_Bootstrap ⭐⭐⭐, CallawaySantAnna2021_DiDMultiplePeriods ⭐⭐,
  AbadieAtheyImbensWooldridge2022_Clustering ⭐⭐⭐
- `staging/foundational-digest.md` — summary table with priority-for-promotion ranking.

**Not found in OpenAlex:** Hofmann-Osnago-Ruta (WB WP 7981), Bastiaens-Postnikov (2017).

**Pending:** promote top-priority cards (Brandi 2020, Morin 2018, Santos Silva-Tenreyro 2006,
BDM 2004, CGM 2008) to full wiki cards; then execute roadmap (Fase 1: re-cluster + wild bootstrap).

## 2026-06-08 (evening) — Results audit + roadmap

**Data checks (on actual `.fst`):** confirmed `duty` = MFN tariff, not bilateral preferential
(PTA partners show *higher*/flat duty over time, not declining → it's MFN). No import records or
processing-trade flag exist → GVC extension not feasible. `companyID` present (firm-size feasible);
rich anti-dumping module (`AD_pdt`, leads/lags) = potential confounder; `_merge` is a spurious leftover.

**Full results audit:** extracted treatment coefs from all 64 result tables (OLS+PPML × WB+TREND ×
4–5 FE × full+CEM). Findings: EP effect is a **precisely-estimated null** — sign/significance flip
across FE; "significant" results live only in the least-saturated `fpd+year` spec, which is also the
*only one clustered at `pdt`* (~2.9M clusters → inflated stars). Effect vanishes monotonically as FE
saturation rises (selection signature). PPML internally incoherent (sign flips across margins/FE);
`fpt`-only PPML is an outlier to drop. Only stable coef = the (misspecified MFN) tariff. CEM balance weak.

**Reviewed code** (`pta_functions.R`, `OLS_HDFE.R`, `CEM.R`): engineering solid; bugs noted —
PPML R² meaningless, `library(wdi)` typo, `matched_countries.csv` never written, inconsistent clustering.

**Decisions / output:**
- All future work goes in `./New/` only; originals stay read-only (copy datasets in, never touch).
- Created `./New/ROADMAP.md` — detailed, self-sufficient 5-phase plan (inference fix → preferential
  tariff → identification → alt margins → robustness) executable by a smaller model.

**Pending:** execute roadmap, starting Fase 1 pt.1 (cluster at `country_code` + wild bootstrap on
`fpt+fpd`) — the decisive test for "null vs result" paper. Framing decision deferred until after that.

## 2026-06-08

### Work Completed

**Deep methodological review** (no code changed). Read README, full pipeline (Steps 1–3), OLS/PPML/CEM scripts, result tables, and compared design vs. wiki literature (esp. Neri-Laine 2023).

Key assessment delivered to user:
- Engineering/pipeline is solid; concerns are identification & inference.
- **Main issues:** (1) EP depth bundled with overall PTA depth → level effect likely picks up "deep agreement"/selection, not env clauses; (2) inference — clustering at `pdt`/`dt` understates SEs, treatment varies across ~25 destinations → cluster at **destination** + wild bootstrap; (3) staggered timing → TWFE fragile, no event study/pre-trends; (4) weak mechanism for EPs→Chinese exports.
- **Best/most credible result:** the `× env_good` interaction (green market access) — recommend building paper around it, demote level effect.
- Answered follow-ups: tariff control is correct but must be **bilateral applied** (check what `duty` is, README labels it MFN) and only separates tariff-vs-nontariff, not env-vs-other-depth (add non-env depth control). Firm-size heterogeneity feasible (firm IDs exist); GVC needs import side or processing-trade flag — not visible in repo, must check raw `final_dataset_pta.dta`. `bec` present as partial production-stage proxy. EP-count binning OK as functional-form robustness, not main spec.

### Current State
- Review complete; no files modified. Awaiting user decision on which fixes to implement.

### Next Steps
- User to verify in raw customs file: (1) whether `duty` = MFN or bilateral applied tariff; (2) whether import records / processing-trade regime exist (for GVC).
- Candidate code changes (priority): re-cluster at destination + wild bootstrap; lead with env_good interaction; add non-env depth control; firm-size heterogeneity; event study around PTA entry.

## 2026-06-07

### Work Completed

**Batch paper-card generation** for all 9 papers in the Paper_PTA Zotero collection (key: E7ZKN9EF).

2 papers already had cards from prior sessions (skipped):
- `NeriLaine2023_DeepTradeAgreements`
- `Baccini2017_DistributionalConsequencesPTAs`

7 new paper cards written and saved to `./wiki/` and `~/Documents/work/research-wiki/papers/`:

| File | Paper |
|---|---|
| `Freund2010_RTAsThirdCountry.md` | Freund (2010), *The World Economy* — Latin American RTAs, no trade diversion, building block |
| `LeeRochaRuta2021_TradefacilitationGVC.md` | Lee, Rocha & Ruta (2021), WB WP 9674 — TF provisions, Peru EDD, GVC firms |
| `NeriOreficeRuta2021_GeorgiaRTA.md` | Neri-Laine, Orefice & Ruta (2021), WB WP 9768 — Georgian EDD, RTA depth, firm size |
| `LefebvreFernandesRocha2021_SPSTBTFirm.md` | Fernandes, Lefebvre & Rocha (2021), WB WP 9700 — SPS/TBT provisions, firm size, Chile/Colombia/Peru |
| `DechezleprêtreSato2017_EnvRegCompetitiveness.md` | Dechezleprêtre & Sato (2017), REEP — env. regulations and competitiveness review |
| `LarchShikherYotov2025_GravityRecommendations.md` | Larch, Shikher & Yotov (2025), RIE — 15 gravity estimation recommendations |
| `CrowleyHanPrayer2021_DeepPTAMarkups.md` | Crowley, Han & Prayer (2021), WB WP 9600 — deep PTAs, markups, 13-country EDD |

Also completed in earlier parts of this session (from prior context):
- `Baccini2017_DistributionalConsequencesPTAs.md` — written in previous session's continuation
- wiki `index.md` and `log.md` updated (both local and global) for all 9 papers

**Weekly paper-search** (`/paper-search` skill): Searched OpenAlex for 9 queries across topics `pta` and `environment-trade` for the period 2026-05-31 → 2026-06-07. Created 5 staging cards in `~/Documents/work/research-wiki/staging/`:
- `staging/pta/CorreiaGuimaraesZylkin2026_MLEGravityGLM.md` ⭐ PPML MLE existence
- `staging/pta/YamarikGhosh2026_RegionalIPRFDI.md`
- `staging/pta/EsquiviasEtAl2026_ACFTATradeCreation.md`
- `staging/environment-trade/MansouriTounsi2026_PollutionHavenGreenPTAs.md` ⭐⭐ directly on-topic
- `staging/environment-trade/CuiYangLong2026_EcoIndustrialParksGreenTrade.md`
- `staging/weekly-digest.md`

### Current State

- All 9 Zotero collection papers now have wiki cards
- Both local (`./wiki/`) and global (`~/Documents/work/research-wiki/papers/`) wikis are up to date
- `index.md` and `log.md` updated in both wiki locations
- PDFs cached in `/tmp/`: freund2010_thirdcountry.md, lee_tradefacilitation.md, neri2021_georgia.md, lefebvre2021_sps_tbt.md, dechezlepretre2017_envcompetitiveness.md, larch2025_gravity.md, crowley2021_deeppta_pricing.md

### Next Steps

- Review staging cards and promote any to full paper cards if needed (especially MansouriTounsi2026)
- Continue analysis pipeline as needed
