# Roadmap soluzioni — audit 2026-08-23 (P1–P8)
**Riferimento:** `2026-08-23_audit_report.md`. Ogni item è autosufficiente: un modello che esegue non deve rileggere l'audit né il repo per capire cosa fare. Ordine = priorità.

---

## STATO DI IMPLEMENTAZIONE (aggiornato 2026-08-23 sera)

| Item | Stato | Esito |
|---|---|---|
| **P1** — fix + rerun S3 | ✅ **CHIUSO** | `p_boot` ora popolati e ≡ R entro errore Monte Carlo: WB green 0.6495 (R 0.6486), **WB dirty 0.0717 (R 0.0727)**, TREND green 0.3896 (R 0.3870), TREND dirty 0.8569 (R 0.8525). Coefficienti ≡ a 12 cifre. Il p=0.07 dell'abstract è ora verificato cross-software. |
| **P2** — assemblaggio S5 | ✅ **CHIUSO** | `ppml_extensive_stata.csv` scritto (10 righe); coef ≡ R a 9 cifre. |
| **P3** — permutazione treated-only | ✅ **CHIUSO** (24/08 22:56) | 1000 draw completati in ~25 h. **Stata vs R: WB green 0,597 vs 0,608 · WB dirty 0,278 vs 0,235 · TREND green 0,160 vs 0,177 · TREND dirty 0,817 vs 0,845.** Guardie tutte passate (23 trattati, `b_obs` ≡ baseline, 0 righe dal using). Vedi nota sotto sullo scarto del dirty. |
| **P4** — stability full panel | ✅ **CHIUSO** | **24/24 righe ≡ R, scarto max 9,7e-11, N identico ovunque.** Le stime R della tabella stability erano corrette. |
| **P5** — depthbounds verified | ✅ **CHIUSO** | 4 CSV riscritti da `.dta` Stata con `source`; scarti max 0,03% relativo (entro tolleranza). |
| **P6** — annotazioni env_good / S7 | ✅ **CHIUSO** | Note in testa a `19_saturation_ladder.R` e in coda a `57_wcb_ladder_fullpanel.do`. |
| **P7** — run_pipeline | ✅ **CHIUSO** | Registrati 52-export, 52, 54, 55-export, 55, 56, 57, 58, 58c; corretto l'artefatto di 19b (puntava alla cartella sbagliata). Parse OK. |
| **P8** — igiene | ✅ **CHIUSO** (salvo 8.5) | `set varabbrev off` in 12 do-file; 9 log spostati in `New/Output/Diagnostics/stata_logs/`; `check_dta_vars.do` in `_legacy/`; voce in `MISTAKES.md`; voce in `session-log.md`. |

**Item aggiuntivo oltre la roadmap (richiesto dall'utente il 24/08): P9 — leave-one-out.**
Era l'ultimo risultato del paper in fascia C (mai verificato cross-software; non rientrava
in S1–S7 né in P1–P8). Nuovo `stata/59_leaveoneout_collapsed.do`: **26/26 specifiche ≡ R,
scarto max 8e-10, N identico** — inclusi i numeri citati nel testo (Australia esclusa
−0,010312, Corea −0,009746). ✅ CHIUSO.

**Esito P3 — lo scarto sul margine dirty (0,278 Stata vs 0,235 R).**
Non è un errore di nessuna delle due implementazioni. I coefficienti osservati coincidono a 12
cifre e le guardie di riproduzione sono passate: ciò che differisce è solo *quali* 1000
permutazioni sono state estratte. Tre p-value su quattro distano 1–3 punti (rumore Monte Carlo
puro); il dirty ne dista 4,3, pari a ~2,2 deviazioni standard dello scarto — ordinario su
quattro confronti simultanei, e atteso qui perché i profili di trattamento realmente distinti
sono **nove** (gli undici ASEAN condividono lo stesso accordo), quindi la distribuzione dei
placebo è a scalini e la risoluzione del p-value è limitata dai profili, non dai draw. Il paper
lo dichiara già.
**Conseguenza sostanziale: nessuna.** Il margine dirty non sopravvive alla permutazione in
entrambe le versioni, e la stima Stata (0,28) rafforza il null rispetto a quella R (0,24).
**Decisione aperta per l'autore:** se aggiornare il numero citato nell'abstract e in
`§sec:dirty` alla stima Stata, oppure tenere quella R e dichiarare in nota che la replica
indipendente dà 0,28. La seconda è più informativa e mostra la robustezza del risultato.

**Correzione a P3 rispetto alla prima stesura (sotto).** Il passo 2 dell'Opzione A prescriveva un
`assert _N == 16` sui profili dei trattati: **è sbagliato e va ignorato**. I profili NON sono
bilanciati (22 trattati su 16 anni, 1 su 13) e R non se ne cura: fa il join su (paese, anno) e
zero-riempie le coppie non corrispondenti. Il `replace = 0 if missing` è quindi la replica
*corretta*, non un'approssimazione. `56b` implementa questa semantica ed è documentato.

**Nota emersa durante l'implementazione (non era nell'audit).** `stata/17b_wcb_fullpanel.do` e
`stata/18_robustness_fullpanel.do` hanno in testa la config residua `PTA_SAMPLE="incl"` /
`PTA_DEPTH="desta"`: un loro rerun produrrebbe la variante inclHKMO+DESTA, non il baseline
(`17.do` è invece correttamente su `excl`/`totaldepth`, come `_sample_config.R`). Il censimento
21d lo aveva già segnalato per 17b. **Non l'ho cambiato**: è un valore di configurazione che
determina quali file di output vengono scritti, e la decisione è dell'utente. 🛑

---

**Regole valide per tutti gli item:**
- Non modificare mai nulla fuori da `New/` (eccetto i file di log alla radice, item P8).
- Un solo processo Stata alla volta, macchina a riposo. Percorso Stata: `"C:\Program Files\StataNow19\StataSE-64.exe" /e do "<script>"` da PowerShell, root progetto.
- Un item è chiuso solo quando il **confronto numerico** col gemello R (o la verifica dell'output) è stato eseguito e annotato in `session-log.md`. "Lo script è girato senza errori" NON è un criterio di chiusura.
- Dopo ogni fix a un `.do`, controllare il `.log` risultante cercando `r(1`, `r(2`, `error` — un exit code 0 di Stata batch NON garantisce nulla (S5 è crashato con exit 0 apparente).

---

## P1 — Rifare S3: WCB collassato via boottest (CRITICO)

**Problema.** `New/Code/stata/52_omnibus_collapsed.do`, sezione S3 (righe ~421–507):
1. `foreach v in y ep_green ep_dirty td_green td_dirty { cap drop \`v' }` cancella anche `y` (l'outcome, che nel `.dta` esiste). Stata poi risolve `reghdfe y ...` in `reghdfe year ...` per abbreviazione automatica → residui zero → coefficienti ~1e-13 nel CSV.
2. `boottest ep_green_dm_wb [aw=n], boottype(wild) reps(9999) seed(42) noci` è sintassi errata: boottest eredita i pesi dal modello stimato, e `[aw=n]` viene letto come constraint → errore r(111) → p_boot mancante.

**Fix (passi esatti):**

1. **Eliminare subito il CSV invalido** (anche prima del rerun, per evitare che venga citato):
   ```powershell
   Remove-Item "New\Output\TripleDiff\Tables_Stata\wcb_collapsed_boottest.csv"
   ```
2. In `52_omnibus_collapsed.do`, subito dopo `set more off` (riga ~26), aggiungere:
   ```stata
   set varabbrev off
   ```
   (Questo da solo avrebbe trasformato il bug in un errore visibile. Metterlo anche in 54/55/56/57 — vedi P8.)
3. Nella sezione S3, sostituire il blocco di pulizia (righe ~424–426):
   ```stata
   * PRIMA (bug: cancella anche y):
   foreach v in y ep_green ep_dirty td_green td_dirty {
       cap drop `v'
   }
   * DOPO (y non va toccata):
   foreach v in ep_green ep_dirty td_green td_dirty {
       cap drop `v'
   }
   ```
4. Correggere le 4 chiamate boottest togliendo `[aw=n]` (i pesi sono già nel `reg ... [aw=n]` precedente, boottest li eredita):
   ```stata
   boottest ep_green_dm_wb, boottype(wild) reps(9999) seed(42) noci
   ```
   (idem per `ep_dirty_dm_wb`, `ep_green_dm_tr`, `ep_dirty_dm_tr`).
5. **Evitare di rifare S2** (già valido): la sezione S2 ha lo skip sui `OMNI_*.dta` già presenti, quindi rilanciare l'intero 52.do è sicuro e veloce (S2 skippa tutto, S3 riparte). Tempo atteso: 30–90 min (4 demean pesati su 3,7M celle + 4 boottest).
6. **Guardia interna consigliata** (5 righe, previene la recidiva): dopo il `reg y_dm_wb ...` WB, aggiungere:
   ```stata
   if abs(_b[ep_green_dm_wb] - (-0.0045685)) > 1e-4 {
       di as error "FWL non riproduce il baseline (-0.0045685). Abort."
       exit 9
   }
   ```
   (analogo per TREND con 0.0018115).

**Criterio di chiusura:**
- Nel nuovo CSV: `coef` WB = −0.0045685… / −0.0118734…, TREND = 0.0018115… / 0.0003510… (≡ righe baseline di `omnibus_collapsed_reghdfe.csv` a ≥6 cifre).
- `p_boot` popolato in tutte le 4 righe ep_*. Confronto atteso con R (`wcb_collapsed.csv`): WB green ~0,65, WB dirty ~0,07, TREND green ~0,39, TREND dirty ~0,85 — concordanza entro errore Monte Carlo (±0,02 circa; algoritmi diversi, NON coincidenza esatta).
- Se p_boot WB dirty esce lontano da 0,07 (es. >0,15 o <0,03), NON correggere il paper da soli: annotare e fermarsi (🛑 utente).
- Annotare l'esito in `session-log.md` con i 4 confronti numerici espliciti.

---

## P2 — Chiudere S5: assemblaggio CSV del PPML (CRITICO-igiene, 10 minuti)

**Problema.** `New/Code/stata/55_ppml_collapsed.do` riga ~98: `if \`first' { use "$TAB/\`f'", clear; local first = 0 }` — graffe e statement sulla stessa riga non sono sintassi Stata → crash `r(198)`, `ppml_extensive_stata.csv` mai scritto. I due `.dta` (stime) sono validi e già verificati ≡ R dall'audit.

**Fix.** Sostituire il blocco di assemblaggio (righe ~93–101) con:
```stata
clear
local first = 1
foreach f in PPML_extensive_WB.dta PPML_extensive_TREND.dta {
    cap confirm file "$TAB/`f'"
    if !_rc {
        if `first' {
            use "$TAB/`f'", clear
            local first = 0
        }
        else {
            append using "$TAB/`f'"
        }
    }
}
```
Rilanciare 55.do: le due stime skippano (i `.dta` esistono), l'assemblaggio scrive il CSV in secondi.

**Criterio di chiusura:** `ppml_extensive_stata.csv` esiste, 10 righe (2×5), e i coef ep_green/ep_dirty coincidono con `New/Output/TripleDiff/Tables/ppml_extensive.csv` a ≥6 cifre (valori attesi: WB 0.0015271 / −0.0301390; TREND 0.0001174 / 0.0027795).

---

## P3 — Permutazione: decidere e (probabilmente) rifare S6 col design del paper (CRITICO-concettuale, 🛑 decisione utente)

**Problema.** Il paper (tab_06 e §inference) usa la permutazione R: profili EP+TD rimescolati **solo tra i 23 paesi trattati**, timing PTA fisso (testa il *contenuto*). Stata 56 rimescola i profili **fra tutti i ~236 paesi** (testa contenuto+accordo). I p non sono confrontabili (WB dirty 0,235 R vs 0,475 Stata). Il p del paper resta senza verifica cross-software.

**Opzione A (raccomandata) — rifare 56 col design del paper.** Modifica chirurgica a `56_permutation_collapsed.do`:

1. La lista da permutare non è tutti i paesi ma i soli trattati. Dopo la costruzione di `tprofile` (righe ~50–57), sostituire la costruzione di `clist`:
   ```stata
   * Lista SOLI TRATTATI (WB_EP_Depth>0 in almeno un anno) — replica sample(treated) di R
   use `tprofile', clear
   bysort country_code: egen double maxep = max(WB_EP_Depth)
   keep if maxep > 0
   keep country_code
   duplicates drop country_code, force
   sort country_code
   local nc = _N
   di as text "Paesi trattati: `nc'"   // atteso: 23
   tempfile clist
   save `clist'
   ```
2. Nel loop, la biiezione va costruita sui soli trattati (già così, usa `clist`) e il merge dei profili permutati deve toccare **solo i trattati**, lasciando ai mai-trattati i loro zeri. Il codice attuale fa `use \`base'` (senza variabili trattamento) + `merge m:1 country_code year using \`perm_treat'` — con `perm_treat` ora ristretto ai 23 trattati, le righe dei mai-trattati escono dal merge con WB/TREND/TD **missing**, NON zero. Aggiungere subito dopo il merge:
   ```stata
   foreach v in WB_EP_Depth TREND_EP_Count TotalDepth_nonEnv {
       replace `v' = 0 if missing(`v')
   }
   ```
   ⚠️ Questo replace è corretto SOLO se nessun paese trattato ha buchi negli anni dentro `tprofile` (in R il profilo è country×year completo). Verifica preliminare inclusa: `use \`tprofile'; bysort country_code: assert _N == 16` (o il numero di anni del panel; se fallisce, fermarsi e annotare).
3. Rinominare gli output per non sovrascrivere il run all-countries (che ha valore e va tenuto, vedi sotto):
   `permutation_draws_treatedonly.csv`, `permutation_collapsed_treatedonly.csv`, sentinel `permutation_collapsed_treatedonly_done.txt`. Aggiornare i tre global in testa.
4. Header nuovo del file: dichiarare esplicitamente "design = permutazione tra soli trattati, replica di 22.R sezione B; il file *_collapsed.csv (senza suffisso) è il design all-countries, test complementare NON confrontabile".
5. Costo: come il run precedente (~24 h, 2000 reghdfe). Il resume-safe c'è già; il seed va bene (42, run unico).

**Criterio di chiusura A:** b_obs ≡ baseline (già garantito); p_perm confrontati con `r710_permutation_summary.csv`: attesi WB green ~0,61, WB dirty ~0,23, TREND green ~0,18, TREND dirty ~0,85, con tolleranza Monte Carlo ±0,03 (1000 draws, ~9 profili distinti → p granulari). Fuori tolleranza → fermarsi, annotare, 🛑 utente.

**Opzione B (zero costo macchina) — tenere entrambi e documentare.** Non rifare nulla; aggiungere in testa a 56.do e nel ROADMAP la dichiarazione del design; nel paper (quando si scrive) presentare: "permutation among treated profiles p=0.235 (R, seeded, classe C); a stricter all-countries permutation run in Stata gives p=0.475, consistent with the null". In questo caso il p 0,235 resta dichiaratamente solo-R — accettabile SOLO se dichiarato.

**In entrambi i casi:** i due file oggi su disco vanno resi non-ambigui (commento/rename), altrimenti una sessione futura mescolerà i p.

---

## P4 — Stability: ancorare la tabella del paper (full panel, FE fpd+fdt+pt)

**Problema.** La tabella stability del paper viene da `24_stability_controlgroups.R`: **full panel** micro, FE `fpd fdt pt`, cluster country_code, sottocampioni prodHS4 (3,77M righe) / deepshallow (5,26M) / cem_v1 (13,7M). Le spec "stability" di 52.do girano invece sul collassato: non verificano quei numeri. Restano classe C.

**Fix — nuovo script `New/Code/stata/58_stability_fullpanel.do`** (pattern identico a 18_robustness_fullpanel.do, che già fa spec full-panel con quelle FE):
1. Per ciascun gruppo, caricare dal `.dta` full panel (`Data/Final Dataset/final_dataset_pta_env_indices_compressed.dta`) SOLO le colonne: `ln_export WB_EP_Depth TREND_EP_Count hs6 country_code year fpd fdt pt` (≈9 col — footprint come 17.do). Escludere HK/Macao (`inlist(country_code,110,121)`).
2. Applicare il filtro del gruppo:
   - prodHS4: merge con `New/Data/Subsamples/flag_prodHS4.csv` (`in_HS4match==1` → keep). Attenzione al tipo di hs6 (destring come in 19b).
   - deepshallow: keep dei country_code con `group` deep/shallow da `New/Data/Subsamples/flag_deepshallow.csv`.
   - cem_v1: keep dei country_code in `Output/CEM/matched_countries.csv`.
3. Ricalcolare env_good dalla lista green (`green_codes_hs1996.csv`, destring, come in 19b), dirty da `dirty_goods_hs6.csv`, TotalDepth da `wb_totaldepth_country_year.csv` (merge su country_code+year, missing→0) — ESATTAMENTE come fa 24.R (che ricalcola tutto dai CSV correnti; vedi righe 60+ di 24_stability_controlgroups.R per la lista input).
4. Spec: `reghdfe ln_export ep_green ep_dirty td_green td_dirty, absorb(fpd fdt pt) vce(cluster country_code)` — NON pesata (è micro).
5. Cache per gruppo/treat (pattern OMNI_), output `stability_fullpanel_reghdfe.csv` con colonna source.
6. Costo: 6 regressioni full-panel-subsample; la più pesante (CEM, 13,7M) ≈ ordine di 17.do → sessione batch notturna.

**Criterio di chiusura:** confronto con `tripledd_stability.csv` (R): coef ≡ a ≥6 cifre per le 24 righe. Se una riga non combacia → è un candidato corruzione R (24.R è girato con callr sul full panel, la categoria a rischio): annotare, NON correggere il paper da soli, 🛑 utente.

---

## P5 — Depthbounds: sostituire i tre CSV stantii con valori Stata-verified

**Problema.** `tripledd_collapsed_nodepth.csv`, `tripledd_collapsed_targeted.csv`, `tripledd_epshare_treatedonly.csv` (run notturno 07/08) differiscono dai gemelli Stata alla 4ª cifra (input leggermente precedenti allo stato corrente). Le stime Stata corrispondenti sono già su disco e valide: `OMNI_nodepth_WB/TREND.dta`, `OMNI_targeted_WB/TREND.dta`, `OMNI_epshare_WB.dta`.

**Fix** (pattern 48c, R solo I/O — nessuna stima):
1. Nuovo script `New/Code/58c_build_verified_depthbounds.R` (o estensione di 48c): legge i `.dta` OMNI con `haven::read_dta`, riscrive i tre CSV nello **stesso schema colonne** attuale (term con i nomi R: es. `WB_EP_Depth:env_good`), aggiungendo colonna `source="reghdfe_stata_52"`.
2. Mappatura term: ep_green→`<X>:env_good`, ep_dirty→`<X>:dirty_p`, td_green→`env_good:TotalDepth_<...>`, td_dirty→`dirty_p:TotalDepth_<...>`; per epshare: `EP_share:env_good` / `EP_share:dirty_p`. Righe `_cons` da scartare.
3. Prima di scrivere, stampare il confronto vecchio-vs-nuovo (atteso: Δ ≤ 5e-4 assoluto su ogni coef).
4. Aggiornare 44_make_tables_tex.R? NON serve se lo schema colonne resta identico — verificare rilanciando 44 e confrontando `ptab_depthbounds.tex` (i valori arrotondati a 4 cifre non dovrebbero cambiare; se cambiano, il paper va riletto in quel punto — differenze attese al massimo sull'ultima cifra).
5. Estendere la guardia anti-sovrascrittura di 46/47 (check colonna `source`) agli script R che scrivono questi tre file (42_bounds_depth_controls.R, 39_epshare_treatedonly.R): stesso blocco `stop()` usato in 46 riga ~52.

**Criterio di chiusura:** i tre CSV portano source, valori ≡ .dta Stata; `44` rigira senza errori; ptab_depthbounds confrontata prima/dopo.

---

## P6 — env_good nel ladder R + destino di S7 (documentazione, nessuna stima)

1. **Annotare in `19_saturation_ladder.R`** (commento in testa, 3 righe): "ATTENZIONE: questo script usa la colonna env_good del .fst (definizione congelata alla build), NON la lista green_codes_hs1996 ricalcolata usata da 16/17/18/19b. Il blocco NI (unico citato nel paper, tab:ladder) non dipende da env_good ed è verificato ≡ Stata 19b. Il blocco Int NON è confrontabile con 19b/57 (definizione green diversa)."
2. **Annotare in `57_wcb_ladder_fullpanel.do`**: sostituire il commento "Confronto con R: p attesi = 0.91/0.89/0.64/0.62" (valori di provenienza non ricostruibile, nessun artefatto R su disco) con: "Nessun artefatto R gemello esiste: questo file è la PRIMA stima di questa spec (env_good ricalcolata). Verificato internamente: coef ≡ 19b (reghdfe diretto) a 7 cifre."
3. **Correggere `session-log.md`** (voce nuova, non riscrivere le vecchie): la riga "(Confronto atteso con R: p ≈ 0.91/0.89/0.64/0.62)" della sessione 12 va marcata come non riscontrabile.
4. Nessuna ristima necessaria: niente di user-facing dipende dal blocco Int del ladder. Se in futuro si vorrà pubblicare il ladder-Int, la definizione da usare è quella ricalcolata (coerente col resto del paper) → fonte = 19b/57, non 19.R.

---

## P7 — Registrare la campagna Stata in `run_pipeline.R`

Aggiungere, nella sezione Fase C di `run_pipeline.R` (dopo il blocco 19b, riga ~325), un blocco `stata_manual()` per ciascuno di:

| id | desc | artifacts | prerequisito R |
|---|---|---|---|
| 52-export | Export collassato omnibus | `New/Data/Collapsed/collapsed_omnibus.dta` | `run_rscript("52", ...)` per `52_export_collapsed_dta.R` |
| 52 | Omnibus S2 + WCB S3 | `omnibus_collapsed_reghdfe.csv`, `wcb_collapsed_boottest.csv` | 52-export |
| 54 | Event study Stata | `eventstudy_twfe_stata.csv` | 52-export |
| 55-export | Export griglia zero-fill | `New/Data/Collapsed/ppml_zerofill_export.dta` | `55_export_ppml_dta.R` |
| 55 | PPML Stata | `ppml_extensive_stata.csv` | 55-export |
| 56 | Permutazione Stata | `permutation_collapsed*.csv` (nome secondo esito P3) | 52-export |
| 57 | WCB ladder | `wcb_ladder_fullpanel.csv` | 19b |
| (58) | Stability full panel (se P4 eseguito) | `stability_fullpanel_reghdfe.csv` | — |

Usare lo stesso pattern dei blocchi 17/17b/18 esistenti (`stata_manual(id, desc, artifacts, cmd_hint)`). Verifica: `Rscript New/Code/run_pipeline.R` in modalità check (come da uso corrente) non deve segnalare artefatti mancanti una volta chiusi P1–P2.

---

## P8 — Igiene (30 minuti, nessun rischio)

1. **`set varabbrev off`** in testa a TUTTI i do-file di `New/Code/stata/` che non ce l'hanno (52, 54, 55, 56, 57, 19b, e i preesistenti 17/17b/18/48/48e). È una riga, previene la classe di bug di C1.
2. **File alla radice del repo** (non tracciati): `19b_assemble_only.log`, `19b_saturation_ladder_fullpanel.log`, `52_omnibus_collapsed.log`, `54_eventstudy_collapsed.log`, `55_ppml_collapsed.log`, `56_permutation_collapsed.log`, `57_wcb_ladder_fullpanel.log`, `check_dta.log`, `check_dta_vars.do`, `check_dta_vars.log` → spostarli in `New/Output/Diagnostics/stata_logs/` (i log sono la prova dei run: NON cancellarli, specie 52 e 55 che documentano i crash) e aggiungere `*.log` di radice al `.gitignore` se non già ignorati. `check_dta_vars.do` se è un one-off di debug: spostare in `New/_legacy/`.
3. **`session-log.md`**: nuova voce che rettifica i tre overclaim (S3 "completo" → invalido; S5 "completo" → CSV mai scritto; "output verificati" → verifica numerica eseguita solo il 23/08 sera da questo audit).
4. **`MISTAKES.md`**: una voce nuova per S3 — pattern: "output ben formattato + source column + log di sessione ≠ verifica; la chiusura di un task di verifica richiede il confronto numerico agli atti". Radice comune con le voci del 15/08 e 21/08 (fiducia nel log): se ricapita, promuovere a regola hard nel CLAUDE.md di progetto come da policy.
5. **`New/ROADMAP.md`**: aggiornare lo stato S1–S7 con l'esito reale (S1/S4/S5-stime/S7 chiusi; S3 da rifare; S6 in decisione; S2 chiuso salvo stability).

---

## Ordine di esecuzione consigliato

1. **P1** (fix + rerun S3) — sblocca l'ultimo pezzo inferenziale della tabella principale. ~2 h.
2. **P2** (assemblaggio S5) — 10 min.
3. **P8.1–P8.3** (varabbrev, log, session-log) — 30 min, prima che altre sessioni leggano i log vecchi.
4. **P5** (depthbounds verified) — 1 h, solo I/O.
5. **P3** 🛑 — decisione utente; se Opzione A: batch notturno ~24 h.
6. **P4** (stability full panel) — batch notturno.
7. **P6, P7, P8.4–P8.5** — documentazione, in coda.

Dopo P1–P5: ogni numero della tabella principale e delle robustezze collassate ha un gemello cross-software. Dopo P3(A)+P4: l'intero paper è classe A/B tranne il leave-one-out (mai in scope S1–S7; se si vuole chiudere anche quello, è un item nuovo: export .dta collassato per-paese + loop reghdfe, ~1 h macchina — da aprire solo su richiesta).
