# Audit Report — Paper_PTA / `New/` (versione aggiornata post-roadmap)
**Data:** 2026-08-21
**Scope:** intera cartella `New/` nello stato attuale (post-fix R1–R15 delle sessioni del 20/08, sincronizzato via commit "Updates"). Confronto con l'audit del 18/08 e con la vecchia versione dove utile.
**Macchina:** Mac (⚠️ NON canonica: `.fst` locale in quarantena). Per regola di progetto **nessuna stima nuova prodotta**: audit statico su codice + verifica dei numeri contro i CSV versionati (prodotti su Windows).
**Replica cross-linguaggio:** saltata (stesso motivo del 18/08). La copertura esistente (`New/verification/equivalence_log.md`, 27 script; identità R↔Stata a 8 cifre) resta adeguata.
**Indipendenza:** questa sessione non ha scritto il codice auditato.

---

## 0. Sintesi (leggere solo questo se hai 2 minuti)

**Il paper è a posto.** Tutti i fix della roadmap del 18/08 sono stati applicati e verificati: le tabelle del draft ora si generano da script (`\input{fragments/…}`), i numeri di testata si riproducono tutti dai CSV, i 4 warning del 18/08 sono chiusi, il commit di consolidamento è stato fatto (working tree pulito).

**Ma i due script nuovi (46 trimming, 47 decomposizione) hanno un problema serio.** I loro worker WCB usano `fixest::demean()` in sottoprocesso **senza la guardia di identità Frisch–Waugh** che tutti gli altri script del progetto hanno — la guardia fu introdotta proprio perché questo bug di corruzione silenziosa era già stato osservato due volte (ROADMAP, memoria di progetto `fixest-callr-crash-can-silently-corrupt-results`). Risultato: in **3 blocchi su 12** i coefficienti WCB **non coincidono** con quelli asintotici della stessa regressione. I p-value bootstrap di quei blocchi sono inferenza su una regressione sbagliata e vanno buttati e rigenerati. Tutti e 3 i blocchi corrotti sono TREND; tutti i blocchi WB coincidono a 12 cifre.

**Nessuno di questi numeri è nel paper** — trimming e decomposizione non sono ancora citati nel draft — quindi il draft non è contaminato. Ma due conclusioni annotate nel session-log del 20/08 sono da correggere (dettaglio in §3).

**Verdetto: CONDITIONAL PASS.** Il paper regge; il critico è confinato a output ausiliari non ancora usati. Prima di portare trimming o decomposizione nel paper, eseguire N1 della roadmap allegata.

---

## 1. Stato dei fix della roadmap 18/08 (verificati uno per uno)

| Item | Contenuto | Stato verificato oggi |
|---|---|---|
| R1 (W3) | Tabelle del draft agganciate ai frammenti | ✅ 5 `\input{fragments/ptab_*}` nel draft (main, stability, robust, depthbounds, pddt) |
| R2 (W1) | Frase sui pesi riscritta (equivalenza algebrica) | ✅ righe ~457–464 |
| R3 (W2+N1) | Footnote APEC distingue WB/TREND; «one quarter» ovunque | ✅ footnote riscritta; 4 occorrenze «quarter», 0 «one fifth» |
| R4 (W4) | Estimando (ATT, con pedice $f$) separato dallo stimatore TWFE | ✅ righe ~450–457 e ~513 |
| R5 (N3) | Conteggi green armonizzati a 248 | ✅ nel draft; ⚠️ residui «247» in un commento di `05_…R` (riga 91) e nel `.md` generato di 43 (vedi N5) |
| R6a | Assenza di trimming dichiarata in §2.2 | ✅ righe ~288–290 — ma ora superata dai fatti: il trimming È stato calcolato (vedi §3/W2) |
| R6b | Robustezza trimming (script 46) | ⚠️ eseguita, ma WCB da rigenerare (vedi §3/C1) e risultato non ancora riportato nel paper |
| R7 (N4) | Control-group batteries senza appello alla letteratura | ✅ opzione A applicata |
| R8 (N6) | Tabella pddt in appendice, con SE | ✅ `app:pddt` + `ptab_pddt.tex`, valori verificati contro i CSV |
| R11 | Meccanica di $\theta_{fdt}$ esplicitata | ✅ righe ~479–486 |
| R13 | Decomposizione quantità/valore unitario (script 47) | ⚠️ eseguita, ma WCB parzialmente corrotto (§3/C1) e risultati non nel paper |
| R14 | `ppml_agg_pdt_zerofill.fst` rigenerato, invarianza confermata | ✅ (session-log 20/08) |
| R15 | `nobs_pre`/`nobs_post` nei CSV WCB | ✅ nei CSV — ma il rename ha rotto una riga del generatore 44 (vedi §5/W1) |
| R16 | Commit di consolidamento | ✅ fatto: working tree pulito, commit «Updates» in cima |
| R10, R12 | Compressione §3.1; stimatore Callaway | 🛑 aperti, decisione utente (invariati) |

## 2. Verifiche numeriche di oggi (tutte ✅ salvo dove indicato)

| Numero nel paper/frammento | Fonte CSV | Esito |
|---|---|---|
| ptab_main: full WB −0.0023 (0.0039)/−0.0044 (0.0022), p 0.57/0.052 | `tripledd_full_reghdfe.csv` | ✅ |
| ptab_main: WCB full 0.69/0.18, CI [−0.035,+0.036]/[−0.043,+0.011] | `OLS/Bootstrap/wcb_fullpanel.csv` | ✅ |
| ptab_main: collapsed −0.0046/−0.0119, WCB 0.65/0.07, perm 0.61/0.23 | `tripledd_collapsed.csv`, `wcb_collapsed.csv`, `r710_permutation_summary.csv` | ✅ |
| ptab_main: TREND perm 0.18/0.85; WCB 0.39/0.85 | idem | ✅ |
| F congiunti 0.31/0.71, nclust 225 | `joint_F_fullpanel.csv` | ✅ |
| ptab_pddt: (0.0070)/(0.0030) vs (0.0069)/(0.0029), 45.7M | `tripledd_collapsed.csv`, `tripledd_full_pddt.csv` | ✅ |
| ptab_depthbounds: −0.0057/−0.0046/−0.0033/−0.0043 | verificati 18/08, CSV invariati (mtime) | ✅ |
| ptab_robust: PPML +0.0015 (0.74)/−0.0301 (0.06), 7.9M celle | `ppml_extensive.csv` (rigenerato post-R14) | ✅ |
| Nota di ptab_main | — | ❌ testo troncato (vedi §5/W1) |

## 3. Code audit dei due script nuovi

### C1 · [CRITICAL] WCB di 46 e 47: coefficenti corrotti in 3 blocchi su 12, senza guardia

I worker WCB di `New/Code/46_robustness_trim.R` e `47_outcome_decomposition.R` residualizzano con `fixest::demean()` e stimano con `lm()` in un sottoprocesso `Rscript` — la stessa architettura per cui il progetto ha già documentato **corruzione silenziosa** (ROADMAP: «un sottoprocesso crashato e ritentato può restituire un coefficiente completamente sbagliato SENZA sollevare errore», osservato su script 16 e 31, mitigato con la verifica Frisch–Waugh + `stop()`). Gli script 16/22/27/29/31 hanno la guardia; **46 e 47 no**. E la corruzione si è puntualmente manifestata. Confronto coefficiente asintotico (feols) vs coefficiente WCB (demean+lm) della **stessa regressione**:

| Blocco | Asintotico (green/dirty) | WCB (green/dirty) | Esito |
|---|---|---|---|
| 46 collapsed **TREND** | +0.000571 / −0.003701 | +0.001766 / +0.000250 | ❌ CORROTTO |
| 46 fullpanel TREND | +0.001832 / +0.000252 | +0.001832 / +0.000252 | ✅ |
| 47 collapsed uv **TREND** | −0.015074 / −0.010536 | −0.000116 / +0.000947 | ❌ CORROTTO |
| 47 fullpanel qua **TREND** | +0.0000406 / −0.002337 | +0.001971 / −0.000381 | ❌ CORROTTO |
| Tutti i blocchi WB (8) e gli altri TREND | — | coincidono a ≥12 cifre | ✅ |

Conseguenze concrete:
1. **`wcb_trimmed_collapsed.csv` (righe TREND), `wcb_decomp_collapsed.csv` (righe uv/TREND), `wcb_decomp_fullpanel.csv` (righe qua/TREND) sono invalidi.**
2. La conclusione del session-log 20/08 «TREND×valore unitario: p<1e-7 asintotico che *svanisce* col WCB (p=0.17/0.87)» **non è supportata**: quei p-value WCB testano i coefficienti corrotti (−0.0001/+0.0009), non quelli veri (−0.0151/−0.0105). Il risultato asintotico più vistoso della decomposizione **oggi non ha alcuna inferenza robusta valida**.
3. Il risultato di testata del trimming (WB dirty: WCB p=0.0398 collassato, 0.0629 full) ha coefficienti che coincidono, quindi *probabilmente* è buono — ma esce dallo stesso script senza guardia: va riconfermato nel rerun.

Fix dettagliato: roadmap N1 (guardia FW + rerun su Windows). Nota anche: 46/47 **non hanno la guardia anti-dataset-stantio** che la roadmap R6b prescriveva di copiare da `16b` (`stop()` se `max(WB_EP_Depth) != 17`) — da aggiungere nello stesso giro.

### C2 · [NOTE] Minori sui nuovi script/output
- `wcb_decomp_fullpanel.csv`, riga WB/uv/green: `conf_low`/`conf_high` vuoti (boottest non ha restituito il CI; p=0.988). Innocuo, ma da rigenerare col rerun N1.
- Nei CSV asintotici di 46, `nobs` WB e TREND differiscono di 1 (3.605.798 vs 3.605.799) sulla stessa base dati — curiosità da un drop di collinearità, senza effetto.
- 46 legge `panel_pdt_collapsed.fst` e 47 il `.fst` grezzo: corretti su Windows; su Mac esploderebbero solo perché il file non c'è (bene così).

## 4. Directory & Replication Package

- ✅ **R16 chiuso**: il lavoro è committato, working tree pulito su questa macchina — il rischio di divergenza Mac/Windows segnalato in tre audit consecutivi è rientrato.
- ✅ `_sample_config.R` sulla baseline (`excl`/`totaldepth`).
- ✅ Master script `run_pipeline.R` presente; 46/47 **non sono ancora elencati** nella pipeline (da aggiungere quando stabilizzati — roadmap N6).

## 5. Output Automation

### W1 · [WARNING] Nota di `ptab_main.tex` troncata nel PDF compilato
La nota della tabella principale del paper oggi recita: «…joint F on the four interactions: $p=0.31$ (WB), fixed-effect singleton removal), FE $pd+dt+pt$, weighted, 236 clusters…» — manca il p del TREND e l'apertura della frase sul collapsed panel. **Causa esatta**: `44_make_tables_tex.R` riga ~1275 legge `wcb_c$nobs[1]`, ma R15 ha rinominato la colonna in `nobs_pre`/`nobs_post` → l'argomento è di lunghezza zero → `sprintf()` restituisce `character(0)` → **la riga sparisce in silenzio** dal frammento. Il PDF attuale (33 pagine, compilato 21/08) contiene la nota mutilata. Fix a una riga: roadmap N3. (Morale: è il primo effetto collaterale del rename R15 — il grep dei consumatori di `nobs` andava fatto allora; gli altri usi in 44 leggono solo `coef`/`p_wcb` e sono sani, verificati.)

### Altro
- ✅ 19 frammenti Tabelle_Stime + 5 frammenti paper generati da script; `tab_20_brandi` via script 45.
- ⚠️ [N5] Il `.md` di 43 e un commento di 05 dicono ancora «247» (residuo R5): innocuo (non entra nel paper), da chiudere alla prossima rigenerazione Windows.

## 6. Econometria

Il disegno resta quello validato il 18/08 (triple-diff su composizione; `fdt` assorbe accordo e selezione; inferenza a tre livelli per few treated clusters; limiti dichiarati: collinearità EP/TD 0.96, dose continua + staggered senza stimatore robusto, potenza). Nessun nuovo problema di disegno. Due punti interpretativi nuovi, entrambi generati dai risultati di 46/47:

### W2 · [WARNING] Il trimming è stato calcolato, rafforza il margine dirty, e il paper non lo dice
Il paper dichiara (correttamente al 18/08) «no trimming or winsorization» con l'inference battery come guardia. Ora però la robustezza trimming esiste su disco e il suo risultato **spinge contro la narrativa**: trimmando p1/p99 l'outcome, il WCB del dirty collassato scende da p=0.073 a **p=0.040** (full panel pd+dt+pt: 0.063). Se confermato dal rerun N1, va riportato: un lettore che scoprisse l'esercizio non citato — con l'unico risultato che indebolisce la tesi del «falso positivo» — avrebbe un'obiezione legittima di selective reporting. La lettura onesta resta difendibile: il verdetto «falso positivo» non poggia sul WCB da solo ma su permutazione (p=0.23, non toccata dal trimming), leave-one-out (Australia) e TREND che non conferma — e il trimming non è la specifica dichiarata. Ma va **scritto**, non taciuto. Testo pronto in roadmap N2 (🛑 decisione di framing all'utente).

### N‑eco · [NOTE] La decomposizione ha un risultato asintotico vistoso senza inferenza robusta valida
TREND×valore unitario nel collassato: green −0.0151 (p=2·10⁻⁴), dirty −0.0105 (p=10⁻⁷), con TD interazioni positive e fortissime — il pattern classico della collinearità EP/TD che si spacca (stesso fenomeno del placebo RegulatorySpace, §Robustness). Con WB non c'è nulla (p>0.85). Molto probabilmente il WCB corretto lo sgonfierà (pochi cluster + split di collinearità), ma **oggi non lo sappiamo**: il WCB di quel blocco è corrotto. Non trarre conclusioni né scriverlo nel paper prima del rerun N1.

## 7. Summary & Required Actions

| # | Issue | Severità | Dove | Stato |
|---|---|---|---|---|
| C1 | WCB 46/47 senza guardia FW; 3 blocchi TREND corrotti | **CRITICAL** | `46_…R`, `47_…R` + 3 CSV | Aperto → roadmap N1 |
| W1 | Nota ptab_main troncata nel PDF (rename `nobs` R15) | WARNING | `44_…R` r.1275 | Aperto → N3 |
| W2 | Trimming calcolato, rafforza il dirty, non riportato nel paper | WARNING | draft §2.2/§4.4 | Aperto → N2 🛑 |
| N1 | Session-log 20/08: conclusione R13 sul TREND×uv non supportata | NOTE | session-log | Corretto a verbale qui → N4 |
| N2 | Decomposizione non ancora nel paper (era lo scopo di R13) | NOTE | draft | 🛑 dopo N1 → N4 |
| N3 | Residui «247» (commento 05, md di 43) | NOTE | 2 file | Aperto → N5 |
| N4 | CI mancanti in una riga di `wcb_decomp_fullpanel.csv` | NOTE | CSV | Si chiude col rerun N1 |
| N5 | 46/47 senza guardia anti-stale; non in `run_pipeline.R` | NOTE | 2 script | Aperto → N1/N6 |
| — | R10 (comprimere §3.1), R12 (Callaway) | — | draft | 🛑 invariati, decisione utente |

## 8. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** — il paper si riproduce integralmente e i fix del 18/08 sono tutti a posto; l'unico CRITICAL riguarda output ausiliari (WCB di trimming e decomposizione) **non ancora citati nel draft**. Non usare quei numeri finché N1 non è eseguito su Windows. I due warning (nota tabella troncata; trimming non dichiarato) sono risolvibili in un'ora più una decisione di framing.
- [ ] FAIL
