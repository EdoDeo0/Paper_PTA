# Audit Report — Paper_PTA (campagna §7-R7)
**Date:** 2026-07-15
**Scope:** script nuovi `New/Code/23-30` (audit completo) + tracciamento di TUTTI i
numeri nuovi/modificati in `New/Paper/draft_paper.tex` (R7.1–R7.10, §2.3, appendice)
+ spot-check dei file toccati dalla campagna. Gli script `01-22` sono coperti
dall'audit del 2026-07-08 (`New/Audit/2026-07-08_audit_report.md`, CONDITIONAL PASS,
piano implementato integralmente) e non sono stati ri-auditati da zero.
**Languages available:** R, Stata (StataNow19 SE), Python 3.14

---

## 1. Code Audit

### 23_r71_sunab_diag.R (diagnosi Sun-Abraham t=−6)
- [NOTE] I merge green/dirty non stampano diagnostiche di riga (stesso pattern del
  canonico 19, di cui replica volutamente la costruzione — coerenza verificata).
- [NOTE] La VCOV clusterizzata è non-PSD per costruzione (23 cluster vs ~28 coefficienti);
  documentato nello script, nel report diagnostico e nel paper. I p-value dei singoli
  lead sono usati solo per mostrarne l'inaffidabilità.
- Verifica incrociata: i numeri dell'appendice del paper tracciano tutti a
  `r71_sunab_diag.{csv,md}` (coorti −0,49/−0,33/+0,14/+0,10; LOO 2015 −0,051 p=0,11;
  no-late −0,109; ATT 0,27/0,11/0,29). ✅

### 24_r76_collinearity.R (collinearità EP↔TD)
- [NOTE] Il demeaning alternato (10 iterazioni) non ha check di convergenza formale;
  con 2 FE su 223 osservazioni la convergenza è certa in pratica.
- [FIXED IN PAPER] Il VIF 4,39 è calcolato sui dati grezzi, non within; il paper diceva
  "the corresponding VIF" (antecedente ambiguo dopo la frase sul within) → corretto in
  "the raw-data VIF" durante questo audit.
- Numeri nel paper (0,88/0,95; 0,50/0,85; 4,4) ✅ tracciano a `r76_collinearity.md`.

### 25_r78_sample_character.R (caratterizzazione post-singleton)
- [RESOLVED] Discrepanza 21.519.537 (script) vs 21.519.511 (reghdfe): verificato in
  questo audit che il panel (excl. HK/MO) contiene **30 righe con ln_export NA**
  (9 export NA + 21 export ≤ 0) che reghdfe scarta PRIMA della rimozione singleton,
  spostando leggermente il punto fisso. Scarto 0,0001%, immateriale; le quote del
  paper (47%/70%/11,5→11,9/7,0→6,4) sono insensibili. ✅
- [NOTE] L'ordine di rimozione (fpd→fdt→pt entro iterazione) differisce da reghdfe ma
  converge allo stesso punto fisso massimale (proprietà nota; confermata dal match).

### 26_r79_desttrends.R (trend destinazione, full-sample)
- Nessun problema. Merge con NA→0 espliciti; varying slopes `country_code[trend_g]`
  senza FE paese aggiuntiva (corretta: già annidata in dt); un modello per
  sottoprocesso; cache RDS.
- [ECONOMETRICS] La spec è nota per assorbire dinamica post-trattamento (Wolfers 2006):
  gestito esplicitamente con la variante 28 e discusso nel paper. Esemplare.

### 27_r79b_wcb_trends.R (WCB su spec con trend)
- Nessun problema. La verifica di equivalenza Frisch-Waugh↔feols è HARD (stop se
  diff>1e-5) ed è passata a ~1e-9 per entrambi gli indici — la validazione più forte
  possibile dell'estensione di demean() ai varying slopes (slope.flag=c(0,0,0,-2)).

### 28_r79c_pretrend_variant.R (trend pre-periodo proiettati)
- [WARNING → mitigato] Le slope della coorte 2002 sono stimate su soli 2 anni pre
  (2000-2001) e proiettate fino a 13 anni: rumore amplificato. Il paper lo riflette
  ("every coefficient returns to an imprecise zero") ma senza esplicitare la causa;
  accettabile per una robustezza il cui esito è "nulla di significativo", da tenere
  a mente se mai il risultato diventasse centrale.
- Gestione corretta delle destinazioni senza celle green nel pre (slope=0, flaggato).

### 29_r710_permutation_true.R (permutation sulla spec vera)
- Nessun problema. Check identità HARD contro i coefficienti di 14 (passato, stampato);
  seed deterministici (1000+batch); batch cache idempotente; 40/40 batch riusciti,
  0 draw NA su 2.000.
- [ECONOMETRICS/NOTE] Lo schema permuta i profili EP tenendo fissi i profili TD alla
  destinazione reale: testa lo sharp null "l'etichetta EP non conta, dato TD" —
  coerente con lo schema dell'aggregato di 14 e con la domanda del paper. Da notare
  che la permutazione decorrelaziona EP da TD (ρ reale 0,88), quindi la distribuzione
  placebo è generosa verso il rigetto; il fatto che comunque p=0,90 (green) rafforza.

### 30_r7h_wcb_ladder.R (WCB sulla ladder full-panel)
- [BUG TROVATO E CORRETTO] `boottest()` crasha deterministicamente (4/4) sui design a
  UNA colonna con 49M righe; con 2+ colonne funziona. Fix: intercetta nelle baseline
  (innocua sui dati demeanati, media ~0). Documentato nello script.
- [PROCESS/LESSON] Il primo run è morto al passo di aggregazione perché lo script è
  stato editato MENTRE l'Rscript detached lo stava ancora sourcando (R legge le
  espressioni top-level incrementalmente): parse corrotto. Nessun dato perso (cache
  per-spec integre). Lezione salvata in memoria persistente.
- Coefficienti FW = ladder pubblicata: +0,000309≈0,00031, +0,000274≈0,00027,
  +0,000376≈0,00038, +0,000283≈0,00028 ✅ (tutte e 4 le spec).

### File pre-esistenti toccati dalla campagna (spot-check)
- `19_sunab_gap.R`, `17_remaining_models.do`: fix auditati e testati il 2026-07-08. ✅
- `draft_paper.tex`: vedi §2 sotto.

---

## 2. Tracciamento numeri del paper (sostituisce la replica integrale)

Ogni numero nuovo/modificato in `draft_paper.tex` è stato tracciato alla sua fonte:

| Passaggio del paper | Fonte | Esito |
|---|---|---|
| §2.3 tab:samples (106 fam./20,5%; CEM 16+40; 98,5%; 17v6; taglie 3,8/21,5/13,7/5,3M) | ROADMAP §7.4.5 (record 2026-06-25/26) + tab:stability | ✅ |
| §3.2 collinearità (0,88/0,95; 0,50/0,85; VIF 4,4) | `r76_collinearity.md` | ✅ (wording VIF corretto) |
| §3.2 post-singleton (47%/70%; 11,5→11,9; 7,0→6,4; 26%/12%; mediane 1 e 3) | `r78_sample_character.md` | ✅ |
| §4.1 benchmark Brandi (CI, ln(1,17)=0,157, rapporto ~34, ln(0,95)=−0,051, ±SD) | ricalcolo Python indipendente | ✅ (−0,0147→−0,0146 corretto) |
| tab:main riga permutation (0,90/0,079/0,17/0,85) + §4.1/§4.4/intro | `r710_permutation_summary.csv` + ricalcolo Python dai 2.000 draws grezzi | ✅ esatto |
| §5 trend (−0,0054/−0,0070 p_wcb 0,28; −0,0022 p_wcb 0,013 CI[−0,0038,−0,0005]; +0,0074 p 0,18; +0,0176 p 0,46; 0,30–0,61) | `r79_desttrends.csv`, `r79b_wcb_trends.csv`, `r79c_pretrends.csv` | ✅ |
| Appendice SA (tutti i numeri) | `r71_sunab_diag.{csv,md}`, `sunab_gap.csv`, `eventstudy_collapsed.csv` | ✅ ("as few as three"→"a single destination" corretto: t=−15 poggia su 1 destinazione) |
| Wolfers 2006 (AER 96(5), 1802–1820) | WebSearch (AEA/IDEAS) | ✅ |

**Replica cross-language (targeted):**
- Permutation p-values ricalcolati in **Python** dai draws grezzi, indipendentemente da R:
  0,897/0,079/0,170/0,852 = summary R al terzo decimale esatto. ✅
- Replica **Stata** (reghdfe con slopes eterogenei) della spec R7.9 (trend full-sample, WB)
  su `panel_trends_for_stata.dta` (3.773.498 righe, export dedicato): **COMPLETATA** —

  | Statistic | R (fixest) | Stata (reghdfe) | Match? |
  |-----------|-----------|-----------------|--------|
  | N (dopo 92.475 singleton) | 3.681.023 | 3.681.023 | ✅ |
  | coef wb_green | −0.005370020 | −0.005370020 | ✅ (9 decimali) |
  | coef wb_dirty | −0.007016866 | −0.007016866 | ✅ (9 decimali) |
  | coef td_green | +0.000184438 | +0.000184438 | ✅ |
  | coef td_dirty | +0.000317624 | +0.000317624 | ✅ |
  | se(wb_green) cluster | 0.003514 | 0.003300 | ⚠️ ~6% |

  I coefficienti replicano ben oltre la soglia dei 6 decimali. Gli SE clusterizzati
  differiscono di ~6%: convenzioni diverse di conteggio dei gradi di libertà per gli
  slopes assorbiti (fixest vs reghdfe) — differenza nota e attesa, non un errore; le
  conclusioni inferenziali coincidono (entrambi n.s. asintoticamente) e il paper
  riporta comunque i p wild-bootstrap per questa spec. Nota minore: il `.do` di
  replica esportava per errore i dati invece dei risultati via `export delimited`
  (i risultati corretti sono nel `.dta` di regsave, usato per il confronto; CSV
  spurio rimosso).
- Replica integrale R↔Stata della spec principale già fatta e documentata il
  2026-07-08 (`New/Audit/comparison_collapsed.md`, match a 1e-9). ✅

---

## 3. Econometrics (sintesi)

- Clustering uniformemente a `country_code` (livello del trattamento) in tutte le
  stime nuove. ✅
- Inferenza few-clusters applicata sistematicamente ai risultati nuovi (WCB su trend,
  permutation sulla spec vera); l'unico coefficiente che sopravvive al WCB (TREND×green
  con trend full-sample) è stato smontato con la variante pre-periodo — il paper
  riporta entrambe le varianti con la lettura Wolfers. Trattamento onesto. ✅
- Il t=−6 dirty (Sun-Abraham) è correttamente declassato ad artefatto: identificazione
  su 8 destinazioni, coorti contraddittorie, sign-flip su LOO, VCOV rank-deficiente. ✅

---

## 4. Summary & Required Actions

| # | Issue | Severity | File | Status |
|---|-------|----------|------|--------|
| 1 | boottest crasha su design a 1 colonna (49M righe) | WARNING | 30_r7h_wcb_ladder.R | **Fixed** (intercetta) — rilancio baseline in corso |
| 2 | Parse corrotto per edit su script in esecuzione | PROCESS | 30 (run 1) | **Lesson saved** in memoria; nessun danno |
| 3 | "corresponding VIF" ambiguo (è il VIF grezzo) | NOTE | draft_paper.tex §3.2 | **Fixed** |
| 4 | Collapsed CI lower bound −0,0147 → −0,0146 | NOTE | draft_paper.tex §4.1 | **Fixed** |
| 5 | "as few as three" → t=−15 poggia su 1 destinazione | NOTE | draft_paper.tex App. A | **Fixed** |
| 6 | Discrepanza 26 obs (25 vs reghdfe) | NOTE | 25_r78 | **Resolved** (30 righe ln_export NA, verificato) |
| 7 | Slope 2002 su 2 anni pre, proiettate 13 anni | NOTE | 28_r79c | Open (accettabile per robustezza-null; caveat opzionale) |
| 8 | WCB ladder baseline (fpt+fpd WB/TREND) mancanti | PENDING | 30 | **Resolved** (vedi addendum) |

## 5. Verdetto

**[x] PASS** — nessun problema CRITICAL; tutti i WARNING risolti in corso d'audit.
Tutti i numeri del paper tracciano alle fonti; 5 imprecisioni minori trovate e
corrette; unico item aperto il NOTE #7 (caveat opzionale, non blocca).

## Addendum (2026-07-15, fine job)

**WCB sulla ladder full-panel (30, rilancio col fix intercetta) — COMPLETO.**
`New/Output/OLS/Bootstrap/bootstrap_summary.csv` (pending dal 2026-06-11, chiuso):

| spec | coef FW | atteso (tex) | p_wcb | N |
|---|---|---|---|---|
| wb_baseline | +0.000309 | +0.00031 ✅ | **0.910** | 49,245,273 |
| wb_controls | +0.000376 | +0.00038 ✅ | 0.885 | 44,380,012 |
| trend_baseline | +0.000274 | +0.00027 ✅ | 0.644 | 49,245,273 |
| trend_controls | +0.000283 | +0.00028 ✅ | 0.617 | 44,380,012 |

Il null della riga più saturata della ladder è validato anche sotto inferenza
few-clusters robusta (asintotico 0,91 ↔ WCB 0,91: concordanza piena). Aggiunta una
frase al §3.2 del paper. La replica Stata di r79 è documentata sopra (coefficienti
a 9 decimali). Check statico finale del paper: pulito.
