# Audit Report — Paper_PTA (New/)

**Data:** 2026-09-03  
**Scope:** Intera cartella `New/` — codice, dati, econometria, paper, replicabilità  
**Linguaggi:** R (fixest, fwildclusterboot, data.table), Stata (reghdfe, boottest, ppmlhdfe)  
**Nota:** Audit condotto in sessione indipendente dal codice auditato, con 7 agenti paralleli.

---

## Verdetto complessivo

**CONDITIONAL PASS** — nessun errore nei risultati pubblicati, ma 4 issue critiche e 14 warning richiedono intervento prima della submission.

| Severità | Conteggio |
|----------|-----------|
| CRITICAL | 4 |
| WARNING  | 14 |
| NOTE     | ~20 |

---

## 1. Codice — Data Pipeline

### CRITICAL

**C1. `run_pipeline.R` chiama `12_cem_matching.R` che è stato cancellato.**  
File: `New/Code/run_pipeline.R`, step 12.  
`git status` conferma `D New/Code/12_cem_matching.R`. Il replacement `New/Code/stata/12_cem_matching_stata.do` esiste ma non è collegato al pipeline. Eseguire `run_pipeline.R` oggi fallisce allo step 12 con `stop()`.

**C2. Merge Stata non verificati nel pipeline di costruzione dati.**  
File: `New/Code/stata/03_build_dataset_customs_merge.do`, righe 51, 57.  
I due merge più importanti del progetto (EP index e green goods sul panel da 49.2M righe) hanno solo commenti descrittivi. Nessun `tab _merge`, `assert`, o `count` verifica l'esito. Un merge silenziosamente fallito azzererebbe il trattamento su tutto il dataset senza alcun errore nel log.

### WARNING

**W1. `set varabbrev off` mancante in `01_wb_dataset_conversion.do` e `03_build_dataset_customs_merge.do`.**  
Rischio basso (nessun `reghdfe`), ma viola la regola di progetto introdotta dopo l'incidente S3 del 52_omnibus.

**W2. `12_cem_matching_stata.do`: drop di missing non contato, diagnostic morto, sanity check non enforced.**  
Righe 61-63: drop senza conteggio. Riga 66-67: `r(N)-r(N)+_N` è un no-op. Righe 104-110: il check 19/40 emette warning ma non `exit`.

**W3. `19b_saturation_ladder_fullpanel.do` è morto/superato.**  
Header dice "NON ANCORA ESEGUITO". Superato da 19c/19d. Non è un rischio se nessuno lo esegue, ma la sua presenza nel repo è confusa.

**W4. `19c_saturation_ladder_fullpanel.do`: `PTA_DEPTH` validato ma ignorato.**  
Righe 41-66: lo script accetta e valida `PTA_DEPTH` ma non lo usa — correre con `desta` o `totaldepth` produce output identici con nomi identici. Trappola già diagnosticata (MISTAKES.md 2026-08-30) ma mai chiusa.

---

## 2. Codice — Stima e Inferenza

### Nessun CRITICAL trovato su stime e risultati.

Il sistema FE/clustering/pesi/singleton è **corretto e consistente** su tutti gli script R e Stata auditati:
- Full panel: `fpd + fdt + pt`, no pesi, cluster `country_code` ✓
- Collapsed: `pd + dt + pt`, pesi = cell counts (`n`), cluster `country_code` ✓
- WCB: B=9999, null imposto, seed 42 (+ dqrng dove applicabile) ✓
- Permutation (paper-cited): treated-only, riassegnazione congiunta EP+TD, p-value con correzione +1 ✓
- R vs Stata: stesse formule, stessi FE, stesse variabili, stessi pesi ✓

### WARNING — Frisch-Waugh guards mancanti

**W5. Script R senza guard FW automatico con `stop()`:**

| Script | Tipo | Problema |
|--------|------|----------|
| `16b_dose_bins.R` | in-process feols | Nessun FW guard, nessun subprocess |
| `20_wcb_collapsed.R` | WCB collapsed | Solo `cat()` per eyeballing, nessun `stop()` |
| `20b_wcb_regulatoryspace.R` | WCB sub-indice | Idem |
| `24_stability_controlgroups.R` | full panel | Nessun guard, nessun retry |
| `25_heterogeneity_subindices.R` | sub-indici | Nessun guard su `feols()` |

Template corretto: `22_permutation_inference.R` (sezione B, righe 187-188).

**W6. `28_robustness_desttrends_pre.R`: guard FW incompleto.**  
Solo `ep_green` verificato, `ep_dirty` no. Se la corruzione colpisce solo dirty (plausibile, documentato in MISTAKES.md), il p_wcb sbagliato verrebbe pubblicato.

**W7. Script Stata senza guard FW automatico:**

| Script | Note |
|--------|------|
| `57_wcb_ladder_fullpanel.do` | Solo commento di "verifica manuale", nessun `exit 9` |
| `63_variants_collapsed.do` blocchi B/D/F/G | FWL senza guard, pur avendo il coefficiente di riferimento nel blocco A |

**W8. Seeding `dqrng` mancante in 3 script WCB R.**  
`27_robustness_desttrends_wcb.R`, `28_...pre.R`, `29_...co2intensity.R`: usano `set.seed(42)` ma non `dqrng::dqset.seed(42)`. I p_wcb risultanti oscillano ~1pp tra run (non altera le conclusioni, ma non è riproducibile bit-a-bit).

### WARNING — Caching

**W9. `pta_functions.R::run_block()`: cache basata solo sul nome file.**  
Se una formula viene modificata senza rinominare il blocco, il vecchio `.rds` viene restituito silenziosamente. Nessun hash della formula o dei parametri. Stesso pattern in `19_saturation_ladder.R` (skip se `.tex` esiste).

### WARNING — Suffissi output diagnostici

**W10. Script 33-43: output `.md` diagnostici non suffissati con `out_path()`.**  
Le cache `.csv`/`.rds` sono correttamente suffissate per variante (excl/incl × totaldepth/desta), ma i report `.md` usano path bare. Rieseguire con `SAMPLE="incl"` sovrascrive il `.md` del baseline.

---

## 3. Paper — Design e narrazione

### Nessun CRITICAL. Numeri nel testo tutti corretti.

Su ~40 valori numerici verificati contro CSV/log Stata, **zero discrepanze**. Il layer numerico è pulito.

### WARNING — Framing e caveats

**W11. Saturation ladder: citazione selettiva del p-value.**  
Sezione 3 / Appendix B, `tab_02_ladder.tex`: il testo cita solo il WB p=0.09 nella riga fpt+pd, ma entrambe le colonne TREND nella stessa riga sono `**` (p<0.05), cioè più significative. Non cambia la conclusione (la ladder è dichiarata suggestiva), ma un referee lo noterà.

**W12. Leave-one-out: lettura alternativa dose-response non discussa.**  
Sezione 5.4: Australia e South Korea sono anche le destinazioni con EP depth massimo (Korea WB=17). Il paper legge la fragilità LOO solo come "pochi cluster / precisione sottile". Non si chiede se quei due paesi siano pivotali *perché* sono i più trattati — una lettura dose-response (effetto reale ma sotto-alimentato) non è mai menzionata come alternativa.

**W13. Nessun framework di test multipli.**  
Con ~40+ test nell'intera batteria di robustezza, il paper non discute il tasso di falsa scoperta. Il segnale RegulatorySpace (WCB p=0.046/0.022, Sezione 5.6) potrebbe essere atteso sotto l'ipotesi nulla globale con tanti test — il paper lo tratta come "unico segnale robusto" senza chiedersi se sia rumore da test multipli.

**W14. "Content, not chapters" nella conclusione è descrittivo, non causalmente identificato.**  
La sezione bundling (5.6) dimostra che il design *non può* separare le componenti EP in-sample (collinearità perfetta). La conclusione (righe 1392-1398) formula "content matters" come se fosse un finding causale, quando è un'inferenza dal conteggio delle provision (Table 5) + il null aggregato. Serve una frase di umiltà esplicita.

### NOTE — Caveats aggiuntivi suggeriti

- **N1.** Il paper controlla continuità HS2007 per i codici green ma non per i dirty (1.139 codici, superficie più ampia).
- **N2.** Nuove celle fpd (firma che inizia a esportare un nuovo prodotto green verso una destinazione già servita) entrano senza pre-period — il paper non discute se questo bias $\beta_1$ verso zero.
- **N3.** WCB è giustificato per "pochi cluster" (Cameron et al. 2008), ma il problema qui è "pochi cluster *trattati*" su molti cluster totali (225-228) — una distinzione sottile che la letteratura recente sta esplorando. Vale una frase per dire che la permutation è il test più diretto per questo design.

---

## 4. Replicabilità

### CRITICAL

**C3. `run_pipeline.R` manca step paper-facing.**  
`70_sumstats_paper.R` (statistiche descrittive per il paper) e `69_assemble_stata_csvs.R` (riassembla i CSV Stata nel formato atteso da `44_make_tables_tex.R`) producono output necessari al paper ma non sono nel pipeline. Un replicatore che segue solo `run_pipeline.R` non li esegue.

**C4. `tab_02_ladder.tex` non ha script generatore tracciabile.**  
Nessun header di generazione, nessun riferimento in `New/Code/`. A differenza dei `tab_01`...`tab_20` (generati da `44_make_tables_tex.R`), sembra editata a mano. Rottura di riproducibilità.

### WARNING

**W15. 26 file `.do` con path assoluti hardcoded.**  
Pattern `~/Documents/work/...` / `~/work/...` in ogni `.do`. Un replicatore su altra macchina deve editare 26 file. Soluzione: un `_root.do` condiviso.

**W16. `ptab_*.tex` (fragments del paper) sono compositi manuali.**  
5 file in `paper_v3/fragments/` (ptab_main, ptab_pddt, ptab_depthbounds, ptab_robust, ptab_stability) non hanno link automatico ai `tab_*.tex` generati da `44_make_tables_tex.R`. Se i numeri cambiano, l'aggiornamento è manuale e a rischio di disallineamento.

**W17. 12 script Stata della campagna di verifica assenti da `run_pipeline.R`.**  
Script 59, 61, 63, 65, 66, 66b, 68, ecc. Comprensibile (sono QA, non core), ma andrebbe documentato.

**W18. Variant coordination Stata è manuale.**  
Ogni `.do` gestisce `PTA_SAMPLE`/`PTA_DEPTH` indipendentemente. Nessun config condiviso Stata equivalente a `_sample_config.R`.

---

## 5. Cross-Language Replication

**PASS.** La verifica R↔Stata è una delle parti più solide del progetto:
- `67_verify_stata_coverage.R`: confronto coefficienti con |δ| < 1e-6
- `69_assemble_stata_csvs.R`: riassemblaggio con comparazione durante l'assembly
- Chain 48/48c/49/50/58c: CSV verificati con tag `source`, upstream scripts rifiutano sovrascrittura
- Identità collapsed↔full-panel verificata a 7-9 cifre significative

Un gap residuo: `67` non copre tutti i `.do` (mancano 18, 19b/c/d, 61, 63, 65, 66/66b, 68).

---

## Tabella riassuntiva

| # | Sev. | Area | Descrizione | File |
|---|------|------|-------------|------|
| C1 | CRITICAL | Pipeline | `12_cem_matching.R` cancellato, pipeline rotto allo step 12 | `run_pipeline.R` |
| C2 | CRITICAL | Dati | Merge Stata non verificati (EP + green su 49M righe) | `03_build_dataset_customs_merge.do` |
| C3 | CRITICAL | Repl. | `69`+`70` assenti dal pipeline, output paper non generati | `run_pipeline.R` |
| C4 | CRITICAL | Repl. | `tab_02_ladder.tex` senza script generatore | `Tabelle/tab_02_ladder.tex` |
| W1 | WARNING | Codice | `varabbrev off` mancante in 01, 03 | `.do` |
| W2 | WARNING | Dati | CEM: drop non contato, diagnostic morto | `12_cem_matching_stata.do` |
| W3 | WARNING | Codice | Script morto nel repo | `19b_saturation_ladder.do` |
| W4 | WARNING | Codice | PTA_DEPTH validato ma ignorato | `19c_saturation_ladder.do` |
| W5 | WARNING | Stima | FW guard mancante in 5 script R | `16b, 20, 20b, 24, 25` |
| W6 | WARNING | Stima | FW guard incompleto (solo green) | `28_desttrends_pre.R` |
| W7 | WARNING | Stima | FW guard mancante in 2 script Stata | `57.do, 63.do` |
| W8 | WARNING | Stima | `dqrng` seed mancante in 3 script WCB | `27, 28, 29` (.R) |
| W9 | WARNING | Cache | Cache basata su nome, non su hash formula | `pta_functions.R` |
| W10 | WARNING | Output | `.md` diagnostici non suffissati per variante | `33-43` (.R) |
| W11 | WARNING | Paper | Citazione selettiva p-value nella ladder | Sez. 3 / App. B |
| W12 | WARNING | Paper | Lettura dose-response LOO non discussa | Sez. 5.4 |
| W13 | WARNING | Paper | Nessun framework test multipli | Paper intero |
| W14 | WARNING | Paper | "Content not chapters" formulato come causale | Conclusione |
| W15 | WARNING | Repl. | 26 `.do` con path hardcoded | Tutti `.do` |
| W16 | WARNING | Repl. | `ptab_*.tex` manuali, non tracciate | `fragments/` |
| W17 | WARNING | Repl. | 12 script QA Stata non nel pipeline | `run_pipeline.R` |
| W18 | WARNING | Repl. | Variant coordination Stata manuale | Tutti `.do` |

---

## Punti di forza

Questo progetto è stato auditato molte volte e le risposte ai rilievi precedenti sono visibili nel codice. Alcune cose da preservare:

1. **Classificazione prodotti ricalcolata a runtime** — mai usate le colonne stantie del .dta originale. Disciplina applicata uniformemente.
2. **Guard FW con `stop()`/`exit 9`** nei 6 script principali (22, 27, 29 R; 52, 56b, 59 Stata). Template da estendere ai restanti.
3. **Chain di CSV verificati** (48c/49/50/58c) con tag `source` e rifiuto sovrascrittura — meccanismo difensivo eccellente.
4. **Numeri nel paper tutti corretti** — zero discrepanze su ~40 verifiche puntuali.
5. **Self-hedging consistente** — quasi ogni claim nel paper ha già il proprio caveat (Sun-Abraham come diagnostico, tariff caveat, regulatory space non pulito, LOO precisione-non-stima).
