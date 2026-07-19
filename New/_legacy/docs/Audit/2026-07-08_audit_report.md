# Audit Report — Paper_PTA (post prima bozza)

**Data:** 2026-07-08
**Modello:** Claude Fable 5 (skill `/audit`)
**Scope:** `New/Code/13–20` + `16/17.do` (campagna di stima 2026-07-06/08), `New/Paper/draft_paper.tex`, tutti i CSV di output in `New/Output/TripleDiff`, coerenza numeri paper ↔ output.
Gli script `01–12` erano già coperti dall'audit del 2026-07-03 (`New/AUDIT_PIANO_2026-07-03.md`); qui sono stati ricontrollati solo dove alimentano direttamente il paper (liste green/dirty, treatment map, deep/shallow).
**Nota posizione:** la skill prevede `correspondence/audit/`; per la regola di progetto "modificare solo /New" il report è salvato in `New/Audit/`.

---

## 1. Code Audit (script 13–20, 16.do, 17.do)

### Verifiche indipendenti superate ✅

Tutti i numeri chiave del paper sono stati ricontrollati contro gli output e, dove possibile, ricalcolati da zero:

| Claim del paper | Fonte | Esito |
|---|---|---|
| Full panel: green −0.0021 (p=0.55), dirty −0.0040 (p=0.038); TREND −0.0001/−0.0009 | `tripledd_full_reghdfe.csv` | ✅ |
| Joint F: p=0.26 (WB), p=0.71 (TREND); 225 cluster | `16_tripledd_full.log` r.233-287 (F(4,224)=1.32/0.53) | ✅ |
| Collassato: −0.0023 (p=0.72), dirty −0.0089 (p=0.006); 236 cluster | `tripledd_collapsed.csv` + ricalcolo cluster | ✅ |
| WCB: 0.88 / 0.18 / 0.39 / 0.85 (B=9999) | `wcb_collapsed.csv` | ✅ |
| Permutation green p=0.45, dirty p=0.50 con sign flip (+0.004) | `permutation_collapsed*.csv` | ✅ |
| Stabilità 8 design (−0.0009…−0.0025, mai significativo) | `tripledd_stability.csv` + `tripledd_robustness_reghdfe.csv` | ✅ |
| LOO: muore senza Corea (−0.0059, p=0.21) | `dirty_leaveoneout.csv` | ✅ |
| ρ(GreenLib, Standards)=1.000 | ricalcolato: ρ=1 sia incl. sia escl. HK-MO | ✅ |
| Sun-Abraham ATT: green −0.044 (p=0.24), dirty +0.073 (p=0.28) | `sunab_gap.csv` | ✅ |
| PPML: green p=0.73/0.95; dirty −0.021 (p=0.16) | `ppml_extensive.csv` | ✅ |
| Within-firm: −0.00014 (p=0.37); TREND −0.00006 (p=0.044) | `tripledd_robustness_reghdfe.csv` (G) | ✅ |
| Quote descrittive: 45.781.211 righe, green 11.5%, dirty 7.0%, EP 20.3%, 4.996 HS6, 236 dest, celle 8.4%/14.0% | log 16/collassato + ricalcolo diretto | ✅ |
| Event study TWFE: pre-trend piatti, drift green a +5 (−0.085, t≈−2.5) | `eventstudy_collapsed.csv` | ✅ |
| Gap 3.773.498 → 3.681.023 celle | singleton fixest (89.440 pd / 83 dt / 3.856 pt) | ✅ spiegato |

### Issues

#### draft_paper.tex
- **[CRITICAL] Claim di magnitudine errato (§4.1).** "one-standard-deviation increase in WB EP depth (≈ 6 provisions) … at most a 1.4% change at the lower 95% bound". Ricalcolato: la SD di WB_EP_Depth è **3.09** tra i dest-anno trattati (2.54 pesata sulle celle), non 6; il lower bound 95% del full panel è −0.0088 per provision → **3.09 × 0.88% ≈ 2.7%**, non 1.4%. Né 6 né 1.4% sono ricostruibili da alcun output.
- **[CRITICAL] "249 treated country-year observations" (§5.1).** Il 249 include HK+Macao, che sono esclusi dalla stima. Il numero corretto in-sample è **223**. ρ=1.000 regge in entrambi i casi (verificato). Fatto più profondo non riportato: GreenLib e Standards sono **non-zero solo in 3 country-year (Corea 2015; Svizzera 2014, 2015)**, sempre in rapporto fisso 1:3 — le clausole con meccanismo commerciale esistono in soli 2 accordi.
- **[CRITICAL] "Caselli et al." citato due volte senza riferimento bibliografico** (intro §1 "multiple-control-group strategy of Caselli et al." e implicitamente da 13). Nessun bibitem: su Overleaf compare come testo nudo, un referee lo nota subito.
- **[WARNING] East Timor trattato come membro ASEAN–Cina.** Il treatment map include il codice 144 (East Timor) con i valori ASEAN dal 2005 (11 righe; 9.069 righe di panel = 0.02%). Timor-Leste **non è parte dell'ACFTA**: errore a monte (fonte WB/TREND o crosswalk codici in Step 1-3). Impatto sulle stime: nullo (0.02% delle righe), ma il paper dice "ASEAN–China (11 destinations)" e "23 treated": entrambi i conteggi dipendono da Timor.
- **[WARNING] Nota permutation in tab:main incompleta.** La permutation green gira su un panel ulteriormente aggregato dest×anno×green (b_obs = −0.0052, non −0.0023) e rimescola i profili solo tra i trattati. La nota attuale lascia intendere che la p si riferisca al coefficiente del collassato.
- **[WARNING] "17 vs. 8 countries" (nota tab:stability).** Lo split deep/shallow è calcolato su 25 trattati incl. HK-MO, ma la stima li esclude: il confronto effettivo è **17 vs 6**.
- **[WARNING] La sezione dati promette i sotto-indici "enforcement" ma non sono nel CSV**: i 2 modelli (WB_EnforcementDSM, TREND_EnforcementDSM) erano crashati e mai ristimati. O si stimano (fattibile sul collassato, cache-friendly) o si toglie la menzione.
- **[NOTE] Arrotondamenti:** no-ASEAN dirty è −0.00415 → in tab:robust va **−0.0041**, non −0.0042; "the dirty coefficient sits between −0.004 and −0.005" ma il modello con controlli dà −0.0055 → "between −0.004 and −0.0055".
- **[NOTE] Bibliografia:** `headmayer2014` e `larch2025` presenti ma mai citati nel testo (citarli o rimuoverli). Voce `neri2023` (CESifo WP 10436) da verificare con /bibcheck.
- **[NOTE] "Section~4.4" hardcoded due volte** → aggiungere `\label{sec:dirty}` e usare `\ref`.
- **[NOTE] Riconciliazione celle:** tab:main dice 3.681.023, descrittive 3.773.498 — aggiungere "after singleton removal" nella nota.
- **[NOTE] Abstract "46 million"** vs testo 45.8M → uniformare.
- **[NOTE] Wording §3.2:** "outcome: within-cell mean of log exports, weighted by cell size" — la media di cella è NON pesata; è la regressione a essere pesata per n.
- **[NOTE] Figura Sun-Abraham** (`eventstudy_sunab.png`) copiata in figures/ ma mai inclusa: aggiungerla (appendice) o rimuoverla. Nota: nel CSV SA i lead lontani (−13, −14) sono significativi; nella finestra plottata [−6, +5] il gap dirty a −6 è +0.047 (p=0.001) — se si include la figura, un commento è dovuto.
- **[NOTE] 168/3.616 dest-anno senza celle green** → gap_green mancante e droppato in SA; nota da aggiungere se si include la figura.

#### 19_sunab_gap.R
- **[NOTE]** `data = gap[entry_year != 10000L | TRUE]` è un filtro no-op (sempre TRUE); il vero filtro è nel `subset`. Inoltre `m_tw` è calcolato ma mai salvato/usato: codice morto.

#### 17_remaining_models.do
- **[NOTE]** Il loop finale di append (`append using "$TAB\`f'"`) fallisce (r(601), quoting): il CSV è stato assemblato con uno script R esterno. Per riproducibilità il loop va corretto nel .do.

#### 15_wcb_collapsed.R
- **[NOTE] WCB dopo Frisch-Waugh:** demeaning pesato + boottest su lm demeanato. pd e dt sono annidate nel cluster paese, **pt no**: il bootstrap sui dati residualizzati non ricalcola le FE pt a ogni draw — approssimazione riconosciuta ma da dichiarare in una footnote metodologica. I coefficienti demeanati coincidono con feols (verifica stampata nel log) e i pesi sono passati a lm (boottest.lm li rispetta).

### Riepilogo
**3 CRITICAL** (tutti nel paper, nessuno nelle stime), **4 WARNING**, **9 NOTE**.
Nessun errore trovato nelle stime stesse: tutti i numeri del paper tracciano correttamente agli output, salvo i punti sopra.

---

## 2. Cross-Language Replication

**Parzialmente eseguita, per vincolo di macchina documentato** (allocatore R instabile: full panel 3-HDFE crasha sempre; cfr. ROADMAP note ambiente). Esiste già una convalida incrociata di fatto: la spec principale è stimata in R (collassato, fixest) e in Stata (full panel, reghdfe) con risultati coerenti (−0.0023 vs −0.0021). **Manca la replica esatta a 6 decimali sullo stesso design**: proposta nel piano (stima Stata del panel collassato — 3.7M righe, fattibile in minuti). Python non in scope (pipeline R/Stata).

---

## 3. Econometrics

- Clustering a livello di destinazione = livello di variazione del trattamento ✅ (Abadie et al. 2023).
- FE fdt/dt assorbono l'accordo: identificazione della sola composizione ✅ coerente col codice.
- Inferenza a tre livelli per few treated clusters ✅ implementata correttamente (WCB B=9999 seed fisso; permutation entro i trattati, seed fisso).
- Caveat da dichiarare nel paper: (i) approssimazione FWL nel WCB (pt non annidata); (ii) design della permutation (aggregato, b_obs diverso); (iii) split deep/shallow 17 vs 6.
- Filtri campionari coerenti tra specifiche (HK-MO esclusi ovunque tranne robustezza C) ✅ verificato in 13/14/15/16/17/18/19/20.

---

## 4. Summary & Required Actions

| # | Issue | Severità | File | Azione |
|---|-------|----------|------|--------|
| 1 | Magnitudine "≈6 provisioni / 1.4%" errata | CRITICAL | draft_paper.tex §4.1 | Riscrivere con SD=3.09 e bound 2.7% |
| 2 | "249 country-year" incl. HK-MO; fatto Korea/Svizzera non detto | CRITICAL | draft_paper.tex §5.1 | 223 + riscrittura col fatto "3 country-year, 2 accordi" |
| 3 | "Caselli et al." senza bibitem | CRITICAL | draft_paper.tex §1 | Citazione corretta o riscrittura |
| 4 | East Timor nel gruppo ASEAN | WARNING | dati/paper | Diagnosi fonte + footnote/esclusione |
| 5 | Nota permutation imprecisa | WARNING | draft_paper.tex tab:main | Esplicitare design e b_obs |
| 6 | "17 vs. 8" → 17 vs 6 | WARNING | draft_paper.tex tab:stability | Correggere nota |
| 7 | Enforcement sub-indices mancanti | WARNING | 18 + paper §5.1 | Ristimare i 2 modelli (cache) |
| 8-16 | NOTE varie (arrotondamenti, bib, ref, wording, dead code, do-file append) | NOTE | vari | Vedi piano |

## 5. Verdetto

- [ ] PASS
- [x] **CONDITIONAL PASS** — nessun problema nelle stime; i CRITICAL riguardano accuratezza del testo del paper e vanno corretti prima di qualunque circolazione della bozza.
- [ ] FAIL

Piano operativo dettagliato per l'implementazione (Sonnet 5 medium): `New/PIANO_SONNET_2026-07-08.md`.
