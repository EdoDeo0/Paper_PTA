# Audit Report — Paper_PTA (New/)

**Data:** 2026-09-05  
**Scope:** Intero progetto New/ — paper (paper_v2.tex), pipeline di codice (50+ script R/Stata), specificazione econometrica, tabelle, dati  
**Lingue disponibili:** R, Stata (Python non usato nel progetto)

---

## Verdetto

**CONDITIONAL PASS** — nessun errore critico nei dati o nelle stime. Il paper è solido nella sostanza, ma ha problemi di forma (refusi nell'abstract, discrepanze numeriche minori, nomi di comandi nel corpo del testo) che devono essere corretti prima dell'invio.

### Giudizio sincero sullo stato del paper

Il paper è in uno stadio avanzato. La pipeline di stima è tra le più robuste che si possano trovare in un progetto accademico: verifica Frisch-Waugh dopo ogni stima, cross-check R↔Stata su ogni risultato citato, permutation test, bootstrap, bounds exercise. L'identificazione è pulita e ben argomentata.

I problemi sono tutti di **presentazione**, non di sostanza:
- L'abstract ha 5+ refusi evidenti — inaccettabile in una submission
- 2 discrepanze numeriche tra testo e tabelle (coefficiente green -0.0022 vs -0.0023; VIF 5.8 vs 5.7)
- Nomi di comandi R/Stata nel corpo del testo (regola del progetto: solo in nota a piè di pagina)
- Path interni in due note di tabella
- Oscillazione tra "I" e "we" nel testo
- t=-6 nel Sun-Abraham dirty non discusso nel testo (un referee lo noterebbe)

Se questi problemi vengono corretti, il paper è pronto per la submission.

---

## 1. Paper — Testo e Lingua

### 1.1 Refusi (CRITICAL per la presentazione)

| # | Dove | Errore | Correzione |
|---|------|--------|------------|
| 1 | Abstract, l.57 | "incresignly" | "increasingly" |
| 2 | Abstract, l.57 | "againts" | "against" |
| 3 | Abstract, l.57 | "matter" | "matters" |
| 4 | Intro, l.97 | "difficoult" | "difficult" |
| 5 | Intro, l.97/107 + abstract | "enforcable" (×4) | "enforceable" |
| 6 | Intro, l.97 | "they the mere presence" | "the mere presence" |
| 7 | Intro, l.99 | "contration" | "contraction" |
| 8 | Intro, l.103 | "represents" (soggetto plurale) | "represent" |
| 9 | Intro, l.103 | "EP enter" | "EPs enter" |
| 10 | Intro, l.107 | "significat" | "significant" |
| 11 | Intro, l.109 | "The reminder of" | "The remainder of" |

### 1.2 Grammatica e stile

| # | Dove | Problema | Correzione |
|---|------|----------|------------|
| 12 | l.95 | "as may serve as a possible tool" | "and may serve as a tool" |
| 13 | l.99 | "risks to confound" | "risks confounding" |
| 14 | l.99 | "which allows to track" | "which allows tracking" |
| 15 | l.103 | "which allows to isolate" | "which allows us to isolate" |
| 16 | l.107 | "somehow informative" | "informative" o "still informative" |
| 17 | tutto il paper | oscillazione I/we | scegliere "I" (single-authored) |

### 1.3 Citazioni

| # | Dove | Problema |
|---|------|----------|
| 18 | l.95 | Frankel 2009: testo libero anziché `\citep` |
| 19 | l.105 | Cameron, Gelbach & Miller 2008: idem |

### 1.4 Nomi di comandi nel corpo del testo

**Regola del progetto:** i nomi di comandi R/Stata vanno solo in nota a piè di pagina.

| # | Dove | Comando | Azione |
|---|------|---------|--------|
| 20 | l.645-646 | `eventstudyinteract`, `fixest::sunab` | spostare in nota |
| 21 | Appendice l.1082-1099 | idem | accettabile in appendice, ma meglio in nota |

### 1.5 Path interni da rimuovere

| # | Dove | Path |
|---|------|------|
| 22 | l.234 (Table 2 note) | `New/Output/Diagnostics/B_treatment_entry.csv` |
| 23 | l.740 (Table 8 note) | `Data/Merged/Merged_TREND_WB_Indices_Only.csv` |

### 1.6 Blocchi commentati nell'abstract

| # | Dove | Problema |
|---|------|----------|
| 24 | l.55-56 | vecchio abstract commentato |
| 25 | l.59-86 | grande blocco commentato |

---

## 2. Paper — Numeri e Consistenza

### 2.1 Discrepanze numeriche

| # | Dove | Testo dice | Tabella dice | Azione |
|---|------|-----------|-------------|--------|
| 26 | l.562 | green coeff = −0.0022 | ptab_main: −0.0023 | correggere testo a −0.0023 |
| 27 | l.455 | VIF = 5.8 | depthbounds note: 5.7 | verificare quale è corretto |
| 28 | l.814 | PPML grid = 8.2M | ptab_robust: 7.9M obs | aggiungere nota sulla rimozione singleton |

### 2.2 Numeri verificati e corretti

- 45.8M osservazioni: coerente con Table 3 (45,781,211)
- 23 destinazioni trattate: coerente tra testo, Table 1, Table 2
- Panel collassato 3.77M → 3,681,023 dopo singleton: coerente
- Coefficienti collapsed (green −0.0046, dirty −0.0119): coerenti
- Equivalenza full/collapsed (−0.0045685): coerente
- Leave-one-out range: coerente
- Permutation p-values: coerenti (R=0.235, Stata=0.278 per dirty; paper cita 0.28)

---

## 3. Paper — Struttura e Logica

### 3.1 Questioni da considerare

| # | Problema | Severità |
|---|----------|----------|
| 29 | "Anatomy of a false positive" come titolo di sezione — un referee potrebbe obiettare che non si può provare che l'effetto vero sia zero | NOTE |
| 30 | Il termine "bounded null" usato senza definizione formale | WARNING |
| 31 | t=-6 nel Sun-Abraham dirty: marginalmente significativo, non discusso nel testo ma visibile nella figura | WARNING |
| 32 | La conclusione non discute la validità esterna | NOTE |
| 33 | 25 vs 23 destinazioni: potenziale confusione al primo incontro. Chiarire al primo uso | NOTE |
| 34 | CEM: procedure di matching mai descritte nella sezione metodologica | WARNING |
| 35 | Brandi comparison table (tab:brandi) inclusa ma mai referenziata con \ref nel testo | NOTE |
| 36 | Notazione: subscript `g` per green e `p` per product sono ambigui (p già indicizza HS6) | NOTE |

---

## 4. Codice — Pipeline

### 4.1 Risultato complessivo

| Severità | Conteggio |
|----------|-----------|
| CRITICAL | 0 |
| WARNING | 6 |
| NOTE | 17 |

### 4.2 Warning

| # | File | Problema |
|---|------|----------|
| W1 | 02_build (l.280) | Inner join WB×TREND senza asserzione sulle righe non matchate |
| W2 | 19_saturation (l.25-36) | `env_good` stale (238 vs 246 codici) — blocco "Int" non citato nel paper, ma rischio latente |
| W3 | 29b_build_ppml (l.14-24) | .fst su disco ha `env_good` stale — script 30 riclassifica a runtime (corretto), ma consumatori futuri del .fst leggerebbero la versione sbagliata |
| W4 | run_pipeline (l.427-431) | Step 44 (tabelle) lanciato prima di step 69 (assemblaggio CSV Stata) — richiede doppio lancio di 44 |
| W5 | 44_make_tables (l.145-148) | FE e clustering hardcoded nelle note delle tabelle, non letti dai CSV |
| W6 | Sun-Abraham SE | Divergenza R/Stata sugli errori standard documentata qualitativamente ("fino a 3-4x") ma non verificata programmaticamente |

### 4.3 Punti di forza

- `here()` usato ovunque in R (nessun path assoluto)
- Semi impostati per tutta la randomness
- Verifica artefatto su disco dopo ogni step della pipeline
- Frisch-Waugh identity check dopo ogni `feols`
- Guardie FWL hardcoded in Stata
- Campagna cross-software R↔Stata completa
- `_sample_config.R` / `_root.do` prevengono contaminazione tra varianti

---

## 5. Econometria

### 5.1 Risultato complessivo

| Severità | Conteggio |
|----------|-----------|
| CRITICAL | 0 |
| WARNING | 6 |
| NOTE | 18 |

### 5.2 Specifiche verificate

| Aspetto | Stato |
|---------|-------|
| Clustering (destination) | Coerente in tutti gli script R e Stata |
| FE (pd+dt+pt collapsed; fpd+fdt+pt full) | Corretti e coerenti |
| Identificazione triple-diff | Internamente coerente |
| Equivalenza numerica collapsed/full | Verificata a 7+ cifre decimali |
| PPML: export in livelli, no pesi | Corretto |
| WCB: semi impostati (set.seed + dqset.seed) | Corretto |
| Permutazione: treated-only, profili congiunti EP+TD | Corretto |
| Formula p-value con +1 (Young 2019) | Corretta |
| TotalDepth_nonEnv: EP sottratto dal totale | Corretto (no collinearità meccanica) |
| Bounds exercise: stabile su 4 controlli di profondità | Verificato |
| Trimming p1/p99: simmetrico, su collassato e full | Corretto |
| Decomposizione quantità/valore unitario | Corretta |

### 5.3 Warning econometrici

| # | Problema |
|---|----------|
| E1 | `env_good` stale nel saturation ladder (blocco "Int" non citato — rischio latente) |
| E2 | FE hardcoded nelle note tabelle |
| E3 | t=-6 Sun-Abraham dirty non discusso nel paper |
| E4 | WCB collassato è un'approssimazione FWL (pt attraversa i cluster) — full panel citato come autoritativo |
| E5 | Refusi nell'abstract |
| E6 | Profondità "targeted" (script 38) non verificata in questo audit |

---

## 6. Directory e Replicabilità

| Aspetto | Stato |
|---------|-------|
| Path relativi (R) | ✅ `here()` ovunque |
| Path Stata | ⚠️ Hardcoded in `_root.do`, documentato ("replicatore modifica solo qui") |
| Matrice 4 varianti | ⚠️ Richiede modifica manuale di `_sample_config.R` |
| Master script | ✅ `run_pipeline.R` documentato e funzionale |
| Dati grezzi separati da generati | ✅ `Data/` vs `New/Data/` vs `New/Output/` |
| Naming coerente | ✅ Numerazione sequenziale, convenzione chiara |

---

## 7. Tabella riepilogativa

| # | Issue | Severità | Area | Status |
|---|-------|----------|------|--------|
| 1 | Refusi nell'abstract (5+) | HIGH | Paper | Open |
| 2 | Coeff green −0.0022 vs −0.0023 | HIGH | Paper | Open |
| 3 | VIF 5.8 vs 5.7 | MEDIUM | Paper | Open |
| 4 | PPML 8.2M vs 7.9M senza spiegazione | MEDIUM | Paper | Open |
| 5 | Nomi comandi nel corpo testo | MEDIUM | Paper | Open |
| 6 | Path interni nelle note tabelle | MEDIUM | Paper | Open |
| 7 | I/we inconsistente | MEDIUM | Paper | Open |
| 8 | "bounded null" non definito | LOW | Paper | Open |
| 9 | t=-6 Sun-Abraham non discusso | MEDIUM | Paper | Open |
| 10 | CEM non descritto in metodologia | MEDIUM | Paper | Open |
| 11 | env_good stale in .fst | LOW | Codice | Open |
| 12 | Step 44 prima di step 69 | LOW | Pipeline | Open |
| 13 | FE hardcoded nelle note tabelle | LOW | Codice | Open |
| 14 | Blocchi commentati nell'abstract | LOW | Paper | Open |
| 15 | Tab Brandi non referenziata con \ref | LOW | Paper | Open |

---

## 8. Verdetto finale

- [x] **CONDITIONAL PASS** — warning da risolvere, nessun critical
- [ ] PASS — no critical issues
- [ ] FAIL — critical issues

**Condizioni per il PASS:** correggere i 15 issue elencati sopra (nessuno richiede ri-stima; sono tutti fix testuali o di documentazione).
