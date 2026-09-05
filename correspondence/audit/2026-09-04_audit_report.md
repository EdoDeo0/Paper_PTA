# Audit Report — Paper_PTA v3
**Data:** 2026-09-04
**Scope:** New/ — codice, paper, pipeline, econometria
**Riferimento precedente:** 2026-09-03_audit_report.md (C1–C4, W1–W18)
**Verdetto:** CONDITIONAL PASS

---

## 1. Problemi aperti dall'audit precedente

### Risolti (tutti i CRITICAL e la maggior parte dei WARNING)

| Issue | Stato | Note |
|-------|-------|------|
| C1 — pipeline rotto allo step 12 | **CHIUSO** | `run_pipeline.R` usa `stata_manual()` per step 12 |
| C2 — merge Stata non verificati | **CHIUSO** | diagnostiche `tab _merge` + `assert` aggiunte in `03.do` |
| C3 — step 69+70 assenti | **CHIUSO** | entrambi presenti in `run_pipeline.R` |
| C4 — `tab_02_ladder.tex` senza generatore | **CHIUSO** | header provenance aggiunto |
| W1 — `set varabbrev off` mancante | **CHIUSO** | via `_root.do` (presente e corretto) |
| W2 — CEM diagnostic morto | **CHIUSO** | conteggio drop + diagnostic corretto |
| W3 — script morto `19b` | **CHIUSO** | rinominato `.ARCHIVED` ✓ |
| W4 — `PTA_DEPTH` ignorato in `19c` | **CHIUSO** | era falso allarme: `19c` non usa depth |
| W5–W7 — FW guards mancanti | **CHIUSO** | guards aggiunti in 8 file (5 R + 2 Stata) |
| W8 — `dqrng` seed mancante | **CHIUSO** | `dqrng::dqset.seed(42)` in 3 script WCB |
| W9 — cache su nome, non hash | **CHIUSO** | `warning()` di mismatch aggiunto in `pta_functions.R` |
| W10 — report `.md` non suffissati | **PARZIALE** | 6/11 script aggiornati; 5 ancora con path bare |
| W15 + W18 — path hardcoded, variant manual | **CHIUSO** | `_root.do` creato e corretto; tutti i 26 `.do` aggiornati |
| W16 — `ptab_*.tex` senza provenance | **CHIUSO** | header aggiunto su tutti i 5 file |
| W17 — 12 script QA non documentati | **CHIUSO** | sezione QA in `run_pipeline.R` |

### Ancora aperti (tutti WARNING/NOTE)

**W10 parziale (NOTE):** 5 script R (in `New/Code/`) scrivono ancora file `.md` diagnostici con path bare, non suffissati per variante. Rieseguire con `SAMPLE="incl"` sovrascrive il `.md` del baseline. Nessun numero citato nel paper è coinvolto.

**W11 — Saturation ladder: citazione asimmetrica (NOTE):** L'appendice ora dice "reaches nominal significance only in the fpt+pd structure" senza distinguere WB (p=0.09) e TREND (p<0.05). È un miglioramento rispetto al testo precedente che citava solo p=0.09, ma il testo non avverte che le due colonne TREND nella stessa riga sono più significative del WB. Non causa un'affermazione sbagliata, ma è un'informazione rilevante per un referee.

**W12 — LOO: lettura dose-response assente (NOTE):** Il testo LOO è ben scritto (fragilità della precisione, non del punto stimato; dipendenza dal depth control). Manca però la frase alternativa: Australia e Korea sono anche le destinazioni con EP depth più alto — un'interpretazione dose-response (effetto reale ma sotto-alimentato) rimane sul tavolo e non è mai nominata. Il paper la implicita ma non la dice.

**W13 — Nessun framework di test multipli (WARNING):** Con ~40 test nella batteria di robustezza, il paper non discute il tasso di falsa scoperta. Il segnale RegulatorySpace (WCB p=0.046/0.022) è trattato come "unico segnale robusto" senza notare che sotto una null globale con 40 test sono attesi ~2 falsi positivi al 5%. Questo è il solo WARNING rimasto dalla sessione precedente non chiuso.

**W14 — "Content, not chapters" formulato come causale (NOTE):** La conclusione dice "The policy implication is direct: a chapter in an agreement is not itself the instrument. What the chapter contains is." L'inferenza si basa su conteggio descrittivo delle provision (Tabella subindici) + null aggregato, non su un contrasto causale in-sample — impossibile per collinearità perfetta. Manca una frase di umiltà esplicita su questo punto.

---

## 2. Nuovi problemi trovati in questo audit

### Tabelle orfane nel paper — WARNING

**P1 — `tab_05_wcb` inclusa senza \ref (WARNING)**
`tab_05_wcb.tex` è inclusa via `\input{Tabelle/tab_05_wcb}` (riga 729 del `.tex`) ma nessuna occorrenza di `\ref{tab:wcb}` esiste nel testo. La tabella appare nel PDF in mezzo a una nota a piè di pagina, ma il testo non la cita mai. Un referee che cerca il riferimento non lo trova.

**P2 — `tab_20_brandi` inclusa senza \ref (WARNING)**
`tab_20_brandi.tex` è inclusa via `\input{Tabelle/tab_20_brandi}` (riga 773) ma `\ref{tab:brandi}` non esiste nel testo. La tabella appare fisicamente dopo il paragrafo dei Brandi bounds ma senza rinvio esplicito.

### File in Tabelle/ non inclusi nel paper — NOTE

I seguenti 10 file esistono nella cartella `New/Paper/paper_v3/Tabelle/` ma non sono inclusi via `\input` in `paper_v3.tex`:

| File | Label | Note |
|------|-------|------|
| `tab_01_trattamento.tex` | `tab:trattamento` | Tabella trattamento (duplicata inline?) |
| `tab_03_main_full.tex` | `tab:main-full` | Stime full panel (incluse inline via `ptab_main`?) |
| `tab_04_main_collapsed.tex` | `tab:main-coll` | Stima collassata |
| `tab_07_matrice.tex` | `tab:matrice` | Mechanism matrix |
| `tab_08_eventstudy.tex` | `tab:eventstudy` | Event study TWFE |
| `tab_10_stability.tex` | `tab:stability` | Stability (il paper ha `tab:stability` inline a riga 784) |
| `tab_11_robustness_full.tex` | `tab:robust-full` | Robustness full panel |
| `tab_17_depthcontrols.tex` | `tab:depthctrl` | Depth controls |
| `tab_18_apec.tex` | `tab:apec` | APEC subsample |
| `tab_19_mde.tex` | `tab:mde` | MDE |

Questi file sono generati da `44_make_tables_tex.R` ma non compaiono nel PDF. Alcuni potrebbero corrispondere a tabelle incluse inline via `\input{fragments/ptab_*}` (ad es. `ptab_main` in riga 748 include colonne da più `tab_*.tex`). La situazione è confusa: non è chiaro quali siano intenzionalmente esclusi e quali siano stati dimenticati. Nessun numero pubblicato è in pericolo — il problema è di chiarezza strutturale.

---

## 3. Econometria

Nessun nuovo problema trovato. La specifica (FE, clustering, pesi) è invariata e già verificata negli audit precedenti. I risultati Stata sono l'autorità e tutti i 44 file sono stati verificati cross-software a ≤4e-13.

Il sotto-indici EP disclosure è stato aggiunto al paper (sezione 5.5, riga 890: "sub-indices reported in this section are constructed by the authors from the raw WB and TREND provision codings; they are not variables native to either database"). Anche la nota della tabella di composizione (righe 1298-1300) lo ribadisce. **Problema chiuso.**

---

## 4. Paper (paper_v3.tex)

### Risolto

- Linguaggio LLM rimosso (verificato da session log 2026-09-04)
- Sotto-indici dichiarati come costruzioni degli autori (✓)
- 6 tabelle tradotte dall'italiano all'inglese (✓)
- 5 nuove tabelle integrate con \input e testo (✓)
- 0 errori di compilazione LaTeX (✓)

### Ancora aperto

- W11–W14 (vedi sopra): testo da modificare
- P1–P2: due tabelle incluse senza \ref

---

## 5. Pipeline e replicabilità

`_root.do` corretto e completo: `set varabbrev off`, path OS-condizionati, global variant (`PTA_SAMPLE`, `PTA_DEPTH`, `OUTSFX`).

`run_pipeline.R` include step 12 (Stata manual), step 69, step 70, e sezione QA documentata. È la struttura più solida vista finora.

---

## 6. Tabella riassuntiva — problemi aperti

| # | Problema | Gravità | File | Stato |
|---|----------|---------|------|-------|
| W10 | 5 script: `.md` diagnostici senza suffisso variante | NOTE | `New/Code/33-43.R` (5 file) | Parziale |
| W11 | Ladder appendix: WB vs TREND significance non distinti | NOTE | `paper_v3.tex` (appendix ladder) | Aperto |
| W12 | LOO: lettura dose-response non menzionata | NOTE | `paper_v3.tex` §5.4 | Aperto |
| W13 | Nessun framework test multipli | WARNING | `paper_v3.tex` §5 | Aperto |
| W14 | "Content matters" senza caveat causale | NOTE | `paper_v3.tex` conclusione | Aperto |
| P1 | `tab_05_wcb` inclusa senza \ref | WARNING | `paper_v3.tex` riga 729 | Nuovo |
| P2 | `tab_20_brandi` inclusa senza \ref | WARNING | `paper_v3.tex` riga 773 | Nuovo |
| P3 | 10 file Tabelle/ non inclusi nel paper (status poco chiaro) | NOTE | `Tabelle/tab_0{1,3,4,7,8}*, tab_1{0,1,7,8,9}*` | Nuovo |

---

## 7. Verdetto

**CONDITIONAL PASS.** Tutti i CRITICAL e la quasi totalità dei WARNING dell'audit precedente sono stati risolti. I problemi rimasti sono:

- **1 WARNING** (W13, test multipli) che richiederebbe ~3 righe di testo in §5
- **2 WARNING** (P1, P2) di LaTeX che non rompono la logica ma rendono le tabelle non referenziate nel testo
- **4 NOTE** (W10, W11, W12, W14) di raffinamento del testo, ciascuno risolvibile in 1-3 frasi

Il paper è publication-ready sui numeri. I problemi rimasti sono tutti di confezione o di framing del testo.
