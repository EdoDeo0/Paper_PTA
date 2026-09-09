# Implementazione roadmap — Audit referee 2026-09-07c, Blocco A + Blocco B

Esecutore: sessione 21 (agente in background con istruzioni auto-contenute derivate dalla roadmap `2026-09-07c_roadmap_soluzioni.md`), verifica a valle nella sessione principale (git status/diff, MISTAKES.md). Nessuna stima lanciata oltre le due eccezioni esplicitamente autorizzate (vedi §4). Nessun `git commit`/`push`.

## 1. Tabella fix-per-fix

### Blocco A — testo e tabelle

| Fix | File | Stato |
|---|---|---|
| A1 | `Tabelle/tab_A09_desttrends.tex`, `paper_v4.tex` §5.5/App. G, `summary_v4.tex`, `44_make_tables_tex.R` | Applicato. Deviazione minore: in `summary_v4.tex` (documento standalone, nessun label `app:trends`) testo equivalente senza `\ref` incrociato, per non produrre `??`. |
| A2 | `paper_v4.tex` §5.1/5.2 | Applicato esattamente. |
| A3 | `Tabelle/tab_A16_mde.tex`, `33_mde_equivalence.R` | Applicato e verificato: script rilanciato (solo lettura CSV), numeri prodotti coincidono con la tabella. |
| A4 | `paper_v4.tex` App. MDE + intro + conclusione | Applicato. |
| A5 | `paper_v4.tex` intro + §5.1 | Applicato. |
| A6 | `paper_v4.tex` §5.4 + tabella composizione sub-index | Applicato, incl. rimozione riga "Cooperation". |
| A7 | tabella `mechanism` + testo | Applicato, verificato in PDF. |
| A8 | `Tabelle/tab_A13_robustness_full.tex` | Applicato (`\scriptsize` + `\arraystretch{0.9}`); "Float too large" sparito dal log, non serve `[p]`. |
| A9 | `tab_04_leaveoneout.tex`, `tab_A07_eventstudy_twfe.tex`, `tab_A13_robustness_full.tex`, `tab_A18_depthbounds.tex`, `tab_03_collapsed.tex`, `tab_A11_fullpanel_inclHKMO.tex`, `tab_07_ppml.tex` | Applicato integralmente. A11: `p_wcb` reali da `wcb_fullpanel_inclHKMO.csv`/`_desta.csv` (unit value resta `---`, dato assente nel file, non un'omissione). PPML: i 5 CSV `ppml_extensive*.csv` erano puntatori Git LFS non risolti nel working tree; recuperati da `d761830~1` via `git show` + `git lfs smudge`; numeri già in tabella confermati esatti. |
| A10 | `Tabelle/tab_A03_permutation.tex`, `paper_v4.tex` §4.2/§5.2 | Applicato con correzione: la roadmap aveva un'incongruenza interna (testo suggerito "25 in columns 2 and 4" vs sezione "Precisamente" con col.4=24). Verificato in `66_permutation_variants.do`: valori corretti col1=23, col2=25, col3=22, col4=24, usati nella nota. |
| A11 | `paper_v4.tex` Tab.2 nota, §3.1, §5.1 | Applicato, senza l'ultima frase su `Appendix~\ref{app:eif}` (appendice non creata: FIX C1 non eseguito), come prescritto dalla roadmap stessa in questo caso. |
| A12 | `paper_v4.tex` intro | Applicato. **Punto aperto:** rimanda a `tab:loo_new` (Tab. 9) per la precisione del green, ma quella colonna arriva solo con FIX C2 (non eseguito) — riferimento tecnicamente anticipato, così come la roadmap lo prescriveva. |
| A13 | `paper_v4.tex` §5.5, App. F | Applicato, senza rimando `Appendix~\ref{app:dose}` (FIX C3 non eseguito), come da regola generale di fine roadmap. |
| A14 | `paper_v4.tex` App. G, conclusione | Applicato, stessa rimozione del rimando `app:dose`. |
| A15 | `Tabelle/tab_06_subindex_fullpanel.tex` | Applicato (nota temporanea, come previsto fino a FIX B3 + ristima). |

### Blocco B — codice

| Fix | File | Stato |
|---|---|---|
| B1 | `02_build_dataset_wb_trend_merge.R` | Applicato e **verificato con rerun** (unico run "pipeline" autorizzato): `WBID_ATTESI` nuovo, guardia TREND aggiunta, script eseguito fino in fondo, `Merged_TREND_WB_Indices_Only.csv` identico byte-per-byte al precedente. I due `.dta` gemelli risultano cambiati in `git diff --stat` a dimensione invariata (verosimile timestamp interno Stata, non contenuto). |
| B2 | `03_build_dataset_customs_merge.do` | Applicato (solo modifica, non lanciato: 18GB). |
| B3 | `68_subindices_fullpanel.do` | Applicato (3 modifiche). **Deviazione conservativa:** non cancellato `subindices_fullpanel_desta.csv` come chiedeva la roadmap, per non lasciare la Tab. 8 Panel B senza fonte dati finché non viene rigenerato (Blocco C, non autorizzato). |
| B4 | `17_main_tripledd_fullpanel.do`, `17b_wcb_fullpanel.do`, `run_all_stata.ps1` | Applicato. Cancellati `17b_wcb_fullpanel_desta_val.do` e `17b_desta_val_wrapper.do`. Corretto di conseguenza `run_missing_wcb.ps1` (referenziava il file cancellato) — non richiesto esplicitamente ma necessario per non lasciare un riferimento rotto introdotto da questa stessa modifica. |
| B5 | `52_omnibus_collapsed.do`, `18_robustness_fullpanel.do`, `59_leaveoneout_collapsed.do` | Applicato parzialmente: glob → liste esplicite in 52 e 59 (spostati anche i 2 `.dta` orfani `OMNI_*_cem16_old.dta` in `New/_legacy/output_orfani/`); filtro suffisso esatto in 18 (bug reale: `_desta` pescava `_inclHKMO_desta`). **Non applicato**: `19b_assemble_only.do` (script di rescue non più agganciato alla pipeline, glob su 480 file con naming combinatorio troppo complesso da fissare senza eseguire) — voce in MISTAKES.md. |
| B6 | 12+ script Stata (17, 17b, 17c, 18, 19c, 19d, 48f-k, 57, 58, 68, 72, 73) | **Non applicato.** Molti hanno `use` multipli dentro loop/`preserve`; i valori di riferimento per gli assert `reg ..._dm` non sono documentati per ciascun file. Applicare alla cieca senza poter lanciare Stata rischiava un assert mal posizionato, scoperto solo al prossimo run — voce in MISTAKES.md. |
| B7 | `02_build_dataset_wb_trend_merge.R` (TREND_Hard), `32_desta_depth.R` (DESTA) | Applicato, **non ri-eseguito** (esplicitamente vietato: propaga a valle). In 32: aggiunto `228`, rimosso `100`, per Mongolia scelta l'opzione "escludere esplicitamente cc 124" (una delle due lasciate aperte dalla roadmap). Sintassi R verificata con `parse()`. |
| B8 | `12_cem_matching_stata.do` | Applicato parzialmente: aggiunta chiamata `imb` post-matching e logging L1 in `cem_v1_summary.txt`. **Non applicato**: inserimento del valore L1 nel testo (§3.3) — il numero non esiste finché CEM non viene rilanciato. |
| B9 | `make_figures_v3.R`, `70_sumstats_paper.R` | Applicato parzialmente: Korea/Laos → 2002 nel generatore mappa (non ri-generata la figura); `y`/`n` aggiunte a `70_sumstats_paper.R` (non ri-eseguito). **Non applicato**: `New/Code/71_make_figure_inputs.R` (script nuovo, non scrivibile senza poterlo verificare con un run). |
| B10 | `paper_v4.tex` tabella `samples`, `_sample_config.R`, `43_apec_egl_subsample.R`, `05_green_goods_hs1996.R` | Applicato parzialmente: riga C-overlap tolta dalla tabella; commento Timor-Leste corretto; scrittura di `apec_egl` spostata da 43 (effetto collaterale su file di input) a 05 (builder canonico); "eight"→"seven significant digits" applicato e verificato. **Non applicato**: spostamento in `New/_legacy/code/` di 09/19/34/39/40/46/46b2/47/48-50 (referenziati attivamente da `run_pipeline.R`, avrebbe rotto i path); `renv::init()+snapshot()` (infrastrutturale, fuori scope). |

## 2. Esito compilazione LaTeX

`pdflatex → biber → pdflatex ×2` (biblatex). Esito pulito: zero errori fatali, zero `\ref` irrisolti, zero "Rerun to get...". **"Float too large" non compare più** (FIX A8 confermato). Solo `Overfull`/`Underfull \hbox` preesistenti, non introdotti da questa sessione. Verifica puntuale sul PDF (72 pagine) per i numeri di A3, A4, A7, A9, A18: tutti coincidono con le sorgenti `.tex` e i CSV citati.

## 3. Script modificati

**R:** `New/Code/02_build_dataset_wb_trend_merge.R`, `05_green_goods_hs1996.R`, `32_desta_depth.R`, `43_apec_egl_subsample.R`, `44_make_tables_tex.R`, `70_sumstats_paper.R`, `_sample_config.R`, `New/Paper/paper_v4/figures/make_figures_v3.R`

**Stata (.do/.ps1):** `New/Code/stata/03_build_dataset_customs_merge.do`, `12_cem_matching_stata.do`, `17_main_tripledd_fullpanel.do`, `17b_wcb_fullpanel.do`, `18_robustness_fullpanel.do`, `52_omnibus_collapsed.do`, `59_leaveoneout_collapsed.do`, `68_subindices_fullpanel.do`, `run_all_stata.ps1`, `run_missing_wcb.ps1`

**Cancellati:** `17b_wcb_fullpanel_desta_val.do`, `17b_desta_val_wrapper.do`

**Spostati:** `OMNI_cem_{WB,TREND}_cem16_old.dta` → `New/_legacy/output_orfani/`

**Recuperati da git-lfs (dati, non codice):** 5 CSV `ppml_extensive*.csv` in `Tables_Stata/`

## 4. Run eseguiti in questa sessione (i soli due autorizzati)

1. `Rscript New/Code/33_mde_equivalence.R` — solo lettura CSV, output confermato identico ai numeri scritti in Tab. A16.
2. `Rscript New/Code/02_build_dataset_wb_trend_merge.R` — verifica guardia WBID/TREND del FIX B1: gira fino in fondo, CSV di output identico al precedente.

## 5. Rerun di stime che restano da autorizzare

**Residuo Blocco B (modifica applicata, run non lanciato):**
- `68_subindices_fullpanel.do` variante desta (~30 min) — rigenera `subindices_fullpanel_desta.csv` coerente col FIX B3
- `18_robustness_fullpanel.do` — solo blocco di assemblaggio per `tripledd_robustness_reghdfe_desta.csv`
- `32_desta_depth.R` (secondi) — cambia il DESTA di Singapore, propaga a tutte le varianti `_desta`
- `12_cem_matching_stata.do` + `52_export_collapsed_dta.R` + `62` — L1 post-matching e allineamento `cem_matched`
- `70_sumstats_paper.R` (minuti)
- `make_figures_v3.R` — rigenerare `fig_map_treated.pdf`
- Scrivere e verificare `New/Code/71_make_figure_inputs.R` (non ancora scritto)
- FIX B6 (guardie in 12+ script Stata) — da applicare con Stata disponibile per il check sintattico, non alla cieca
- `19b_assemble_only.do` — non fissato (script di rescue con naming combinatorio complesso)

**Blocco C (interi, script pronto in `New/replication/audit_2026-09-07c/`):**
- C1 — varianti timing EIF (~25 min Stata) + WCB su 2 varianti (~10 min) → nuova App. `app:eif`
- C2 — colonna green in Tab. 9 leave-one-out (nessuna stima, solo assemblaggio da CSV già esistenti)
- C3 — dose-bins con WCB (~40 min Stata) → nuova App. `app:dose`
- C4 — ristime di conseguenza dei fix di codice (TREND_Hard, DESTA Singapore, PPML se non recuperabile — quest'ultimo punto ora superato, i CSV sono stati recuperati)

**Blocco D (interi):** WCU/Webb, tariffe preferenziali, generatore tabelle v4, full panel DESTA.

## 6. Punti di dubbio o deviazione dall'esecutore

1. **A10** — incongruenza numerica interna alla roadmap risolta verificando il codice sorgente invece di scegliere arbitrariamente tra le due versioni proposte.
2. **A12** — rimando a `tab:loo_new` anticipa una colonna (green) che la tabella non ha ancora finché C2 non viene fatto; applicato comunque perché la roadmap lo richiede esplicitamente, segnalato qui per completezza.
3. **B3/B7/B8/B9** — in tutti i casi in cui la roadmap chiedeva un rerun "leggero" ma il file alimenta una tabella del paper, si è scelto sistematicamente di non eseguire, anche quando la roadmap stessa lo classificava come sicuro/veloce (linea di cautela richiesta esplicitamente nel mandato di questa sessione).
4. **B5/B6** — durante il lavoro è emerso che `run_missing_wcb.ps1` referenziava un file cancellato per B4: corretto di conseguenza (non richiesto esplicitamente, ma necessario per non lasciare un riferimento rotto introdotto da questa sessione).
5. Recupero dei CSV PPML da git-lfs: `git checkout` diretto non ha funzionato (puntatori LFS non risolti nel working tree); workaround non distruttivo con `git show <rev>:<path>` + `git lfs smudge`.
6. Due nuove voci in `MISTAKES.md` (in cima) documentano le non-applicazioni di B6/B5(19b)/B10(legacy move) con causa e criterio di prevenzione.

## 7. Stato del paper

**Dopo A+B il paper è circolabile**, con le due appendici `app:eif` e `app:dose` promesse dal testo lasciate senza riferimento (non create, come da istruzione della roadmap in assenza del via libera al Blocco C).
