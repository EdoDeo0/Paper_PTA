# Roadmap soluzioni — audit 2026-08-18
**Scopo:** istruzioni autosufficienti, eseguibili da un altro modello senza decisioni da prendere. Ogni item ha: file esatti, testo/passi, criterio di verifica. Dove serve una decisione dell'utente è marcato 🛑.

**Regole vincolanti per chi esegue:**
1. Le stime NON si rifanno su Mac (dataset stantio, vedi `MISTAKES.md` 2026-08-14). Gli item che richiedono run vanno eseguiti su Windows (macchina canonica).
2. Nessun `git commit`/`push` senza richiesta esplicita dell'utente.
3. Dopo ogni edit a `draft_paper.tex`: ricompilare con 2 passate `pdflatex` (su Mac: `~/.local/bin/pdflatex`) e verificare 0 errori, 0 riferimenti irrisolti.
4. I fix nel testo che riflettono numeri devono citare i CSV sorgente indicati, mai numeri a memoria.

---

## Priorità 1 — Correzioni al draft (solo testo, mezza giornata totale)

### R1 · [W3] Agganciare le tabelle del draft ai frammenti generati
**Problema:** in `New/Paper/draft_paper.tex` tutte le tabelle (tab:main, tab:stability, tab:robust, tab:depthbounds, tab:treatment, tab:cohorts, tab:descriptives, tab:samples, tab:mechanism-share) sono `tabular` scritte a mano; solo `tab_20_brandi` è `\input{}`. Ogni rerun della pipeline richiede ri-trascrizione manuale — classe di errore già avvenuta due volte.
**Soluzione (due opzioni, 🛑 scegliere):**
- **Opzione A (consigliata, più lavoro una tantum):** estendere `New/Code/44_make_tables_tex.R` per generare frammenti *in formato paper* (i frammenti attuali sono pensati per `Tabelle_Stime.tex` e hanno commenti/layout diversi). Passi: (1) per ciascuna delle 4 tabelle a più alto rischio (tab:main, tab:stability, tab:robust, tab:depthbounds) aggiungere in `44` una funzione `write_paper_tab_XX()` che legga gli stessi CSV già usati e scriva `New/Paper/fragments/ptab_XX.tex` con l'esatto layout attuale del draft (copiare il preambolo `threeparttable` dal draft, sostituire solo i numeri con valori formattati dai CSV); (2) nel draft sostituire il corpo di ogni tabella con `\input{fragments/ptab_XX.tex}`; (3) verifica: compilare e fare diff visivo del PDF prima/dopo — le tabelle devono essere identiche carattere per carattere.
- **Opzione B (minima):** non automatizzare, ma aggiungere in `44_make_tables_tex.R` un blocco finale «check di allineamento» che legga `draft_paper.tex` con regex sui numeri chiave (coefficienti/p-value di tab:main) e stampi WARNING se differiscono dai CSV. Protegge dall'errore senza rifattorizzare.

### R2 · [W1] Riscrivere la frase sui pesi (§3.2)
**File:** `New/Paper/draft_paper.tex`, riga ~462 («…rather than every HS6–destination–year cell counting equally regardless of how much trade it represents; no weighting is by any post-treatment outcome.»).
**Sostituire l'ultima proposizione** («no weighting is by any post-treatment outcome») **con:**
> "the weights are the cell counts of underlying transactions, so the weighted cell-level regression is algebraically identical to the unweighted micro-level regression under the same fixed effects (Section~\ref{sec:strategy} verifies this to seven significant figures); weighting is thus not a modelling choice but the condition for that equivalence, and no weighting is by the outcome itself."
**Motivo:** i pesi n sono contemporanei (potenzialmente post-treatment sul margine estensivo delle imprese); la difesa corretta è l'equivalenza algebrica, non l'assenza di post-treatment weighting.
**Verifica:** rileggere il paragrafo intero per coerenza; compilare.

### R3 · [W2+N1] Footnote APEC e «one fifth»
**(a) Footnote 1** (`draft_paper.tex`, righe ~297-305). Sostituire da «The green coefficient is $+0.0050$…» fino a «…broader list.» con:
> "With the WB index the green coefficient flips sign to $+0.0050$ (s.e.\ 0.0127, $p=0.69$), with the standard error roughly doubled, as expected from an 80\% reduction in the green sample; with TREND it moves from $+0.0018$ to $+0.0032$ ($p=0.13$), same sign and similar precision. Neither is distinguishable from zero: the null is not an artifact of borderline products in the broader list."
Fonte numeri: `New/Output/TripleDiff/Tables/tripledd_collapsed_apecgreen.csv` (verificati 2026-08-18).
**(b) «one fifth»:** il rapporto vero è 0.0355/0.157 = 0.226. Sostituire «roughly one fifth» con «roughly one quarter» in: abstract (riga ~44), §4.1 (riga ~640 «about one fifth of the Brandi-equivalent effect» → «about one quarter»; e riga ~640-642 «anything above roughly a fifth» → «a quarter»), conclusione (riga ~1218-1219). Coerente con `tab_20_brandi.tex` («1/4 (WCB)»). 🛑 Se l'utente preferisce la formula prudente, in tutti i punti usare «about one quarter» — NON lasciare «one fifth» da nessuna parte.

### R4 · [W4] Separare estimando e stimatore in §3.2
**File:** `draft_paper.tex`. Due edit:
1. Riga ~452: «The parameter of interest is the average treatment effect on the treated of…» → «The target parameter is an average treatment effect on the treated: the effect of…» e, se assente, aggiungere il pedice impresa nell'estimando: usare $y_{fgpdt}$ al posto di $y_{gpdt}$ (e $y_{fnpdt}$ per i neutri), con una frase subito dopo: «The collapsed implementation aggregates the firm dimension with cell-count weights, which leaves this estimand unchanged (Section~\ref{sec:strategy}).»
2. Riga ~505-511 (il paragrafo «One qualification on the estimand is due»): cambiare l'attacco in «One qualification is due, and it concerns the \emph{estimator}, not the target: …the two-way fixed-effects \emph{coefficient} is not in general the ATT defined above…» (il resto del paragrafo resta).
**Verifica:** dopo l'edit, il testo deve dire chiaramente: target = ATT; TWFE = media pesata che in generale non coincide col target; β₁ letto come media pesata.

### R5 · [N3] Armonizzare i conteggi green a 248
Tre punti:
1. `draft_paper.tex` righe ~289-297: riscrivere il passaggio con la sequenza corretta: lista OECD CLEG (Sauvage 2014, Table A.1) = **248** codici HS2012; match col file di progetto 246/248, discrepanza = granularity split di 8714.1x in 871411+871419; traduzione a HS1996: 246 concordanze 1:1, i 2 codici dello split mantenuti all'originale e flaggati. Eliminare ogni occorrenza di «247».
2. `New/Code/05_green_goods_hs1996.R` riga ~9 (commento header «247 codici») → 248. Solo commento, nessun effetto sui numeri.
3. `New/Output/Diagnostics/43_apec_egl_subsample.md`: è un output generato — correggere invece la stringa «247» nello script generatore `New/Code/43_apec_egl_subsample.R` (cercare "247"), e rigenerare il solo `.md` su Windows al prossimo giro (non urgente: il md non entra nel paper).
**Verifica:** `grep -n "247" New/Paper/draft_paper.tex` deve restituire zero righe riferite ai green codes.

### R6 · Dichiarare (e opzionalmente testare) l'assenza di trimming
**(a) Testo (subito):** in §2.2, dopo la descrizione dell'outcome, aggiungere una frase:
> "Export values enter in logs with no trimming or winsorization; Section~\ref{sec:robust}'s inference battery, which is design-based rather than moment-based, is the primary guard against influential observations."
**(b) 🛑 Robustezza opzionale (Windows, ~1h):** se l'utente la vuole, nuovo script `New/Code/46_robustness_trim.R` ricalcato su `16_main_tripledd_collapsed.R`: identico ma con `cell <- cell[y >= quantile(y,0.01) & y <= quantile(y,0.99)]` prima delle stime (trim sull'outcome collassato, p1/p99 globali), output `tripledd_collapsed_trim.csv`; una riga in più in tab:robust o una frase nel testo. Copiare la guardia anti-dataset-stantio da `16b_dose_bins.R` (`stop()` se `max(WB_EP_Depth) != 17`).

### R7 · [N4] Citazione per le control-group batteries
**File:** `draft_paper.tex` righe ~368-370. 🛑 Due opzioni:
- **Opzione A (senza nuova citazione, zero rischio):** riscrivere «Four control-group subsamples, in the spirit of the control-group batteries used in the transaction-level trade-policy literature, each tighten…» → «Four control-group subsamples, each tightening the comparison in the direction of a specific threat:». Nessun claim di pedigree, nessuna citazione dovuta.
- **Opzione B (con citazione):** prima verificare con `/ref-verify` (o Zotero MCP) un riferimento reale che usi batterie di gruppi di controllo su dati transazionali di trade policy; candidati da vagliare: Bown \& Crowley (2013, JIE, antidumping trade-policy con gruppi di confronto), Baccini, Pinto \& Weymouth (2017, già in bibliografia). NON citare senza verifica del contenuto.

### R8 · [N6] (Opzionale) Tabella d'appendice per le stime pddt
🛑 Solo se l'utente la vuole. Aggiungere in appendice del draft una mini-tabella a 2 colonne (collapsed vs full-panel-pddt, green e dirty, coefficiente e SE) leggendo `New/Output/TripleDiff/Tables/tripledd_collapsed.csv` e `tripledd_full_pddt.csv`. Se si fa R1-Opzione A, generarla da `44_make_tables_tex.R` con lo stesso pattern.

---

## Priorità 2 — Scrittura/chiarezza richiesta dalle domande

### R10 · Comprimere §3.1 (domanda 12)
🛑 Decisione utente. Se sì: riscrivere §3.1 così — (1) apertura con l'argomento di assorbimento in 2-3 frasi: EP_dt varia a livello (d,t) e ogni FE che copre (d,t) lo assorbe per costruzione: il livello non è stimabile nel disegno principale, punto; (2) ladder ridotta a un paragrafo: «in disegni meno saturi il livello è stimabile ma il coefficiente muore monotonicamente al saturare — firma di selezione (Bertrand et al. 2004)», rimando a tab_02/appendice; (3) conservare la frase sul «first-stage bite» (serve a rispondere a un'obiezione da referee). Non toccare la sostanza, solo l'ordine e la lunghezza.

### R11 · Esplicitare il ruolo di fdt (domanda 15)
**File:** `draft_paper.tex`, dopo la frase «The firm–destination–year effects θ_fdt are the key…» (riga ~470-474). Aggiungere:
> "Mechanically, $\theta_{fdt}$ places a separate intercept on every firm–destination–year cell: any variable constant within that cell --- the agreement dummy, its overall depth, destination demand, and the non-random selection of destinations into agreements --- is perfectly collinear with those intercepts and drops out. Selection into agreements can therefore contaminate $\beta_1$ only if it operates \emph{differentially} on green versus neutral products within the same firm--destination--year, the residual threat that the destination-trend exercise of Section~\ref{sec:robust} targets."

### R12 · (On demand) Stimatore continuous-dose alla Callaway (domande 18-20)
Parcheggiato per decisione utente del 2026-08-14 (ridondante dopo 16b). Se riattivato, procedura già istruita: (1) residualizzare il contrasto green/neutro per cella rispetto a pd+pt; (2) collassare a paese-anno il contrasto residualizzato; (3) darlo a `contdid`/`did` con dose = WB_EP_Depth massima post-entrata; (4) inferenza: bootstrap a cluster di destinazione, dichiarando l'outcome generato del passo 1-2. Stima ~mezza giornata su Windows. Da presentare come appendice «continuous-dose robustness», non come nuova specifica principale.

### R13 · (Opzionale, domanda 5) Triple-diff su quantità e valore unitario
🛑 Decisione utente. Se sì (Windows): duplicare `10_collapsed_panel.R` + `16_main_tripledd_collapsed.R` con outcome `ln_export_qua` e `ln_export_value` (il collasso deve rifare la media di cella sul nuovo outcome: aggiungere le colonne al `read_fst` e al `.( )` del collasso; cache con suffisso `_qua`/`_uv`). Solo stime asintotiche + WCB (niente permutazione: costosa e il punto è la scomposizione prezzo/quantità). Presentare come tabella di scomposizione: valore ≈ quantità + prezzo.

---

## Priorità 3 — Igiene (non urgente, già tracciata)

- **R14 · Rigenerare `ppml_agg_pdt_zerofill.fst`** con `New/Code/29b_build_ppml_zerofill.R` su Windows (porta la colonna congelata `env_good` a 246; le stime non cambiano — verificato 17/08). Poi rilanciare `30_robustness_extensive_ppml.R` per conferma di invarianza.
- **R15 · Uniformare `nobs` nei CSV WCB:** in `New/Code/20_wcb_collapsed.R`, riportare in `wcb_collapsed*.csv` il numero di osservazioni post-singleton (3.681.023) o aggiungere entrambe le colonne (`nobs_pre`, `nobs_post`), per allinearsi a `tripledd_collapsed.csv`. Rigenerare i 4 CSV su Windows al prossimo giro utile (i p-value non cambiano: seeding deterministico).
- **R16 · Commit di consolidamento** 🛑: molto lavoro vive solo nel working tree su due macchine. Suggerito un commit (deciso e lanciato dall'utente) prima di ulteriori modifiche.

---

## Ordine di esecuzione suggerito
1. R2, R3, R4, R5, R11 (solo testo, nessun rischio, ~2h) → compilare → rileggere.
2. R7 (opzione A se non si vuole verificare citazioni).
3. R1 (opzione scelta dall'utente).
4. R6a subito; R6b/R8/R10/R13 su decisione utente.
5. R14, R15 al prossimo giro Windows; R16 quando l'utente vuole.
