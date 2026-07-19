# Piano di implementazione post-audit — per Sonnet 5 (medium)

**Origine:** audit Fable 5 del 2026-07-08 (`New/Audit/2026-07-08_audit_report.md`).
**Regole vincolanti:** modificare SOLO file in `/New`. Un solo job di stima pesante alla volta. In R: mai `Rscript -e` inline (segfault noto) — sempre file .R temporanei; mai full panel 3-HDFE in R; un `feols` per sottoprocesso `callr`. Stata in batch SOLO da PowerShell (`Start-Process ... '/e','do',...`), mai da Git Bash. Verificare ogni numero contro i CSV citati prima di scriverlo nel paper.

Ordine consigliato: A (paper) → B (stime leggere) → C (igiene). Ogni task ha il suo criterio di verifica.

---

## A. Correzioni al paper (`New/Paper/draft_paper.tex`)

### A1. [CRITICAL] Claim di magnitudine §4.1
Sostituire la frase:
> `To gauge magnitudes: a one-standard-deviation increase in WB EP depth ($\approx$ 6 provisions) is associated with at most a 1.4\% change in green relative to neutral exports at the lower 95\% bound --- economically negligible against the effects in \citet{brandi2020}.`

con (numeri verificati nell'audit: SD trattati dest-anno = 3.09; CI lower full panel = −0.0088):
> `To gauge magnitudes: a one-standard-deviation increase in WB EP depth across treated destination--years ($\approx$ 3 provisions) is associated with at most a 2.7\% decline in green relative to neutral exports at the lower 95\% confidence bound of the full-panel estimate --- economically negligible against the effects in \citet{brandi2020}.`

→ verifica: 3.09 × 0.0088 = 0.0272; il CI lower −0.0087998 è in `tripledd_full_reghdfe.csv` riga wb_green.

### A2. [CRITICAL] Sotto-indici §5.1: "249" → 223 + fatto Corea/Svizzera
Sostituire:
> `Across the 249 treated country-year observations, the two WB sub-indices with a direct mechanism (\texttt{GreenLiberalization} and \texttt{StandardsNonRegression}) are \emph{perfectly} collinear ($\rho = 1.000$): the same agreements contain both, in fixed proportion.`

con:
> `Across the 223 treated country--year observations in the estimation sample, the two WB sub-indices with a direct mechanism (\texttt{GreenLiberalization} and \texttt{StandardsNonRegression}) are \emph{perfectly} collinear ($\rho = 1.000$) --- and the reason is stark: both are non-zero in only three country--years (Korea from 2015; Switzerland from 2014), always in the same 1:3 proportion. Mechanism-bearing provisions effectively exist in two Chinese agreements of the period.`

→ verifica: già ricalcolato nell'audit (ρ=1 incl/escl HK-MO; Standards>0 solo in 133-2015, 331-2014, 331-2015).

### A3. [CRITICAL] "Caselli et al." senza riferimento (§1 e nota metodologica)
Nel §1: `three control-group subsamples in the spirit of the multiple-control-group strategy of Caselli et al.` — risolvere così:
1. Cercare in Zotero (collezione Paper_PTA, MCP `zotero_search_items`, query "Caselli") e in `wiki/` il paper inteso (probabile fonte: la struttura "stability across control groups, Table 5" citata nell'header di `New/Code/13_tripledd_stability.R`).
2. Se trovato → aggiungere bibitem completo e `\citep{...}`.
3. Se NON trovato con certezza → riscrivere senza il nome: `three control-group subsamples that probe the stability of the coefficient across comparison sets`. NON inventare la citazione.

### A4. [WARNING] Nota permutation in tab:main
Nella tablenotes di tab:main sostituire:
> `Permutation: 1{,}000 reassignments of EP profiles across treated destinations. $^{\dagger}$sign reverses at the aggregate level.`

con:
> `Permutation: 1{,}000 reassignments of entire EP profiles across treated destinations, estimated on a destination--year--product-type aggregate of the collapsed panel (observed coefficients: $-0.0052$ green, $+0.004$ dirty$^{\dagger}$); p-values are the share of placebo assignments producing a larger absolute coefficient. $^{\dagger}$sign reverses at this aggregate level.`

### A5. [WARNING] "17 vs. 8" → 17 vs 6 (nota tab:stability)
Sostituire `Deep/shallow split at the median of maximum EP depth (17 vs.\ 8 countries).` con `Deep/shallow split at the median of maximum EP depth (17 deep vs.\ 6 shallow countries in the estimation sample, which excludes Hong Kong and Macao).`

### A6. [WARNING] East Timor / "11 destinations" — dopo B3
In tab:treatment, riga ASEAN: `ASEAN--China (11 destinations)` → `ASEAN--China (10 members + Timor-Leste, see note)` e aggiungere alla tablenotes:
> `Timor-Leste is coded as an ACFTA party in the source databases although it is not an ASEAN member; it accounts for 0.02\% of observations and results are identical to the fourth decimal if it is dropped (see Section~\ref{sec:robust}).`
(La frase sui risultati va scritta SOLO dopo il check B3; se B3 dà differenze oltre la quarta cifra, adattare.)

### A7. [NOTE] Arrotondamenti in tab:robust e §5.4
- Riga `Excluding ASEAN`: `$-0.0042$ (0.031)` → `$-0.0041$ (0.031)` (CSV: −0.0041469).
- Testo §5.4: `sits between $-0.004$ and $-0.005$` → `sits between $-0.004$ and $-0.0055$`.

### A8. [NOTE] Riferimenti e coerenze minori
1. Aggiungere `\label{sec:dirty}` alla subsection `The dirty margin: anatomy of a false positive`; sostituire le due occorrenze hardcoded `Section~4.4` con `Section~\ref{sec:dirty}`.
2. Nota tab:main: `Collapsed panel: 3{,}681{,}023 cells` → `Collapsed panel: 3{,}681{,}023 cells (3{,}773{,}498 before fixed-effect singleton removal)`.
3. Abstract: `46 million` → `45.8 million` (coerente col testo).
4. §3.2: `outcome: within-cell mean of log exports, weighted by cell size` → `outcome: within-cell mean of log exports; regressions weighted by cell size`.
5. Bibliografia: `headmayer2014` e `larch2025` non sono citati. Citare `\citep{headmayer2014}` dove si introduce il panel collassato hs6-dest-anno (§3.2, struttura gravity-like) e `\citep{larch2025}` nella subsection PPML accanto a `\citep{santossilva2006}`; in alternativa eliminare le due voci. Scegliere UNA delle due opzioni e applicarla coerentemente.
6. Sotto-indici §5.1: dopo B1, se i 2 modelli enforcement risultano stimati, aggiungere una frase con i loro coefficienti; altrimenti rimuovere `enforcement` dall'elenco dei sub-indices nel §2.1.
7. Footnote metodologica al WCB (in §3.3 o nota tab:main): il WCB è calcolato su dati residualizzati via Frisch--Waugh (le FE non vengono ristimate a ogni draw; pd e dt sono annidate nel cluster, pt no — approssimazione dichiarata).

### A9. Verifica finale della bozza
Non c'è pdflatex in locale: fare solo un check statico — conteggio `\begin{...}`/`\end{...}` bilanciato, nessuna `\cite`/`\ref` orfana (grep di tutte le chiavi contro i bibitem/label), nessun `[pending]`. Compilazione vera su Overleaf a carico dell'utente.

---

## B. Stime leggere mancanti (fattibili su questa macchina)

### B1. Sotto-indici enforcement (completa la promessa del §2.1)
Rieseguire `New/Code/18_subindices_collapsed.R` così com'è: la cache RDS (`SUBIDX_*.rds`) fa saltare i 6 modelli già fatti; girano solo `WB_EnforcementDSM` e `TREND_EnforcementDSM`. Se un modello crasha, rilanciare una volta (i crash dei sottoprocessi sono ~50% random). → verifica: `subindices_collapsed.csv` passa da 24 a 32 righe.

### B2. Replica cross-language esatta del collassato (chiude lo Step 2 dell'audit)
Nuovo script `New/Code/21_collapsed_replication.do` (+ un .R di esportazione se serve):
1. In R (file .R temporaneo nello scratchpad, non inline): leggere `New/Data/Collapsed/panel_pdt_collapsed.fst`, ricostruire env_good/dirty_p/TotalDepth e le interazioni ESATTAMENTE come in `14_tripledd_collapsed.R` (righe 67-81), esportare in `New/Data/Collapsed/panel_pdt_for_stata.dta` (haven, versione ≤118) con y, n, interazioni, pd, dt, pt, country_code.
2. In Stata: `reghdfe y wb_green wb_dirty td_green td_dirty [aw=n], absorb(pd dt pt) vce(cluster country_code)`.
3. Confronto a 6 decimali con `tripledd_collapsed.csv` in `New/Audit/comparison_collapsed.md` (tabella R vs Stata). ATTENZIONE: reghdfe rimuove i singleton iterativamente, fixest una volta sola — se N differisce, annotarlo e confrontare su coefficienti (attesa differenza ≤ 1e-6 se il set di singleton coincide; altrimenti spiegare).

### B3. Diagnosi East Timor
Script leggero `New/Code/22_check_timor.R` (pattern callr non necessario: solo CSV/fst a colonne):
1. Cercare Timor in `Data/Merged/Merged_TREND_WB_FULL_NAMES.csv` (o file equivalente pre-rinomina) per capire se l'errore è nella fonte WB/TREND o nel crosswalk nomi→codici doganali di Step 1.
2. Stima di conferma sul COLLASSATO senza il codice 144 (un solo feols in sottoprocesso callr, pattern di 18): attesa variazione ≤ 4ª cifra decimale su tutti e 4 i coefficienti WB.
3. Documentare l'esito in `New/Output/Diagnostics/timor_check.md`; poi completare A6.

### B4. (Opzionale, solo se B1-B3 lisci) Figura Sun-Abraham in appendice
Aggiungere al paper un'appendice con `figures/eventstudy_sunab.png`, nota: 168 dest-anno senza celle green esclusi dal gap green; lead lontani (−13/−14) rumorosi per costruzione (poche coorti early); gap dirty a −6 positivo (p=0.001) da commentare come rumore pre-Bangkok. Se non si vuole l'appendice, rimuovere il PNG da figures/ per non lasciare file orfani.

---

## C. Igiene codice e repo

1. **19_sunab_gap.R:** rimuovere il filtro no-op `gap[entry_year != 10000L | TRUE]` (basta `data = gap` col `subset`) e il modello `m_tw` mai usato — oppure salvarne l'output accanto a sunab_gap.csv. Scegliere: rimuovere (più semplice).
2. **17_remaining_models.do:** correggere il loop di append finale (bug quoting r(601)): usare `append using "$TAB/`f'"` con forward slash o costruire la lista con percorsi completi; testare SOLO il blocco finale (i .dta esistono già, non ristimare — le `cap confirm file` proteggono).
3. **Commit:** proporre all'utente il commit di tutto `/New` + wiki (la campagna 2026-07-06/08 e la bozza non sono committate). NON committare senza conferma dell'utente.
4. **/bibcheck** sulla voce `neri2023` (CESifo WP 10436) e, già che c'è, su `larch2025` (forthcoming): verificare estremi con la skill `/bibcheck` o Zotero.
5. Aggiornare `session-log.md` e la ROADMAP (§7-R6) a fine lavoro con: audit chiuso, correzioni applicate, esiti B1-B3.

---

## Criteri di completamento complessivi

- [ ] `draft_paper.tex`: A1-A8 applicati, A9 pulito (nessuna cite/ref orfana, nessun pending)
- [ ] `subindices_collapsed.csv` con 8 sotto-indici (32 righe) e §5.1 aggiornato di conseguenza
- [ ] `New/Audit/comparison_collapsed.md` con confronto R↔Stata e spiegazione di eventuali differenze
- [ ] `New/Output/Diagnostics/timor_check.md` + A6 coerente con l'esito
- [ ] 19 e 17.do ripuliti; session-log e ROADMAP aggiornati
- [ ] Nessuna modifica fuori da `/New` (eccetto session-log.md e wiki/log.md se serve)
