# Roadmap Soluzioni — Audit referee 2026-09-07c (paper_v4)

Ogni fix è descritto in modo da poter essere eseguito da un altro modello senza decisioni da prendere. Dove serve una scelta, è già fatta e motivata. Report di riferimento: `correspondence/audit/2026-09-07c_audit_referee_report.md` (numerazione #1–#28 della sua tabella §6 richiamata qui tra parentesi).

**File di riferimento:** `New/Paper/paper_v4/paper_v4.tex` (righe sulla versione del 7/9/2026 11:33), tabelle in `New/Paper/paper_v4/Tabelle/`. CSV di verità: `New/Output/TripleDiff/Tables_Stata/` dove esiste, altrimenti i file scritti da `.do` in `Tables/` e `New/Output/OLS*/Bootstrap/`.

**Regola generale.** I blocchi A e B sono solo editing (`.tex`, `.do`, `.R`): nessuna stima. Il blocco C contiene stime **leggere sul panel collassato** (minuti, non ore) da lanciare solo dopo il via dell'utente. Il blocco D è per la sottomissione a rivista. Dopo ogni blocco: ricompilare (`pdflatex → biber → pdflatex ×2`), controllare nel log `undefined`, `multiply`, **`Float too large`** e `Overfull`.

**Ancore.** Quando si sostituisce prosa, la stringa cercata deve coprire l'intera citazione (vedi MISTAKES.md 2026-09-07): ricostruire la stringa multilinea esatta, `count == 1`, rileggere le 3-4 righe attorno al punto toccato.

---

## BLOCCO A — Critici di testo e tabelle (nessuna stima; ~1 giornata)

### FIX A1 — Tab. A9 Panel B/B′: rietichettare e riscrivere l'argomento sui trend (#2)
**Fatto verificato:** `Tables_Stata/r79c_pretrends.csv` contiene i coefficienti `ep_green`/`ep_dirty` della regressione principale sull'outcome detrendizzato `y_adj` (28.R:99-125, 63.do:592-616). Le pendenze per destinazione (`slope_g`, `slope_b`) non sono mai scritte. Le celle di Panel B/B′ sono quindi "EP × g_p / EP × b_p dopo il detrending", non pendenze.

1. `Tabelle/tab_A09_desttrends.tex`, righe 26-36: sostituire le quattro etichette `Pre-agreement slope, Green` → `EP $\times$ Green, detrended outcome` e `Pre-agreement slope, Dirty` → `EP $\times$ Dirty, detrended outcome`; le intestazioni di pannello `Panel~B --- pre-trend test (WB index)` → `Panel~B --- pre-trend-detrended outcome (WB index)` (idem B′).
2. Nella nota, sostituire `Panels~B/B$'$: destination-specific slopes estimated on pre-treatment years only and projected forward.` con:
   ```
   Panels~B/B$'$: for each destination, a linear trend of the green and dirty gaps is estimated on pre-entry years only (all years for never-treated destinations), projected over the full period and subtracted from the outcome; the rows report the EP interactions re-estimated on the detrended outcome. The pre-entry slopes themselves are not reported.
   ```
3. §5.5, righe 1075-1077. Sostituire:
   ```
   Estimating the same trends on pre-treatment years only and projecting them
   forward returns every coefficient to an imprecise zero, and the pre-agreement slope of the green gap
   is itself positive, which makes a pre-existing trend the more economical account of the first result.
   ```
   con:
   ```
   Estimating destination-specific trends on pre-treatment years only, projecting them forward and
   re-estimating on the detrended outcome leaves no coefficient significant under the bootstrap. The
   detrended dirty interaction, however, changes sign and grows to $+0.06$ per provision (bootstrap
   $p = 0.61$), which says that the pre-entry slopes extrapolated from two to four years are too
   imprecise to detrend anything: this variant bounds nothing and is reported for completeness.
   ```
4. App. G, righe 1287-1291. Sostituire:
   ```
   More directly, Panel~B$'$ shows that the green gap was already rising before entry into force
   (pre-agreement slope $+0.0073$, bootstrap $p = 0.19$), which makes a pre-existing trend the more
   economical account of the same coefficient. Under the pre-treatment-only variant every coefficient
   returns to an imprecise zero.
   ```
   con:
   ```
   Panels~B and~B$'$ re-estimate the interactions after removing destination-specific trends fitted on
   pre-entry years only. The TREND green interaction becomes $+0.0073$ (bootstrap $p = 0.19$), the WB
   dirty interaction $+0.063$ ($p = 0.61$): the detrended estimates are imprecise and, for dirty, of
   the opposite sign, because most cohorts offer only two to four pre-entry years on which to fit a
   slope. The variant therefore cannot adjudicate; the Wolfers argument above is what carries the
   reading of the full-period trend result.
   ```
5. Togliere "the only green coefficient in the robustness battery" (righe 1074 e 1282) sostituendo con `the only green coefficient among the robustness checks of Appendix~\ref{app:trends}` (riga 1074) e `The only green coefficient in this appendix that survives the wild cluster bootstrap` (riga 1282). Stessa correzione in `summary_v4.tex` riga ~242. (Chiude anche il residuo W3 del 09-07b.)
6. `New/Code/44_make_tables_tex.R:838-856, 884-897`: rinominare l'etichetta "Pendenza pre-accordo" → "EP x green/dirty su outcome detrendizzato" (solo stringa).

### FIX A2 — Correlazioni "net of FE" e VIF: dire cosa sono (#5)
**Fatto verificato:** 0.959 e 0.891 vengono da `14_descriptives_collinearity.R:58-73` (223 country-year trattati, demeaning alternato paese/anno sui livelli) e `35_desta_correlation_check.R:69-87` (212 obs); VIF 5.76 bivariato su TotalDepth. Nessuno script residualizza EP×g su fpd+fdt+pt.

1. Riga 775-779: sostituire `Net of the fixed effects, environmental depth correlates $0.959$ with World Bank non-environmental depth and $0.891$ with DESTA depth.\footnote{These are partial correlations computed on the residualized regressors, and they are therefore higher than the raw correlation of $\rho = 0.91$ across treated destination--years reported in Section~\ref{sec:strategy}. The fixed effects remove variation common to the two depth measures, which tightens what remains.}` con:
   ```
   Within destinations and years, environmental depth correlates $0.959$ with World Bank
   non-environmental depth and $0.891$ with DESTA depth.\footnote{Correlations of the two depth
   measures after removing destination and year means, computed on the treated destination--years
   (223 with the World Bank control, 212 with DESTA, which does not cover Timor-Leste). They are
   higher than the raw correlation of $\rho = 0.91$ because the demeaning removes the variation the
   two measures share across destinations. They are a property of the treatment variables, not of the
   interacted regressors of equation~\eqref{eq:main}.}
   ```
2. Riga 637: `and the VIF for EP depth in the main specification is 5.8` → `and the variance inflation factor of EP depth regressed on non-environmental depth across treated destination--years is 5.8`. Nella footnote, dopo "orthogonal design." aggiungere: `Here it is computed on the two depth variables at the destination--year level, not on the interacted regressors of the estimating equation.`

### FIX A3 — Tab. A16: numeri Stata e nota (#6; residuo W6)
1. `Tabelle/tab_A16_mde.tex` righe 12-15, sostituire le quattro righe con:
   ```
   WB & Green & 0.0070 & 4.64\% & 6.00\% & [-1.85\%, 3.18\%] \\
   WB & Dirty & 0.0030 & 1.97\% & 2.40\% & [-1.85\%, 0.17\%] \\
   TREND & Green & 0.0018 & 4.16\% & 3.60\% & [-0.17\%, 0.71\%] \\
   TREND & Dirty & 0.0016 & 3.65\% & 3.56\% & [-0.31\%, 0.56\%] \\
   ```
   (Fonte: `Tables_Stata/wcb_collapsed.csv` conf_low/conf_high; semi-ampiezza = (hi−lo)/2 × SD, SD 2.3827 / 8.1645.)
2. Nota: sostituire `Standard deviation of regressors computed on the effective sample and weighted.` con `Standard deviation of the EP indices weighted by cell size over all 3,773,498 cells of the collapsed panel, never-treated destinations included (2.38 for WB, 8.16 for TREND). Minimum detectable effect = $2.8 \times$ asymptotic standard error, which understates the true MDE by the same factor by which the bootstrap widens the interval; the bootstrap half-width column is the relevant bound.`
3. Rilanciare `Rscript New/Code/33_mde_equivalence.R` (solo lettura di CSV, secondi) così `33_mde_equivalence.md` coincide con la tabella. Questo è l'unico script R del blocco A; non stima nulla.

### FIX A4 — App. MDE, benchmark Brandi (#16; residuo C5)
Riga 1442, sostituire `For scale, \citet{brandi2020} report an effect of 0.157 log points per trade-relevant provision on the green export share in their aggregate panel; the upper bound of 3.2 percent per provision here is roughly one quarter of that figure.` con:
```
For scale, \citet{brandi2020} find that one additional liberalising environmental provision raises
the green export share by about 17 percent (0.157 log points) in their aggregate bilateral panel of
developing-country exporters. The comparison is only indicative: their unit is a liberalising
provision, ours any environmental provision category, and their outcome a share rather than a
within-firm log gap. On that reading, the full-panel upper bound of Section~\ref{sec:fullpanel},
3.5 percent per provision, is roughly one quarter of their figure and the collapsed-panel bound in
Table~\ref{tab:mde}, 3.2 percent, roughly one fifth.
```
Intro riga 126 e conclusione riga ~1125: dopo "one quarter of the benchmark magnitude in the aggregate literature" aggiungere `, on an indicative per-provision comparison`.

### FIX A5 — Magnitudini per accordo, non solo per provisione (#16)
1. Riga 128: `small in the firm-level panel` → `$-0.4$ percent per provision in the firm-level panel (about $-2.6$ percent for an agreement with the ASEAN chapter's six provisions)`.
2. Righe 817-819: dopo `...for a one-standard-deviation increase in environmental content.` aggiungere:
   ```
   Stated per agreement rather than per provision, the bound is looser: for the six provisions of the
   ASEAN chapter it is about $\pm 21$ percent, and for the sixteen-provision jump of the Korea agreement
   it exceeds $\pm 40$ percent. The null is bounded at the provision level, not at the level of a deep
   agreement.
   ```

### FIX A6 — §5.4 footnote "0.90" e riga "Cooperation" (#17; residui W9)
1. Riga 1036: `and 0.90 with World Bank non-environmental depth` → `and 0.89 with World Bank non-environmental depth within destinations and years (0.70 in the raw data)`.
2. Righe 1338-1339: eliminare `\addlinespace` e la riga `Cooperation & TREND & Information exchange, capacity building, technical assistance, joint committees \\`.
3. Riga 1032-1033: la frase `so the green--dirty differential is $+0.0017$ with $p = 0.80$` non ha script: sostituire con `so the two interactions are statistically indistinguishable`.

### FIX A7 — Tab. `mechanism`: etichette (#27)
Riga 1005: `Panel~A sums each depth index across the 25 treated destinations at each destination's maximum (post-entry) value. ``Checked'' is the total number of provision slots coded;` → `Panel~A sums each depth index across the 25 PTA destinations (including Hong Kong and Macao) at each destination's maximum value. ``Checked'' is the total number of provisions present;`. Riga 971: `checks 150 provision slots` → `records 150 provisions`. Aggiungere alla nota: `The two WB trade-mechanism sub-indices are non-zero in the same three destination--years (Korea 2015, Switzerland 2014--15), so the 8 counts one collinear pair twice.`

### FIX A8 — Tab. A13 sfora ancora (#17; residuo W15)
`Tabelle/tab_A13_robustness_full.tex`: `\footnotesize` → `\scriptsize` e aggiungere `\renewcommand{\arraystretch}{0.9}` dopo `\centering`; se non basta, `[htbp]` → `[p]`. Ricompilare e cercare `Float too large` nel log: deve sparire.

### FIX A9 — Arrotondamenti e celle (#26b)
- `tab_04_leaveoneout.tex`: Thailand col. (4) `$-0.0082^{*}$` → `$-0.0083^{*}$`.
- `tab_A07_eventstudy_twfe.tex`: `$t=1$` col. (3) `0.010` → `0.011`.
- `tab_A13_robustness_full.tex` Panel A green: `-0.00023` → `-0.00024`, `-0.00252` → `-0.00253`, `-0.00089` → `-0.00090`, `-0.00225` → `-0.00226`; Panel B common-support green `-0.00011` → `-0.00010`; green-share p `[0.043]` → `[0.044]` (fonte `Tables/tripledd_robustness_reghdfe.csv`, 18.do).
- `tab_A18_depthbounds.tex`: p `0.06` → `0.07`, `0.64` → `0.68`, `0.30` → `0.32`.
- `tab_03_collapsed.tex` Panel A p asintotici: `[0.508]` → `[0.512]`, `[0.316]` → `[0.320]`, `[0.825]` → `[0.826]`.
- `tab_A11_fullpanel_inclHKMO.tex`: sostituire i `---` dei bootstrap qua/uv con i `p_wcb` di `Tables_Stata/wcb_fullpanel_inclHKMO.csv` e `_desta.csv`.
- `tab_07_ppml.tex`: ripristinare `Tables_Stata/ppml_extensive*.csv` da git (`git checkout d761830~1 -- New/Output/TripleDiff/Tables_Stata/ppml_extensive*.csv`) e verificare le colonne (2)-(4); se i file LFS non sono recuperabili, rigenerare con `65_ppml_variants.do` (blocco C).

### FIX A10 — Note di tabella e testo sulla permutazione (#12)
1. `tab_A03_permutation.tex` nota: `randomly reassigned among the 23 treated destinations` → `randomly reassigned among the treated destinations (23 in columns 1 and 3 excluding Timor-Leste under DESTA, 22; 25 in columns 2 and 4)`. Precisamente: col. (1) 23, col. (2) 25, col. (3) 22, col. (4) 24 — verificare i due ultimi nei log `66_permutation_variants.log` prima di scrivere.
2. Riga 685-687: sostituire `The non-environmental depth control is permuted together with the environmental profile, so the null hypothesis is that, given which destinations have an agreement and when, it does not matter which agreement profile --- environmental and non-environmental content alike --- each of them received.` con `The whole time path of each profile moves with it, including entry timing and the non-environmental depth control, so the null hypothesis is that, given the set of destinations that ever sign, it does not matter which of them received which profile.` e togliere la frase ridondante alla riga 681-683 che enuncia la stessa nulla ("The null hypothesis is that, given the set of 23 treated destinations, it does not matter which destination received which EP profile: the test permutes profiles among the treated, without adding or removing destinations from the treated group.").
3. Riga 864 (`The reported $p$-value is the share of these reassignments that produce a coefficient at least as large in absolute value as the observed one.`): aggiungere subito dopo:
   ```
   The placebo distribution is not centred at zero (its mean is $+0.003$ for the dirty interaction),
   so the two-sided figure adds a right tail the alternative does not concern; the one-sided share of
   reassignments at least as negative as the observed coefficient is 0.08, and 0.17 after centring on
   the placebo mean.
   ```
   Riga 869-870 (`For the dirty coefficient the answer is that it would, 27.8 percent of the time.`): aggiungere `, or 8.1 percent counting only reassignments as negative as the estimate`.

### FIX A11 — Timing dell'entrata in vigore: dichiararlo (#9, parte testo)
1. Tab. 2, nota (riga 245): dopo `Entry-into-force years from the World Bank Deep Trade Agreements database and TREND.` aggiungere: `A destination is coded as treated from the calendar year of entry into force. Seven agreements enter in the second half of the year (Chile 1 October 2006, Pakistan 1 July 2007, New Zealand 1 October 2008, Costa Rica 1 August 2011, Iceland and Switzerland 1 July 2014, Australia and Korea 20 December 2015).`
2. §3.1, dopo la riga 260 (`There is no treatment reversal in the sample.`) aggiungere il paragrafo:
   ```
   The last two agreements deserve a caveat. Australia and Korea entered into force on 20 December
   2015, the final year of the panel, so their treated year contains eleven days of exposure; Korea's
   jump from one to seventeen provisions, the deepest chapter in the sample, is identified from that
   year alone. Appendix~\ref{app:eif} re-estimates the main specification coding these two
   destinations, and all second-half entrants, as untreated in their entry year.
   ```
   (Il riferimento `app:eif` viene creato dal FIX C1; fino ad allora lasciare il paragrafo senza l'ultima frase.)
3. §5.2 riga 887-888: dopo `they supply the variation that identifies it` aggiungere `--- and both entered into force on 20 December 2015, so the precision of the dirty estimate rests on two destinations observed for eleven days of treatment`.

### FIX A12 — Stabilità del green: qualificare (#11, parte testo)
Riga 126, `The estimate is essentially unchanged across every design we can construct from these data.` → `The point estimate stays within a band of a few tenths of a percent per provision across every design we can construct from these data, although its precision varies with which destinations are in the sample (Appendix Table~\ref{tab:loo_new}).` La tabella riceve la colonna green col FIX C2.

### FIX A13 — Event study: "flat" e coorti (#10 testo)
Riga 1056-1057: `The pre-treatment coefficients are flat on both margins` → `No pre-treatment coefficient is individually distinguishable from zero on either margin; the green leads rise from $-0.04$ at $t \le -6$ to zero at $t=-1$, and the $t \le -6$ bin is identified by the 2005 and later cohorts only, since the 2002 cohort has two pre-entry years`. In App. F (dopo la riga 1263) aggiungere: `The binary event studies do not reproduce the negative dirty interaction of the dose specification: the post-entry dirty coefficients are positive in Table~\ref{tab:eventstudy_twfe} and in the Sun--Abraham aggregate ($+0.073$). The negative dose coefficient is therefore a contrast across agreements of different depth rather than a decline after entry within agreements; Appendix~\ref{app:dose} returns to this point.` (`app:dose` creato dal FIX C3.)

### FIX A14 — Tariffe preferenziali come confondente del dirty (#15, parte testo)
App. G, riga 1375, dopo `...so the omission works against finding a positive effect rather than toward it.` aggiungere:
```
The dirty margin is exposed in the opposite direction. Pollution-intensive products --- steel,
chemicals, refined petroleum, paper --- are the sectors most often placed on partners' sensitive
lists, where tariffs fall later and less: under the ASEAN--China agreement up to 400 tariff lines per
member remained above 20 percent until 2012 and at 50 percent until 2015. If partners cut tariffs less
on dirty than on neutral products, dirty exports grow less than neutral ones after entry into force
and the dirty interaction turns negative with no role for the environmental chapter. The
non-environmental depth control cannot separate this, because within the ASEAN bloc environmental
and non-environmental depth are constant. We cannot rule this channel out with the data at hand; it
is the most economical alternative reading of the collapsed-panel dirty coefficient, and it is
consistent with the fact that the coefficient is carried by the medium-depth ASEAN cohort
(Appendix~\ref{app:dose}).
```
Conclusione, riga 1160-1161: `The preferential tariff rate was not obtainable; the headline estimates are independent of it, but the MFN rate used as a control is an imperfect proxy.` → `The preferential tariff rate was not obtainable. For the green margin its omission can only bias the estimate upward; for the dirty margin it is a genuine confounder, since the sectors we call dirty are those most often excluded from tariff liberalisation, and the negative dirty coefficient should be read with that in mind.`

### FIX A15 — Tab. 8, nota (#3, parte testo)
`Tabelle/tab_06_subindex_fullpanel.tex` riga 44: `restricted to observations with valid sub-index data.` → `; sub-indices are set to zero for destinations without an agreement.` Aggiungere: `Panel~B is estimated on the full sample of Panel~A (the DESTA restriction that drops Timor-Leste's treated observations elsewhere is not applied here; see Table~\ref{tab:fullpanel} for the 1,845-observation difference).` Dopo il FIX B3 e la ristima (blocco C), sostituire con la nota standard.

---

## BLOCCO B — Codice, senza ristime (~mezza giornata)

### FIX B1 — Guardia WBID in `02_build_dataset_wb_trend_merge.R` (#1)
Riga 214: `WBID_ATTESI <- as.double(c(8, 15, 10, 1, 9, 2, 12, 3, 4, 7, 13, 5, 6, 11))` → `WBID_ATTESI <- as.double(c(106, 90, 268, 125, 210, 85, 270, 84, 165, 162, 252, 133, 179, 249))`. Aggiungere la guardia TREND (K4 del 09-07): dopo la riga 245, `stopifnot(identical(sub("_.*", "", df_trend$Trade.Agreement), c("199","220","221","222","224","227","228","67","804","840","862","909","955","100","62")))`. Verificare che lo script giri fino in fondo (~2 min, nessuna stima) e che `Merged_TREND_WB_Indices_Only.csv` sia identico al precedente (`git diff --stat`, atteso: nessuna differenza).

### FIX B2 — `03_build_dataset_customs_merge.do` (#20)
Riga 48: `merge m:1 hs6 using "$ROOT\Data\Env_Codes_HS.dta"` → `merge m:1 hs6 using "$ROOT\Data\Env_Codes_HS.dta", keep(master match)`. Non rilanciare (18 GB): annotare nel commento di testa che la versione su disco contiene 9 righe fantasma innocue.

### FIX B3 — `68_subindices_fullpanel.do` (#3)
Riga 77: aggiungere `WB_EP_Depth` alla `use`. Righe 94-98: sostituire il ramo vuoto con `if \`drop_unmeasured' { drop if missing(\`depthvar') & WB_EP_Depth > 0 }` (stesso pattern di 17c:155). Riga 101: `merge m:1 country_code year using \`subidx', nogen` → `..., keep(master match) nogen`. Cancellare `subindices_fullpanel_desta.csv` e rigenerare (blocco C, ~30 min Stata).

### FIX B4 — `17` e `17b` leggono le variabili d'ambiente (#4)
In `17_main_tripledd_fullpanel.do` righe 35-36 e `17b_wcb_fullpanel.do` righe 41-43: sostituire l'assegnazione fissa con il blocco di 17c righe 19-25 (`local env_sample : env PTA_SAMPLE` … default `excl`/`totaldepth`). Cancellare `17b_wcb_fullpanel_desta_val.do` e `17b_desta_val_wrapper.do`. In `run_all_stata.ps1` riga 63 sostituire `19b_saturation_ladder_fullpanel.do` con `19d_ladder_tripledd_fullpanel.do`. Nessuna ristima: i CSV esistenti restano validi.

### FIX B5 — Glob di assemblaggio (#23)
- `52_omnibus_collapsed.do:380`: sostituire il glob `OMNI_*.dta` con la lista esplicita degli spec (pattern di 58:274-296) e spostare `OMNI_*_cem16_old.dta` in `New/_legacy/output_orfani/`.
- `18_robustness_fullpanel.do:272-278`: filtro `strpos(lower("\`f'"), lower("$OUTSFX.dta"))` → confronto esatto sul suffisso (`regexm("\`f'", "_(WB|TREND)_[a-z]+$OUTSFX\.dta$")`) così `_desta` non pesca `_inclHKMO_desta`. Rigenerare `tripledd_robustness_reghdfe_desta.csv` solo dall'assemblaggio (nessuna stima: le cache `.dta` esistono).
- `19b_assemble_only.do:11` e `59:152`: idem, liste esplicite.

### FIX B6 — Guardie e log (#24)
- Aggiungere in testa a 17, 17b, 17c, 18, 19c, 19d, 48f-k, 57, 58, 68, 72, 73, dopo la `use`: `qui su WB_EP_Depth, meanonly` + `assert r(max) == 17`.
- In 48e-k, 72, 73: dopo la `reg …_dm`, `assert abs(_b[wb_green_dm] - \`b_wg') < 1e-6` (pattern 17b:188-193).
- Spostare `log using` dopo il controllo cache in 17b, 65, 71, 72, 73; aggiungere `log using` in 55 e 66b.

### FIX B7 — Indici e DESTA (#18, #19)
- `02.R:376-382`: `TREND_Hard = rowSums(select(., matches("^X2_"), matches("^X5_"), matches("^X10_"), matches("^X14_")), na.rm = TRUE) - X5_01_02` (sottrarre solo la voce che si sovrappone al soft). Rilanciare 02 (blocco B1). **Attenzione:** cambia `TREND_Hard`/`TREND_Soft` → le colonne Hard/Soft di Tab. 8 e A10 vanno ristimate (blocco C; 63 blocco C ~5 min, 68 ~30 min).
- `32_desta_depth.R:40-41`: aggiungere `228` a `our_base_treaties` e togliere il duplicato `100` (tenere `62`); rimuovere il fallback che assegna Mongolia (righe 94-101) o escludere esplicitamente cc 124. Rilanciare 32 (secondi). **Attenzione:** cambia il DESTA di Singapore → tutte le varianti `_desta` cambiano al 4o decimale; da rigenerare in blocco C (collassato) e D (full panel).

### FIX B8 — CEM (#22)
- `12_cem_matching_stata.do`: dopo `cem`, aggiungere `imb gdp_growth_2000 log_gdppc_2000 mfn_tariff_2000 if cem_matched, treatment(treated)` e scrivere L1 pre/post nel `cem_v1_summary.txt`. Nel paper (§3.3, riga ~529) riportare l'L1 post.
- `52_export_collapsed_dta.R` e `62`: rilanciare (minuti) così `cem_matched` legge il CEM del 2/9; poi rigenerare in blocco C le sole righe `cem` dell'omnibus collassato (non usate dal paper: solo per coerenza dei file).

### FIX B9 — Figure e CSV senza generatore (#25)
- `make_figures_v3.R:92-104`: Korea → 2002, Laos → 2002 (definizione "primo anno con EP>0"); oppure cambiare la legenda in "year of first agreement with substantive environmental content" e lasciare 2015/2005 — scelta: **allineare a Tab. A1 (2002)** per coerenza.
- Scrivere `New/Code/71_make_figure_inputs.R` che produce `timeline_ep_data.csv` (da `Merged_TREND_WB_Indices_Only.csv`) e `green_dirty_shares_by_year.csv` (dal collassato: quota di valore green/dirty per anno × trattato, con la definizione "EP>0 nell'anno"); verificare che riproduca i CSV esistenti.
- `70_sumstats_paper.R:43-46`: aggiungere `y` e `n` del collassato alla lista; rilanciare (minuti) e verificare 8.974/9.029/1.957 e 12.13/3/43.64.

### FIX B10 — Pulizia (#27, #28)
- `11_subsamples.R`: nel paper, riga ~525, sostituire la nota su C-overlap con `In practice the restriction is non-binding: it removes 314 of 21,519,511 observations` (già quasi così) e togliere C-overlap da Tab. `samples`; oppure ridefinirlo per (hs6, year). Scelta: **toglierlo dalla tabella**, tenere la riga in A13.
- Timor-Leste: lasciare la nota esistente; correggere `_sample_config.R:30` ("Timor Est è codificato ASEAN nelle liste manuali ma non è parte dell'accordo").
- `43_apec_egl_subsample.R:52`: non scrivere su `green_codes_hs1996.csv`; spostare la colonna `apec_egl` in `05_green_goods_hs1996.R`.
- Spostare in `New/_legacy/code/`: 09, 19, 34, 39, 40, 46, 46b2, 47, 48-50 (trim). 16b resta (usato dal FIX C3).
- `renv::init(); renv::snapshot()` nella root del progetto (N7, aperto dal 5/9).
- Riga 692: `at least eight significant digits` → `at least seven significant digits`.

---

## BLOCCO C — Stime leggere sul panel collassato (solo dopo il via dell'utente)

Tutte su `collapsed_omnibus.dta` (3.7M celle, ~1.5 min per reghdfe, ~10 min per boottest 9,999). Script pronto: `New/replication/audit_2026-09-07c/replicate_collapsed_eif_variants.do` (baseline già riprodotto). Ordine consigliato per costo crescente.

### FIX C1 — Varianti sul timing (#9): nuova Appendice `app:eif`
Lanciare lo script da PowerShell (`& "C:\Program Files\StataNow19\StataSE-64.exe" /e do "…\replicate_collapsed_eif_variants.do"`, ~25 min). Produce `collapsed_eif_variants_stata.csv` con: baseline; **A1** Corea/Australia 2015 codificate ai valori pre-EIF; **A2** tutti gli ingressi in H2 codificati pre-EIF nell'anno di entrata; **B** senza 2015; **C** senza Corea e Australia; **D** dose 1 (APTA) = non trattato; **E** baseline con cluster per profilo di accordo (9 ASEAN → 1, 3 Bangkok → 1). Poi aggiungere un WCB (boottest, stesso pattern di 48g) sulle sole righe A1 e E. Scrivere `Tabelle/tab_A22_eif.tex` (6 righe × WB/TREND × green/dirty, SE, N, cluster) e un'appendice di mezza pagina che riporta i numeri senza interpretarli oltre quanto mostrano. Il testo di §3.1 (FIX A11) rimanda qui.

### FIX C2 — LOO del green in Tab. 9 (#11)
Nessuna stima: `Tables_Stata/dirty_leaveoneout*.csv` ha già `coef_green`. Aggiungere a `tab_04_leaveoneout.tex` due colonne (green, WB e DESTA control) o una seconda tabella `tab_A23_loo_green.tex` con le stesse 26 righe; nella nota: `Dropping Switzerland moves the green coefficient to −0.011 (asymptotic p < 0.001); dropping Korea or Australia to +0.003 and +0.002 with standard errors two to three times the baseline.` In §5.2 una frase che lo dica.

### FIX C3 — Dose-bins (#10): nuova Appendice `app:dose`
`dose_bins_collapsed.csv` esiste (16b, 14/8) ma senza WCB. Riprodurre in Stata (10 righe in 63.do, blocco nuovo "H": tre dummy di fascia sulla dose corrente × g_p e × b_p, `[aw=n]`, `absorb(pd dt pt)`, cluster destinazione; boottest sulle fasce bassa e media, 4 chiamate ≈ 40 min). Tabella `tab_A24_dosebins.tex` con le tre fasce, la colonna "implied by the linear fit" (−0.0046 / −0.027 / −0.055) e i p bootstrap. Testo: fascia media = ASEAN 2005 + Singapore/NZ/Islanda; l'effetto dirty lineare è un contrasto tra fasce, non una pendenza; collegamento con FIX A13 e A14. Riformulare la nota di §4.1 (righe 652-662): sostituire `Since the coefficient is near zero across specifications, the choice of weighting scheme does not affect the conclusion: a weighted average of near-zero effects stays near zero regardless of the weights.` con `For the green margin the dose-specific estimates of Appendix~\ref{app:dose} are all close to zero, so the weighting is immaterial; for the dirty margin they are not, and the linear coefficient should be read as a summary of a contrast between the medium-depth ASEAN cohort and the rest rather than as a per-provision slope.`

### FIX C4 — Ristime di conseguenza dei fix di codice
- Dopo B3: `68_subindices_fullpanel.do` variante desta (~30 min, full panel: **questa è l'unica stima full panel del blocco**; in alternativa lasciare il FIX A15 come nota permanente).
- Dopo B7 (TREND_Hard): 63 blocco C (5 min) → colonne Hard/Soft di A10; 68 (30 min) → Tab. 8.
- Dopo B7 (DESTA Singapore): 63 variante desta (blocchi A, B, E; ~30 min) → Tab. 4 Panel B, A3/A2 col. 3, Tab. 9 col. 3. Il full panel DESTA (17c, 48f) è blocco D.
- Se `Tables_Stata/ppml_extensive*.csv` non è recuperabile da git: `65_ppml_variants.do` (~20 min) → Tab. 13.

---

## BLOCCO D — Per la sottomissione a rivista

### D1 — Inferenza con pochi cluster trattati (#13, #14)
In 63 blocco B e 48g: aggiungere alle chiamate boottest le varianti `nonull` (WCU) e `weighttype(webb)`; riportare in una tabella di appendice i quattro p (WCR-Rademacher, WCU-Rademacher, WCR-Webb, cluster per profilo da C1-E) per le 8 celle di Tab. 4. Nel testo di §4.2 dichiarare "Rademacher weights, null imposed" e citare MacKinnon–Webb (2018, *Econometrics Journal*) sulla conservatività del WCR con pochi trattati. Aggiungere `mackinnon2018` a `references.bib`.

### D2 — Tariffe preferenziali (#15)
Ottenere da WITS/TRAINS (o dalle schedule ACFTA, Corea, Australia sul sito WTO RTA-IS) le tariffe preferenziali applicate dai partner alle importazioni dalla Cina per HS6 × anno; costruire `pref_tariff_pdt`; (i) aggiungerla come controllo nella riga "Additional controls" di A13; (ii) stimare la spec principale sui soli prodotti a MFN zero nel partner; (iii) escludere le linee delle Sensitive/Highly Sensitive List ACFTA. Se i dati non sono ottenibili, il paragrafo del FIX A14 resta come limite dichiarato.

### D3 — Generatore delle tabelle v4 (#8; K1)
Estendere `44_make_tables_tex.R` con una funzione per layout (6 colonne × 2 pannelli; LOO; ladder; sub-indici; MDE) che scriva direttamente in `paper_v4/Tabelle/` con i nomi attuali, leggendo `Tables_Stata/` prima e `Tables/` solo per i file scritti da `.do`. Criterio di accettazione: `git diff` sulle 27 tabelle vuoto dopo il primo run (a meno dei fix A9), poi ogni ristima propaga da sola.

### D4 — Full panel DESTA dopo B7
17c, 48f, 18, 58, 19d in variante `desta` (ore, su macchina adeguata). Fino ad allora una nota a piè di Tab. 3 Panel B: "DESTA depth for Singapore reflects the ASEAN agreement only; the 2009 bilateral is added in the collapsed-panel estimates of Section 5.2."

---

## Ordine di esecuzione e tempi

| Blocco | Contenuto | Tempo | Stime |
|---|---|---|---|
| A | 15 fix di testo/tabelle (A1-A15), ricompilazione, controllo `Float too large` | 1 giornata | nessuna (A3 rilancia 33.R: legge CSV) |
| B | 10 fix di codice (B1-B10), rilancio di 02, 32, 52/62, 70 (minuti) | mezza giornata | nessuna |
| C | varianti timing, LOO green, dose-bins, ristime di conseguenza | 2-3 ore Stata sul collassato + 30-60 min full panel opzionali | sì, **dopo via libera** |
| D | WCU/Webb, tariffe preferenziali, generatore tabelle, full panel DESTA | giorni | sì |

**Dopo A+B il paper è circolabile** con le appendici `app:eif` e `app:dose` promesse dal testo ma ancora vuote: se C non viene fatto subito, togliere i due rimandi dai FIX A11 e A13 e lasciare le frasi senza riferimento.

**Dopo A+B+C** il paper risponde ai quattro punti principali del referee (timing, dose, LOO green, permutazione) con evidenza propria; D1 e D2 sono ciò che un referee di rivista top chiederebbe comunque in prima tornata.
