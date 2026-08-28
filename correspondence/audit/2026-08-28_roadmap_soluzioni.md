# Roadmap soluzioni — audit 2026-08-28

Istruzioni operative, pensate per essere eseguite da un altro modello **senza decisioni da
prendere**. Ogni item dice: cosa fare, dove, come verificare. Regole trasversali:

- Non modificare MAI i file in `New/Output/**` né i do-file/script che hanno prodotto
  artefatti verificati. Si lavora solo su paper, figure e bibliografia.
- Ogni numero scritto nel paper deve esistere in un CSV di
  `New/Output/TripleDiff/Tables{,_Stata}/` o `New/Output/OLS/**` (regola d'oro di
  `New/Paper/GUIDA_RISCRITTURA.md` §8).
- R va lanciato con `& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' <script>` (Rscript non è nel PATH).

> **Stato: F1, F2, F3, S1 sono già stati eseguiti nella sessione di audit del 28/08
> (paper_v3).** Restano da fare: S2 (opzionale), H2, e il riordino (documento separato).

---

## F1 — Figure: rigenerare in inglese, Sun-Abraham con SE Stata ✅ fatto in v3

**Problema.** `eventstudy_sunab.png` era stato generato con gli SE di `fixest` (bande che
escludono lo zero a t=−6 e t=0 sul dirty), mentre testo e didascalia dichiarano gli SE di
`eventstudyinteract` (nessun coefficiente significativo). Entrambe le figure erano in italiano.

**Soluzione applicata.** Script `New/Paper/paper_v3/figures/make_figures_v3.R`:
- Fig. event study TWFE: legge `New/Output/TripleDiff/Tables_Stata/eventstudy_twfe_stata.csv`
  (bin ≤−6…≥+5, riferimento t=−1), bande 90/95% cluster-robuste, etichette inglesi.
- Fig. Sun–Abraham: legge `New/Output/TripleDiff/Tables_Stata/sunab_stata.csv`
  (termini `g_*`/`d_*` per spec `gap_green`/`gap_dirty`), IC 95% con **gli SE del CSV**
  (che sono quelli `eventstudyinteract`), finestra [−10,+8] come da testo, etichette inglesi.

**Verifica.** Aprire i PNG: sul pannello Sun-Abraham nessun punto in [−10,+8] deve avere
IC che esclude lo zero (spot-check: dirty t=0, coef 0,0579, se 0,0357 → IC ≈ [−0,012, +0,128]).

## F2 — Bibliografia ✅ fatto in v3

In `New/Paper/paper_v3/references.bib`:
1. `abman2024` → Journal of the European Economic Association, 22(6), 2507–2548,
   DOI `10.1093/jeea/jvae023`.
2. `morin2018` → anno **2017** (chiave invariata per non toccare i \cite).
3. `correia2017` → sostituito col working paper reghdfe: Correia, S. (2017), *Linear Models
   with High-Dimensional Fixed Effects: An Efficient and Feasible Estimator*, Working Paper.
   Aggiunta `correia2020` (ppmlhdfe, Stata Journal 20(1), 95–115) citata nella nota
   software di §Inference.
4. Aggiunta `frankel2009` (Frankel, J., 2009, *Environmental Effects of International
   Trade*, HKS Faculty Research WP RWP09-006) e sostituito il testo "(Frankel 2009)" con
   `\citep{frankel2009}`; "(Cameron, Gelbach, and Miller 2008)" → `\citep{cameron2008}`.
5. Rimossa `fischer2021` (entry con autori sbagliati, mai citata). Le altre entry non
   citate ma corrette sono state lasciate (non stampano).

**Verifica.** `biber` senza warning "entry not found"; nel PDF la voce Abman riporta JEEA.

## F3 — Refusi abstract/introduzione ✅ fatto in v3

Correzioni di ortografia/grammatica senza cambiare contenuto o tono: increasingly, against,
difficult, enforceable, contraction, "The remainder of the paper", "the null result we find
is", concordanze verbali, "somehow"→"also" (nel senso inteso). L'utente rilegge comunque.

## S1 — Riconciliazione con Zhu-Sun (2026) ✅ fatto in v3

Aggiunto in §Related Literature (paragrafo Cina) il punto che il disaccordo con Zhu-Sun è
informativo, non imbarazzante: i loro effetti emergono in disegni che identificano da
variazione *fra* imprese e destinazioni — esattamente il canale che qui θ_fdt assorbe — e il
paper documenta in proprio che passare dal confronto fra imprese (collassato) al within-firm
(full panel) riduce il coefficiente dirty di un fattore 2,7. La loro stima e questa possono
essere entrambe "vere": misurano canali diversi, e solo il within-firm è la riallocazione
che l'argomento di policy ha in mente.

## S2 — (Opzionale) letteratura 2026 marginale — APERTO

Se si vuole blindare la review: aggiungere una frase con "Greening Regional Trade Agreements
and Domestic Regulation" (Review of World Economics, 2026, DOI 10.1007/s10290-026-00638-3)
nel filone (c). Non necessario per la sottomissione.

## H2 — Colonne 2-4 di T10 in Tabelle_Stime — APERTO

I numeri delle 3 varianti di stability esistono già in
`New/Output/TripleDiff/Tables_Stata/stability_fullpanel_reghdfe_{inclHKMO,desta,inclHKMO_desta}.csv`
(24 righe ciascuno, gruppo `cem_v1` incluso). Da fare: estendere il blocco T10 di
`New/Code/44_make_tables_tex.R` perché `tab_10_stability.tex` mostri le 4 colonne
(baseline + 3 varianti) come già fanno T3/T4, poi rilanciare `44` e ricompilare
`Tabelle_Stime.tex`. **Attenzione:** usare `rd_pref()` (mai leggere i CSV `.SUPERSEDED`);
dopo il rilancio controllare che la coda di `44` stampi ancora "53/53 Stata" (diventeranno
di più) e che nessun altro `.tex` cambi (`git diff --stat` su `New/Paper/Tabelle/`).

## H3 — Riordino cartelle — piano separato

Vedi `2026-08-28_piano_riordino.md`. Da eseguire con un modello economico (Sonnet), è puro
spostamento file con checklist.
