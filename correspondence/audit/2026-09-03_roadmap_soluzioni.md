# Roadmap Soluzioni — Audit Paper_PTA (New/)

**Data:** 2026-09-03  
**Riferimento:** `2026-09-03_audit_report.md` (C1–C4, W1–W18)  
**Regola:** ogni soluzione è autocontenuta — un modello può implementarla senza leggere altro.

---

## C1 — `run_pipeline.R` rotto allo step 12 (CEM cancellato)

**Problema:** `12_cem_matching.R` è stato cancellato (`git status: D`). Lo step 12 di `run_pipeline.R` chiama `source("New/Code/12_cem_matching.R")` e fallisce con `stop()`.

**Soluzione:** In `New/Code/run_pipeline.R`, sostituire lo step 12 con un lancio Stata di `12_cem_matching_stata.do`, usando lo stesso pattern degli altri step Stata già presenti nel pipeline (es. step 17).

```r
# Step 12 — CEM matching (Stata)
run_stata_step(
  do_file  = here("New", "Code", "stata", "12_cem_matching_stata.do"),
  log_file = here("New", "Output", "12_cem_matching_stata.log"),
  step     = 12,
  desc     = "CEM matching (Stata)"
)
```

Se `run_stata_step()` non esiste nel pipeline, copiare il pattern usato per gli altri step Stata (cerca `cmd_hint` nel file — lo step Stata è un `system2()` o un messaggio manuale). Se il pipeline è tutto manuale per Stata (solo `cmd_hint`), trasformare lo step 12 nello stesso formato `cmd_hint`:

```r
# Step 12
cat(">>> Step 12: run manually in Stata:\n")
cat("  do \"New/Code/stata/12_cem_matching_stata.do\"\n")
```

**Verifica:** eseguire `run_pipeline.R` — lo step 12 non deve più dare errore.

---

## C2 — Merge Stata senza diagnostica (`03_build_dataset_customs_merge.do`)

**Problema:** Le righe 51–54 e 57–62 di `03` fanno `merge` e poi `drop _merge` senza nessun check.

**Soluzione:** Dopo ogni `merge`, aggiungere prima del `drop _merge`:

```stata
* --- Merge 1: EP index ---
merge m:1 country_code year using "`ep_index'", nogen
* ↑ cambiare nogen in gen(_merge) se non già presente, oppure tenere _merge

* Aggiungere PRIMA del drop _merge:
tab _merge
count if _merge == 1
local unmatched_master = r(N)
count if _merge == 2
local unmatched_using = r(N)
di "Merge EP: unmatched master = `unmatched_master', unmatched using = `unmatched_using'"
assert `unmatched_using' == 0   // nessuna riga EP deve restare senza match
drop _merge
```

Ripetere lo stesso blocco per il secondo merge (green goods, riga 57). Per il merge green goods il comportamento atteso è diverso: molti prodotti nel master non hanno match (non sono green), quindi l'assert giusto è:

```stata
assert `unmatched_using' == 0   // ogni green code deve trovare almeno un hs6 nel panel
```

**Verifica:** rieseguire `03` — il log deve mostrare i conteggi e nessun assert deve fallire.

---

## C3 — Step 69 e 70 assenti dal pipeline

**Problema:** `69_assemble_stata_csvs.R` e `70_sumstats_paper.R` producono output necessari al paper ma non sono nel pipeline.

**Soluzione:** In `run_pipeline.R`, aggiungere dopo l'ultimo step esistente:

```r
# Step 69 — Assemble Stata CSVs
source(here("New", "Code", "69_assemble_stata_csvs.R"))

# Step 70 — Summary statistics for paper
source(here("New", "Code", "70_sumstats_paper.R"))
```

Posizionarli **dopo** tutti gli step Stata (perché 69 legge i CSV prodotti da Stata) e **prima** di `44_make_tables_tex.R` (perché 44 ha bisogno dei CSV riassemblati da 69).

**Verifica:** `run_pipeline.R` deve completare senza errori fino allo step 70.

---

## C4 — `tab_02_ladder.tex` senza script generatore

**Problema:** Il file `New/Paper/paper_v3/Tabelle/tab_02_ladder.tex` non è prodotto da nessuno script. Sembra editato a mano.

**Soluzione (opzione A — minima):** Aggiungere un header di provenance al file:

```latex
% tab_02_ladder.tex
% Fonte: costruita manualmente da output di 19c_saturation_ladder_fullpanel.do
%        e 19_saturation_ladder.R (sezione collapsed)
% Ultima verifica: 2026-09-03
```

**Soluzione (opzione B — completa):** Aggiungere un blocco a `44_make_tables_tex.R` che genera `tab_02_ladder.tex` dai CSV della saturation ladder. I CSV sorgente sono:

- `New/Output/TripleDiff/Tables/saturation_ladder_collapsed.csv` (R)
- `New/Output/TripleDiff/Tables_Stata/saturation_ladder_fullpanel_reghdfe.csv` (Stata)

Il blocco deve leggere i CSV, estrarre coefficienti/SE/stelle per ogni riga della ladder (4 strutture FE × 2 indici × 2 coefficienti), e formattare il LaTeX nello stesso stile degli altri `tab_*.tex`.

**Verifica (A):** il header nel file documenta la fonte.  
**Verifica (B):** cancellare `tab_02_ladder.tex`, rieseguire `44`, verificare che il file ricreato sia identico.

---

## W1 — `set varabbrev off` mancante in `01` e `03`

**Soluzione:** Aggiungere come prima riga eseguibile (dopo i path) in entrambi i file:

```stata
set varabbrev off
```

In `01_wb_dataset_conversion.do` (dopo la riga `cd`), e in `03_build_dataset_customs_merge.do` (dopo la riga `cd`).

---

## W2 — CEM diagnostic morto in `12_cem_matching_stata.do`

Tre fix puntuali:

1. **Conteggio drop (righe 61–63):** prima del `drop if missing(...)`, salvare `_N`:
   ```stata
   local pre_drop = _N
   drop if missing(gdp_growth) | missing(ln_gdp_pc) | missing(mfn_tariff)
   di "Dropped `=`pre_drop'-_N' obs with missing covariates (from `pre_drop' to `=_N')"
   ```

2. **Diagnostic morto (righe 66–67):** `r(N) - r(N) + _N` è un no-op. Sostituire con:
   ```stata
   di "Obs after drop: `=_N'"
   ```

3. **Sanity check non enforced (righe 104–110):** cambiare il `di` warning in un `assert` soft:
   ```stata
   count if cem_treated == 1
   local n_treated = r(N)
   count if cem_treated == 0
   local n_control = r(N)
   if `n_treated' != 19 | `n_control' < 30 {
       di as error "WARNING: CEM yielded `n_treated' treated, `n_control' controls (expected 19/~40)"
       // Non exit, ma logga l'avviso. Se si vuole enforced: exit 9
   }
   ```

---

## W3 — Script morto `19b_saturation_ladder_fullpanel.do`

**Soluzione:** Rinominare a `19b_saturation_ladder_fullpanel.do.ARCHIVED` oppure spostare in una cartella `New/Code/stata/archive/`. Non cancellare (potrebbe servire come riferimento).

---

## W4 — `PTA_DEPTH` validato ma ignorato in `19c`

**Problema:** Lo script accetta `$PTA_DEPTH` (totaldepth/desta) ma poi usa sempre `TotalDepth_nonEnv` indipendentemente dal valore.

**Soluzione:** Alla riga dove viene definita la variabile depth nel `reghdfe`, sostituire il nome hardcoded con una local che riflette `$PTA_DEPTH`:

```stata
if "$PTA_DEPTH" == "desta" {
    local depth_var "desta_depth"
}
else {
    local depth_var "TotalDepth_nonEnv"
}
```

Poi usare `` `depth_var' `` nel `reghdfe` al posto di `TotalDepth_nonEnv`. Stesso pattern usato in `52_omnibus_collapsed.do` (controllare come 52 gestisce la scelta depth per copiare il pattern esatto).

Aggiungere anche il suffisso corretto all'output:

```stata
local outsfx ""
if "$PTA_SAMPLE" == "incl" local outsfx "`outsfx'_inclHKMO"
if "$PTA_DEPTH" == "desta" local outsfx "`outsfx'_desta"
```

---

## W5 — FW guard mancante in 5 script R

**Problema:** `16b`, `20`, `20b`, `24`, `25` non hanno il guard Frisch-Waugh che verifica che i coefficienti post-demeaning siano identici a quelli del `feols()` diretto.

**Template da copiare** (da `22_permutation_inference.R`, righe 187–188):

```r
# Dopo aver stimato il modello FWL:
b_fwl <- coef(lm_fwl)[c("ep_green", "ep_dirty")]
b_direct <- coef(feols_direct)[c("ep_green", "ep_dirty")]
if (max(abs(b_fwl - b_direct)) > 1e-6) {
  stop("FW GUARD FAILED: max |delta| = ", max(abs(b_fwl - b_direct)))
}
```

Per ogni script:
- **`16b_dose_bins.R`**: aggiungere dopo ogni `feols()` un check che il coefficiente di `ep_green` e `ep_dirty` non sia `NA` e che il modello converga.
- **`20_wcb_collapsed.R`**: dopo `lm()` sui dati demeaned, confrontare i coefficienti con quelli del `feols()` diretto. Il `cat()` esistente va sostituito con `stop()`.
- **`20b_wcb_regulatoryspace.R`**: idem come 20.
- **`24_stability_controlgroups.R`**: aggiungere guard FW dopo ogni `feols()` nel loop di subsamples.
- **`25_heterogeneity_subindices.R`**: idem, dopo ogni `feols()`.

---

## W6 — FW guard incompleto in `28_robustness_desttrends_pre.R`

**Problema:** Solo `ep_green` viene verificato, `ep_dirty` no.

**Soluzione:** Nella riga del guard, aggiungere anche `ep_dirty`:

```r
# Trovare la riga tipo:
# stopifnot(abs(b_fwl["ep_green"] - b_ref["ep_green"]) < 1e-6)
# Sostituire con:
stopifnot(
  abs(b_fwl["ep_green"] - b_ref["ep_green"]) < 1e-6,
  abs(b_fwl["ep_dirty"] - b_ref["ep_dirty"]) < 1e-6
)
```

---

## W7 — FW guard mancante in 2 script Stata

**`57_wcb_ladder_fullpanel.do`:** Dopo il `regress` (FWL step), aggiungere:

```stata
* FW guard
local b_fwl_green = _b[ep_green]
local b_fwl_dirty = _b[ep_dirty]
* Confronta con il reghdfe diretto (eseguito prima)
assert abs(`b_fwl_green' - `b_direct_green') < 1e-6
assert abs(`b_fwl_dirty' - `b_direct_dirty') < 1e-6
```

Dove `b_direct_green` e `b_direct_dirty` sono salvati dal `reghdfe` precedente. Stesso pattern in `52_omnibus_collapsed.do` sezione S3.

**`63_variants_collapsed.do` blocchi B/D/F/G:** Stesso pattern — dopo ogni FWL `regress`, confrontare con il coefficiente del blocco A (che è il `reghdfe` diretto).

---

## W8 — `dqrng::dqset.seed(42)` mancante in 3 script WCB

**File:** `27_robustness_desttrends_wcb.R`, `28_robustness_desttrends_pre.R`, `29_robustness_co2intensity.R`.

**Soluzione:** In ciascun file, subito dopo la riga `set.seed(42)`, aggiungere:

```r
dqrng::dqset.seed(42)
```

Se `dqrng` non è in `library()`, aggiungere anche `library(dqrng)` in testa.

---

## W9 — Cache `run_block()` basata solo sul nome file

**Problema:** Se modifichi la formula senza rinominare il blocco, il vecchio `.rds` viene restituito.

**Soluzione (minima, non intrusiva):** In `pta_functions.R`, funzione `run_block()`, aggiungere un hash della formula al nome del file cache:

```r
run_block <- function(block_name, formulas, ...) {
  # Calcola hash delle formule
  formula_hash <- substr(digest::digest(formulas), 1, 8)
  cache_file <- paste0(block_name, "_", formula_hash, ".rds")
  # ... resto della logica
}
```

**Attenzione:** questo invalida tutte le cache esistenti (file rinominati). Rieseguire i modelli dopo la modifica. Alternativa meno intrusiva: aggiungere solo un check — se il `.rds` esiste, leggere la formula salvata e confrontarla con quella passata, emettendo `warning()` se diverse.

---

## W10 — Report `.md` diagnostici non suffissati per variante

**File:** Script 33–43 (R).

**Soluzione:** In ogni script che scrive un `.md` diagnostico, sostituire il path bare con `out_path()`:

```r
# Prima:
writeLines(report, here("New", "Output", "Diagnostics", "33_diagnostic.md"))

# Dopo:
writeLines(report, out_path(here("New", "Output", "Diagnostics"), "33_diagnostic", ".md"))
```

`out_path()` è già definito in `_sample_config.R` e aggiunge automaticamente il suffisso variante.

---

## W11 — Citazione selettiva p-value nella saturation ladder

**Problema:** Il testo cita solo il WB p=0.09 nella riga fpt+pd, ma TREND è `**` (p<0.05) nella stessa riga.

**Soluzione:** Nel paper (`paper_v3.tex`), dove si cita il p-value della ladder, modificare da:

> "nominally significant in exactly one structure (p = 0.09)"

a qualcosa come:

> "nominally significant in the fpt+pd structure — p = 0.09 for WB, p < 0.05 for TREND"

oppure:

> "both EP indices are nominally significant in the fpt+pd structure (WB p = 0.09, TREND p < 0.05), but not in the saturated structures"

---

## W12 — Lettura dose-response LOO non discussa

**Problema:** Il paper interpreta la fragilità LOO di Australia e South Korea solo come "pochi cluster / precisione sottile", senza notare che sono anche le destinazioni con EP depth massimo.

**Soluzione:** Nella sezione 5.4 (LOO), aggiungere una frase tipo:

> "Australia and South Korea are also among the destinations with the deepest EP coverage (WB depth 12 and 17, respectively). An alternative reading of their pivotal role is that the dirty margin effect is driven primarily by the highest-dose observations — a dose-response pattern consistent with a genuine but under-powered effect, rather than pure noise. These two interpretations are not mutually exclusive."

---

## W13 — Nessun framework test multipli

**Soluzione:** Aggiungere un paragrafo nella sezione robustness (o in appendice) che discuta il tasso di falsa scoperta:

> "Across the full robustness battery, we report approximately 40 hypothesis tests. Under a global null with independent tests, we would expect ~2 rejections at the 5% level. The RegulatorySpace signal (WCB p = 0.046) is the only robust significance we detect — consistent with, but not distinguishable from, expected false-discovery rates. We note this limitation without discounting the finding, which is internally consistent across WB and TREND indices."

---

## W14 — "Content, not chapters" formulato come causale

**Soluzione:** Nella conclusione (`paper_v3.tex`, righe ~1392–1398), aggiungere una frase di umiltà:

> "This inference rests on descriptive counting of provision types (Table 6) and the aggregate null, rather than on a within-sample causal contrast between mechanism-bearing and cooperation-only provisions — a test that the collinearity structure of Chinese EPs, documented in Section 5.6, does not allow."

---

## W15 — 26 file `.do` con path hardcoded

**Soluzione:** Creare un file `New/Code/stata/_root.do`:

```stata
* _root.do — impostare qui il path del progetto
* Il replicatore deve modificare SOLO questo file.

if c(os) == "MacOSX" {
    global ROOT "~/Documents/work/projects/Paper_PTA"
}
else if c(os) == "Unix" {
    global ROOT "~/work/projects/Paper_PTA"
}
else {
    global ROOT "C:/Work/projects/Paper_PTA"
}

cd "$ROOT"
set varabbrev off
```

In ogni `.do`, sostituire il blocco di path detection (tipicamente righe 30–45) con:

```stata
do "New/Code/stata/_root.do"
```

I 26 file coinvolti sono elencati nel report di replicabilità (sezione 2, tabella completa).

---

## W16 — `ptab_*.tex` manuali, non tracciate

**Soluzione (minima):** Aggiungere un header in ogni `ptab_*.tex` che documenti da quali `tab_*.tex` è composto:

```latex
% ptab_main.tex — composite table for paper
% Built manually from: tab_01_baseline.tex (cols 1-4), tab_03_wcb.tex (cols 5-6)
% To rebuild: copy relevant columns from the tab_* sources after re-running 44_make_tables_tex.R
% Last verified: 2026-09-03
```

**Soluzione (completa):** Aggiungere una funzione a `44_make_tables_tex.R` che genera i `ptab_*.tex` automaticamente combinando i `tab_*.tex` appropriati. Questo richiede di definire la mappa:

```r
ptab_map <- list(
  ptab_main = list(
    sources = c("tab_01", "tab_03"),
    cols    = list(c(1:4), c(1:2))
  ),
  ptab_pddt = list(
    sources = c("tab_04"),
    cols    = list(c(1:4))
  )
  # ... completare per tutti i 5 ptab_*
)
```

---

## W17 — 12 script QA Stata non nel pipeline

**Soluzione:** Aggiungere a `run_pipeline.R` una sezione finale commentata:

```r
# ============================================================
# QA / Cross-software verification (not required for replication)
# Run these manually after the core pipeline to verify R↔Stata agreement.
#
# Stata scripts (run in Stata):
#   59_leaveoneout_collapsed.do
#   61_secondary_wcb_collapsed.do
#   63_variants_collapsed.do
#   65_ppml_variants.do
#   66_permutation_variants.do
#   66b_permutation_chunk.do
#   68_treatment_map.do
#
# R verification:
#   source(here("New", "Code", "67_verify_stata_coverage.R"))
# ============================================================
```

---

## W18 — Variant coordination Stata manuale

**Soluzione:** Usare il `_root.do` creato per W15 e aggiungere le global di variante:

```stata
* In _root.do, aggiungere:
global PTA_SAMPLE "excl"       // "excl" o "incl" (HK/Macao)
global PTA_DEPTH  "totaldepth" // "totaldepth" o "desta"

* Suffisso output
global OUTSFX ""
if "$PTA_SAMPLE" == "incl" global OUTSFX "${OUTSFX}_inclHKMO"
if "$PTA_DEPTH" == "desta"  global OUTSFX "${OUTSFX}_desta"
```

Ogni `.do` che oggi definisce `$PTA_SAMPLE` / `$PTA_DEPTH` internamente deve rimuovere quella definizione e usare le global da `_root.do`. Il replicatore modifica un solo file per cambiare variante.

---

## Ordine di implementazione suggerito

| Priorità | Issue | Tempo stimato | Note |
|-----------|-------|---------------|------|
| 1 | W15 + W18 | 1h | Creare `_root.do`, poi aggiornare tutti i `.do` in batch |
| 2 | C1 | 5 min | Una riga in `run_pipeline.R` |
| 3 | C2 | 15 min | 2 blocchi di assert in `03.do` |
| 4 | C3 | 5 min | 2 righe in `run_pipeline.R` |
| 5 | W5 + W6 + W7 | 30 min | Copiare il template FW guard in 8 file |
| 6 | W8 | 5 min | Una riga in 3 file |
| 7 | C4 | 30 min (opz. A: 2 min) | Opzione A: header. Opzione B: nuovo blocco in `44` |
| 8 | W11–W14 | 30 min | Modifiche al paper, solo testo |
| 9 | W1–W4, W9, W10, W16, W17 | 1h | Cleanup e documentazione |
