# Code Pipeline Audit — 2026-09-05

Auditor: Claude (automated audit requested by author)

## A. MERGE DIAGNOSTICS

**[NOTE A1]** `02_build_dataset_wb_trend_merge.R` (line 280): the WB x TREND merge uses `inner_join` — rows present in only one source are silently dropped. Post-audit fix partially in place: lines 283-292 now count and print `solo_wb` / `solo_trend`, and line 292 has a `stopifnot` that the merged result is non-empty. However, no assertion checks that the number of unmatched rows is *expected* (e.g. `stopifnot(nrow(solo_wb) == 0)`). If a future data update introduces a legitimate mismatch, it passes silently.

**[NOTE A2]** `10_collapsed_panel.R` (line 55): the collapse `first(WB_EP_Depth)` assumes WB_EP_Depth is constant within (hs6, country_code, year). This is true by construction (EP varies at country x year, not product), but there is no `stopifnot` guarding it. Same pattern for `first(TREND_EP_Count)`.

**[NOTE A3]** `29b_build_ppml_zerofill.R` (lines 97-101): the zero-fill grid merge uses `all.x = TRUE` and then replaces NA with 0 for export — correct for the design. But the subsequent covariate merges (tariffs, HHI, EP, lines 107-127) also use `all.x = TRUE` with silent NA fill. Residual NA counts are printed (line 144) but not asserted against a threshold.

**[NOTE A4]** `16_main_tripledd_collapsed.R` (line 72): dirty codes are joined with `on = "hs6"` and unmatched are set to 0 — correct. But the join is left-implicit (`cell[dirty, on = "hs6", dirty_p := i.dirty_p]`) — if `dirty` had duplicate hs6 keys, this would silently pick the first. The dirty file is deduplicated by construction in `06_dirty_goods.R`, but no assertion guards this at point of use.

**[WARNING A5]** `02_build_dataset_wb_trend_merge.R` (line 297-304): the country_code merge now has a guard (lines 301-305, `stop()` if NA). Good. But the earlier WB x TREND merge (line 280) lacks a similar guard — if a country name differs between WB and TREND, it drops from the inner join without error.

## B. MISSING VALUES

**[NOTE B1]** `10_collapsed_panel.R` (line 55): `d[!is.na(ln_export), ...]` explicitly drops NA exports before collapsing. This is correct and documented.

**[NOTE B2]** `16_main_tripledd_collapsed.R` (line 82): `cell[is.na(get(DEPTH_VAR)), (DEPTH_VAR) := 0]` — NA depth is set to 0 for untreated destinations. This is the correct convention (no PTA = depth 0). Same pattern correctly repeated in 22, 23, 30.

**[NOTE B3]** `29b_build_ppml_zerofill.R` (line 138): `uv_agg := ifelse(agg_exp_qua > 0, ...)` — unit value is NA for zero-quantity cells. This is correct but the column is not used downstream (30 uses `agg_export` as the PPML outcome).

## C. VARIABLE CONSTRUCTION

**[WARNING C1]** `19_saturation_ladder.R` (lines 25-36, documented in header): uses `env_good` as-is from the .fst (frozen at dataset creation time, 238 codes), while all other scripts reclassify from `green_codes_hs1996.csv` (246 codes). The header documents this and notes the affected "Int" block is not cited in the paper. No fix needed if the "Int" block stays out of the paper, but this is a latent inconsistency.

**[WARNING C2]** `29b_build_ppml_zerofill.R` (lines 14-24, documented in header): the .fst on disk has a stale `env_good` column (238 codes). Script 30 correctly reclassifies at runtime, so results are not affected. But any future consumer reading `env_good` directly from `ppml_agg_pdt_zerofill.fst` would get the wrong classification. The header documents this as "da ricostruire in futuro".

**[NOTE C3]** `08_total_depth.R` (line 59): hardcoded year vector `year_wb` and country lists. These are structural (one entry per agreement, verified against the WB database) and validated in section 5 against the existing `Merged_TREND_WB_Indices_Only.csv`. Same pattern in `02_build_dataset_wb_trend_merge.R` (lines 193-199, 245-252). Hardcoded but verified.

**[NOTE C4]** `02_build_dataset_wb_trend_merge.R` (line 174): seven "header" rows removed by hardcoded row indices `c(1, 7, 15, 20, 22, 34, 51)`. These are section headers in the WB questionnaire. If WB_DTA.dta changes row order, these indices would silently remove wrong rows. However the file is static (one-time conversion from a fixed Excel source), so the risk is low.

**[NOTE C5]** `02_build_dataset_wb_trend_merge.R` (lines 192-216): positional alignment of `Merge_ID`, `Year_WB`, `Country_WB` to `df_wb` rows — the `WBID_ATTESI` stopifnot (line 215-216) now guards against reordering. Good.

## D. PATHS

**[NOTE D1]** `_root.do`: Stata root path hardcoded per OS (`C:\Work\projects\Paper_PTA` on Windows). Documented: "Il replicatore deve modificare SOLO questo file."

**[NOTE D2]** R scripts use `here()` throughout — relative to project root. No absolute paths in R code.

**[NOTE D3]** `run_pipeline.R` (line 40): `RSCRIPT_BIN` constructed from `R.home("bin")` — portable across installations.

## E. SEEDS

**[NOTE E1]** `22_permutation_inference.R`: Section A uses `set.seed(42)` (line 92). Section B uses `set.seed(1000L + batch_id)` per batch (line 192) — deterministic and resumable. Good.

**[WARNING E2]** WCB (wild cluster bootstrap) in R (`20_wcb_collapsed.R`, not read but referenced): per project memory (`fwildclusterboot-pwcb-not-exactly-reproducible.md`), `boottest()` uses `dqrng` which is not seeded by `set.seed()`. WCB p-values oscillate ~1pp between runs. Coefficients are deterministic. Stata's `boottest` uses `seed(42)` (line 484 of `52_omnibus_collapsed.do`) — deterministic there.

**[NOTE E3]** No other scripts involve randomness (OLS/PPML estimation is deterministic; Sun-Abraham is deterministic).

## F. PIPELINE ORDER

**[NOTE F1]** `run_pipeline.R` correctly enforces dependencies: each `run_rscript` call checks artifact existence after execution; `stata_manual` blocks with `stop()` if the required artifact is missing. The REBUILD_FST flag correctly gates the heavy steps (3, 10).

**[WARNING F2]** `run_pipeline.R` (lines 427-431): step 44 (`make_tables_tex.R`) runs BEFORE steps 69 and 70 (lines 443-449). The comment at line 439 acknowledges this: "Necessari DOPO tutti gli step Stata e PRIMA di 44_make_tables_tex.R se quest'ultimo viene rieseguito." This means on first run, 44 runs with potentially missing Stata CSVs (it handles this gracefully with `rd()` returning NULL), but step 69 (assembling Stata CSVs) runs after 44 — requiring a second run of 44 to pick up the assembled Stata output. Not a correctness issue (44 is idempotent) but the ordering is suboptimal.

**[NOTE F3]** The 4-variant matrix (excl/incl x totaldepth/desta) requires 4 separate runs with manual edits to `_sample_config.R`. `run_pipeline.R` does not automate this — documented in the config file.

## G. OUTPUT AUTOMATION

**[NOTE G1]** `44_make_tables_tex.R` reads all result CSVs and generates .tex fragments — no manual transcription. The provenance tracking (`PROV` environment, lines 104-122) logs which tables come from Stata vs R-only. Good.

**[NOTE G2]** The event study plots (23, section C) reuse coefficients from the CSV of section A of 16 — no re-estimation, only re-plotting. Good separation.

**[WARNING G3]** `44_make_tables_tex.R` (lines 145-148): FE structure and clustering level are hardcoded as string constants (`FE_FULL`, `FE_COLL`, `CLUSTER`), not read from the estimation output. The header (line 27) documents this: "NOTA ONESTA: la struttura delle fixed effects e il livello di clustering NON sono registrati nei CSV." If an estimation script changes its FE structure without updating 44, the table notes would be wrong.

**[NOTE G4]** Figures (event study plots in 16 and 23) are generated programmatically with `ggsave()`. No manual figure creation.

## H. CROSS-SOFTWARE CONSISTENCY

**[NOTE H1]** The cross-software verification campaign (steps 52-60 in `run_pipeline.R`) is comprehensive: every main result has a Stata twin. The `rd_pref()` mechanism in 44 prefers Stata output when available and logs R-only tables.

**[NOTE H2]** `52_omnibus_collapsed.do` (lines 474-479, 508-513): hardcoded FWL guard values (`-0.0045685`, `-0.0118734` for WB; `0.0018115`, `0.0003510` for TREND) with tolerance 1e-4. These are the baseline coefficients — if data changes, the guard fires. Good.

**[NOTE H3]** `16_main_tripledd_collapsed.R` (lines 96-117): Frisch-Waugh internal verification after every `feols` call, with tolerance 1e-6. Catches silent corruption from allocator crashes. Good.

**[NOTE H4]** `22_permutation_inference.R` (lines 179-189): identity permutation must reproduce the coefficients from script 16. Tolerance 1e-5. Good.

**[WARNING H5]** Sun-Abraham SE divergence (documented in 44, lines 648-654 and in 23 header): `fixest::sunab` treats cohort shares as known weights; Stata's `eventstudyinteract` estimates them. The paper correctly cites Stata. However, there is no automated comparison between the two SE vectors — the divergence magnitude ("up to 3-4x") is stated qualitatively, not checked programmatically.

**[NOTE H6]** Permutation p-values (R vs Stata): documented as not exactly reproducible due to granularity of the permutation distribution (~9 distinct EP profiles for 23 treated destinations). The paper cites Stata values. The `run_pipeline.R` comment at line 370 documents this explicitly.

## SUMMARY

| Severity | Count |
|----------|-------|
| CRITICAL | 0     |
| WARNING  | 6     |
| NOTE     | 17    |

No critical issues found. The pipeline is well-documented, with internal consistency checks (Frisch-Waugh, identity permutation, FWL guards) that exceed typical academic code quality. The main warnings concern:
- Stale `env_good` in two cached files (C1, C2) — documented, not affecting cited results
- Suboptimal ordering of step 44 vs 69-70 (F2) — cosmetic, requires double-run
- FE/clustering metadata not in CSVs (G3) — documented limitation
- WCB non-reproducibility in R (E2) — inherent to fwildclusterboot, paper cites Stata
- Sun-Abraham SE divergence not programmatically bounded (H5)
- WB x TREND merge lacks unmatched-row assertion (A5)
