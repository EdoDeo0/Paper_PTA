# Session Log — Paper_PTA

## 2026-06-09 — Foundational literature search

**Task:** Modified `/paper-search` skill — no date filter, sorted by citations, topic-driven queries targeting
gaps in the existing wiki (env provisions in PTAs, gravity/PPML methodology, staggered DiD, clustering/inference).

**Output:** 13 staging cards written to `~/Documents/work/research-wiki/staging/`:
- `staging/environment-trade/`: Brandi2020_EPsGreenExports ⭐⭐⭐ (closest direct precedent),
  Morin2018_TRENDDataset ⭐⭐ (TREND data reference), Morin2019_KickStartingDiffusion
- `staging/pta/`: SantosSilvaTenreyro2006_LogGravity ⭐⭐⭐, HeadMayer2014_GravityWorkhorse ⭐⭐⭐,
  Melitz2003_ImpactTrade ⭐⭐⭐, ManovaZhang2012_ExportPrices ⭐⭐
- `staging/econometrics/` (new folder): BertrandDufloMullainathan2004_TrustDiD ⭐⭐⭐,
  GoodmanBacon2021_DiDVariation ⭐⭐⭐, deChaisemartinDHaultfoeuille2020_TWFE ⭐⭐⭐,
  CameronGelbachMiller2008_Bootstrap ⭐⭐⭐, CallawaySantAnna2021_DiDMultiplePeriods ⭐⭐,
  AbadieAtheyImbensWooldridge2022_Clustering ⭐⭐⭐
- `staging/foundational-digest.md` — summary table with priority-for-promotion ranking.

**Not found in OpenAlex:** Hofmann-Osnago-Ruta (WB WP 7981), Bastiaens-Postnikov (2017).

**Pending:** promote top-priority cards (Brandi 2020, Morin 2018, Santos Silva-Tenreyro 2006,
BDM 2004, CGM 2008) to full wiki cards; then execute roadmap (Fase 1: re-cluster + wild bootstrap).

## 2026-06-08 (evening) — Results audit + roadmap

**Data checks (on actual `.fst`):** confirmed `duty` = MFN tariff, not bilateral preferential
(PTA partners show *higher*/flat duty over time, not declining → it's MFN). No import records or
processing-trade flag exist → GVC extension not feasible. `companyID` present (firm-size feasible);
rich anti-dumping module (`AD_pdt`, leads/lags) = potential confounder; `_merge` is a spurious leftover.

**Full results audit:** extracted treatment coefs from all 64 result tables (OLS+PPML × WB+TREND ×
4–5 FE × full+CEM). Findings: EP effect is a **precisely-estimated null** — sign/significance flip
across FE; "significant" results live only in the least-saturated `fpd+year` spec, which is also the
*only one clustered at `pdt`* (~2.9M clusters → inflated stars). Effect vanishes monotonically as FE
saturation rises (selection signature). PPML internally incoherent (sign flips across margins/FE);
`fpt`-only PPML is an outlier to drop. Only stable coef = the (misspecified MFN) tariff. CEM balance weak.

**Reviewed code** (`pta_functions.R`, `OLS_HDFE.R`, `CEM.R`): engineering solid; bugs noted —
PPML R² meaningless, `library(wdi)` typo, `matched_countries.csv` never written, inconsistent clustering.

**Decisions / output:**
- All future work goes in `./New/` only; originals stay read-only (copy datasets in, never touch).
- Created `./New/ROADMAP.md` — detailed, self-sufficient 5-phase plan (inference fix → preferential
  tariff → identification → alt margins → robustness) executable by a smaller model.

**Pending:** execute roadmap, starting Fase 1 pt.1 (cluster at `country_code` + wild bootstrap on
`fpt+fpd`) — the decisive test for "null vs result" paper. Framing decision deferred until after that.

## 2026-06-08

### Work Completed

**Deep methodological review** (no code changed). Read README, full pipeline (Steps 1–3), OLS/PPML/CEM scripts, result tables, and compared design vs. wiki literature (esp. Neri-Laine 2023).

Key assessment delivered to user:
- Engineering/pipeline is solid; concerns are identification & inference.
- **Main issues:** (1) EP depth bundled with overall PTA depth → level effect likely picks up "deep agreement"/selection, not env clauses; (2) inference — clustering at `pdt`/`dt` understates SEs, treatment varies across ~25 destinations → cluster at **destination** + wild bootstrap; (3) staggered timing → TWFE fragile, no event study/pre-trends; (4) weak mechanism for EPs→Chinese exports.
- **Best/most credible result:** the `× env_good` interaction (green market access) — recommend building paper around it, demote level effect.
- Answered follow-ups: tariff control is correct but must be **bilateral applied** (check what `duty` is, README labels it MFN) and only separates tariff-vs-nontariff, not env-vs-other-depth (add non-env depth control). Firm-size heterogeneity feasible (firm IDs exist); GVC needs import side or processing-trade flag — not visible in repo, must check raw `final_dataset_pta.dta`. `bec` present as partial production-stage proxy. EP-count binning OK as functional-form robustness, not main spec.

### Current State
- Review complete; no files modified. Awaiting user decision on which fixes to implement.

### Next Steps
- User to verify in raw customs file: (1) whether `duty` = MFN or bilateral applied tariff; (2) whether import records / processing-trade regime exist (for GVC).
- Candidate code changes (priority): re-cluster at destination + wild bootstrap; lead with env_good interaction; add non-env depth control; firm-size heterogeneity; event study around PTA entry.

## 2026-06-07

### Work Completed

**Batch paper-card generation** for all 9 papers in the Paper_PTA Zotero collection (key: E7ZKN9EF).

2 papers already had cards from prior sessions (skipped):
- `NeriLaine2023_DeepTradeAgreements`
- `Baccini2017_DistributionalConsequencesPTAs`

7 new paper cards written and saved to `./wiki/` and `~/Documents/work/research-wiki/papers/`:

| File | Paper |
|---|---|
| `Freund2010_RTAsThirdCountry.md` | Freund (2010), *The World Economy* — Latin American RTAs, no trade diversion, building block |
| `LeeRochaRuta2021_TradefacilitationGVC.md` | Lee, Rocha & Ruta (2021), WB WP 9674 — TF provisions, Peru EDD, GVC firms |
| `NeriOreficeRuta2021_GeorgiaRTA.md` | Neri-Laine, Orefice & Ruta (2021), WB WP 9768 — Georgian EDD, RTA depth, firm size |
| `LefebvreFernandesRocha2021_SPSTBTFirm.md` | Fernandes, Lefebvre & Rocha (2021), WB WP 9700 — SPS/TBT provisions, firm size, Chile/Colombia/Peru |
| `DechezleprêtreSato2017_EnvRegCompetitiveness.md` | Dechezleprêtre & Sato (2017), REEP — env. regulations and competitiveness review |
| `LarchShikherYotov2025_GravityRecommendations.md` | Larch, Shikher & Yotov (2025), RIE — 15 gravity estimation recommendations |
| `CrowleyHanPrayer2021_DeepPTAMarkups.md` | Crowley, Han & Prayer (2021), WB WP 9600 — deep PTAs, markups, 13-country EDD |

Also completed in earlier parts of this session (from prior context):
- `Baccini2017_DistributionalConsequencesPTAs.md` — written in previous session's continuation
- wiki `index.md` and `log.md` updated (both local and global) for all 9 papers

**Weekly paper-search** (`/paper-search` skill): Searched OpenAlex for 9 queries across topics `pta` and `environment-trade` for the period 2026-05-31 → 2026-06-07. Created 5 staging cards in `~/Documents/work/research-wiki/staging/`:
- `staging/pta/CorreiaGuimaraesZylkin2026_MLEGravityGLM.md` ⭐ PPML MLE existence
- `staging/pta/YamarikGhosh2026_RegionalIPRFDI.md`
- `staging/pta/EsquiviasEtAl2026_ACFTATradeCreation.md`
- `staging/environment-trade/MansouriTounsi2026_PollutionHavenGreenPTAs.md` ⭐⭐ directly on-topic
- `staging/environment-trade/CuiYangLong2026_EcoIndustrialParksGreenTrade.md`
- `staging/weekly-digest.md`

### Current State

- All 9 Zotero collection papers now have wiki cards
- Both local (`./wiki/`) and global (`~/Documents/work/research-wiki/papers/`) wikis are up to date
- `index.md` and `log.md` updated in both wiki locations
- PDFs cached in `/tmp/`: freund2010_thirdcountry.md, lee_tradefacilitation.md, neri2021_georgia.md, lefebvre2021_sps_tbt.md, dechezlepretre2017_envcompetitiveness.md, larch2025_gravity.md, crowley2021_deeppta_pricing.md

### Next Steps

- Review staging cards and promote any to full paper cards if needed (especially MansouriTounsi2026)
- Continue analysis pipeline as needed
