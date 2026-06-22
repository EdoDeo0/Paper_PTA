# The Effectiveness of Environmental Provisions in Regional Trade Agreements

**Abman, Lundberg & Ruta (2024)**
*Journal of the European Economic Association*, 22(6), 2507–2548
[Paper Link](https://doi.org/10.1093/jeea/jvae023)

---

## Abstract (synthesis)

The paper provides plausibly causal evidence that environmental provisions in RTAs are effective in limiting deforestation following the entry into force of trade agreements. Using high-resolution satellite data on forest loss combined with detailed data on the content of RTAs, the authors show that RTAs without environmental provisions increase forest loss, while the inclusion of specific provisions aimed at protecting forests and/or biodiversity entirely offsets the net increase. The provisions limit agricultural land expansion but do not fully offset increases in total agricultural production. Effects are concentrated in tropical, developing countries with greater biodiversity.

---

## Research Question

Do environmental provisions in RTAs have measurable effects on environmental outcomes — specifically deforestation — or are they cheap talk? Variation: staggered entry into force of RTAs (with vs. without forest/biodiversity provisions) across countries and time, with satellite-measured forest loss as the outcome.

---

## Results

- RTAs **without** relevant environmental provisions are followed by net increases in forest loss (consistent with Abman & Lundberg 2020, JAERE).
- RTAs **with** forest/biodiversity-specific provisions show **no net increase**: the provisions fully offset the deforestation effect of trade liberalization.
- Mechanism: provisions restrict agricultural land expansion (extensive margin of land use), though total agricultural output still rises.
- Heterogeneity: effects concentrated in tropical, biodiverse, developing countries — where both the deforestation risk and the bite of provisions are largest.

---

## Data

- Satellite-based forest loss (Hansen Global Forest Change-type data), gridded/cell-level panel.
- Content of trade agreements: detailed coding of environmental provisions (World Bank Deep Trade Agreements / TREND-type provision data), distinguishing provisions specifically targeting forests and biodiversity from generic environmental language.
- Global sample of RTAs entering into force during the satellite era.

---

## Methodology

- Staggered difference-in-differences / event-study designs around RTA entry into force, comparing RTAs with and without forest/biodiversity provisions.
- Selection into provisions addressed explicitly (countries that include provisions may differ): pre-trend analysis and robustness to modern staggered-DiD estimators.
- The "treatment contrast" is *within* the set of RTA signers — agreements with vs. without specific provisions — which is the same logic as separating EP content from PTA entry.

---

## Relevance to Paper_PTA

**The closest top-journal precedent and the key benchmark to cite and differentiate from.**

1. It legitimizes the core question (do EPs have real effects?) at the JEEA level.
2. Its outcome is *land use*, not trade flows; its unit is *grid cells*, not firms. Paper_PTA's comparative advantage: **firm-level trade outcomes** (composition, margins, within-firm reallocation) — a margin ALR cannot observe.
3. Methodological lesson: the credible contrast is **EP content conditional on having an agreement**, not EP depth vs. no agreement — exactly the logic of our triple-diff with `fdt` fixed effects and of the permutation test (reshuffling EP content across agreements at fixed PTA timing).
4. Framing lesson: a well-identified effect (or null) of *specific* provisions beats a diffuse depth-index level effect.

---

### References (Wikilinks)

[[Brandi2020_EPsGreenExports]]
[[Shapiro2021_EnvironmentalBiasTradePolicy]]
[[CopelandShapiroTaylor2022_GlobalizationEnvironment]]
[[LarchShikherYotov2025_GravityRecommendations]]
