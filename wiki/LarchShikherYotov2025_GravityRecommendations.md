# Estimating Gravity Equations: Theory Implications, Econometric Developments, and Practical Recommendations

**Larch, Mario, Serge Shikher & Yoto V. Yotov (2025)**
*Review of International Economics*, 33(5):1066–1092
[Paper Link](https://doi.org/10.1111/roie.12789)

---

## Abstract

We trace the developments in the empirical trade literature to make fifteen recommendations for estimating gravity equations, which are structured in three categories: data, estimating equation, and heterogeneity. We also offer practical tips and identify areas where further research is needed. Based on these recommendations, we specify a comprehensive estimating model, which can serve as a benchmark for gravity estimations even when it is not possible to implement all of our recommendations. The proposed methods should be useful for gravity estimations beyond international trade, e.g., migration, foreign investment, cross-border patenting, and other flows.

---

## Research Question

What are the current best practices for estimating gravity equations for trade (and bilateral economic flows more broadly)? The paper surveys theoretical foundations and empirical developments to produce actionable recommendations, grounded in structural gravity theory. This is a methods paper — no new empirical application is the primary goal; Monte Carlo evidence and illustrative data from the ITPD-E database are used to motivate specific recommendations.

---

## Results (15 Recommendations)

**Data (Recommendations 1–5):**
1. Use bilateral trade data for *all* possible countries — sample restriction introduces selection bias
2. Use administrative data on nominal trade flows in common currency at delivered prices; prefer goods imports (c.i.f.) and services exports
3. Use disaggregated data — aggregate estimates mask heterogeneity and may average out important effects
4. Use panel data for consecutive years — Egger, Larch & Yotov (2022) show time-interval data biases RTA estimates
5. Include domestic trade flows — they are theoretically required, resolve several empirical puzzles, and are now available via ITPD-E

**Estimation (Recommendations 6–11):**
6. Estimate in multiplicative form with **PPML** — addresses heteroskedasticity, handles zeros, has attractive structural properties (`ppmlhdfe` recommended)
7. Use exporter(-sector)-time and importer(-sector)-time fixed effects — they control for the multilateral resistance terms (OMR and IMR) required by theory
8. Use asymmetric country-pair fixed effects — time-invariant bilateral determinants, including endogeneity of time-varying policies like RTAs
9. Model bilateral trade costs carefully — tariffs, NTMs, distance, contiguity, language; allow heterogeneous effects across country groups
10. Allow for non-discriminatory trade costs (e.g., sanctions, standards) — they affect all trade partners and must be modelled with domestic flows
11. Cluster standard errors at the country-pair level

**Heterogeneity (Recommendations 12–15):**
12. Obtain disaggregated policy estimates — aggregate tariff/RTA estimates hide sector heterogeneity
13. Allow for dynamic adjustments — phasing-in and anticipation effects of RTAs; use consecutive years not 5-year intervals
14. Consider other sources of heterogeneity — firm heterogeneity, country-specific effects, non-linear responses
15. Consider heterogeneity-robust DiD methods — staggered treatment designs require modern DiD estimators to avoid bias

---

## Data

This is a methods/survey paper. Illustrative data used:

- **ITPD-E-R02** (International Trade and Production Database for Estimation): ~250 countries, 170 industries, 1986–2019; includes both international and domestic trade flows — used to illustrate the prevalence and relevance of zero trade flows
- Monte Carlo simulations referenced across cited studies
- No original empirical application of the benchmark model to a new policy question

---

## Methodology

The paper grounds all 15 recommendations in the structural gravity model derived from Arkolakis, Costinot & Rodriguez-Clare (2012) and Anderson & van Wincoop (2003):

$$X^k_{ij,t} = \frac{Y^k_{i,t}/Y^k_t}{(\Pi^k_{i,t})^{\theta^k}} \times E^k_{j,t} (P^k_{j,t})^{\theta^k} \times (t^k_{ij,t})^{-\theta^k}$$

The three structural terms $O^k_{it}$, $D^k_{jt}$, $T^k_{ijt}$ motivate, respectively: exporter-(sector)-time FEs, importer-(sector)-time FEs, and the set of bilateral trade cost proxies. The multiplicative form of this equation motivates PPML estimation. The panel structure motivates asymmetric country-pair FEs to absorb time-invariant bilateral costs and address endogeneity of RTAs (Baier & Bergstrand 2007). Domestic trade flows are required by theory and resolve downward bias in RTA estimates that arises when only international flows are used.

The recommended benchmark specification, implementable with `ppmlhdfe` in Stata, is:

$$X^k_{ij,t} = \exp\!\left(\delta^k_{it} + \delta^k_{jt} + \delta_{ij} + \beta \cdot T^k_{ij,t}\right) + \varepsilon^k_{ij,t}$$

where $\delta^k_{it}$ and $\delta^k_{jt}$ are exporter-sector-time and importer-sector-time FEs, $\delta_{ij}$ are directional pair FEs, and $T^k_{ij,t}$ is a vector of time-varying bilateral trade costs (tariffs, RTA dummies, etc.).

---

## Relevance to Project

This paper is a key methodological reference for the project's analysis scripts (OLS_HDFE.R, PPML.R, OLS_CEM.R, PPML_CEM.R). Recommendations 4 (consecutive years), 6 (PPML), 7 (exporter-time and importer-time FEs), 8 (pair FEs), and 13 (dynamic RTA effects) are directly implemented or should be cross-checked in the project's gravity specifications. Recommendation 5 (domestic trade) is relevant when the baseline gravity model is extended.

---

### References (Wikilinks)

[[AndersonVanWincoop2003_GravityTrade]]
[[BaierBergstrand2007_RTAsPanelData]]
[[SantosSilvaTenreyro2006_PPML]]
[[CorreiaGuimaraesZylkin2020_PPMLHDFE]]
[[EatonKortum2002_TechnologyGeographyTrade]]
[[EggerLarchYotov2022_RTAsDynamics]]
[[HeadMayer2014_GravitySurvey]]
[[YotovEtAl2016_GravityGuide]]
[[ArkolakisCostinotRodriguezClare2012_NewTradeModels]]
