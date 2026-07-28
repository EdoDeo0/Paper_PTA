---
area: trade/firms
tags:
  - area/trade/firms
---

# Let's Try Next Door: Technical Barriers to Trade and Multi-Destination Firms (2018)

**Fontagné, Lionel & Gianluca Orefice (2018)**
*European Economic Review*, 101: 643–663
[Paper Link](https://doi.org/10.1016/j.euroecorev.2017.11.002) · Zotero key `A8445ICS`

---

## Abstract (paper has no formal abstract — CEPII working-paper highlights reproduced)

Stringent Technical Barriers to Trade (TBTs) drive the average firm out of the market, with a magnified effect for multi-destination players, who are encouraged to redirect their exports to other destinations free of TBT concerns. Multi-destination firms are more likely to exit as a response to a stringent TBT: by pushing multi-destination (high-productivity) firms out of the imposing market, a stringent TBT reduces the average productivity of incumbent firms — a welfare cost for the imposing country. Combining aggregate sector-destination estimations with firm-level estimations, stringent TBTs are shown to operate mainly through fixed (rather than variable) trade costs, with the trade-elasticity effect magnified in more homogeneous sectors.

---

## Research Question

Does a stringent Technical Barrier to Trade (TBT) affect exporters differently depending on how many alternative ("TBT-free") destinations they already serve? Does the option to redirect exports elsewhere ("try next door") make multi-destination firms more likely to abandon a TBT-imposing market than single-destination firms? Source of variation: WTO Specific Trade Concerns (STCs) raised against TBT measures, matched to French firm-level export declarations, exploited both as a triggering event and instrumented for endogeneity. Methodology: linear probability model (firm-product-destination-year panel) and PPML/gravity-type aggregate regressions, both with high-dimensional fixed effects.

---

## Results

Incumbent firms facing a stringent TBT in a destination are, on average, more likely to exit that market — and multi-destination firms (those already exporting the same HS4 product to many TBT-free destinations, above the 90th percentile of the 1995 destination-count distribution, i.e. 17+ destinations) are significantly more likely to exit than single-destination firms, consistent with them having an outside option. Multi-destination firms with above-median reach account for 32–33% of total French exports in 2000 and 2005, so this reallocation channel is quantitatively material. Decomposing the aggregate export response into intensive (incumbent) and extensive (entry/exit) margins shows the effect is dominated by the extensive margin, and TBTs operate mainly by raising fixed (market-entry) trade costs rather than variable costs — with the elasticity effect stronger in more homogeneous product sectors, where price competition is fiercer and non-price barriers bite harder.

---

## Data

The dataset combines French customs export declarations with a database of WTO Specific Trade Concerns on TBT measures.

- **Type:** Unbalanced panel (firm-level) and a separate aggregate sector-destination-year panel
- **Unit of observation:** Firm i (defined as legal unit × HS4 product category s) × destination j × year t, for the firm-level analysis; sector (HS4) × destination × year for the aggregate gravity-type analysis
- **Source:** French customs export data (Douanes Françaises); WTO Specific Trade Concerns (STC) database on TBT measures raised at the WTO TBT Committee; applied tariffs
- **Time period:** 1997–2007 (destination-count/multi-destination status measured as of the pre-sample year 1995)
- **Key variables:** Firm export value (log), export participation (extensive-margin dummy), export price (unit value, log), TBT dummy (STC raised on destination j at t−1), number of TBT-free destinations served by firm-product in 1995 ($k_{i,s,1995}$, multi-destination proxy), applied tariff (log), Pareto shape parameter (sector heterogeneity), domestic-only status in 1995
- **Sample restrictions:** The multi-destination indicator is fixed at its pre-sample (1995) value to avoid mechanical endogeneity with the outcome; robustness checks include HS4-destination fixed effects and instrument STC-TBT with a first-stage regression when addressing reverse causality

### Illustrative dataset structure

| Firm i (legal unit × HS4 s) | Destination j | Year t | Export value (i,j,t) | TBT dummy (j,t−1) | # TBT-free dest. 1995 ($k_{i,s,1995}$) | Multi-destination (>17) |
|---|---|---|---|---|---|---|
| F001–HS4 8501 | Brazil | 2001 | 420,000 | 1 | 22 | 1 |
| F001–HS4 8501 | Canada | 2001 | 310,000 | 0 | 22 | 1 |
| F002–HS4 8501 | Brazil | 2001 | 45,000 | 1 | 4 | 0 |
| F002–HS4 8501 | 0 (exit) | 2002 | 0 | — | 4 | 0 |

*Note: values illustrative. "Firm" is defined as the legal-unit–HS4-category combination, consistent with the paper's theoretical framework. Multi-destination status is fixed at its 1995 value and does not vary over the sample period.*

---

## Methodology

**Identification strategy.** WTO Specific Trade Concerns proxy for stringent (contested) TBT measures. The core prediction — from a heterogeneous-firm, heterogeneous-destination trade model — is that firms with more alternative destinations respond more elastically to a TBT in any one market, because they can substitute toward TBT-free destinations at lower cost. The multi-destination indicator is pre-determined (1995), so it is not mechanically driven by the contemporaneous TBT/export outcome.

**Fixed effects structure — the key point for this project.** The main firm-level specification (equation 1) includes:
$$Y_{i,j,t} = \beta_1 TBT_{j,t-1} + \beta_2 (TBT_{j,t-1} \times k_{i,s,1995}) + \beta_3 \ln(tariff_{s,j,t}+1) + \mu_i + \phi_{HS2,j,t} + \varepsilon_{i,j,t}$$

with **firm fixed effects ($\mu_i$)** to control for time-invariant firm characteristics, and **three-way HS2-sector–destination–year fixed effects ($\phi_{HS2,j,t}$)** — **`pdt` in this project's notation** — explicitly justified by the authors as controlling for "country-time-HS2-level varying factors such as business cycles, import-demand shocks and multilateral trade resistance," citing Head & Mayer (2014). In robustness, the paper also uses **firm-product-year fixed effects** (in the extensive-margin/new-destination-count specification) and, separately, **HS4-destination fixed effects** (`pd`) to control for the time-invariant sensitivity of a specific product-destination pair to being the target of a TBT complaint. The aggregate decomposition exercise (Section 5, following Berman et al. 2012) uses **country-year and product-year fixed effects** (`dt` and `pt`) in a gravity-type regression on total exports by destination-product-year.

**Why `pdt` is admissible here (relevant cross-check for Paper_PTA).** The main regressor `TBT_{j,t-1} × k_{i,s,1995}` varies at the firm level (through $k_{i,s,1995}$, a firm-specific 1995 destination count) even though the TBT dummy itself varies only at (destination, year). Because the interaction term is firm-specific, the `pdt` fixed effect does **not** absorb the coefficient of interest — it only absorbs the *level* effect of the TBT dummy, which is not separately reported as the object of interest. This is the same logic (regressor variation at the firm level) that allows Crowley, Han & Prayer (2021) to use `pdt`-type fixed effects; see [[Fixed_Effects_Guide]] §3 for why this option is *not* available in a single-origin-country design like Paper_PTA's.

**Target parameter.** $\beta_2$ is the differential effect of a TBT on multi-destination firms relative to single-destination firms, i.e. the interaction coefficient capturing the "try next door" reallocation channel.

**Threats to identification and robustness.** (i) Reverse causality — a French firm's own lobbying could trigger an EU-level STC; addressed by using EU-wide (not France-specific) STCs and by an instrumental-variable strategy in a second step (first-stage F-statistics reported, with two specifications flagged as weak, F≈7); (ii) endogenous entry/exit into "multi-destination" status — addressed by fixing $k_{i,s,1995}$ at a pre-sample year; (iii) confounding with tariff protection — addressed by including applied tariffs directly; (iv) sector-specific sensitivity to being targeted by an STC — addressed with HS4-destination fixed effects in the Online Appendix, noted to leave the main results qualitatively unchanged (though the tariff coefficient's sign flips, attributed to selection).

---

### Relevance to Paper_PTA

Verified example of `pdt` (three-way sector–destination–year fixed effects, here `φ_HS2,j,t`) used deliberately and explicitly justified by citing Head & Mayer (2014) — and a clean illustration of *why* it is admissible: the regressor of interest is firm-specific, not just destination-time-specific. This is the sharpest available contrast case for [[Fixed_Effects_Guide]] §3 ("the `pdt` trap"), alongside Crowley, Han & Prayer (2021).

---

### References (Wikilinks)

[[Arkolakis2010_MarketPenetrationCostsConsumers]]
[[Bao2012_TechnicalBarriersTradeInfluence]]
[[Bas2017_FromMicroMacroDemand]]
[[Berman2012_DifferentExportersReactExchange]]
[[Bernard2011_MultiproductFirmsTradeLiberalization]]
[[Chaney2008_DistortedGravityIntensiveExtensive]]
[[Chen2008_RegionalismStandardsGoodTrade]]
[[Chen2006_StandardsMatterExportSuccess]]
[[Disdier2015_NorthsouthStandardsHarmonizationInternational]]
[[Disdier2008_ImpactRegulationsAgriculturalTrade]]
[[Essaji2008_TechnicalRegulationsSpecializationInternational]]
[[HeadMayer2014_GravityWorkhorse]]
[[Head2014_WelfareTradeWithoutPareto]]
[[Hricourt2016_MultidestinationFirmsShapeEffect]]
[[Li2012_MetaanalysisEstimatesImpactTechnical]]
[[Mayer2008_HappyInternationalisationEuropeanFirms]]
[[Moenius2004_InformationVersusProductAdaptation]]
[[Wagner2012_GermanMultipleproductMultipledestinationExporters]]

<!-- Unresolved (3):
  - Baller, S. (2007), Trade effects of regional standards liberalization: A heterogeneous...
  - WTO (2011), The WTO and Preferential Trade Agreements: From Co-Existence to Coherence
  - WTO (2012, 2014) — annual/technical reports
-->
