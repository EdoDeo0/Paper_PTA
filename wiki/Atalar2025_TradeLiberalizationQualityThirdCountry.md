---
area: trade/firms
tags:
  - area/trade/firms
---

# Trade Liberalization and Quality Upgrading: Third-Country Effects (2025)

**Atalar, Deniz (2025)**
*Journal of Development Economics*, 177: 103548
[Paper Link](https://doi.org/10.1016/j.jdeveco.2025.103548) · Zotero key `WEAFSXE5`

---

## Abstract (synthesis — paywalled, no verbatim abstract or fulltext accessible)

The paper studies how trade liberalization between two countries affects firms in a third country that is not party to the agreement but previously benefited from the trade barrier. When a country eliminates a trade barrier with a low-quality-goods producer, competing exporters from third countries face intensified competitive pressure in the liberalizing market. Mid-productivity third-country firms respond by upgrading export quality to differentiate themselves from the newly-unconstrained low-cost competitor, rather than competing directly on price. The empirical test uses the elimination of EU import quotas on China in 2005 and its effect on Turkish exporters to the EU: following the quota removal, mid-productivity Turkish firms raised the unit value of their EU-bound exports by about 8 percentage points more than a control group, alongside a 3 percentage point rise in the unit value of their imported inputs — consistent with quality upgrading via imported-input quality rather than pure price competition.

---

## Research Question

Does trade liberalization between two countries (here, the EU's 2005 removal of import quotas on China) generate spillover effects on the export quality of firms in an unrelated third country (Turkey) that compete in the same destination market? Source of variation: the 2005 EU-China quota elimination as a quasi-natural experiment, exploited via a triple-difference design comparing Turkish firms' EU-bound export quality before/after 2005, across products more/less exposed to Chinese competition, and across firms of different initial productivity. Methodology: firm-product-destination-year customs panel with high-dimensional fixed effects (Turkish exporter data, 2002–2009).

---

## Results

Following the 2005 EU-China quota removal, mid-productivity Turkish firms increased the unit value (a standard proxy for quality) of products exported to the EU by roughly 8 percentage points more than firms in a less-exposed control group. This quality upgrading is accompanied by a roughly 3 percentage point increase in the unit value of the firms' imported inputs, consistent with the mechanism operating through imported-input quality rather than through markup adjustment alone. The response is concentrated among mid-productivity firms — the theoretical model predicts and the data confirm that the very lowest- and highest-productivity firms have comparatively little scope to reposition via quality, while mid-productivity firms have the most to gain from differentiating away from the newly price-competitive Chinese varieties.

---

## Data

The dataset is Turkish customs export data merged with balance-sheet information, disaggregated at the firm-product-destination-year level.

- **Type:** Unbalanced panel
- **Unit of observation:** Firm × HS product × destination country × year
- **Source:** Turkish customs transaction data (exporting firm, product, destination, year), merged with firm balance-sheet data (labor, value added)
- **Time period:** 2002–2009 (spans the 2005 EU-China quota removal)
- **Key variables:** Export unit value (quality/price proxy), imported-input unit value, an exposure measure to Chinese competition (product-level), firm productivity (pre-2005), destination (EU vs. other)
- **Sample restrictions:** The empirical design is a triple difference — before/after 2005 × EU vs. other destinations × high/low exposure to Chinese competition in the product — used to isolate the third-country spillover from confounding trends

### Illustrative dataset structure

| Firm f | HS product p | Destination j | Year t | Export unit value (f,p,j,t) | China exposure (p) | Firm productivity tercile |
|---|---|---|---|---|---|---|
| F001 | 620462 (trousers) | Germany | 2004 | 4.10 | high | mid |
| F001 | 620462 (trousers) | Germany | 2006 | 4.55 | high | mid |
| F002 | 620462 (trousers) | USA | 2006 | 4.05 | high | mid |
| F003 | 851712 (phones) | France | 2006 | 22.30 | low | high |

*Note: values illustrative — the paper's underlying micro-data are not directly accessible; structure inferred from the described triple-difference design (before/after 2005 × destination × exposure).*

---

## Methodology

**Identification strategy.** The 2005 EU removal of import quotas on Chinese goods is treated as an exogenous shock to competitive pressure faced by third-country (Turkish) exporters selling into the EU. Because the shock affects Turkish firms only insofar as (i) they sell into the EU and (ii) their products compete with the newly-unconstrained Chinese varieties, the design compares EU-bound exports to non-EU-bound exports, before and after 2005, interacted with a product-level measure of exposure to Chinese competition — a triple-difference structure.

**Fixed effects structure.** Based on the data description surfaced in secondary sources (not verified against the primary PDF, which is paywalled and was not accessible during card construction): the paper is reported to use **product-country fixed effects (`pd`)** together with **firm-product-year fixed effects (`fpt`)** to net out time-invariant product-destination determinants and firm-product-specific shocks, leaving the triple-difference interaction (post-2005 × EU × exposure) as the source of identification. **This FE structure should be re-verified against the published paper or its working-paper version before being cited for a specific claim** — see the flag in [[Fixed_Effects_Guide]] §7.

**Target parameter.** The coefficient of interest is on the triple interaction (post-2005 dummy × EU-destination dummy × product-level China-exposure measure), interpreted as the differential quality-upgrading response of Turkish exporters to the EU, in exposed products, after the EU-China liberalization — relative to the same firms' response in non-EU destinations or in non-exposed products.

**Threats to identification.** The main concern is that EU-bound and non-EU-bound exports, or exposed and non-exposed products, could be on differential trends for reasons unrelated to the 2005 liberalization (e.g. the EU business cycle, other simultaneous EU trade policy changes). The triple-difference structure is designed to net out any two-way version of these confounds, leaving only a shock that is specific to EU-destination, China-exposed products, right at 2005.

---

### Relevance to Paper_PTA

The closest methodological analogue among the papers cited in [[Fixed_Effects_Guide]]: a triple-difference design on firm-product-destination-year customs data, with a treatment defined by the interaction of a market-level shock and a product-level characteristic — structurally the same shape as this project's `EP_depth_{dt} × green_p` interaction. If the `pd` + `fpt` fixed-effects claim is confirmed on re-reading, it would be a useful direct precedent for the paper's discussion of which three-dimensional fixed effect remains available (`fpt`, per [[Fixed_Effects_Guide]] §6).

---

### References (Wikilinks)

[[Amiti2014_ImportersExportersExchangeRate]]
[[Amiti2013_ImportCompetitionQualityUpgrading]]
[[Bernard2006_SurvivalBestExposureLowwage]]
[[Bernard2011_MultiproductFirmsTradeLiberalization]]
[[Bloom2016_TradeInducedTechnicalChange]]
[[Brambilla2010_ChinasExperienceUnderMultifiber]]
[[Broda2006_GlobalizationGainsFromVariety]]
[[Buelens2005_TradeAdjustmentsFollowingRemoval]]
[[Bustos2011_TradeLiberalizationExportsTechnology]]
[[Chen2022_MarkupsQualityTradeCosts]]
[[Costinot2010_MatchingInequalityWorldEconomy]]
[[Demir2018_DontThrowTowelThrow]]
[[Eckhardt2009_EvolutionTradePolicyTowards]]
[[Fernandes2013_DoesTradeStimulateProduct]]
[[Fieler2018_TradeQualityUpgradingInput]]
[[Fieler2023_EscapingImportCompetitionChina]]
[[Fitzgerald2024_ExportersGrow]]
[[Goldberg2010_ImportedIntermediateInputsDomestic]]
[[Harrigan2009_TestingTheoryTradePolicy]]
[[Khandelwal2010_LongShortQualityLadders]]
[[Khandelwal2013_TradeLiberalizationEmbeddedInstitutional]]
[[Kugler2012_PricesPlantSizeProduct]]
[[Manova2017_MultiproductFirmsProductQuality]]
[[Medina2024_ImportCompetitionQualityUpgrading]]
[[Piveteau2024_ForeignCompetitionAlongQuality]]
[[Teshima2008_ImportCompetitionInnovationPlant]]
[[Utar2014_WhenFloodgatesOpenNorthern]]
[[Verhoogen2008_TradeQualityUpgradingWage]]
[[Yang2021_ImpactEmergingMarketCompetition]]

<!-- Unresolved (8):
  - Crowley (2021)
  - Dang (2017)
  - Defever (2019)
  - Directorate General for Trade of the European Commission (2006)
  - Fajgelbaum (2022)
  - Feenstra (1996)
  - Lim (2022)
  - Vaziri (2023)
-->
