---
area: trade/firms
tags:
  - area/trade/firms
---

# How do Different Exporters React to Exchange Rate Changes? (2012)

**Berman, Nicolas, Philippe Martin & Thierry Mayer (2012)**
*The Quarterly Journal of Economics*, 127(1): 437–492
[Paper Link](https://doi.org/10.1093/qje/qjr057) · Zotero key `AAZXJ2UB`

---

## Abstract

This article analyzes the heterogeneous reaction of exporters to real exchange rate changes using a very rich French firm-level data set with destination-specific export values and volumes on the period 1995–2005. We find that high-performance firms react to a depreciation by increasing significantly more their markup and by increasing less their export volume. This heterogeneity in pricing-to-market is robust to different measures of performance, samples, and econometric specifications. It is consistent with models where the demand elasticity decreases with firm performance. Since aggregate exports are concentrated on high-productivity firms, precisely those that absorb more exchange rate movements in their markups, heterogeneous pricing-to-market may partly explain the weak impact of exchange rate movements on aggregate exports.

---

## Research Question

Do exporters of different productivity respond differently to a real exchange rate depreciation — some passing it through to volumes, others absorbing it in markups? Identification comes from bilateral real exchange rate movements between France and each destination over 1995–2005, which vary within firm–destination pairs over time. Methodology: within (fixed-effects) panel estimation on firm–destination–year data, run separately for high- and low-performance firms.

---

## Results

High-performance firms respond to a depreciation by raising their markup substantially more and increasing export volumes substantially less than low-performance firms. Pricing-to-market is therefore strongly heterogeneous across the productivity distribution: the most productive exporters behave close to full absorption, the least productive close to full pass-through. The pattern survives alternative performance measures (TFP, size, capital intensity), alternative samples, and alternative econometric specifications, and is robust to controlling for distribution costs and imported-input exposure. Because aggregate French exports are heavily concentrated in high-productivity firms — precisely those that absorb depreciations in markups rather than volumes — heterogeneous pricing-to-market provides a micro-foundation for the weak aggregate elasticity of exports to exchange rates ("exchange rate disconnect").

---

## Data

The dataset is an unbalanced firm–destination–year panel of French exporters.

- **Type:** Unbalanced panel
- **Unit of observation:** Firm (f) × destination country (d) × year (t); a firm–product–destination–year version is used in robustness
- **Source:** French customs (Direction Générale des Douanes) export declarations, merged with firm balance-sheet data from the French fiscal administration (BRN/EAE files)
- **Time period:** 1995–2005
- **Key variables:** Export value by destination (FOB, euros), export volume (physical quantity), export unit value (price proxy), bilateral real exchange rate, firm TFP, firm size, capital intensity, destination GDP and distribution-cost measures
- **Sample restrictions:** Firms with at least some continuity across years; destinations with sufficient observations. Subsamples are computed by destination-year (and by year in two columns of the main tables) to compare across the performance distribution.

### Illustrative dataset structure

| Firm f | Destination d | Year t | Export value (firm f, dest. d, year t) | Export volume | Unit value (price) | Real exch. rate (dest. d, year t) | Firm TFP |
|---|---|---|---|---|---|---|---|
| F001 | USA | 1998 | 1,240,000 | 84,000 | 14.76 | 1.08 | high |
| F001 | Japan | 1998 | 480,000 | 31,000 | 15.48 | 0.94 | high |
| F002 | USA | 1998 | 92,000 | 11,500 | 8.00 | 1.08 | low |
| F001 | USA | 2002 | 1,410,000 | 88,000 | 16.02 | 0.87 | high |

*Note: values illustrative. The real exchange rate varies at the destination–year level; the firm-level performance measure is time-varying but firm-specific.*

---

## Methodology

**Identification strategy.** The bilateral real exchange rate between France and destination d varies over time for reasons largely exogenous to any individual French exporter. Comparing how firms of different measured performance adjust prices and volumes to the *same* exchange rate movement in the *same* destination isolates heterogeneity in pricing-to-market from any common shock.

**Fixed effects structure — the key point for this project.** The baseline specifications are **within estimations with firm–destination fixed effects (`fd`) plus year dummies (`t`)**. The authors state explicitly that the firm–destination effects deliver "a pure within effect of the exchange rate variation over time," absorbing all time-invariant firm–destination determinants of exports (transport and distribution costs, established buyer relationships, market-specific know-how), while year dummies capture the common evolution of French variables such as the wage rate. In robustness, firm–destination fixed effects are replaced by firm–product–destination fixed effects (`fpd`), tightening identification to within-variety variation. Standard errors are clustered and subsamples are computed by destination-year.

**Estimating equations.** For the price (markup) equation and the volume equation respectively:

$$\ln p_{fdt} = \beta \ln RER_{dt} + \gamma X_{fdt} + \mu_{fd} + \lambda_t + \varepsilon_{fdt}$$
$$\ln q_{fdt} = \beta' \ln RER_{dt} + \gamma' X_{fdt} + \mu_{fd} + \lambda_t + \varepsilon'_{fdt}$$

where $p_{fdt}$ is the export unit value of firm f in destination d at time t, $q_{fdt}$ the export volume, $RER_{dt}$ the bilateral real exchange rate, $\mu_{fd}$ the firm–destination fixed effect, and $\lambda_t$ the year dummy. Both equations are estimated separately for high- and low-performance firms; the contrast in $\beta$ (and $\beta'$) across the two groups is the object of interest.

**Target parameter.** $\beta$ is the within-firm–destination elasticity of the export price to the real exchange rate — the degree of pricing-to-market. A larger $\beta$ means more of the depreciation is absorbed into the markup rather than passed to the foreign buyer.

**Threats to identification and robustness.** The main concerns are (i) that firm performance is endogenous to exchange-rate exposure, addressed with alternative and lagged performance measures; (ii) that distribution costs drive the heterogeneity, addressed with interaction terms (distribution-cost data are time-invariant and so cannot enter alongside firm–destination FE directly); (iii) that imported inputs generate a mechanical marginal-cost channel — the authors note that firm–destination FE control only for the time-invariant component of this dependence and use French customs import data to probe it directly. A firm–destination fixed-effects logit is used for the export-participation margin.

---

### Relevance to Paper_PTA

Verified example of the `fd` (firm–destination) fixed effect in a firm-level trade panel, and of the explicit "pure within" language used to justify it. Useful reference for the argument that firm–destination effects absorb established buyer relationships and market-specific fixed costs — the same logic that motivates the `fpd` and `fdt` components of this project's specification. See [[Fixed_Effects_Guide]].

---

### References (Wikilinks)

[[Atkeson2008_PricingtomarketTradeCostsInternational]]
[[Auer2009_ExchangeRatePassthroughCompetitive]]
[[Baldwin2007_ZerosQualitySpace]]
[[Bergin2001_PricingtomarketStaggeredContractsReal]]
[[Bernard2011_MultiproductFirmsTradeLiberalization]]
[[Berthou2008_EuroIntensiveExtensiveMargins]]
[[Broda2006_GlobalizationGainsFromVariety]]
[[Burstein2005_LargeDevaluationsRealExchange]]
[[Chatterjee2013_MultiproductFirmsExchangeRate]]
[[Corsetti2005_MacroeconomicModelInternationalPrice]]
[[Crucini2002_PersistenceLawofonepriceDeviationsEvidence]]
[[Dekle2005_ReexaminationExchangeRateDisconnect]]
[[Eaton2004_DissectingTradeFirmsIndustries]]
[[Eaton2007_ExportDynamicsColombiaFirmlevel]]
[[Ekholm2008_ManufacturingRestructuringRoleReal]]
[[Engel2006_EquivalenceResultsOptimalPassthrough]]
[[Feenstra1996_MarketShareExchangeRate]]
[[Fitzgerald2013_PricingtomarketEvidenceFromPlantlevel]]
[[Gaulier2006_StructuralDeterminantsExchangeratePassthrough]]
[[Goldberg2001_EvolutionPriceDispersionEuropean]]
[[Goldberg2010_SensitivityExchangeRatesDistribution]]
[[Gopinath2010_CurrencyChoiceExchangeRate]]
[[Gourinchas1999_ExchangeRatesMatterFrench]]
[[Greenaway2007_ExchangeRatesExportsEvidence]]
[[Halpern2007_PricingFirmAnalysisFirm]]
[[Hellerstein2008_BearsCostChangeExchange]]
[[Hooper1998_TradeElasticitiesCountries]]
[[Mayer2007_HappyInternationalisationEuropeanFirms]]
[[Mayer2011_MarketSizeCompetitionProduct]]
[[Nakamura2012_LostTransitProductReplacement]]
[[Ottaviano2005_MarketSizeTradeProductivity]]
[[Verhoogen2008_TradeQualityUpgradingWage]]
