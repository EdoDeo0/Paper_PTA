---
area: trade/firms
tags:
  - area/trade/firms
---

# Trade Liberalization, Quality, and Export Prices (2015)

**Fan, Haichao, Yao Amber Li & Stephen R. Yeaple (2015)**
*The Review of Economics and Statistics*, 97(5): 1033–1051
[Paper Link](https://doi.org/10.1162/rest_a_00524) · Zotero key `TXW26VDF`

---

## Abstract

This paper presents theory and evidence from disaggregated Chinese data that tariff reductions induce a country's producers to upgrade the quality of their exports. We first document stylized facts regarding the effect of trade liberalization on export prices. Next, we develop an analytic framework that relates a firm's choice of quality to its access to imported intermediates. In the model, a reduction in import tariffs induces a firm to increase export quality and raise its export price in industries where the scope for quality differentiation is large and lower its export price in industries where the scope is small. The predictions are consistent with the stylized facts and are highly robust econometrically.

---

## Research Question

Does a reduction in tariffs on imported intermediate inputs cause Chinese exporters to upgrade the quality (and hence unit value) of their exports, and does this response depend on the industry's scope for quality differentiation? The source of variation is China's WTO-accession input-tariff cuts 2001–2006, which vary by firm depending on the composition of imported inputs. Methodology: firm-level long-difference and levels regressions on customs micro-data, with an explicit demand-side quality estimation step.

---

## Results

A firm-specific 10-percentage-point reduction in import tariffs on intermediates raises export unit values (prices) at the firm-product-destination level by about 4.8%. The effect is concentrated in industries with a large scope for quality differentiation, where it operates through genuine quality upgrading (estimated via a demand-side residual), and is muted or reversed in industries with little scope for differentiation, consistent with the theoretical model. Firms experiencing larger input-tariff cuts are more likely to enter new product-destination markets, providing extensive-margin evidence of quality upgrading. Results are robust to alternative tariff measures, to controlling for firm TFP, capital intensity, size, and industry competition (Herfindahl index), and to distinguishing entering, continuing, and exiting firm-product-destination combinations.

---

## Data

The dataset combines three Chinese administrative sources at the firm-product(-destination)-year level.

- **Type:** Unbalanced panel, aggregated to long differences (2001 vs. 2006) for the main specification
- **Unit of observation:** Firm (f) × HS6 product (h) × destination country (c) × year (t)
- **Source:** China's General Administration of Customs (firm-product export/import transactions), WTO tariff schedules (product-level applied tariffs), National Bureau of Statistics of China (NBSC) firm production database (TFP, employment, capital, industry classification)
- **Time period:** 2001–2006 (WTO-accession tariff phase-in)
- **Key variables:** Export unit value (price proxy), firm-specific import tariff on intermediates (aggregated across imported inputs), estimated "effective quality" (residual of a demand equation following Khandelwal et al.), firm TFP, capital intensity, employment, industry Herfindahl index (4-digit CIC, 2001)
- **Sample restrictions:** Export-processing firms are excluded from the main analysis (they never paid tariffs) but retained for a placebo check; pure intermediary/trading firms are excluded following Ahn, Khandelwal & Wei (2011)
- **Sample size:** Aggregation to HS6-country-firm level for consistent tracking 2001–2006; the paper reports firm-product, firm-product-country, and firm-level versions of every table

### Illustrative dataset structure

| Firm f | HS6 product h | Destination c | Year t | Unit value (f,h,c,t) | Firm import tariff Δ | Estimated quality (f,h,c,t) | Firm TFP |
|---|---|---|---|---|---|---|---|
| F001 | 850110 (motors) | USA | 2001 | 12.4 | — | 0.31 | 1.02 |
| F001 | 850110 (motors) | USA | 2006 | 14.1 | −0.08 | 0.44 | 1.15 |
| F002 | 610910 (T-shirts) | Germany | 2001 | 3.2 | — | 0.05 | 0.87 |
| F002 | 610910 (T-shirts) | Germany | 2006 | 3.0 | −0.03 | 0.04 | 0.91 |

*Note: values illustrative. "Firm import tariff Δ" is the firm-specific weighted change in tariffs on imported intermediates, 2001–2006. Estimated quality is the residual of the demand-side quality equation (Section 5.1.2 of the paper).*

---

## Methodology

**Identification strategy.** China's WTO accession generated exogenous, pre-scheduled cuts in input tariffs. Because different firms import different baskets of intermediates, the resulting firm-specific tariff reduction varies even within narrowly defined industries, letting the authors compare quality/price responses across firms facing different-sized shocks while holding the industry's scope for quality differentiation fixed. Exogeneity of the tariff schedule is checked against pre-accession (2000) industry characteristics, finding no significant correlation.

**Fixed effects and estimating equations — the key point for this project.** The paper runs two families of specifications:

1. **Levels equation**, estimated with **firm-product(-country) fixed effects plus time dummies**:
   $$\ln p_{fh(c)t} = \beta \, Duty_{ft} + \chi_f \gamma + HHI_i \delta + \phi_{fh(c)} + \lambda_t + \varepsilon_{fh(c)t}$$
   where $\phi_{fh(c)}$ is a firm-product or firm-product-country fixed effect (`fp` or `fpd` in this project's notation) and $\lambda_t$ is a year dummy. The authors note that estimating in levels risks autocorrelation (citing Trefler 2004) and prefer the long-difference version below as the baseline.

2. **Long-difference equation** (the baseline, 2001→2006), which differences out $\phi_{fh(c)}$ and $\lambda_t$ by construction:
   $$\Delta \ln p_{fh(c)} = \beta \, \Delta Duty_f + \Delta\chi_f \gamma + \Delta HHI_i \delta + \varepsilon_{fh(c)}$$

3. **Quality equation** (Section 5.1.2), a demand-side estimation following Khandelwal, Schott & Wei: effective quality is recovered as the residual $\varepsilon_{fhct}$ of
   $$\ln q_{fhct} + \sigma \ln p_{fhct} = \phi_h + \phi_{ct} + \varepsilon_{fhct}$$
   where $\phi_{ct}$ is a **country(destination)-year fixed effect** (`dt` in this project's notation, absorbing destination price index and income) and $\phi_h$ is a **product fixed effect** (`p`) capturing inherent characteristics of the product category. This is the equation most directly comparable to the `pt`/`dt` logic used in Paper_PTA: the destination-year term here plays exactly the role of absorbing market-level demand and price-index shocks that the composition design also needs to net out.

**Target parameter.** $\beta$ is the elasticity of export unit value (or estimated quality) to the firm-specific change in input tariffs — how much quality/price upgrading a given tariff cut induces, conditional on firm characteristics and industry competition.

**Threats to identification.** (i) Reverse causality/political-economy targeting of tariff cuts — addressed by testing correlation with pre-accession (2000) industry performance, finding none; (ii) compositional bias from extensive-margin entry/exit — addressed by tracking "entry," "continuing," and "exit" firm-product(-country) combinations separately (Table 7) and by robustness with export processors as a placebo group; (iii) simultaneity between quality and price — addressed by the structural demand-side quality estimation rather than treating price as a direct quality proxy.

---

### Relevance to Paper_PTA

This paper is the cleanest example found of the `dt` + `p` combination doing exactly the job it does in this project's composition design: absorbing the destination-year price/demand level so that a *within-cell* margin (quality, here; green-vs-dirty composition, in Paper_PTA) can be identified. It also documents `fpd` as a standard levels specification in the same customs-data tradition as Manova & Zhang (2012) and Fernandes, Lefebvre & Rocha (2021). See [[Fixed_Effects_Guide]].

---

### References (Wikilinks)

[[Ackerberg2006_StructuralIdentificationProductionFunctions]]
[[Ahn2011_RoleIntermediariesFacilitatingTrade]]
[[Amiti2013_ImportCompetitionQualityUpgrading]]
[[Anderson2004_TradeCosts]]
[[Bernard2007_FirmStructureMultinationalsManufacturing]]
[[Brandt2012_CreativeAccountingCreativeDestruction]]
[[Broda2006_GlobalizationGainsFromVariety]]
[[Goldberg2010_ImportedIntermediateInputsDomestic]]
[[Halpern2007_PricingFirmAnalysisFirm]]
[[Kasahara2008_DoesImportedIntermediatesIncrease]]
[[Khandelwal2010_LongShortQualityLadders]]
[[Kugler2009_PlantsImportedInputsFacts]]
[[Manova2012_ExportPricesAcrossFirms]]
[[Melitz2008_MarketSizeTradeProductivity]]
[[Pierce2013_SurprisinglySwiftDeclineManufacturing]]
[[Rauch1999_NetworksVersusMarketsInternational]]
[[Schott2004_AcrossproductVersusWithinproductSpecialization]]
[[Tang2012_QualityDifferentiationTradeIntermediation]]
[[Trefler2004_LongShortCanadauFree]]
[[Verhoogen2008_TradeQualityUpgradingWage]]

<!-- Unresolved (10): Cai & Liu 2009; Fan 2012; Ge, Lai & Zhu 2011; Gopinath & Neiman 2011; Head & Ries 2001; Kleibergen & Paap 2006; Levinsohn & Petrin 2003; Olley & Pakes 1996; Smeets & Warzynski 2013; Stock & Yogo 2005 — matched from PDF reference-list parsing, not resolved to DOI/Crossref record. -->
