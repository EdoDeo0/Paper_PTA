---
area: trade/environment
tags:
  - area/trade/environment
---

# The Stringency of Environmental Regulations and Trade in Environmental Goods (2014)

**Sauvage, J. (2014)**
*OECD Trade and Environment Working Papers*, No. 2014/03, OECD Publishing
[Paper Link](https://doi.org/10.1787/5jxrjn7xsnmq-en)

---

## Abstract

This report assesses conceptually and empirically the extent to which the stringency of environmental regulations drives international trade in environmental goods. Many of the measures governments adopt to address issues such as local air and water pollution or GHG emissions take the form of regulations that aim to change the behaviour of firms or households. Compliance by private actors with those regulations in turn generates a growing market for environmental goods and services that is increasingly international in scope as more countries tighten their environmental regulations. Regulatory stringency thus spurs the development of a market for a whole range of equipment specifically meant for preventing and abating pollution, with important implications for international trade in such equipment. The different indicators of regulatory stringency considered in the present analysis generally support the notion that the stringency of environmental regulations positively affects countries' specialisation in environmental products, even when considering specific sectors such as solid-waste management or wastewater treatment. While increased trade in environmental products is not an end in itself, the environmental benefits this entails can contribute to global improvements in environmental quality. By increasing demand for environmental products and technologies, environmental policy can complement trade policy in supporting pollution-reduction efforts not just domestically, but also abroad.

---

## Research Question

Does the stringency of a country's environmental regulations drive its exports of environmental goods, by creating a larger domestic market for the equipment firms and households need to comply with those regulations (a "home-market effect" argument)? The source of variation is cross-country and over-time differences in a composite index of environmental policy stringency (2002–2012) and, for two sectors, in observed regulatory outcomes (the share of municipal solid waste landfilled; the share of wastewater given at least secondary treatment). The methodology is OLS on a panel of countries and years, with revealed comparative advantage (RCA) in environmental goods as the outcome and country-clustered standard errors.

---

## Results

Countries with more stringent environmental regulations (higher EPS index) show significantly higher revealed comparative advantage in the 248 products of the Combined List of Environmental Goods (CLEG): a 10% increase in the EPS index is associated with a 6.1–6.9% increase in the RCA index, robust to controls for capital-labour ratio, education, export diversification, and to 3–5 year lags of the stringency measure. The relationship is stronger for products related to air-pollution control, environmental monitoring, and renewable energy (i.e. more energy-sector-weighted subsets), and weaker for the narrower Core CLEG and Core CLEG+ lists dominated by water/waste equipment. Import tariffs applied on CLEG products have a small but negative effect on a country's own CLEG exports, independent of and opposite in sign to the stringency effect — countries that protect their environmental-goods industry with tariffs do not export more of it. Sector-level analysis confirms the pattern: a lower share of municipal solid waste landfilled (proxying stringency in waste regulation) and a higher share of wastewater given advanced treatment are both associated with higher RCA in the corresponding CLEG sub-categories (SWM and WAT), with the wastewater results the most robust and largest in magnitude of the three exercises.

---

## Data

The paper combines several country-year (and country-pair-year, for tariffs) panels covering roughly 2002–2012.

**Trade data**: bilateral trade values in current USD for the 248 HS6 products of the CLEG, from UN Comtrade, natively coded in HS 2007 and converted by the OECD to HS 2002 to extend time coverage back to 2002 (with environmental relevance re-checked case by case where the crosswalk was ambiguous). Outcome variable: the revealed comparative advantage (RCA) index, $RCA_{ik} = 100 \times \big[(X_{ik}/X_{wk})/(X_{i.}/X_{w.})\big]$, where $X_{ik}$ is country $i$'s exports of product $k$, $X_{wk}$ world exports of product $k$, $X_{i.}$ country $i$'s total exports, $X_{w.}$ world total exports.

**Stringency indicators**: (1) the Environmental Policy Stringency (EPS) index, a composite 0–6 scaled, equally-weighted measure across regulatory instruments (taxes on air pollutants and diesel, renewable-energy certificates and feed-in tariffs, emission limit values for SO2/NOX/PM), available for 26 OECD countries, mostly 1990–2012 but used here only 2002–2012 for trade-side data availability (from a companion OECD productivity-growth study); (2) the share of municipal solid waste (MSW) landfilled, from OECD/Eurostat plus UN Statistics Division and World Bank sources for non-OECD countries, with some values imputed via a binomial GLM (logit link) using GDP per capita, GDP growth, population density, country dummies, and a time trend; (3) the share of population connected to wastewater treatment with at least secondary treatment (the *TRT* indicator), from OECD/Eurostat with linear interpolation for missing years, and a custom utility-level aggregation for Australia.

**Controls**: capital-labour ratio (capital stock via perpetual-inventory method on WDI gross fixed capital formation, 15-year asset life, straight-line depreciation), share of labour force with secondary education (WDI), export diversification (UNCTAD Herfindahl-type concentration index), and applied import tariffs on CLEG lines by country pair and HS line (UNCTAD TRAINS, with zero imputed for intra-EU trade by accession year).

**The CLEG itself**: 248 HS6 codes assembled by the OECD by combining three pre-existing lists (an OECD/Eurostat 1999 effort of 132 codes, the WTO "Friends group" list of 154 products, and other negotiated lists), organized into 11 environmental themes/media (air pollution control, waste management, wastewater treatment, renewable energy plant [37% of 2011 world CLEG imports], environmental monitoring, etc. — see Table 1 of the paper). Two narrower, expert-vetted subsets are also used for robustness: the Core CLEG (11 products where environmental use exceeds two-thirds of measured trade) and Core CLEG+ (40 products, one-third threshold). Full list of the 248 HS6 codes in Annex 1 (Table A.1).

### Illustrative dataset structure

| Country $i$ | Year $t$ | RCA (CLEG, $i,t$) | EPS index ($i,t$) | Landfill share ($i,t$) | K/L ratio | Export conc. |
|---|---|---|---|---|---|---|
| Germany | 2005 | 142.3 | 3.8 | 0.18 | 62,400 | 0.09 |
| China | 2008 | 128.7 | 1.6 | — | 8,900 | 0.14 |
| Korea | 2010 | 156.1 | 3.1 | 0.09 | 41,200 | 0.11 |
| Chile | 2011 | 41.2 | 1.9 | 0.62 | 25,700 | 0.31 |

*Note: illustrative only. RCA=100 means a country's export share in CLEG products exactly matches its share in world trade overall; values above 100 indicate revealed comparative advantage.*

---

## Methodology

**Identification strategy.** The paper does not claim a causal design in the DiD/IV sense; it presents OLS correlations between measures of environmental regulatory stringency and a country's revealed comparative advantage in environmental goods, arguing on conceptual grounds (a "home-market effect" from new-trade-theory models of monopolistic competition, following Krugman 1980 and Feenstra et al. 2001) that stringent regulation creates domestic demand which, for differentiated products, translates into larger net exports.

**Estimator.** OLS with country-clustered standard errors and year fixed effects. Baseline equation:
$$\ln RCA_{i,t} = \beta_0 + \beta_1 \ln STR_{i,t} + \gamma X_{i,t} + \delta_t + \varepsilon_{i,t}$$
where $STR_{i,t}$ is one of the three stringency measures (EPS index; MSW landfill share; wastewater treatment share), $X_{i,t}$ the control vector (capital-labour ratio, secondary education share, export concentration, sometimes import tariffs), and $\delta_t$ year dummies. The same equation is estimated on the full CLEG, the Core CLEG, the Core CLEG+, and on sector-specific product subsets (SWM for the landfill analysis, WAT for the wastewater analysis).

**Target parameter.** $\beta_1$ is interpreted as an elasticity: the percentage change in a country's RCA in environmental goods associated with a percentage change in regulatory stringency, holding constant year effects and the included controls. It is a cross-sectional/panel correlation, explicitly not identified against reverse causality or omitted country characteristics (e.g. infrastructure, factor endowments) that could jointly drive both stringency and export performance.

**Threats to identification, acknowledged directly by the author**: (1) omitted-variable bias from unobserved country characteristics correlated with both regulatory stringency and general export capacity — partially mitigated by using an RCA index (which nets out generic export-competitiveness factors common to the numerator and denominator) and by controlling for factor endowments; (2) reverse causality is not addressed with an instrument; robustness to 3–5 year lags of the stringency measure is used as an informal check instead; (3) the EPS index is described as biased toward the power sector and air-pollution control, motivating the two sector-specific outcome-based checks (MSW landfilling, wastewater treatment) as complementary, less energy-skewed evidence.

**Robustness checks performed**: narrower product lists (Core CLEG, Core CLEG+) as an alternative to the full 248-code CLEG; a sector-restricted CLEG subset (APC+MON+REP, 103 products) to isolate the energy-sector channel; lagged stringency measures (3 and 5 years); year dummies to absorb the 2008–09 crisis and commodity-price swings; addition of import tariffs as a competing explanatory variable to rule out an infant-industry story; dropping two outlier countries (Finland, Italy) in the MSW analysis.

---

## Relevance to Paper_PTA

This is the primary source for the project's **green-goods classification** (`env_good`, `Data/Env_Codes_HS.dta`, 247 codes). A direct code-by-code comparison (2026-07-15) against Table A.1 of this paper found 246/248 CLEG codes present in the project's list (99.2%), with the single discrepancy a granularity split of one HS heading (871410 vs. 871411/871419) — not a substantive classification error. Note for future reference: the CLEG here is natively coded in **HS 2007** (converted by the OECD to HS 2002 for the 2002–2012 trade panel used in this paper), not HS 2012 as an earlier project note had assumed without verification — the vintage label matters less than the code list itself, which matches almost exactly regardless.

---

### References (Wikilinks)

[[Arrow1962_EconomicImplicationsLearning]]
[[Krishna2004_AdvancedInternationalTrade]]
[[Feenstra2001_UsingGravityEquation]]
[[Costantini2011_GreenInnovativeSide]]
[[Duflo2013_TruthTellingThird]]
[[Tietenberg1990_EconomicInstrumentsEnvironmental]]
[[Cole2003_DoEnvironmentalRegulations]]
[[York2012_AsymmetricEffectsEconomic]]
[[Kozluk2013_EnvironmentalPoliciesProductivity]]
[[Woods1999_PhosphorusRecoveryTechnology]]
[[Giesen1999_CrystallisationProcessEnables]]
[[Soest2005_ShadowPricesEnvironmental]]
[[WTO2012_InternationalTradeStatistics]]
[[Elofsson2010_CostsMeetingEnvironmental]]
[[Brandt2004_ClimateChangeNegotiations]]
[[OECD2011_GlobalisationComparativeAdvantage]]
[[IMF2003_ManualStatisticsInternational]]
[[Adlung2009_GatsCommitmentsEnvironmental]]
[[Berg2007_PhosphorusRecoveryWaste]]
[[Gren2017_CostEffectiveNutrient]]
[[Kamp2007_ImportanceLearningProcesses]]
[[Broner2012_SourcesComparativeAdvantage]]
[[Chor2009_UnpackingSourcesComparative]]
[[Brunel2013_MeasuringEnvironmentalRegulatory]]
[[Bahar2013_DomesticIncentiveMeasures]]
[[Condon2013_BorderCarbonAdjustment]]

<!-- Unresolved (2): OpenAlex/Crossref returned title+DOI but no verifiable author for either
  - "Scale Economies, Product Differentiation, and the Pattern of Trade" (1990), reprinted in Rethinking International Trade (MIT Press) — DOI 10.7551/mitpress/5933.003.0005
  - "Managing Water for All: An OECD Perspective on Pricing and Financing" (2013), Water Intelligence Online — DOI 10.2166/9781780406084
-->
