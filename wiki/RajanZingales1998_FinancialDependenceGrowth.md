---
area: methods/program-eval
tags:
  - area/methods/program-eval
---

# Financial Dependence and Growth

Rajan and Zingales (1998)

The American Economic Review

[Zotero Link: to be added]

## Abstract

This paper examines whether financial development facilitates economic growth by scrutinizing one rationale for such a relationship: that financial development reduces the costs of external finance to firms. Specifically, we ask whether industrial sectors that are relatively more in need of external finance develop disproportionately faster in countries with more-developed financial markets. We find this to be true in a large sample of countries over the 1980's. We show this result is unlikely to be driven by omitted variables, outliers, or reverse causality.

## Research Question

Does financial development causally facilitate growth by lowering the cost of external finance to firms, and can this channel be isolated from confounding country-level determinants of growth? The source of variation is the interaction between an industry's technological need for external finance — estimated from US firms assumed to face frictionless capital markets — and a country's level of financial development. The methodology is a cross-industry, cross-country interaction (difference-in-differences) regression.

## Results

Industries that are more dependent on external finance (by technological necessity, not by choice) grow disproportionately faster in countries with more developed financial markets. The interaction effect of financial development and external dependence is positive, economically large, and robust to dropping individual countries or industries, to alternative measures of financial development, and to indicators built to rule out reverse causality. A composite measure of "predicted growth" built from this interaction outperforms aggregate financial-development indicators in standard cross-country growth regressions.

## Data

The unit of observation is an industry–country cell: the growth rate of value added (or output) for a given manufacturing industry in a given country, averaged over the 1980s. This is a cross-section of industry–country cells rather than a true time-series panel — the dependent variable is a single average growth rate per cell over the decade, but the regressors vary along two cross-sections (industry and country), which is what supports the interaction design.

Industry-level "external finance dependence" is constructed from US Compustat firm data: for each of ~36 manufacturing industries (ISIC classification), dependence is measured as the median firm's capital expenditure minus cash flow from operations, scaled by capital expenditure, averaged over the 1980s. The US is used as the benchmark because its financial markets are assumed close to frictionless, so the measure proxies the industry's technological need for external funds rather than a country-specific financing constraint. Country-level value added and growth come from UNIDO Industrial Statistics; financial development indicators (e.g., accounting standards, stock market capitalization relative to GDP) are drawn from standard cross-country finance datasets of the period. Country coverage is large (40+ countries) but unbalanced — not every country reports all 36 industries, and developing-country coverage is sparser than OECD coverage.

Illustrative structure of the dataset:

| Industry (i)        | Country (c) | External Dependence (industry i, US benchmark) | Financial Development (country c) | Value-Added Growth (industry i, country c, 1980s) |
|----------------------|-------------|--------------------------------------------------|-------------------------------------|------------------------------------------------------|
| Pharmaceuticals      | Germany     | 0.34                                              | 0.62                                | 0.071                                                |
| Pharmaceuticals      | India       | 0.34                                              | 0.21                                | 0.018                                                |
| Iron and Steel       | Germany     | 0.06                                              | 0.62                                | 0.022                                                |
| Iron and Steel       | India       | 0.06                                              | 0.21                                | 0.015                                                |

## Methodology

Identification rests on treating the industry's external finance dependence as a technological characteristic that is exogenous to any individual country's financial system, since it is measured purely from US firm behavior. Conditional on that assumption, the country's own level of financial development should matter more for the growth of industries that are intrinsically more reliant on external capital — generating a testable interaction effect that does not require taking a stand on the level of financial development's effect on aggregate growth.

The baseline estimator is OLS on the interaction term:

Growth(i,c) = β · [FinancialDevelopment(c) × ExternalDependence(i)] + FE(c) + FE(i) + ε(i,c)

where FE(c) (country fixed effects) absorb everything about a country's growth that is common across industries — including the level effect of financial development itself — and FE(i) (industry fixed effects) absorb everything about an industry's growth that is common across countries — including the level effect of external dependence itself. The coefficient β is therefore identified purely off the differential growth of high-dependence industries in financially developed countries relative to financially underdeveloped ones; it cannot be confounded by a country simply being financially developed and growing faster across the board, nor by an industry simply being capital-intensive and growing faster everywhere.

The main threat to identification is that US-measured external dependence might not be exogenous to other countries' financial systems if the underlying production technology itself varies systematically with a country's level of financial development (e.g., countries select into producing industries suited to their financial system). Reverse causality is also a concern if industries with high (realized, not technological) financing needs are the ones that lobby for or otherwise drive financial market development. The authors address these threats with robustness checks: dropping one country or industry at a time, using alternative financial-development proxies, and checking that the interaction effect survives controls for other standard growth covariates (human capital, initial income, trade openness).

This is the paper referenced in the Paper_PTA working-paper build script ([New/working_paper_build.py:316](../New/working_paper_build.py:316)) as the template for the project's triple-difference specification: there, the role of "industry × external dependence" is played by "product × green/dirty classification," and the role of "country × financial development" is played by "destination × EP depth," with firm–destination and firm–time fixed effects absorbing the corresponding level effects.

### References (Wikilinks)

[[Jensen1976_TheoryOfTheFirm]]
[[Mankiw1992_EmpiricsEconomicGrowth]]
[[Stiglitz1981_CreditRationing]]
[[King1993_FinanceGrowthSchumpeter]]
[[Rajan1995_CapitalStructure]]
[[Nove1963_EconomicBackwardness]]
[[King1993_FinanceEntrepreneurshipGrowth]]
[[Fry1970_FinancialStructureDevelopment]]
[[Bekaert1995_WorldMarketIntegration]]
[[Ranis1961_TheoryEconomicDevelopment]]
[[Jayaratne1996_FinanceGrowthNexus]]
[[Holmstrom1993_MarketLiquidityMonitoring]]
[[James1987_UniquenessBankLoans]]
[[Diamond1989_ReputationDebtMarkets]]
[[Mayer1990_FinancialSystemsCorporateFinance]]
[[Aoki1995_JapaneseMainBankSystem]]
[[Levine1991_StockMarketsGrowthTax]]
[[Boyd1996_CoevolutionRealFinancialSectors]]
[[Barro1989_GrowthCrossSection]]
[[Levine1997_FinancialDevelopmentViewsAgenda]]
[[Easterly1993_GoodPolicyGoodLuck]]
[[Mankiw1995_GrowthOfNations]]

<!-- Unresolved (10):
  - OpenAlex referenced_works entries with deleted/merged IDs (no recoverable metadata): W3122280770, W3122318539, W3124560121, W4230809332, W6630691344, W6645065712, W6656866782, W6667129605, W6675294754, W6680933668
-->
