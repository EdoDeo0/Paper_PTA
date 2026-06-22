# The Distributional Consequences of Preferential Trade Liberalization: Firm-Level Evidence

**Baccini, Pinto & Weymouth (2017)**
*International Organization*, 71(2): 373–395
[Paper Link](https://doi.org/10.1017/S002081831700011X)

---

## Abstract

While increasing trade and foreign direct investment, international trade agreements create winners and losers. Our paper examines the distributional consequences of preferential trade agreements (PTAs) at the firm level. We contend that PTAs expand trade among the largest and most productive multinationals by lowering preferential tariffs. We examine data covering the near universe of US foreign direct investment and disaggregated tariff data from PTAs signed by the United States. Our results indicate that US preferential tariffs increase sales to the United States from the most competitive subsidiaries of multinational corporations operating in partner countries. We also find increases in market concentration in partner countries following preferential liberalization with the United States. By demonstrating that the gains from preferential liberalization are unevenly distributed across firms, we shed new light on the firm-level, economic sources of political mobilization over international trade and investment policies.

---

## Research Question

Do PTAs benefit large and productive multinational firms disproportionately more than smaller ones, even within the same industry? The authors exploit cross-sectional and within-country-industry variation in US preferential tariff cuts (at the HS 6-digit level, collapsed to NAICS 4-digit) to identify heterogeneous firm-level responses. The estimator is OLS with country, industry, and period fixed effects, augmented by IV to address the endogeneity of US tariff concessions to pre-existing affiliate activity.

---

## Results

US preferential tariff cuts increase trade-related (vertical) sales of MNC affiliates, but the effect scales sharply with affiliate size and productivity. A 10 percent tariff cut is associated with a 6 percent increase in sales for affiliates with about 45 employees — the threshold at which the marginal effect turns positive — rising to roughly 25 percent for affiliates with 570 employees (one standard deviation above the mean of 110) and 37 percent for those with 3,000 employees. For the smallest affiliates the effect is negative. The IV estimates, using host-country de jure tariff commitments as instruments, are larger than OLS but directionally identical and robust to demanding fixed effects. PTAs also raise Herfindahl-Hirschman concentration among US MNC affiliates in partner countries: a 10 percent host-country preferential cut is associated with a 0.5-point increase in the HHI. Interestingly, in industries without tariff cuts, deeper PTAs (more market-friendly provisions) reallocate sales from the largest to the smallest affiliates — the opposite pattern.

---

## Data

The study combines BEA survey data on US multinationals with product-level preferential tariff data.

**BEA Benchmark Surveys of US Multinational Companies** (US Bureau of Economic Analysis): legally mandated surveys covering the near universe of US MNC foreign affiliates, conducted at five-year intervals for 1989, 1994, 1999, 2004, and 2009. The data record financial and operating activities at the individual affiliate level, including the destination of sales (United States, host country, third countries), employment, and value added. Restricted-access confidential data, analysis conducted at the BEA under confidentiality agreements.

**World Integrated Trade Solution (WITS)** tariff data at HS 6-digit disaggregation, providing MFN and preferential tariff schedules for all US PTAs. These are linked to the BEA industry classifications via a HS→NAICS crosswalk and collapsed to the four-digit NAICS level.

**De jure tariff commitments** were extracted by the authors directly from the annexes of PTA treaty texts signed by the United States, covering more than 5,000 products per agreement. These differ from applied (de facto) tariffs in WITS and serve as instruments.

**PTA Depth (PTADEPTH)** from the DESTA database (Dür, Baccini & Elsig, 2014), a continuous count of 48 market-friendly provision dummies. Available at [www.designoftradeagreements.org](http://www.designoftradeagreements.org/).

**Key variables:**
- `SALES_ajit` — log of total affiliate sales to the US (affiliate a, industry j, host country i, period t)
- `PTATARIFFCUTS(US)_ijt` — proportional cut: (MFN − preferential tariff) / MFN, US-implemented; equals 0 for non-PTA country-sectors
- `LNEMPLOYEES` — log of affiliate employees (proxy for size)
- `PRODUCTIVITY` — Solow residual from affiliate-level regression of log value added on physical assets, employment, and industry dummies (available for ~64,000 obs)
- `PTADEPTH` — count of market-friendly provisions in the PTA
- `HHI` — Herfindahl-Hirschman Index of affiliate sales concentration at country-industry-period level (range 1–100); also four-firm concentration ratio

**Panel dimensions:** affiliate × host country × industry × benchmark period (unbalanced). 163 host countries; five periods (1989–2009). 70,561 affiliate-level observations in the main regressions; 17,093 country-industry-period observations in the concentration analysis.

| Affiliate a | Host Country i | Industry j (NAICS 4-digit) | Period t | Sales to US (USD '000) | Employees | PTA Tariff Cut (US) | PTA Depth |
|---|---|---|---|---|---|---|---|
| Aff001 | Mexico | 3361 (Motor Vehicles) | 1999 | 45,200 | 820 | 0.42 | 12 |
| Aff002 | Canada | 3341 (Computer) | 2004 | 12,800 | 90 | 0.18 | 12 |
| Aff003 | Germany | 3251 (Chemicals) | 2004 | 0 | 55 | 0.00 | 0 |
| Aff004 | Chile | 3361 (Motor Vehicles) | 2009 | 8,100 | 40 | 0.35 | 9 |

*Note: Values illustrative. Sales destination is recorded only for majority-owned affiliates. About 30% of US MNC foreign affiliates export to the United States; about half sell only to the host market.*

---

## Methodology

### Identification strategy

The key variation is industry-level preferential tariff cuts implemented by the United States across PTA partner countries and benchmark periods. By interacting tariff cuts with affiliate size, the authors test whether the effect on vertical (trade-related) sales scales with firm competitiveness. Country, industry, and period fixed effects absorb time-invariant country-specific factors and industry-level political influence.

The main endogeneity threat is that US preferential tariff concessions may be endogenous to pre-existing affiliate sales levels — Blanchard & Matschke (2015) show this relationship at the industry level. The authors address this with an **IV using host-country de jure tariff commitments** (`HOST COUNTRY DE JURE CUTS`): the proportional tariff cut that the partner country commits to in the PTA annexes, extracted from treaty texts. The identifying assumption is that partner-country commitments are correlated with US cuts through reciprocal negotiation but do not directly affect affiliate sales to the United States through any channel other than the US tariff reduction itself. A second instrument weights de jure cuts by export product similarity (Finger-Kreinin index), on the assumption that the US has stronger incentives to grant cuts where the partner is a close competitor.

### Baseline equation

$$\ln(\text{SALES}_{a,j,i,t}) = \alpha + \beta_1 \text{PTATARIFFCUTS(US)}_{i,j,t-1} + \beta_2 \text{LNEMPLOYEES}_{a,j,i,t} + \beta_3 \left[\text{PTATARIFFCUTS(US)}_{i,j,t-1} \times \text{LNEMPLOYEES}_{a,j,i,t}\right] + \beta_4 \mathbf{C}_{i,t-1} + \phi_i + \varsigma_j + \tau_t + \varepsilon_{a,j,i,t}$$

where $\phi_i$, $\varsigma_j$, $\tau_t$ are country, industry, and period fixed effects respectively, and $\mathbf{C}_{i,t-1}$ is a vector of country-level controls (log GDP per capita, GATT/WTO membership, BIT with US, cumulative PTA depth). The coefficient $\beta_3$ on the interaction captures how the effect of preferential tariff cuts varies with affiliate size. Separately, models substitute LNEMPLOYEES with affiliate-level PRODUCTIVITY (Solow residual).

### Estimators and diagnostics

OLS with robust standard errors clustered at the country or industry level. The IV is implemented as two-stage least squares. First-stage diagnostics: Kleibergen-Paap Wald F-statistic of 513 (baseline instrument) and 431 (similarity-weighted instrument), far exceeding any weak-instrument threshold. Kleibergen-Paap LM test confirms the model is not underidentified; Anderson-Rubin Wald test validates orthogonality.

### Market concentration analysis

At the country-industry-period level, OLS regresses HHI (and four-firm ratio) on PTA presence, PTA depth, and tariff cuts (separately, by host country and by US), controlling for democracy, political instability, GDP per capita, trade openness, GATT/WTO, BIT, and cumulative PTA depth, with country-industry and period fixed effects.

### Robustness checks

1. HQ-period fixed effects (absorbing all firm-level time-varying confounders)
2. Country-industry-period fixed effects (absorbing any industry-country-time shocks)
3. Country-specific time trends
4. Industry-specific time trends
5. Dropping affiliates with positive pre-PTA US sales (mitigating pre-existing trading relationship bias)
6. Analysis at the headquarters level (aggregating affiliates by MNC-country-period)
7. Flexible interactions with employment quintile dummies (non-parametric test)
8. Restriction to industries with no tariff cuts (turning off the tariff channel)

---

### References (Wikilinks)

[[AlleeElsig2016_PTADisputeSettlement]]
[[BacciniUrpelainen2014_CuttingGordianKnot]]
[[BagwellStaiger1999_EconomicTheoryGATT]]
[[BaierBergstrand2007_FTAsIncreaseTrade]]
[[BaileyGoldsteinWeingast1997_InstitutionalRootsTradePolicy]]
[[BernardJensen1999_ExceptionalExporterPerformance]]
[[BernardJensenSchott2006_TradeCostsFirmsProductivity]]
[[BernardJensenSchott2009_ImportersExportersMultinationals]]
[[Bilir2014_PatentLawsProductLifecycle]]
[[Blanchard2007_FDIEndogenousTariffs]]
[[BlanchardMatschke2015_USMultinationalsPreferentialAccess]]
[[Bombardini2008_FirmHeterogeneityLobby]]
[[ButheMillner2008_PoliticsFDIDevelopingCountries]]
[[ButheMillner2014_FDIInstitutionalDiversityTrade]]
[[CarrMarkusenMaskus2001_KnowledgeCapitalModel]]
[[Chase2003_EconomicInterestsRegionalTrade]]
[[DurBacciniElsig2014_DesignTradeAgreements]]
[[FingerKreinin1979_ExportSimilarity]]
[[GoldsteinRiversTomz2007_GATTWTOTrade]]
[[GowanKim2005_ExclusiveCountryClub]]
[[GrossmanHelpman1994_ProtectionForSale]]
[[Helpman2006_TradeFDIOrganizationFirms]]
[[HelpmanMelitzYeaple2004_ExportVersusFDI]]
[[JensenQuinnWeymouth2015_SupplyChainsTradeDisputes]]
[[JohnsWellhausen2016_UnderOneRoof]]
[[KimIS2016_PoliticalCleavagesIndustry]]
[[KimSY2015_RegionalisationProductionNetworks]]
[[Manger2009_InvestingProtection]]
[[MelitzOttaviano2008_MarketSizeProductivity]]
[[Milner1988_ResistingProtectionism]]
[[OsgoodEtAl2016_FirmGlobalization]]
[[Trefler2004_LongShortCanadaFTA]]
