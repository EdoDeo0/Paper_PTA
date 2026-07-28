---
area: trade/firms
tags:
  - area/trade/firms
---

# Input-Trade Liberalization, Export Prices and Quality Upgrading (2015)

**Bas, Maria & Vanessa Strauss-Kahn (2015)**
*Journal of International Economics*, 95(2): 250–262
[Paper Link](https://doi.org/10.1016/j.jinteco.2014.12.005) · Zotero key `V9KX3FWW`

---

## Abstract

This paper explores the impact of input-trade liberalization on imported input and exported product prices. Using Chinese transaction data for 2000–2006, we capture causal effects between tariff reductions and within-firm changes in prices. Identification is based on a quasi-natural experiment where some firms are exempt from paying tariffs. Both imported input and export prices rise. The effect on export prices is specific to firms sourcing inputs from developed economies and exporting output to high-income countries. Results are consistent with a scenario within which firms exploit the input tariff cuts to access high-quality inputs in order to quality-upgrade their exports.

---

## Research Question

Does a reduction in tariffs on imported intermediate inputs cause Chinese firms to raise the price (and, by inference, the quality) of both their imported inputs and their exported outputs? The source of variation is China's WTO-accession input-tariff cuts (2000–2006) combined with a dual customs regime: "ordinary" importers pay tariffs while "processing" importers are exempt, generating a natural treatment/control contrast for the same product-destination cell in the same year. Methodology: firm-product fixed-effects panel regressions on Chinese customs transaction data.

---

## Results

Following input-tariff cuts, ordinary (tariff-paying) firms import more varieties of intermediate inputs — but only from developed-country origins, not from developing-country origins. Import prices for intermediates rise with tariff cuts, and the effect is roughly twice as large for inputs sourced from high-income countries. Export prices at the firm-HS6-product level also rise following input-tariff cuts, and this effect is specific to output exported to high-income destinations, mirroring Schott (2004), Hallak & Schott (2011), and Khandelwal (2010) on the destination-income correlation with quality. The demand-side quality decomposition (Khandelwal 2010; Khandelwal, Schott & Wei) confirms the price increase reflects quality upgrading rather than higher markups or demand shocks; the "processing" control group and pre-accession placebo checks rule out that tariff reductions were politically targeted at firms already on a different price trajectory.

---

## Data

The dataset is an unbalanced panel of Chinese firm-level trade transactions merged with the National Bureau of Statistics firm-production database.

- **Type:** Unbalanced panel
- **Unit of observation:** Firm (i) × HS6 product (k/h) × [origin/destination] country (c) × year (t); both an import-side panel (firm-product-origin) and an export-side panel (firm-product-destination) are used
- **Source:** Chinese Customs Trade Statistics (General Administration of Customs), National Bureau of Statistics of China (NBSC) firm-level production data, WTO/UN Comtrade tariff schedules
- **Time period:** 2000–2006 (spans China's 2001 WTO accession)
- **Key variables:** Firm-specific input tariff (weighted average across imported inputs, fixed initial-year weights), import unit value (price), export unit value (price), estimated export quality (Khandelwal-style demand residual), firm "ordinary" vs. "processing" trade-regime status, origin/destination GDP, real exchange rate, Herfindahl index of supplier concentration
- **Observations:** 2,286,393 firm-product-origin-year observations for the import-price regressions; 3,208,484 firm-HS6-product-destination-year observations for the main export-price specifications
- **Sample restrictions:** The processing-trade regime (tariff-exempt) firms serve as the control group; robustness checks exclude foreign-owned firms (over-represented among processing importers), non-private firms (over-represented among ordinary importers), and specific sectors (textiles, electronics)

### Illustrative dataset structure

| Firm i | HS6 product k | Origin/destination c | Year t | Import price (i,k,c,t) | Firm input tariff | Trade regime | Export price (i,k,c,t) |
|---|---|---|---|---|---|---|---|
| F001 | 854140 (semiconductors) | Germany | 2001 | 4.80 | 0.09 | ordinary | 11.20 |
| F001 | 854140 (semiconductors) | Germany | 2005 | 5.30 | 0.03 | ordinary | 12.90 |
| F002 | 854140 (semiconductors) | Germany | 2005 | 4.75 | 0.00 | processing | 11.40 |
| F003 | 610910 (T-shirts) | Vietnam | 2005 | 1.10 | 0.02 | ordinary | 3.10 |

*Note: values illustrative. "Firm input tariff" is the firm-specific weighted average tariff on imported intermediates, with weights fixed at initial-year import shares. Processing-regime firms are exempt from paying the tariff and serve as the control group.*

---

## Methodology

**Identification strategy.** The paper exploits two sources of variation jointly: (i) the schedule of HS6-level input-tariff cuts following WTO accession, which is argued to be exogenous to individual firms' import/export patterns (verified against pre-accession, i.e. 2000, industry characteristics, with no significant correlation found); and (ii) the dual customs regime, under which "processing" firms — which must re-export all output made with imported inputs — are exempt from the same tariffs that "ordinary" firms pay. Processing firms exporting the same product-destination variety in the same year serve as a control group, allowing a difference-in-differences-style comparison net of common product-destination-year shocks.

**Fixed effects structure — the key point for this project.** The baseline import-price regression is:

$$\ln(\text{import price})_{ikct} = \beta \, Tariff_{kt-1} + \gamma X_{ikct} + \mu_{ik} + \eta_c + \lambda_t + \varepsilon_{ikct}$$

with **firm–HS6-product fixed effects ($\mu_{ik}$, i.e. `fp` in this project's notation), origin-country fixed effects ($\eta_c$, i.e. `d`), and year fixed effects ($\lambda_t$, i.e. `t`)** — all three included jointly and explicitly reported as separate rows in the regression tables ("Firm-hs6 product fixed effects: yes / Origin country fixed effects: yes / Year fixed effects: yes"). The corresponding export-price regression uses the analogous **firm-HS6-product fixed effects** on the destination side, with firm-level controls (TFP, size, imported-variety count) and an industry Herfindahl index. Standard errors are clustered at the firm-product level (or firm level, depending on whether the tariff measure is firm- or firm-product-specific).

**Target parameter.** $\beta$ is the within-firm-product elasticity of the price (import or export) to the firm-specific tariff reduction — the price/quality response to input liberalization, identified purely from the timing and magnitude of tariff cuts affecting a firm's own input basket, net of any firm-product-specific level and any common origin- or year-level shock.

**Threats to identification and robustness.** (i) Endogeneity of tariff schedule to firm-level lobbying or anticipated trade patterns — tested and rejected via pre-accession correlations; (ii) selection into "ordinary" vs. "processing" status — argued to be exogenous to the tariff level itself, and robustness drops foreign/non-private firms and specific sectors to probe this; (iii) demand shocks or rising marginal costs mimicking quality upgrading — addressed via the structural quality decomposition (Khandelwal 2010; Khandelwal, Schott & Wei) rather than treating the raw price increase as sufficient evidence of quality; (iv) status-specific time trends — addressed by interacting trade-regime status with a time dummy.

---

### Relevance to Paper_PTA

Documents `fp` (firm-product) fixed effects jointly with a market-side fixed effect (`d`, origin country) and `t` (year) in a Chinese customs-data panel — the same broad family of data as this project, though with `fp` rather than the more saturated `fpd`/`fdt` combination used here. Useful as a comparison point on how much a "lighter" specification (firm-product + country + year, i.e. essentially `fp`+`d`+`t`, well short of `fpd`) can still support causal claims when identification instead rests on a quasi-experimental control group (processing-regime firms) rather than on fixed-effects saturation alone. See [[Fixed_Effects_Guide]].

---

### References (Wikilinks)

[[Ahn2011_RoleIntermediariesFacilitatingTrade]]
[[Amiti2013_ImportCompetitionQualityUpgrading]]
[[Amiti2007_TradeLiberalizationIntermediateInputs]]
[[Amiti2014_ImportersExportersExchangeRate]]
[[Baldwin2011_ZerosQualitySpaceTrade]]
[[Bas2012_InputtradeLiberalizationFirmExport]]
[[Bas2014_DoesImportingMoreInputs]]
[[Bastos2010_QualityFirmsExportsWhere]]
[[Bernard2003_PlantsProductivityInternationalTrade]]
[[Boler2012_TechnologicalChangeInternationalSourcing]]
[[Branstetter2006_ChinasEmbraceGlobalization]]
[[Broda2006_FromGroundnutsGlobalizationStructural]]
[[Crozet2012_QualitySortingTradeFirmlevel]]
[[De2012_PricesMarkupsTradeReform]]
[[Ethier1982_NationalInternationalReturnsScale]]
[[Fajgelbaum2011_IncomeDistributionProductQuality]]
[[Fernandes2007_TradePolicyTradeVolumes]]
[[Fernandes2011_DoesTougherImportCompetition]]
[[Goldberg2010_ImportedIntermediateInputsDomestic]]
[[Gorg2010_WithinFirmproductExportPrices]]
[[Hallak2006_ProductQualityDirectionTrade]]
[[Hallak2011_EstimatingCrosscountryDifferencesProduct]]
[[Hallak2013_FirmsExportingBehaviorUnder]]
[[Harrigan2012_ExportPricesFirms]]
[[Hummels2005_VarietyQualityNationsExports]]
[[Iacovone2010_MultiproductExportersProductChurning]]
[[Johnson2012_TradePricesWithHeterogeneous]]
[[Kasahara2008_DoesImportedIntermediatesIncrease]]
[[Khandelwal2010_LongShortQualityLadders]]
[[Khandelwal2013_TradeLiberalizationEmbeddedInstitutional]]
[[Kugler2009_PlantsImportedInputsFacts]]
[[Kugler2012_PricesPlantSizeProduct]]
[[Manova2012_FirmsCreditConstraintsAlong]]
[[Manova2012_ExportPricesAcrossFirms]]
[[Martin2012_MarkupsQualityTransportCosts]]
[[Melitz2003_ImpactTradeIntraindustryReallocations]]
[[Pavcnik2002_TradeLiberalizationExitProductivity]]
[[Rauch1999_NetworksVersusMarketsInternational]]
[[Schor2004_HeterogeneousProductivityResponseTariff]]
[[Schott2004_AcrossproductVersusWithinproductSpecialization]]
[[Schott2008_RelativeSophisticationChineseExports]]
[[Topalova2011_TradeLiberalizationFirmProductivity]]
[[Upward2010_WeighingChinaExportBasket]]
[[Verhoogen2008_TradeQualityUpgradingWage]]
[[Yu2012_ChinasProcessingTradeFirmlevel]]

<!-- Unresolved (5):
  - Brandt, L. & Morrow, P. (2013), Tariffs and the Organization of Trade in China
  - Brandt, L., Van Biesebroeck, J., Wang, L. & Zhang, Y. (2012), WTO Accession and ...
  - Demir, B. (2012), Trading Tasks and Quality (mimeo)
  - Fan, H. & Li, Y. (2013), Imported Intermediate Inputs, Export Prices, and Trade
  - Halpern, L., Koren, M. & Szeidl, A. (2009), Imports and Productivity (mimeo, CEU)
-->
