---
area: trade/environment
tags:
  - area/trade/environment
---

# In Search of Pollution Havens? Dirty Industry in the World Economy, 1960 to 1995 (1998)

**Mani, M., & Wheeler, D. (1998)**
*The Journal of Environment & Development*, 7(3), 215–247
[Paper Link](https://doi.org/10.1177/107049659800700302)

---

## Abstract

The past three decades have witnessed rapid economic development, particularly in countries that have pursued relatively open economic policies. Rising environmental awareness in the 1960s also led to a rapid tightening of pollution regulation in the industrial economies. According to the "pollution havens" hypothesis, the result should have been more rapid growth of dirty industries in unregulated economies that were open to international trade. Using data for the period 1960 to 1995, the authors find that the displacement of pollution to developing countries has not been a major phenomenon for several reasons. Tendencies toward formation of pollution havens have been self-limiting because economic growth has generated countervailing effects through increases in regulation, technical expertise, and investment in cleaner production. In practice, the authors argue that pollution havens have apparently been as transient as low-wage havens.

---

## Research Question

Did the tightening of environmental regulation in industrial economies from the 1960s onward push "dirty" (pollution-intensive) industrial production toward developing countries that remained open to trade, as the pollution-havens hypothesis predicts? The paper is not a causal econometric exercise: it is a descriptive/comparative study exploiting cross-country and cross-time variation in production, consumption, and trade shares of pollution-intensive industries (Japan, North America, Western Europe, the Asian NIEs, developing Asia, Latin America), read jointly against the historical timing of environmental regulation and income growth in each region.

---

## Results

Pollution-intensive output as a share of total manufacturing fell in the OECD and initially rose in parts of the developing world (Asia, Latin America) from 1960–1995, but the authors argue this reflects normal industrialization sequencing rather than a durable "haven" effect: as developing economies grow, the same forces that curbed dirty production in the OECD (tightening regulation, rising technical expertise, cleaner investment, income-driven demand shifts) reassert themselves, making any competitive advantage from lax regulation self-limiting and transient — comparable to the erosion of a low-wage advantage. The Japan case study is presented as the clearest illustration: dirty-sector output share declined steadily from the mid-1960s, tracking closely with the timing of Japanese pollution-control legislation (1967–1971) and rising energy/land/capital costs, more than with the 1970s energy-price shocks. Newly industrializing economies (Korea, Singapore, and others) show the same later-stage decline once income and regulation reach comparable thresholds, and the paper concludes that trade-restrictive policy responses to the pollution-havens hypothesis are not well supported by the evidence.

---

## Data

Comparative panel of industry-level statistics, primarily descriptive/graphical rather than a single regression sample, covering Japan, North America, Western Europe, the Asian NIEs (Korea, Singapore, and others), developing Asia (including Korea, Singapore, Pakistan, the Philippines, India), and Latin America, from 1960 to 1995 (data availability varies by region and series).

**Data sources**: industry production/output data from the United Nations Industrial Development Organization (UNIDO) annual industrial statistics database, at the 3-digit International Standard Industrial Classification (ISIC) level; trade data (imports/exports by ISIC classification and country) from the United Nations TARS database; openness index (exports + imports over nominal GDP) from the Summers-Heston international database.

**Key variables**: sectoral output/production share of total manufacturing; consumption/production ratio; import/export ratio; openness index; timing of national environmental regulation by pollutant type (air/water/toxics) and country.

**Pollution-intensive sector definition**: five ISIC-level "dirty" industries used as the headline classification — 341 pulp and paper, 351 industrial chemicals, 353 petroleum refining, 371 iron and steel, 372 non-ferrous metals — ranked using average pollutant-intensity scores across air, water, and metals categories (Table 1 of the paper). The paper notes that petroleum refining (353) is excluded from some of its own regional comparisons "because a very few countries are actually involved in its production," and that 369 (non-metallic mineral products, including cement) also ranks among the most pollution-intensive sectors in the underlying ranking table even though it is not always carried in the headline five — a source-level ambiguity worth flagging (see Relevance note below).

### Illustrative dataset structure

| Region | Year | Dirty-sector share of manufacturing | Openness index | Regulation enacted (water) |
|---|---|---|---|---|
| Japan | 1965 | high | — | 1958 |
| Japan | 1985 | low | — | 1958 |
| Korea | 1970 | rising | moderate | — |
| Korea | 1990 | falling | high | — |

*Note: illustrative only — the paper reports these as time-series charts by region/sector, not as a single pooled micro-panel.*

---

## Methodology

**Identification strategy.** Not a causal design: the paper documents co-movement between (a) the share of pollution-intensive production/trade in a region and (b) the historical timing of environmental regulation and income growth in that region, and argues the pattern is consistent with a "self-limiting" pollution-haven story rather than a permanent trade-driven reallocation. There is no instrument, fixed-effects panel regression, or formal treatment/control comparison.

**Method.** Descriptive time-series and cross-region comparison of industry shares (production, consumption, trade) for the five ISIC dirty sectors, benchmarked against qualitative regulation timelines (e.g., Japan's water-quality law of 1958, national pollution agencies established in the late 1960s/1970s) and against energy/capital/labor intensity ratios computed for Japan as a case study (dirty sectors run roughly 3× the land intensity and 2:1 capital/investment intensity of the five "clean" comparison sectors).

**Target parameter.** None in the causal-inference sense; the object of interest is the qualitative shape and timing of the production/trade share trajectories relative to regulation and income growth.

**Threats to identification, acknowledged by the authors.** The paper explicitly notes it cannot separate the regulation channel from confounding factors moving on a similar timeline — energy price shocks (1973–74, 1978–80) and income-elasticity-driven demand shifts are both discussed as plausible alternative explanations for declining dirty-sector shares, and the Japan energy-price-shock hypothesis is specifically tested against timing and found less consistent than the regulation-timing story.

**Robustness / triangulation.** Rather than formal robustness checks, the paper triangulates across multiple regions (Japan, North America, Western Europe, NIEs, developing Asia, Latin America) and multiple outcome measures (production share, consumption/production ratio, import/export ratio) to argue the self-limiting pattern is not idiosyncratic to one country or one measure.

---

## Relevance to Paper_PTA

This is (jointly with [[LowYeats1992_DoDirtyIndustriesMigrate]]) the source of the project's **`dirty_p` binary classification** (`New/Code/02_dirty_goods.R`, `New/Data/Classifications/dirty_goods_hs6.csv`). The project uses the "classic" 5-sector Mani-Wheeler core (341/351/353/371/372) plus an extended variant adding 369 (cement/non-metallic minerals), mapped ISIC Rev.2 → ISIC Rev.3 → HS6 via the official WITS/UNSD correspondence table. **Note found while building this card**: this paper's own Table 1 ranking treats 369 (non-metallic minerals) as consistently pollution-intensive across all four rankings (air/water/metals/overall) and explicitly *excludes* petroleum (353) from parts of its regional analysis "because a very few countries are actually involved in its production" — i.e., the paper's own internal ranking does not single out exactly the same "core five" that the secondary literature (and this project) conventionally attributes to "Mani-Wheeler." The project's choice (353 in core, 369 in extended only) matches the version most commonly cited downstream, not a literal reading of this paper's Table 1 — worth keeping in mind if a referee probes the sector list closely.

---

### References (Wikilinks)

[[BirdsallWheeler1993_TradePolicyIndustrialPollution]]
[[Brandon1993_EnvironmentalStrategyAsia]]
[[Dasgupta1995_EnvironmentalRegulationDevelopment]]
[[Dasgupta1997_CitizenComplaintsEnvironmental]]
[[GrossmanKrueger1995_EconomicGrowthEnvironment]]
[[Gruver1976_OptimalInvestmentPollutionControl]]
[[Hettige1998_IndustrialPollutionKuznets]]
[[Leonard1988_PollutionStruggleWorldProduct]]
[[Mani1996_EnvironmentalTariffsPollutingImports]]
[[ModyWheeler1990_AutomationWorldCompetition]]
[[PargalWheeler1996_InformalRegulationIndustrialPollution]]
[[Pethig1976_PollutionWelfareEnvironmentalPolicy]]
[[Robison1988_IndustrialPollutionAbatementBalance]]
[[SeldenSong1994_EnvironmentalQualityKuznetsCurve]]
[[SeldenSong1995_NeoclassicalGrowthJCurve]]
[[Shafik1994_EconomicDevelopmentEnvironmentalQuality]]
[[Tobey1990_EffectsDomesticEnvironmentalPolicies]]
[[WheelerMody1992_InternationalInvestmentLocation]]
[[WorldBank1997_WorldDevelopmentReport]]
[[LowYeats1992_DoDirtyIndustriesMigrate]]
[[LucasWheelerHettige1992_EconomicDevelopmentEnvironmentalRegulationMigration]]
[[Wang1996_PricingIndustrialPollutionChina]]

<!-- Unresolved (8): no DOI in Crossref reference list; author+year bibliographic search did not return a confident (>=0.75 title-similarity) match
  - Duerkson, C. (1980) — Columbia Journal of World Business, p.52
  - Hettige, H. et al. (1992) — American Economic Review Papers and Proceedings, p.478
  - Hettige, H. et al. (1995) — "IPPS: The Industrial Pollution Projection System", Policy Research Dept. Working Paper No. 1431
  - John, A. (1992) — "An overlapping generations model of growth and the environment"
  - Kalt, J.P. (1985) — "Impact of domestic environmental policies on U.S. international competitiveness", Energy and Environmental Policy Center Discussion Paper No. 1411
  - Leonard, J. (1984) — "Are environmental regulations driving U.S. industry overseas?", Conservation Foundation
  - Pearson, C.S. (1987) — "Multinational corporations, environment, and the Third World: business matters"
  - Walter, I. (1982) — "Environment and Trade", vol. 2, p.235
-->
