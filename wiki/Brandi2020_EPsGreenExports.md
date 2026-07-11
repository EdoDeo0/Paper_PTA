---
area: trade/environment
tags:
  - area/trade/environment
---

# Do Environmental Provisions in Trade Agreements Make Exports from Developing Countries Greener?

**Brandi, Schwab, Berger & Morin (2020)**
*World Development*
[Paper Link](https://doi.org/10.1016/j.worlddev.2020.104899)

---

## Abstract

Environmental provisions in preferential trade agreements (PTAs) are increasing in terms of their number and variety. The economic effects of these environmental provisions remain largely unclear. It is, therefore, necessary to determine whether the trend to incorporate environmental provisions in PTAs counteracts the goal to spur economic development through trade via these PTAs. This is the first article in which the trade effects of environmental provisions in PTAs are thoroughly investigated. The spotlight is put on developing countries for which the assumed trade-off between economic development and environmental protection is particularly acute. This article uses a new fine-grained dataset on a broad range of environmental provisions in 680 PTAs, combined with a panel of worldwide bilateral trade flows from 1984 to 2016. We show that environmental provisions can help reduce dirty exports and increase green exports from developing countries. This effect is particularly pronounced in developing countries with stringent environmental regulations. By investigating how environmental provisions in PTAs affect trade flows, this article contributes to the literature on the following topics: international trade and the environment; design and impacts of trade agreements; and greening the economy in developing countries. It also shows that the design of trade agreements matters. Environmental provisions can be used as targeted policy tools to promote the green transformation and to leverage synergies between the economic and environmental effects of including environmental provisions in trade agreements.

---

## Research Question

Do environmental provisions (EPs) in PTAs reshape the export composition of developing countries — specifically, do they reduce the share of dirty (polluting) goods and increase the share of green (environmental) goods? The source of variation is the number and type of EPs across 680 PTAs from the TREND database (Morin et al. 2018), which varies both across country pairs and over time as new agreements enter into force. The methodology is a panel gravity regression with country-pair, exporter-year, and importer-year fixed effects (OLS baseline; PPML robustness).

---

## Results

Environmental provisions in PTAs do not reduce overall export volumes — there is no general trade-off between trade and environmental goals. However, they do reshape the composition of exports: trade-restrictive provisions significantly reduce the share of dirty goods in developing country exports (−0.72 pp per average EP, roughly −5% of the mean dirty share of 14%). Liberal provisions increase the share of green goods in developing country exports (+0.4 pp per liberal provision, +17% of the mean green share). Both effects are concentrated in developing countries already exhibiting stronger environmental performance (above-median Yale EPI score). Brown developing countries show no significant response, suggesting that baseline regulatory capacity is a prerequisite. Effect on overall export volumes: null across all specifications.

---

## Data

The paper combines two datasets into a country-pair-year panel spanning 1984–2016.

**Trade data** come from UN Comtrade, aggregated at the exporter-importer-year level. The dependent variables are constructed as shares: DIRTSHARE (share of dirty goods in total merchandise exports, 0–100%) and GREENSHARE (share of environmental/green goods, 0–100%). Total observations: 476,152 directed bilateral trade flows (all countries); 348,844 for the developing-country exporter subsample. About 29–30% of observations are under a PTA.

**Environmental provision data** come from the TREND database (Morin, Dür & Lechner 2018), which codes 286 types of environmental provisions across 630 PTAs signed 1947–2016. The paper uses a subset of 567 PTAs (568 with complete data). Mean ENVPROVS = 14.4 per PTA; mean under an active PTA in the trade sample = 27.6. Provisions are separated into trade-restrictive (mean 1.58 per PTA; e.g. ratification requirements, hazardous trade restrictions) and liberal (mean 0.41 per PTA; e.g. reduction of tariffs on environmental goods, harmonization).

**PTA depth** is controlled using the DESTA depth index (Dür, Baccini & Elsig 2014), ranging 0–3.69 (normalized), to prevent confounding EP effects with overall PTA depth. Correlation between ENVPROVS and DEPTH = 0.67; VIF < 5 for all variables.

**Dirty goods** are defined following Low & Yeats (1992) based on pollution abatement and control expenditure intensity — approximately 15% of world exports. Classification is at SITC 3-digit level.

**Green goods** are defined using the combined OECD (132 items) and APEC (54 items) list totalling 142 HS6 products — approximately 2.8% of world exports. WTO Friends' list used as robustness check.

**Brown/green classification** of developing countries: Yale Environmental Performance Index (EPI) 2018, median cutoff = 58.8; 64% of developing-country export flows come from brown (below-median EPI) countries.

### Illustrative dataset structure

| Exporter e | Importer i | Year t | DIRTSHARE (e,i,t) % | GREENSHARE (e,i,t) % | ENVPROVS (e,i,t) | RESTRICTIVE | LIBERAL | PTA (e,i,t) | DEPTH (e,i,t) |
|---|---|---|---|---|---|---|---|---|---|
| Brazil | Chile | 2004 | 15.8 | 3.1 | 8 | 2 | 0 | 1 | 1.45 |
| India | USA | 2010 | 22.1 | 2.3 | 0 | 0 | 0 | 0 | 0.00 |
| Thailand | Australia | 2012 | 11.2 | 4.7 | 12 | 1 | 1 | 1 | 1.82 |
| Peru | USA | 2009 | 14.5 | 2.8 | 24 | 5 | 2 | 1 | 2.15 |
| China | EU | 2005 | 18.3 | 1.9 | 0 | 0 | 0 | 0 | 0.00 |

*Note: Values illustrative. DIRTSHARE and GREENSHARE are shares (0–100%) of dirty/green goods in total merchandise exports from e to i in year t. ENVPROVS, RESTRICTIVE, and LIBERAL are the maximum counts across all PTAs in force between e and i. DEPTH is the normalized DESTA index (0–3.69).*

---

## Methodology

**Identification strategy.** The authors exploit the dyadic panel structure: variation in EP depth comes from which PTAs a country pair has in force and how many environmental provisions those PTAs include. The strategy compares the change in export composition induced by PTAs with more EPs to the change induced by PTAs with fewer EPs. Since the number of provisions within a given PTA does not vary over time (EPs are fixed at signing), within-pair identification is off the *entry into force* event — i.e., comparing pre- vs. post-PTA periods.

**Estimator.** OLS with two-way FE (country-pair and country-year) as the baseline. PPML used as robustness (Santos Silva & Tenreyro 2010; results unchanged). The dependent variables are shares bounded between 0 and 1, so log-linearization is not necessary and PPML's zero-handling advantage is less relevant here; OLS with clustered SEs is the preferred specification.

**Target parameter.** The coefficient β on ENVPROVS (or RESTRICTIVE/LIBERAL) is the within-pair effect of an additional environmental provision on the share of dirty or green exports, comparing across PTA pairs with different EP depth. It is an average treatment effect across all country pairs that signed PTAs during the sample.

**Baseline equation:**

SHARE_eit = β · ENVPROVS_eit + c · PTA_eit + d · DEPTH_eit + α_ei + α_et + α_it + ε_eit

where:
- SHARE_eit = share of dirty (DIRTSHARE) or green (GREENSHARE) goods in total exports from e to i at time t
- ENVPROVS_eit = max number of EPs in any PTA in force between e and i at time t
- PTA_eit = binary indicator for PTA in force
- DEPTH_eit = max DESTA depth index for any PTA in force between e and i
- α_ei = country-pair fixed effect (absorbs distance, common language, colonial ties, average trade propensity)
- α_et = exporter-year fixed effect (absorbs GDP, income, domestic regulations, exporter-level shocks)
- α_it = importer-year fixed effect (absorbs importer demand, multilateral resistance)
- Standard errors clustered at the exporter-importer (country-pair) level

**Key threats to identification** and how they are addressed:
1. *Selection into PTA signing and EP inclusion* — partially absorbed by country-pair FEs (time-invariant selection) and exporter/importer-year FEs (time-variant country characteristics). Residual endogeneity from anticipatory effects of future trade changes is tested using lead variables (not significant for composition outcomes). A two-stage "surprise provisions" IV and a Heckman selection model both confirm robustness.
2. *Confounding from overall PTA depth* — directly controlled by including the DESTA depth index alongside ENVPROVS.
3. *Multicollinearity between ENVPROVS, DEPTH, RESTRICTIVE, LIBERAL* — VIF check: max VIF = 4.62 (ENVPROVS), well below the threshold of 10.
4. *Phase-in effects* — tested with 1-, 2-, 3-year lags of treatment. Not significant for composition outcomes.
5. *Wrong classification of green/dirty goods* — robustness with WTO Friends' list (results directionally consistent).

**Robustness checks conducted:** (1) varying FE combinations (no FE, pair FE only, full FE); (2) PPML; (3) standard gravity controls without FE; (4) enforcement mechanism interaction; (5) multiple PTA indicator instead of binary; (6) lead variables (pre-trends); (7) 1–3 year lags; (8) Heckman selection model; (9) IV with surprise provisions; (10) WTO Friends' list for green goods.

---

### References (Wikilinks)

[[Baccini2017_DistributionalConsequencesPTAs]]
[[DechezleprêtreSato2017_EnvRegCompetitiveness]]
[[Melitz2003_ImpactTrade]]
[[Morin2018_TRENDDataset]]
[[Morin2019_KickStartingDiffusion]]
[[SantosSilvaTenreyro2006_LogGravity]]
[[NeriLaine2023_DeepTradeAgreements]]
[[Freund2010_RTAsThirdCountry]]
