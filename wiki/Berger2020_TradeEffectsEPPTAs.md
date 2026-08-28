---
area: trade/environment
tags:
  - area/trade/environment
---

# The Trade Effects of Environmental Provisions in Preferential Trade Agreements (2020)

Berger et al. (2020) — Axel Berger, Clara Brandi, Jean-Frédéric Morin, Jakob Schwab
Book chapter in *International Trade, Investment, and the Sustainable Development Goals*, Cambridge University Press
[Paper Link (Academia.edu)](https://www.academia.edu/download/61680617/Trade_Effects_of_EPs_in_PTAs20200104-105042-13oy4um.pdf)

## Abstract

The international community has acknowledged that international trade can be an effective means of helping to achieve the 2030 Agenda for Sustainable Development and its 17 SDGs. Traditionally, PTAs were designed to promote trade flows; they have become more comprehensive and now also cover non-economic policy areas, such as the environment. This chapter examines whether the inclusion of environmental provisions in PTAs changes the overall positive contribution that PTAs make to economic outcomes. Specifically, the authors ask whether environmental provisions reduce export flows between PTA partner countries. Using the TREND dataset and a gravity panel regression, they find that membership in PTAs with more environmental provisions is associated with less trade among partners — an effect fully driven by negative impacts on South-North trade flows (exports from developing to high-income countries).

## Research Question

Does the inclusion of environmental provisions in PTAs reduce bilateral trade flows, and does this effect differ by the level of development of trading partners? Identification exploits variation in the number of EPs across PTAs for country pairs that enter into at least one agreement. Methodology: gravity panel OLS with country-pair and exporter/importer-year fixed effects (PPML robustness check).

## Results

More environmental provisions in PTAs are associated with lower bilateral exports: each additional provision reduces trade by approximately 0.2%, so the average PTA (with ~27 EPs per trade flow) is associated with about 5% less trade creation than a zero-EP PTA. This negative effect is driven entirely by South-North flows (developing-country exports to high-income countries); North-North, North-South, and South-South effects are statistically indistinguishable from zero. The PPML estimates are not statistically significant across any subsample, so the aggregate OLS result is fragile. The finding is consistent with developing countries' concern that EPs serve as green protectionism.

## Data

Unbalanced panel of bilateral country-pair exports, 1984–2016. Unit of observation: exporter–importer–year. Trade data from the World Trade Flows database (Feenstra 2017); EP data from the TREND database (Morin et al. 2018, 286 provision types across 598 PTAs); PTA depth from DESTA (Dür et al. 2014). World Bank country income classification as of 2000 used to split North/South. Sample restricted to country pairs that signed at least one PTA during the period (~36% of all possible dyads). Main estimation sample: 250,014 observations across ~50,000 exporter-importer pairs.

| Variable | Description |
|---|---|
| EXPORTS (log) | Log bilateral exports, exporter e to importer i, year t |
| ENVPROVS | Max number of EPs in any PTA between the dyad |
| PTA | Binary: any PTA in force between the dyad in year t |
| DEPTH | Max DESTA depth index across PTAs between the dyad |
| # PTAs | Count of PTAs in force between the dyad in year t |

## Methodology

The identification strategy compares the change in exports when two countries enter a PTA with more EPs against those entering a PTA with fewer. Country-pair fixed effects control for time-invariant dyadic characteristics (distance, common border, historical ties) and partially for selection into agreements. Exporter-year and importer-year fixed effects absorb multilateral resistance and all country-specific time-varying factors (GDP, trade openness). Standard errors clustered at the exporter-importer level. PPML used as robustness check (insignificant results); sample robustness to excluding major oil exporters shown in appendix. The main threat to identification is that selection into agreements with more EPs may be correlated with bilateral trade trends — the within-pair FE absorbs the average level but not bilateral trends.

## References (Wikilinks)

[[Baghdadi2013_RTAsEmissions]]
[[Brandi2020_EPsGreenExports]]
[[Dur2014_DesignTradeAgreements]]
[[Morin2018_TradeEnvironmentNexus]]
[[SantosSilvaTenreyro2006_LogGravity]]
[[Baccini2017_DistributionalConsequencesPTAs]]

<!-- Unresolved (several): Baier & Bergstrand 2007, 2009; Egger et al. 2008, 2011; Lechner 2016, 2018; Milewicz et al. 2018 — no wikilinks created as these cards do not exist in the wiki -->
