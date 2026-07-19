---
area: trade/environment
tags:
  - area/trade/environment
---

# Do Dirty Industries Migrate? (1992)

**Low, P., & Yeats, A. (1992)**
Chapter in P. Low (Ed.), *International Trade and the Environment*, World Bank Discussion Paper 159, pp. 89–103, Washington, DC: World Bank
[Volume Link](https://documents.worldbank.org/en/publication/documents-reports/documentdetail/691041468765613139/international-trade-and-the-environment) — no DOI (pre-1992 World Bank discussion paper chapter)

> **Sourcing note**: this card is built from the chapter's citation record, from its treatment in [[ManiWheeler1998_PollutionHavensDirtyIndustry]] (which quotes and summarizes its finding directly), and from secondary literature that describes its data and method in detail. Direct access to the chapter's full text was attempted but not obtained (the World Bank's hosted PDF for this volume could not be retrieved). The Abstract, Data, and Methodology sections below are therefore reconstructed from citing sources, not read verbatim from the original — treat specific figures with appropriate caution and verify against the primary source before citing precise numbers in the paper.

---

## Abstract

No verbatim abstract available (pre-DOI World Bank discussion paper chapter; not indexed with an abstract on Crossref/OpenAlex). Per secondary-source description: the chapter tests whether developing countries gained comparative advantage in "dirty" (pollution-intensive) manufactured goods relative to industrial countries between 1965 and 1988, using a revealed comparative advantage (RCA) framework, and finds a shift in relative RCA toward developing countries over the period, while industrial (OECD) countries remained by far the largest absolute exporters of the same goods.

---

## Research Question

Did developing countries acquire a growing comparative advantage in pollution-intensive ("dirty") industries relative to industrial countries between 1965 and 1988, consistent with the pollution-havens hypothesis? Source of variation: cross-country, over-time changes in each country's revealed comparative advantage (RCA) in a set of dirty-industry product categories. Methodology: descriptive RCA index computation and ranking, not a causal regression design — the chapter explicitly does not attribute the pattern it documents to environmental policy specifically, noting it "is unlikely to be adequately explained by environmental policy" alone and could equally reflect labor-cost differences, natural-resource endowments, or ordinary industrialization sequencing.

---

## Results

Reported secondhand (see sourcing note above): RCA in dirty-industry products shifted toward developing countries over 1965–1988 relative to industrial countries, a pattern read by later authors as early, suggestive (not causal) evidence consistent with the pollution-havens hypothesis. At the same time, the chapter stresses that OECD countries remained overwhelmingly the largest absolute exporters of these same "dirty" products throughout the period (secondary sources cite the top 25 exporters of Low-Yeats dirty products accounting for roughly 85% of world trade in those products, all but a handful of them OECD members) — i.e., a rising relative RCA in developing countries coexisted with continued OECD dominance in absolute trade volumes. Mani & Wheeler (1998) cite this chapter specifically for the finding that "the developed-country share of 'dirty' exports in overall world exports has remained higher than the developing-country share, although the latter has increased."

---

## Data

Cross-country trade panel, 1965–1988, reported to cover on the order of 100+ countries (a commonly cited figure in the secondary literature is 109 countries).

**Key variables (per secondary-source description)**: bilateral or total export values by product category and country, used to construct a revealed comparative advantage (RCA) index per country per pollution-intensive product group; abatement-cost-based classification of "dirty" industries.

**Dirty-industry definition**: products of industries identified as incurring the highest pollution-abatement control costs in the United States — commonly summarized in the secondary literature as including iron and steel, non-ferrous metals, refined petroleum, metal manufactures, and pulp and paper, i.e. essentially the same sector list later carried forward (with headline "five sectors" framing) by [[ManiWheeler1998_PollutionHavensDirtyIndustry]].

**Data granularity**: country-level (not firm- or plant-level), single cross-section-over-time comparison (1965 vs. 1988 endpoints, per most secondary descriptions) rather than an annual panel.

### Illustrative dataset structure

| Country | Product group ("dirty") | RCA, 1965 | RCA, 1988 |
|---|---|---|---|
| USA | Iron & steel | high | lower |
| Korea | Iron & steel | low | rising |
| Germany | Industrial chemicals | high | high |
| Brazil | Pulp & paper | moderate | rising |

*Note: illustrative only — exact structure and index construction not independently verified against the primary text (see sourcing note).*

---

## Methodology

**Identification strategy.** None in the causal sense: a descriptive comparison of revealed comparative advantage indices across countries and two time points (1965, 1988), explicitly not attributed by the authors to environmental regulation alone.

**Estimator.** Revealed comparative advantage (RCA) index, the standard trade-economics measure of a country's export share in a product category relative to its share in world trade overall (the same construct used, e.g., in [[Sauvage2014_StringencyEnvironmentalGoods]]) — computed for each country and dirty-industry product group at each time point and compared.

**Target parameter.** Change in relative RCA ranking of developing vs. industrial countries in dirty-industry products, 1965 vs. 1988 — a descriptive shift, not an estimated causal effect.

**Threats to identification, acknowledged by the authors (per secondary sources).** The chapter is explicit that the RCA shift it documents could be driven by factors other than environmental-policy differentials — labor-cost differences between industrial and developing countries, natural-resource endowments, or the ordinary product cycle of industrialization — and states the pattern observed is unlikely to be fully explained by environmental policy alone.

**Robustness checks.** Not established from available sources; likely limited given the descriptive nature of the chapter and its era (a 1992 book chapter predates most modern robustness-check conventions).

---

## Relevance to Paper_PTA

This is (jointly with [[ManiWheeler1998_PollutionHavensDirtyIndustry]]) the source of the project's **`dirty_p` binary classification** (`New/Code/02_dirty_goods.R`). The sector list used by the project (pulp & paper, industrial chemicals, petroleum refining, iron & steel, non-ferrous metals as core; cement/non-metallic minerals in the extended variant) matches the abatement-cost-based dirty-industry definition commonly attributed to this chapter in the secondary literature. **Open item flagged during construction of this card**: the chapter's own primary text was not directly verified (see sourcing note) — if a referee questions the precise sector list or RCA methodology of Low & Yeats (1992) specifically, the primary source should be read directly before responding, since this card's Data/Methodology sections are reconstructed from citations rather than the original text.

---

### References (Wikilinks)

<!-- Non verificate - capitolo pre-DOI, non indicizzato su Crossref/OpenAlex, testo integrale non recuperato: nessuna lista di riferimenti bibliografici della fonte primaria disponibile per questa card -->
