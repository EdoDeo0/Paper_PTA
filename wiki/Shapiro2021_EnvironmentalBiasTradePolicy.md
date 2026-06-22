# The Environmental Bias of Trade Policy

**Shapiro (2021)**
*The Quarterly Journal of Economics*, 136(2), 831–886
[Paper Link](https://doi.org/10.1093/qje/qjaa042)

---

## Abstract (synthesis)

The paper documents that in most countries import tariffs and non-tariff barriers are substantially **lower on dirty than on clean industries**, where dirtiness is defined as CO2 emissions per dollar of output. This asymmetry amounts to an implicit global subsidy to CO2 emissions embodied in traded goods of several hundred billion dollars per year. The pattern is explained by upstreamness: dirty industries tend to be upstream, and downstream industries lobby for low tariffs on their inputs while final consumers are poorly organized. A quantitative general-equilibrium model shows that harmonizing trade policy across clean and dirty goods would cut global emissions with little change in global real income.

---

## Research Question

Is trade policy systematically biased with respect to the environment? I.e., does the *structure* of protection across products correlate with their emission intensity — and what would de-biasing it do to emissions and welfare?

---

## Results

- Tariffs and NTBs are systematically lower on emission-intensive (upstream) industries in nearly all countries.
- Implicit subsidy to embodied carbon: USD 550–800 billion/year globally.
- Driver: upstream location of dirty industries + lobbying structure (Grossman-Helpman logic), not environmental intent.
- Counterfactual: equalizing protection across clean/dirty goods reduces global CO2 with negligible real-income loss.

---

## Data

- Industry-level CO2 intensity (emissions per dollar of output), including indirect/embodied emissions via input-output linkages (Exiobase/WIOD-type MRIO data).
- Tariffs and ad-valorem equivalents of NTBs by country × industry.
- Public replication package (Harvard Dataverse) with industry emission intensities — **directly usable to build our `dirty_p` classification**.

---

## Methodology

- Descriptive cross-industry regressions of protection on emission intensity, within country, controlling for standard political-economy determinants.
- Quantitative GE model of trade and emissions for counterfactual harmonization.

---

## Relevance to Paper_PTA

1. **Source for the `dirty_p` measure (Fase R2):** the replication data provide CO2 intensity per industry; concord ISIC/NAICS → HS6 to define dirty products (top quartile of emission intensity).
2. **Warning for identification:** protection levels correlate mechanically with dirtiness. In our triple-diff, preferential tariff cuts under a PTA may be deeper for dirty (upstream) goods — so `tariffs_pref_pdt` must be controlled, otherwise EP×dirty picks up the tariff structure.
3. **Framing:** trade policy is *de facto* environmentally biased; EPs are the *de jure* attempt to correct it. A paper asking whether EPs bite fits squarely in this agenda.

---

### References (Wikilinks)

[[AbmanLundbergRuta2024_EPsRTAsDeforestation]]
[[CopelandShapiroTaylor2022_GlobalizationEnvironment]]
[[Brandi2020_EPsGreenExports]]
[[DechezleprêtreSato2017_EnvRegCompetitiveness]]
