# The Trade Impact of RTAs: Firm-Level Evidence from Georgia

**Neri-Laine, Barbara, Gianluca Orefice & Michele Ruta (2021)**
*World Bank Policy Research Working Paper 9768*
[Paper Link](https://doi.org/10.1596/1813-9450-9768)

---

## Abstract

This paper uses transaction-level data on Georgian exporters combined with information on the content of regional trade agreements (RTAs) from the World Bank Deep Trade Agreements database to study the impact of RTAs on exports at the firm level. The analysis shows that a 10 percent increase in RTA depth leads to a 0.46 percent increase in the average Georgian firm's exports. This average effect masks important heterogeneity: larger, more productive firms experience significantly larger export gains from deeper RTAs, while smaller firms do not benefit and may even be harmed. Legally enforceable provisions, WTO-plus core provisions, and GVC participation are the main channels through which deep RTAs promote exports.

---

## Research Question

Does the depth of an RTA — measured by the number of legally binding policy areas — increase Georgian firm-level exports, and are the effects heterogeneous across the firm size distribution? The source of variation is the staggered signing of 11 RTAs by Georgia over 2000–2020 with partners of varying depth (6–48 policy areas). The methodology is OLS with firm-year and sector fixed effects estimated at the firm-product-destination-year level.

---

## Results

A 10% increase in RTA depth raises the average Georgian firm's exports by 0.46%. Legally enforceable provisions drive most of this effect (+0.5% per 10% depth for WTO+ and Core provisions; WTO-X provisions are not significant). Heterogeneity is substantial: large firms (above the 75th percentile) gain +1.1% per 10% depth, while small firms experience a –1.2% effect. GVC-participating firms also benefit significantly more. Deep RTAs do not affect export prices, suggesting they work through reductions in non-iceberg trade costs (e.g., regulatory and fixed market-entry costs) rather than variable iceberg costs. Agriculture benefits significantly; manufacturing effects are null on average but heterogeneous by sector. Deep RTAs also increase the probability that a firm begins exporting to a market for the first time (extensive margin).

---

## Data

The dataset is an unbalanced panel at the firm–product–destination–year level.

- **Type:** Unbalanced panel
- **Unit of observation:** Firm (f) × HS product (k) × destination country (j) × year (t)
- **Source:** Georgian Exporter Dynamics Database (World Bank EDD) 2000–2020; World Bank DTA database; Georgia's 11 RTAs
- **Time period:** 2000–2020
- **Key variables:** Export value (USD), RTA depth (number of covered policy areas as share of maximum), enforceability dummy, WTO+ provision count, WTO-X provision count, GVC status, firm size (percentile in initial export-value distribution)
- **Observations:** 224,889
- **RTA depth range:** 6–48 policy areas across Georgia's 11 RTAs

### Illustrative dataset structure

| Firm f | Product k (HS4) | Destination j | Year t | ln(export value) (f,k,j,t) | RTA depth (0–1) (j,t) | Enforceable depth | GVC firm (f,t) | Firm size cat. |
|---|---|---|---|---|---|---|---|---|
| F001 | 0901 (Coffee) | Turkey | 2004 | 10.2 | 0.21 | 0.15 | 0 | Small |
| F001 | 0901 (Coffee) | Ukraine | 2007 | 11.8 | 0.38 | 0.25 | 0 | Small |
| F002 | 2710 (Mineral oils) | EU | 2014 | 14.3 | 0.91 | 0.82 | 1 | Large |
| F003 | 6204 (Women's suits) | Russia | 2010 | 9.5 | 0.42 | 0.31 | 0 | Medium |
| F004 | 7208 (Steel sheets) | Armenia | 2016 | 12.6 | 0.25 | 0.19 | 0 | Small |

*Note: Values illustrative. RTA depth = share of 48 policy areas covered by the RTA in force. Enforceable depth counts only provisions with binding dispute settlement. GVC firm = 1 if firm imports inputs in that year. Firm size is fixed at initial observed year.*

---

## Methodology

The estimating equation is a structural gravity model at the firm level:

$$\ln(\text{Export}_{fkjt}) = \beta_1 \cdot \text{Depth}_{jt} + \beta_2 \cdot \ln(\text{GDP}_{jt}) + \alpha_{ft} + \alpha_k + \varepsilon_{fkjt}$$

where $\alpha_{ft}$ are firm-year fixed effects (absorbing all time-varying productivity shocks at the firm level) and $\alpha_k$ are product (sector) fixed effects. Variation comes from within-firm changes in export performance across destinations that differ in the depth of RTAs with Georgia. To test heterogeneity, depth is interacted with firm size categories and GVC status. The panel structure and firm-year fixed effects address concerns about omitted variable bias from firm-level trends. A potential remaining threat is that deeper RTAs tend to be signed with larger trading partners that also experience independent demand growth; robustness checks include destination-year fixed effects.

**Estimator:** OLS (PPML results reported as robustness checks).

---

## References (Wikilinks)

[[HofmannOsnagoRuta2017_DeepTradeAgreements]]
[[MattooRochaRuta2020_HandbookDeepTrade]]
[[Melitz2003_ImpactTrade]]
[[BaierBergstrand2007_RTAsPanelData]]
[[FernandesFreundPierola2016_ExporterDynamics]]
