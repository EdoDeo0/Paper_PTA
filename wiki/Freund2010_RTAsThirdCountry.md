# Are Regional Trade Agreements Building or Stumbling Blocks?

**Freund, Caroline (2010)**
*The World Economy*, 33(11):1589–1605
[Paper Link](https://doi.org/10.1111/j.1467-9701.2010.01283.x)

---

## Abstract

This paper examines whether regional trade agreements (RTAs) divert trade from non-members and whether they stimulate or retard multilateral trade liberalization. Using panel data on six RTAs in Latin America, the results indicate no evidence of significant trade diversion. On the contrary, preference margins — the gap between the preferential and MFN tariff rate — are positively correlated with subsequent reductions in MFN tariffs, consistent with the "building block" hypothesis. These findings suggest that RTAs tend to foster rather than impede multilateral liberalization.

---

## Research Question

Does preferential trade liberalization through RTAs divert imports from non-members (stumbling block) or is it compatible with — and perhaps even supportive of — multilateral tariff reductions (building block)? Identification comes from within-country, within-good variation in preference margins and MFN tariffs over time across six Latin American RTAs. The methodology is panel OLS with bilateral pair × good fixed effects and country-year fixed effects.

---

## Results

There is no evidence of trade diversion: preferential tariffs do not reduce imports from non-member countries, and if anything, the conditional effect is slightly positive. Preference margins are positively associated with lower future MFN tariffs: a 1 percentage point increase in the preference margin in a given sector predicts roughly a 0.3 percentage point decline in the MFN tariff the following year. The results are robust to controlling for MFN tariff levels and are consistent across RTAs. The building block interpretation is further supported by the absence of a strong diversion effect in the import-level regressions.

---

## Data

The dataset is an unbalanced panel at the country × industry × year level.

- **Type:** Panel data, industry–year
- **Unit of observation:** Country-pair × ISIC 4-digit industry × year (trade equations); country × ISIC 4-digit industry × year (MFN equation)
- **Countries:** 9 Latin American countries party to 6 RTAs (NAFTA, Mercosur, Andean Community, EU-Mexico, EU-Chile, EU-Mercosur negotiations — primarily NAFTA, Mercosur, Andean Community in the empirical section)
- **Source:** Latin American tariff data from Estevadeordal, Freund & Ornelas (2008); bilateral import data
- **Time period:** ~1990–2001
- **Key variables:** Preference margin (MFN tariff minus preferential tariff), MFN tariff, bilateral import value, ISIC 4-digit industry codes

### Illustrative dataset structure

| Importer i | Exporter j | Industry g (ISIC 4-digit) | Year t | ln(Imports) (i,j,g,t) | Preference margin (i,g,t) | MFN tariff (i,g,t) | ΔMFN tariff (i,g,t) |
|---|---|---|---|---|---|---|---|
| Mexico | USA | 3410 (Motor vehicles) | 1993 | 15.2 | 0.00 | 0.25 | 0.00 |
| Mexico | USA | 3410 (Motor vehicles) | 1997 | 16.4 | 0.08 | 0.25 | 0.00 |
| Brazil | Argentina | 3520 (Other chemicals) | 1994 | 12.1 | 0.05 | 0.18 | −0.02 |
| Chile | Germany | 1511 (Meat products) | 1999 | 13.5 | 0.00 | 0.22 | 0.00 |
| Chile | Germany | 1511 (Meat products) | 2001 | 13.8 | 0.12 | 0.20 | −0.02 |

*Note: Values illustrative. Preference margin = MFN tariff − preferential tariff, set to 0 for non-PTA pairs. ΔMFN is the year-on-year change in the applied MFN rate. Trade equations estimated with country-pair × industry and year FEs.*

---

## Methodology

The paper estimates two complementary equations:

**Import equation** (tests trade diversion):
$$\ln(M_{ijtg}) = c_{ijg} + c_{jtg} + \beta_1 \cdot \text{margin}_{igt} + \beta_2 \cdot \text{MFN}_{igt} + \varepsilon_{ijtg}$$

where $c_{ijg}$ are bilateral pair × industry fixed effects (absorbing all time-invariant bilateral determinants) and $c_{jtg}$ are exporter × industry × year effects. $\beta_1 < 0$ would indicate trade diversion.

**MFN equation** (tests building blocks):
$$\Delta\text{MFN}_{ijt} = c_{ij} + c_{jt} + \beta_0 \cdot L.\Delta\text{margin}_{ijt} + \varepsilon_{ijt}$$

$\beta_0 < 0$ means rising preference margins lead to subsequent MFN cuts (building blocks). The key identification assumption is that changes in preference margins are exogenous to future MFN changes conditional on the fixed effects. Potential concern: large trading partners that sign RTAs may also independently pursue WTO-level liberalization, but the lagged specification mitigates this.

**Estimator:** OLS with high-dimensional fixed effects. No IV used; the paper relies on the richness of the fixed effects structure to address endogeneity.

---

### References (Wikilinks)

[[BagwellStaiger1999_PoliticalEconomyWTO]]
[[BaierBergstrand2004_RTAsTrade]]
[[EstevadeordealFreundOrnelas2008_RTAsLiberalization]]
[[Limao2006_PreferentialTradeMultilateral]]
[[Magee2008_NewPTAs]]
[[Romalis2007_NAFTATradeCreation]]
[[Trefler2004_NAFTA]]
[[Viner1950_CustomsUnion]]
