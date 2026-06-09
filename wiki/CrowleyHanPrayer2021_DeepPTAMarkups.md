# The Value of Deep Trade Agreements in the Presence of Pricing-to-Market

**Crowley, Meredith A., Lu Han & Thomas Prayer (2021)**
*World Bank Policy Research Working Paper 9600*
[Paper Link](https://doi.org/10.1596/1813-9450-9600)

---

## Abstract

Do preferential trade agreements (PTAs) lead to greater market integration, more intense competition and less market power for firms? This paper integrates the detailed data on 257 preferential trade agreements from the World Bank's Deep Trade Agreements (DTA) database with administrative customs datasets of product-level exports by firms from thirteen developing and emerging countries to estimate the responsiveness of firm-level exports, export prices, and destination-specific markups to trade and domestic policy commitments enshrined in deep trade agreements. The findings suggest that both the direct and indirect effects of deep trade agreement provisions on export sales are quantitatively significant. Perhaps more interestingly, there is suggestive evidence of a pro-competitive effect of PTAs — markups decline when a firm's origin participates in a PTA or when more of its competitors gain PTA access to a destination.

---

## Research Question

Do deep PTA provisions — both directly (between origin and destination) and indirectly (through competitors' PTAs with the destination) — affect firm-level export values, prices, and destination-specific markups? Do PTAs exert pro-competitive pressure that reduces market power? Variation comes from the staggered entry into force of 257 PTAs across 13 exporting countries, 1993–2016. Methodology: OLS with rich fixed effects at the firm-origin-product-time, destination-product-time, and origin-destination levels.

---

## Results

- **Export values:** A PTA between origin and destination is associated with 52% higher firm-level exports (partial effect 0.42). When 10% more of a firm's competitors sign a PTA with the destination, the average firm's exports decline by 7.9% (β₂ = –0.82).
- **Prices:** PTA formation modestly lowers export prices (–3%). Competitors' PTA access reduces prices by 0.7% for every 10% additional competitors — a statistically significant pro-competitive effect operating through increased third-country competition.
- **Markups:** Markups are 3% lower when a firm has a PTA with a destination. Each additional 10% of competitors signing a PTA with the destination reduces markups by 0.5% — evidence of a pro-competitive spillover (after controlling for destination-product-time multilateral resistance). For highly differentiated goods (CCHS classification), the pro-competitive effect on markups is –0.9% per 10% additional competitors, roughly double the average.
- **Tariff puzzle:** Higher bilateral tariffs are positively associated with both prices and markups. This is explained by a market-share mechanism: higher tariffs reduce a firm's market share among all competitors from the destination's perspective, but increase its share among rivals from its own origin, generating higher markups in an oligopolistic framework.
- **DTA provisions:** Rules of origin (self-certification) provisions are associated with higher markups; competition policy and mutual recognition provisions with lower markups.

---

## Data

The dataset is an unbalanced panel at the firm–product–origin–destination–year level.

- **Type:** Unbalanced panel (positive trade flows only; PPML not feasible at this granularity)
- **Unit of observation:** Firm (f) × HS 6-digit product (i) × origin (o) × destination (d) × year (t)
- **Countries:** 13 developing/emerging economies — Albania, Bulgaria, Burkina Faso, Guatemala, Jordan, Malawi, Mexico, Peru, Senegal, Uruguay, Yemen (from WB EDD); Egypt (Economic Research Forum); China (Chinese Customs Database)
- **Source:** World Bank Exporter Dynamics Database; WB DTA database (257 PTAs, 1958–2015); WTO tariff data (applied MFN and preferential); UN Comtrade (product-level imports for competitor trade shares); CCHS commodity classification for product differentiation
- **Time period:** 1993–2016 (84% of obs from 2000–2006, 98% from 2000–2012)
- **Observations:** 27,549,039 quintuplets total; estimation samples range from ~15M to ~23M depending on specification
- **Key variables:** Firm export value, quantity, unit value (proxy for price), markup (from unit value with firm-origin-product-time FEs), PTA indicator, preferential/MFN tariff, competitor_pta (trade-weighted share of origin's competitors with PTA access), competitor_τ (trade-weighted average tariff on competitors), DTA provision indicators (ROO, competition, mutual recognition of standards/conformity assessment)

### Illustrative dataset structure

| Firm f | HS6 product i | Origin o | Dest. d | Year t | ln(export value) | ln(unit value — price) | ln(markup) | PTA (o,d,t) | Tariff τ (i,o,d,t) | Competitor PTA share |
|---|---|---|---|---|---|---|---|---|---|---|
| F001 | 841781 (Pumps) | Mexico | USA | 2001 | 12.3 | 2.1 | 0.8 | 1 | 0.04 | 0.62 |
| F001 | 841781 (Pumps) | Mexico | Germany | 2001 | 10.8 | 2.3 | 0.9 | 0 | 0.08 | 0.45 |
| F002 | 610910 (T-shirts) | China | Chile | 2005 | 14.1 | 1.5 | 0.2 | 1 | 0.12 | 0.38 |
| F003 | 392690 (Plastics) | Peru | USA | 2010 | 11.5 | 1.9 | 0.4 | 1 | 0.00 | 0.71 |
| F004 | 720839 (Steel) | Bulgaria | Turkey | 2008 | 15.2 | 2.8 | 1.1 | 0 | 0.05 | 0.29 |

*Note: Values illustrative. Markup is estimated from the residual of a unit-value regression absorbing firm-origin-product-time fixed effects. Competitor PTA share is the trade-weighted fraction of competing origins with PTA access to the destination.*

---

## Methodology

The baseline gravity equation for export values:
$$\ln(v_{fodit}) = \beta_1 \cdot \text{pta}_{odt} + \delta_{foit} + \delta_{dit} + \delta_{od} + \varepsilon_{fodit}$$

The main specification adds competitors' trade policies:
$$\ln(v_{fodit}) = \beta_1 \cdot \text{pta}_{odt} + \beta_2 \cdot \text{pta}_{(-o)dit} + \beta_3 \ln(1+\tau_{odit}) + \beta_4 \ln(1+\tau_{(-o)dit}) + \beta_5 \text{prov}_{odt} + \beta_6 \text{prov}_{(-o)dit} + \delta_{foit} + \delta_{dit} + \delta_{od} + \varepsilon$$

For prices and markups the same covariates are used with the log unit value as dependent variable. The markup specification includes firm-origin-product-time FEs ($\delta_{foit}$) to absorb time-varying marginal costs, so the residual variation in the unit value identifies the pricing-to-market (destination-specific markup) component.

**Fixed effects and their role:**
- $\delta_{foit}$ (firm-origin-product-time): absorbs marginal cost shocks and common markup — residuals are destination-specific markup
- $\delta_{dit}$ (destination-product-time): captures multilateral resistance, including price-level changes in the destination market from its own PTAs
- $\delta_{od}$ (origin-destination pair): controls for time-invariant bilateral features and PTA endogeneity

**Competitor variables** are constructed as lagged trade-share-weighted averages of the trade policy faced by each of the firm's competitors in the destination, excluding the firm's own origin. Their construction rules out mechanical endogeneity (policymakers only consider bilateral trade when signing a PTA).

**Estimator:** OLS (PPML infeasible at firm-product level for 13 countries — would require several billion zero observations). The paper acknowledges the resulting omission of the extensive margin and notes that origin-destination FEs absorb much of the selection process generating positive trade flows.

---

## References (Wikilinks)

[[AndersonVanWincoop2003_GravityTrade]]
[[AtkesonBurstein2008_PricingMarkups]]
[[BaierBergstrand2007_RTAsPanelData]]
[[CorsettyCrowleyHanSong2019_PricingToMarket]]
[[DeLoekerGoldbergKhandelwalPavcnik2016_Markups]]
[[HofmannOsnagoRuta2017_DeepTradeAgreements]]
[[MattooRochaRuta2020_HandbookDeepTrade]]
[[Melitz2003_ImpactTrade]]
[[SantosSilvaTenreyro2006_PPML]]
[[CorreiaGuimaraesZylkin2020_PPMLHDFE]]
