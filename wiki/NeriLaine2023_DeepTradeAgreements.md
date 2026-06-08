# Deep Trade Agreements and Heterogeneous Firms Exports

**Neri-Lainé, Orefice & Ruta (2023)**
CESifo Working Paper No. 10436
[Paper Link](https://hdl.handle.net/10419/279185)

---

## Abstract

This paper studies the effect of regional trade agreements on firms' exports. Using detailed information on the content of trade agreements and firm-level exports for 31 developing countries between 2000 and 2020, the analysis shows that the depth of trade agreements matters for the export performance of firms. Moving from shallow to deep trade agreements boosts firms' exports, on average, by 3.6 percent. In line with models of trade with heterogeneous firms and mark-ups, the trade impact of deep trade agreements depends on the firm's characteristics. The impact is stronger for large firms and firms involved in global value chains and is negative for small firms. Robustness tests, an event study approach and an Instrumental Variable strategy confirm the causal interpretation of the results. These heterogeneous impacts on firms' exports imply a selection (pro-competitive) effect of deep trade agreements with significant welfare consequences for signatory countries.

---

## Research Question

Do deeper RTAs — agreements extending beyond tariff cuts to regulate behind-the-border policies — differentially affect the export performance of firms with different characteristics in developing countries? Identification exploits within country-pair variation in RTA depth from newly signed or amended agreements over 2000–2020, with an IV based on the domino effect of RTA formation (Baldwin & Jaimovich, 2012). The baseline estimator is PPML with saturated fixed effects; an event study and a robust DiD à la De Chaisemartin & D'Haultfoeuille (2020) are used to study dynamic effects.

---

## Results

One additional legally enforceable provision in an RTA boosts the average firm's exports by 0.3%, implying a 3.6% increase when moving from a shallow agreement (only tariff provisions) to a deep one (at the 75th percentile of the depth distribution, covering 14 provisions). WTO-extra provisions have a larger per-provision effect (+0.7%). These averages mask strong heterogeneity: large firms (above the 75th size percentile) gain 0.3%–0.6% per additional provision, while small firms lose 0.5%–0.6%, consistent with a pro-competitive selection effect. GVC firms exporting and importing to the same destination benefit the most (+9.6% from moving shallow to deep). The event study confirms a causal interpretation — effects emerge one to two years after the change in RTA depth and then vanish, while pre-trends are flat. Deep RTAs reduce the number of surviving exporters but raise average export value per firm, confirming a selection (reallocation) effect.

---

## Data

The analysis combines four datasets.

The **World Bank Exporter Dynamics Database (EDD)** (Fernandes, Freund & Pierola, 2016) provides customs-based firm-level exports for 55 developing countries from the late 1990s to 2020 (one database per country). Since the EDD is customs-based, it contains no real zeros. The data are originally at firm-destination-HS6-year granularity; the authors aggregate to firm-destination-year by summing across products. Coverage varies across countries: 31 exporting countries have within-variation in RTA depth during 2000–2020 and form the estimation sample. In cases of breaks in firm identifiers, only the post-break period is retained. The EDD is available on request from the World Bank.

The **World Bank Deep Trade Agreements (DTA) database** (Hofmann, Osnago & Ruta, 2017) documents the content of 300+ RTAs notified to the WTO, with provision-level dummies and enforceability information, for the period 2000–2020. A limitation is that the database covers only RTAs active at the time of its construction; inactive RTAs are absent. This is supplemented with the **CEPII RTA database** (which includes both active and inactive agreements) to construct a complete RTA dummy. The DTA database is publicly available at the World Bank data portal.

Gravity controls (distance, common border, language, colonial ties) come from the **CEPII gravity database** (publicly available at [www.cepii.fr](http://www.cepii.fr)). Effective applied tariffs at the firm-destination level are from **MacMap (CEPII)**, available at [www.macmap.org](https://www.macmap.org), and aggregated across products using initial-year export shares as weights.

**Key variables:**
- `Exports (firm f, origin i, destination j, year t)` — total export value in USD, summed across HS6 products
- `DTA_ijt` — RTA depth; five variants: count of all provisions, legally enforceable provisions (baseline), WTO+, WTO-X, and core provisions; mean legally enforceable depth = 8.3 (SD 7.5), range 0–43
- `ln(1+τ_fijt)` — log applied tariff faced by firm f in destination j, weighted by initial export shares across products
- `RTA_ijt` — binary indicator for any RTA in force (from CEPII, used as robustness check only)
- Firm-type indicators: total exports above 75th/90th percentile (current and at entry year t₀); GVC dummy (firm both imports and exports); GVC bilateral dummy (imports and exports to/from same destination j)
- `IV_ijt` — domino-effect instrument: product of average DTA depth of i with other partners in j's macro-region and average DTA depth of j with other partners in i's macro-region (leave-one-out)

**Panel dimensions:** firm × origin × destination × year (unbalanced). 31 exporting countries; 701 country-pairs with time variation in RTA depth; 2000–2020.

**Observations:** Raw EDD covers 29,009,865 observations (firm-destination-product-year). After restricting to 2000–2020 and to countries with within-variation in RTA depth: 4,659,362 firm-destination-year observations with non-missing export and RTA data. The estimation sample drops to 2,924,126 due to missing tariff data, and to **2,388,213** in the baseline regressions.

**Sample restrictions:** Only 31 of 55 EDD countries have within-variation in RTA depth and are retained. The product dimension is dropped by aggregation. The 15 countries providing 87% of RTA depth variation are: Croatia, Georgia, Colombia, Chile, South Africa, Serbia, Slovenia, Peru, Mauritius, Guatemala, Madagascar, Nicaragua, Tanzania, Ecuador, and Malawi.

### Illustrative dataset structure

| Firm f | Origin i | Destination j | Year t | Exports (f, i, j, t) USD | DTA legally enf. (i, j, t) | ln(1+τ) (f, i, j, t) | Firm size > p75 | GVC bilateral |
|---|---|---|---|---|---|---|---|---|
| F001 | Colombia | Chile | 2004 | 0 | 0 | 0.02 | 1 | 0 |
| F001 | Colombia | Chile | 2009 | 128,400 | 12 | 0.01 | 1 | 1 |
| F002 | Croatia | Germany | 2012 | 45,000 | 7 | 0.03 | 0 | 0 |
| F003 | Chile | Peru | 2003 | 0 | 3 | 0.05 | 0 | 0 |
| F003 | Chile | Peru | 2010 | 12,800 | 18 | 0.01 | 0 | 0 |

*Note: Zeros in exports may reflect non-exporting in that year; the EDD is customs-based and does not include true structural zeros.*

---

## Methodology

### Identification strategy

The key source of variation is the change over time in the depth of RTAs between country pairs — from the entry into force of new agreements or the amendment of pre-existing ones. With firm-year, destination-year, and origin-destination fixed effects fully saturating the model, the DTA variable is identified on within-country-pair, within-year variation: it compares a firm's exports to destinations that experienced a change in RTA depth against exports to destinations that did not, conditional on all firm-level and country-level shocks.

The remaining endogeneity concern is that unobserved factors (e.g. large exporters lobbying for deeper agreements) may jointly drive RTA depth and export performance. The authors address this with an **IV based on the domino effect** of RTA formation (Baldwin & Jaimovich, 2012): countries tend to set the depth of new RTAs in response to existing agreements their partners have signed with third countries. The instrument is:

$$IV_{ijt} = \left[\frac{1}{K-1}\sum_{k \neq j} DTA_{ikt}\right] \times \left[\frac{1}{Z-1}\sum_{z \neq i} DTA_{zjt}\right]$$

where the first bracket is the leave-one-out average depth of RTAs signed by exporter i with other partners k ≠ j within j's macro-region, and the second bracket is the analogous average for importer j with partners z ≠ i within i's macro-region. The exclusion restriction rests on the absence of a direct effect of third-country RTA depth on bilateral firm-level exports f→j. Trade diversion is the main threat: if i deepens RTAs with k, it may divert firm f's exports from j to k. The authors argue this channel is absorbed by firm-year and destination-year FEs. A **plausible exogeneity test** (Conley et al., 2012) shows the direct effect of the IV on exports is small and insignificant, and confidence bounds from the deviation-from-exclusion exercise never cross zero.

### Estimators

**Baseline — PPML:** To address the heteroskedasticity problem inherent in log-linearized gravity models and preserve zero-trade observations, the authors adopt the PPML estimator (Santos-Silva & Tenreyro, 2006). Standard errors are clustered by origin-destination-year (the source of variation in DTA).

**Equation (1) — Baseline:**

$$X_{fijt} = \exp\left[\theta_{ft} + \theta_{jt} + \theta_{ij} + \beta_1 DTA_{ijt} + \beta_2 \ln(1+\tau_{fijt})\right] \times \varepsilon_{fijt}$$

where $\theta_{ft}$ are firm-year fixed effects, $\theta_{jt}$ destination-year fixed effects, and $\theta_{ij}$ origin-destination fixed effects. $DTA_{ijt}$ is the depth of the RTA (if any) between i and j at time t.

**Equation (2) — Firm heterogeneity:**

$$X_{fijt} = \exp\left[\theta_{ft} + \theta_{jt} + \theta_{ij} + \beta_1 DTA_{ijt} + \beta_2 \ln(1+\tau_{fijt}) + \beta_3 \left(DTA_{ijt} \times I(k_f > \bar{k})\right)\right] \times \varepsilon_{fijt}$$

where $I(k_f > \bar{k})$ is a binary indicator for firm characteristics (size above 75th/90th percentile, GVC status) exceeding threshold $\bar{k}$. The coefficient $\beta_1$ captures the effect for small/non-GVC firms; $\beta_1 + \beta_3$ gives the effect for large/GVC firms.

**IV implementation:** The 2SLS is estimated in log-linear form; a two-stage OLS/PPML approach (first-stage OLS, second-stage PPML with bootstrapped clustered SE, following Lin & Wooldridge 2019) is used as the main comparable non-linear IV check. The interaction terms in eq. (2) are instrumented by interacting the firm indicators with the domino IV. First-stage F-statistics are well above 10.

**Event study — Equation (4):**

$$X_{fijt} = \exp\left[\theta_f + \theta_{jt} + \theta_{ij} + \sum_{z=-2}^{3}\beta_{0z}I(event_{ijt}=z) + \sum_{z=-2}^{3}\beta_{1z}I(event_{ijt}=z) \times target_{fj}\right] \times \varepsilon_{fijt}$$

where $target_{fj}$ is a dummy for firm-destination pairs that experience a change in RTA depth. The $\beta_{1z}$ coefficients trace the dynamic effect relative to z = 0 (year of first change). Non-targeted varieties are assigned the event date of the earliest depth change in their destination with any partner.

**Robust DiD:** Following De Chaisemartin & D'Haultfoeuille (2020), the authors restrict identification to first-time switchers (firm-destination pairs at their first change in RTA depth) compared to pairs with stable treatment up to t, avoiding negative weighting bias from staggered adoption with heterogeneous treatment.

### Target parameter

The coefficient $\beta_1$ in equation (1) is the semi-elasticity of firm exports to RTA depth, conditional on all firm-level and country-level factors. It measures the percentage increase in firm exports from one additional legally enforceable provision, averaged across all firm types. The interaction coefficient $\beta_3$ measures the differential effect for large/GVC firms relative to the baseline.

### Fixed effects and identifying variation

Firm-year FEs ($\theta_{ft}$) absorb all time-varying firm-specific characteristics — productivity, size, workforce — and, since each firm belongs to one origin country, they subsume all origin-year shocks including the exporter-side multilateral resistance term. Destination-year FEs ($\theta_{jt}$) capture importer-side demand shocks and import-side multilateral resistance. Origin-destination FEs ($\theta_{ij}$) absorb all time-invariant bilateral factors (distance, language, colonial ties), rendering standard gravity controls redundant. Given this saturation, the DTA coefficient is identified purely from the within-country-pair, across-time change in RTA depth.

### Robustness checks

Beyond the IV and event study, the authors: (i) use a weighted count of RTA provisions (weighting by 1 minus provision frequency) as an alternative depth measure; (ii) explicitly control for the RTA binary dummy despite collinearity concerns; (iii) restrict the sample to country-pairs that change RTA status during the period; (iv) test with non-parametric binned interactions of DTA with three size bins; (v) apply the robust TWFE estimator of De Chaisemartin & D'Haultfoeuille (2020) to check for negative weights bias from heterogeneous staggered treatment; (vi) run OLS log-linear regressions as a benchmark (setting log of zero exports to zero).

---

## References (Wikilinks)

[[AtkesonBurstein2008_PricingToMarket]]
[[BaierBergstrand2018_HeterogeneousEIAs]]
[[BaierBergstrand2014_MarginsOfTrade]]
[[BaierYotovZylkin2019_DifferingEffectsFTAs]]
[[BaldwinJaimovich2012_ContagiousFTAs]]
[[BermanMartinMayer2012_ExchangeRateExporters]]
[[BernardJensenReddingSchott2007_FirmsInternationalTrade]]
[[Chaney2008_DistortedGravity]]
[[ConleyHansenRossi2012_PlausiblyExogenous]]
[[CrowleyHanPrayer2022_ProCompetitiveEffects]]
[[DaiYotovZylkin2014_TradeDiversion]]
[[DeChaisemartinDHaultfoeuille2020_TWFE]]
[[FajgelbaumGoldbergKennedyKhandelwal2020_ReturnProtectionism]]
[[FengLiSwenson2016_ImportedInputsExports]]
[[FernandesLefebvreRocha2021_SPSTBTFirmLevel]]
[[FernandesRochaRuta2021_EconomicsDeepTradeAgreements]]
[[FernandesFreundPierola2016_ExporterDynamicsDatabase]]
[[FontagneOreficePiermartiniRocha2015_ProductStandardsMargins]]
[[FreundOrnelas2010_RegionalTradeAgreements]]
[[GabaixIbragimov2011_RankOLS]]
[[GoldbergKhandelwalPavcnikTopalova2010_ImportedIntermediates]]
[[HandleyLimao2017_PolicyUncertaintyTrade]]
[[HeadMayer2014_GravityEquations]]
[[HeadMayerThoenig2014_WelfareTradePareto]]
[[HofmannOsnagoRuta2017_DTADatabase]]
[[HornMavroidisSapir2010_EUUSPTAs]]
[[LeeMulabdicRuta2019_ThirdCountryEffectsRTAs]]
[[Limao2016_PreferentialTradeAgreements]]
[[LinWooldridge2019_EndogeneityNonlinear]]
[[MattooMulabdicRuta2017_TradeCreationDiversionDeep]]
[[MattooRochaRuta2020_HandbookDeepTradeAgreements]]
[[Melitz2003_HeterogeneousFirms]]
[[MelitzOttaviano2008_MarketSizeTradeProductivity]]
[[OreficeRocha2014_DeepIntegrationProductionNetworks]]
[[ReddingWeinstein2019_AggregationGravity]]
[[SantosSilvaTenreyro2006_LogOfGravity]]
[[Spearot2013_VariableDemandElasticities]]
[[vanKippersluis2018_BeyondPlausiblyExogenous]]
