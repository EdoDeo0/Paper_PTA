# Heterogeneous Impacts of SPS and TBT Regulations: Firm-Level Evidence from Deep Trade Agreements

**Fernandes, Ana Margarida, Kevin Lefebvre & Nadia Rocha (2021)**
*World Bank Policy Research Working Paper 9700*
[Paper Link](https://doi.org/10.1596/1813-9450-9700)

---

## Abstract

This paper estimates the impacts of regulating the use of sanitary and phytosanitary and technical barriers to trade measures through preferential trade agreements on exports of firms in Chile, Colombia, and Peru along the firm size spectrum. The analysis exploits novel data from the World Bank Deep Trade Agreements database and customs covering the universe of exporting firms in each country over 1996–2015. The paper uses a firm-product gravity equation with a stringent set of fixed effects and controls for the overall depth of the preferential trade agreements and product-specific bilateral tariffs. The findings show that firms' exports increase significantly in destination markets with preferential trade agreements, including a larger number of sanitary and phytosanitary and technical barriers to trade provisions, and the effect is stronger for smaller firms. Provisions for the harmonization of sanitary and phytosanitary regulations in preferential trade agreements also have greater benefits for the exports of smaller firms, and so do preferential trade agreements, including stronger transparency provisions for sanitary and phytosanitary and technical barriers to trade regulations. The results are robust to dropping larger exporters and highly concentrated export sectors to address endogeneity. The benefits of sanitary and phytosanitary and technical barriers to trade provisions are mainly driven by sectors with more heavily-regulated products. Entry into new product markets and increases in export quality partly explain the rising exports of smaller firms. Finally, the estimated impacts are similar regardless of the income level of the preferential trade agreement partners.

---

## Research Question

Do SPS and TBT provisions in PTAs improve the export performance of firms, and are these effects heterogeneous across the firm size distribution? The variation is provided by the staggered entry into force of ~38 new PTAs signed by Chile, Colombia, and Peru over 1996–2015, which differ in their SPS/TBT provision counts (none to maximum). Methodology: PPML firm-product gravity equation with triple fixed effects (firm-product-destination, firm-product-year) and interaction with firm size terciles.

---

## Results

Essential SPS provisions in PTAs raise average firm exports of agricultural/agro-food products by 23% (normalized from 0 to 1 provision index). This average masks strong size heterogeneity: smaller firms (bottom tercile) gain 45–93% from SPS provisions, while larger firms show no significant effect. For harmonization of SPS specifically, smaller firms gain 44% vs. 20% for larger firms. Transparency provisions drive an 85% export boost for smaller firms with no significant effect for larger firms. TBT provisions show qualitatively similar but less precisely estimated patterns. The main mechanism is expansion of the export product range (extensive margin), partly complemented by quality upgrading. Effects are robust to excluding top exporters and concentrated sectors (addressing PTA content endogeneity), and are similar for North-South vs. South-South PTAs.

---

## Data

The dataset is constructed at the firm–HS 6-digit product–destination–year level, with zeros filled in for all firm-product-destination-year combinations where no export took place, for PPML estimation.

- **Type:** Unbalanced panel (expanded with zeros)
- **Unit of observation:** Firm (f) × HS 6-digit product (k) × destination (j) × year (t)
- **Countries:** Chile (1997–2015), Colombia (1996–2015), Peru (2000–2015)
- **Source:** Exporter Dynamics Database (Fernandes et al. 2016) — universe of exporters; World Bank DTA database (Mattoo et al. 2020); tariff data from Teti (2020); WDI for GDP
- **PTAs covered:** ~38 new PTAs entering into force during the sample period; 269 PTAs with SPS chapters and 263 with TBT chapters in the DTA database globally
- **Key variables:** Export value (FOB, USD), firm initial-period market share in destination × HS6 product (size proxy), SPS provision count (normalized), TBT provision count (normalized), essential provisions dummy, harmonization dummies (SPS/TBT), mutual recognition dummies, transparency provisions, bilateral tariff (Teti 2020 database, HS6), PTA depth in other policy areas (non-SPS/TBT)
- **Exclusions:** HS chapters 25–27 (mining/commodities) to avoid commodity price cycle bias; SPS analysis restricted to HS 01–24 (agricultural/agro-food)

| Variable | Dimension |
|---|---|
| Export value (USD) | firm f × product k × destination j × year t |
| SPS provision count (0–1) | origin i × destination j × year t |
| TBT provision count (0–1) | origin i × destination j × year t |
| Harmonization (SPS) dummy | origin i × destination j × year t |
| Bilateral tariff | product k × origin i × destination j × year t |
| Depth (other provisions) | origin i × destination j × year t |
| Firm size tercile | firm f × product k × destination j (initial year) |

---

## Methodology

**Main equations:**

Equation (1) — average effect:
$$y_{fjkt} = \exp\!\left(\alpha_{fjk} + \alpha_{fkt} + \beta_1 \ln\tau_{ijkt} + \beta_2 \ln\text{GDP}_{jt} + \beta_3 \text{Depth}_{ijt} + \beta_4 \text{Prov}_{ijt}\right) + \varepsilon_{fjkt}$$

Equation (2) — heterogeneous effects by firm size:
$$y_{fjkt} = \exp\!\left(\alpha_{fjk} + \alpha_{fkt} + \gamma_1 \ln\tau_{ijkt} + \gamma_2 \ln\text{GDP}_{jt} + \gamma_3 \text{Depth}_{ijt} + \sum_{s=1}^{3} \gamma_s \cdot \text{Prov}_{ijt} \times \text{Tercile}_{s,fjk}\right) + \varepsilon_{fjkt}$$

The triple fixed effects structure is central: $\alpha_{fjk}$ (firm × destination × product) control for all time-invariant bilateral pair-level factors including endogeneity of PTA signing; $\alpha_{fkt}$ (firm × product × year) absorb all time-varying productivity and demand shocks at the firm-product level. The identifying variation is within-firm-product-destination changes in exports before and after a PTA with SPS/TBT provisions enters into force, relative to destinations without a PTA.

**Estimator:** PPML (`ppmlhdfe`, Correia et al. 2020). Standard errors clustered by origin × destination.

**Endogeneity strategy:** Beyond the triple FEs, the paper explicitly drops the largest exporters (potential lobby pressure on PTA content) and drops the most export-concentrated sectors. Results are also separately estimated for South-South vs. North-South PTAs.

---

## References (Wikilinks)

[[BaierBergstrand2007_RTAsPanelData]]
[[CorreiaGuimaraesZylkin2020_PPMLHDFE]]
[[FernandesFreundPierola2016_ExporterDynamics]]
[[HofmannOsnagoRuta2017_DeepTradeAgreements]]
[[MattooRochaRuta2020_HandbookDeepTrade]]
[[Melitz2003_ImpactTrade]]
[[SantosSilvaTenreyro2006_PPML]]
[[FontagnéOrefice2018_TBTFirms]]
[[Rodrik2018_PoliticalEconomyPTAs]]
