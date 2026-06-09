# The Role of Trade Facilitation Provisions in Trade Agreements: Evidence from Peruvian Firms

**Lee, Woori, Nadia Rocha & Michele Ruta (2021)**
*World Bank Policy Research Working Paper 9674*
[Paper Link](https://hdl.handle.net/10986/35649)

---

## Abstract

This paper examines how trade facilitation (TF) provisions in preferential trade agreements (PTAs) affect the export performance of firms in Peru. Combining transaction-level customs data for Peruvian exporting firms with data on the content of PTAs from the World Bank Deep Trade Agreements (DTA) database, the paper documents that the benefits of PTAs and their TF provisions are concentrated among firms that participate in global value chains (GVC). GVC firms show larger gains in export participation and export value from PTAs in general and from TF provisions in particular. Importantly, the export benefits of TF provisions spill over non-discriminatorily to GVC firms' exports to countries with which Peru has no PTA.

---

## Research Question

Do trade facilitation provisions in PTAs help Peruvian firms export more, and are the benefits heterogeneous across GVC and non-GVC firms? The source of variation is the staggered entry into force of Peru's PTAs over 2000–2017. The methodology combines OLS and PPML gravity models estimated at the firm–year–destination level, with firm-year and destination fixed effects.

---

## Results

PTAs increase average firm export participation by 0.3%. GVC firms benefit significantly more: a PTA raises their export participation probability by 1.8%, rising to 6.3% for bilateral GVC firms (those importing inputs from the PTA partner). When TF provisions are specifically included, bilateral GVC firms see a 9.9% increase in participation and a 19.8% gain in export values with the deepest TF provisions. The intensive margin response is similarly concentrated among GVC firms. Critically, TF benefits operate primarily through improvements at Peru's own border (the import channel), not the partner's border — consistent with GVC firms' sensitivity to upstream input costs. Non-discriminatory spillovers are confirmed: GVC firms importing from non-PTA countries also benefit, suggesting TF reduces fixed trade costs broadly.

---

## Data

The dataset is a large unbalanced panel at the firm–destination–year level (with zeros for non-exporting spells).

- **Type:** Unbalanced panel
- **Unit of observation:** Firm (f) × destination country (j) × year (t)
- **Source:** Peru Exporter Dynamics Database (World Bank EDD); World Bank DTA database (Hofmann, Osnago & Ruta 2017); WDI for GDP
- **Time period:** 2000–2017
- **Coverage:** 15,484 exporting firms, 147 destination countries; ~40 million observations (including zero spells)
- **Key variables:** Export participation dummy, export value, GVC status (firm imports inputs), bilateral GVC (imports from the PTA partner), TF provision count (0–1 normalized; 15–34 provisions across Peru's PTAs), RTA dummy, PTA depth

### Illustrative dataset structure

| Firm f | Destination j | Year t | Export partic. (f,j,t) | Export value USD | PTA (Peru,j,t) | TF provisions (0–1) | GVC firm (f,t) | Bilateral GVC (f,j,t) | ln(GDP_j) |
|---|---|---|---|---|---|---|---|---|---|
| F001 | USA | 2005 | 1 | 45,200 | 1 | 0.82 | 0 | 0 | 30.2 |
| F001 | Chile | 2010 | 1 | 12,300 | 1 | 0.91 | 0 | 0 | 26.4 |
| F002 | China | 2008 | 0 | 0 | 0 | 0.00 | 1 | 0 | 29.6 |
| F003 | Canada | 2012 | 1 | 8,900 | 1 | 0.74 | 1 | 1 | 29.8 |
| F004 | Brazil | 2003 | 0 | 0 | 0 | 0.00 | 0 | 0 | 28.1 |

*Note: Values illustrative. GVC firm = 1 if the firm imports inputs in that year. Bilateral GVC = 1 if the firm imports from the PTA partner country. Zero export value = non-exporting spell (included via zero-inflated data construction).*

---

## Methodology

The main estimation equations are:

**PTA heterogeneity (eq. 1):**
$$y_{ijt} = \beta_1 \cdot \text{PTA}_{jt} + \beta_2 \cdot (\text{PTA}_{jt} \times \text{GVC}_i) + \beta_3 \cdot \ln(\text{GDP}_{jt}) + \alpha_{it} + \alpha_j + \varepsilon_{ijt}$$

**TF provisions (eq. 2):**
$$y_{ijt} = \beta_1 \cdot \text{RTA}_{jt} + \beta_2 \cdot \text{TF}_{jt} + \beta_3 \cdot (\text{TF}_{jt} \times \text{GVC}_i) + \beta_4 \cdot \text{Depth}_{jt} + \beta_5 \cdot \ln(\text{GDP}_{jt}) + \alpha_{it} + \alpha_j + \varepsilon_{ijt}$$

The firm-year fixed effects ($\alpha_{it}$) absorb all time-varying firm-level productivity shocks and remove the selection process determining which firms survive. Destination fixed effects ($\alpha_j$) absorb time-invariant destination characteristics including geography. The key identification assumption is that the timing of PTAs' entry into force is uncorrelated with unobserved firm-level export shocks conditional on these FEs. Both OLS and PPML are estimated; PPML accounts for the large share of zeros in the data (extensive margin). Endogeneity of PTA signing is addressed partly through the fixed effects structure and robustness checks excluding specific bilateral pairs.

**Target parameter:** Average treatment effect of PTA and TF provisions on firm-level export participation and value, separately for GVC and non-GVC firms.

---

## References (Wikilinks)

[[BaierBergstrand2007_RTAsPanelData]]
[[FernandesFreundPierola2016_ExporterDynamics]]
[[HofmannOsnagoRuta2017_DeepTradeAgreements]]
[[MattooRochaRuta2020_HandbookDeepTrade]]
[[Melitz2003_ImpactTrade]]
